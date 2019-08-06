#include <algorithm>
#include <ctype.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>

#include "TcpClient.h"

#include <iostream> // debug use

using namespace std;
using namespace May;

TcpClient::TcpClient(
    string ip,
    int port,
    EventHandlerTable* table,
    TcpClientOwner* owner)
:   EventHandler(table),
    m_srv_addr_str(ip),
    m_srv_port(port),
    m_ip_version(None),
    m_socket(SOCKET_NOT_SET),
    m_state(Idle),
    m_flag_set_event(false),
    m_owner(owner)
{
    memset(&m_event, 0, sizeof(struct epoll_event));
}

TcpClient::~TcpClient()
{
    Close();
}

void
TcpClient::Close()
{
    if (m_flag_set_event)
    {
        ResetEvent();
    }

    if (m_socket != SOCKET_NOT_SET)
    {
        close(m_socket);
        m_socket = SOCKET_NOT_SET;
    }
}

bool
TcpClient::SetInetAddr()
{
    // remove spaces in address string
    m_srv_addr_str.erase(
        remove_if(m_srv_addr_str.begin(), m_srv_addr_str.end(), ::isspace),
        m_srv_addr_str.end());
    
    if (inet_pton(AF_INET,
                  m_srv_addr_str.c_str(),
                  &(m_srv_addr_inet.addr_in4.sin_addr)) == 1)
    {
        cout << "SetInetAddr IP is: " << m_srv_addr_str.c_str() << " Port is: " << m_srv_port << endl;
        m_ip_version = IPv4;
        m_srv_addr_inet.addr_in4.sin_family = AF_INET;
        m_srv_addr_inet.addr_in4.sin_port = htons(m_srv_port);
        return true;
    }
    else if (inet_pton(AF_INET6,
                       m_srv_addr_str.c_str(),
                       &(m_srv_addr_inet.addr_in6.sin6_addr)) == 1)
    {
        m_ip_version = IPv6;
        m_srv_addr_inet.addr_in6.sin6_family = AF_INET6;
        m_srv_addr_inet.addr_in6.sin6_port = htons(m_srv_port);
        return true;
    }
    else
    {
        return false;
    }    
}

bool
TcpClient::MakeNonBlocking(int socket)
{
    int flags = fcntl(socket, F_GETFL, 0);
    if (fcntl(socket, F_SETFL, flags | O_NONBLOCK) != 0)
    {
        return false;
    }

    return true;
}

bool
TcpClient::Init()
{
    cout << "Init connection!" << endl;
    if (!SetInetAddr())
    {
        cout << "Set inet addr failed!" << endl;
        // Inet address set failed.
        return false;
    }

    m_socket = socket(m_srv_addr_inet.addrRaw.sa_family,
                      SOCK_STREAM,
                      0);
    if (m_socket == -1)
    {
        cout << "Create socket failed!" << endl;
        // failed to create a socket.
        return false;
    }

    int flag = 1;
    if (setsockopt(m_socket,
                   SOL_SOCKET,
                   SO_REUSEADDR,
                   (const char*)&flag,
                   sizeof(flag)) != 0)
    {
        cout << "Set sockopt failed!" << endl;
        Close();
        return false;
    }

    // todo - set recv/snd buffer?

    if (!MakeNonBlocking(m_socket))
    {
        cout << "Set nonblocking failed!" << endl;
        Close();
        return false;
    }

    if (connect(m_socket,
                &(m_srv_addr_inet.addrRaw),
                sizeof(m_srv_addr_inet.addrRaw)) != 0)
    {
        if (errno != EINPROGRESS)
        {
            cout << "Connect failed! errno is: " << errno << endl;
            Close();
            return false;
        }
        else
        {
            // connection in progress
            cout << "Connect in progress!" << endl;
            m_state = Connecting;
            SetEvent(EPOLLOUT);
            GetTable()->HandleEvents();            
            return true;
        }
    }

    cout << "Connected!" << endl;

    m_state = Established;
    SetEvent(EPOLLIN | EPOLLOUT);
    return true;
}

void
TcpClient::SetEvent(EVENT_TYPE events)
{
    // always set with ET.
    m_event.events = events | EPOLLET;
    EpollData ed;
    ed.fd = m_socket;
    ed.ptr = this;
    m_event.data.ptr = &ed;

    EventHandlerTable* table = GetTable();

    if (m_flag_set_event)
    {        
        table->ModifyEvent(&m_event);
    }
    else
    {
        table->AddEvent(&m_event);
        m_flag_set_event = true;
    }    
}

void
TcpClient::ResetEvent()
{
    EventHandlerTable* table = GetTable();
    table->DeleteEvent(&m_event);

    memset(&m_event, 0, sizeof(struct epoll_event));
    m_flag_set_event = false;
}

bool
TcpClient::IsConnected()
{
    if (m_state == Established)
    {
        return true;
    }

    if (m_state == Connecting)
    {
        SetEvent(EPOLLOUT);
        GetTable()->HandleEvents();
        return (m_state == Established);
    }

    return false;
}

TcpClient::Action
TcpClient::Send(
    const void* data,
    size_t length)
{
    if (!IsConnected() && (m_state == Connecting))
    {
        cout << "Not connected yet!" << endl;
        return CallAgain;
    }

    ssize_t result = send(m_socket, data, length, 0);

    if (result < 0)
    {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
        {
            cout << "Socket not ready yet! Errno is: " << errno << endl;
            return WaitForEvent;
        }
        return RemoveConnection;
    }

    if (static_cast<size_t>(result) == length)
    {
        return JobDone;
    }

    return RemoveConnection;
}

TcpClient::Action
TcpClient::Receive(
    list<Buffer>* buffer_list,
    size_t& recvlen)
{
    unique_ptr<unsigned char> buffer(new unsigned char[BUFFER_SIZE]);
    recvlen = 0;
    ssize_t result = recv(m_socket, buffer.get(), BUFFER_SIZE, 0);

    if (result < 0)
    {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
        {
            return WaitForEvent;
        }
        return RemoveConnection;
    }

    if (result == 0)
    {
        return RemoveConnection;
    }

    while (result > 0)
    {
        recvlen += result;
        buffer_list->emplace_back(buffer.get(), result);
        result = recv(m_socket, buffer.get(), BUFFER_SIZE, 0);
    }
    return JobDone;
}

void
TcpClient::HandleEvent(
    EventType events,
    int fd)
{
    cout << "Assert fd == socket!" << endl;
    assert(fd == m_socket);
    cout << "Assert success!" << endl;

    if (events & (EPOLLHUP | EPOLLERR))
    {
        // Close();
        m_owner->HandleEventErr(this);
    }

    switch (m_state)
    {
        case Connecting:
        {
            if (events & EPOLLOUT)
            {
                m_state = Established;
                SetEvent(EPOLLIN | EPOLLOUT);
            }
            break;
        }
        case Established:
        {
            m_owner->HandleEventResult(this, events);
            break;
        }
    
        default:
            break;
    }
}