#include <algorithm>
#include <ctype.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>

#include "EnvoyTcp.h"

using namespace std;
using namespace May;

EnvoyTcp::EnvoyTcp() : m_addr_str(DEFAULT_CLIENT_ADDR),
                       m_port(DEFAULT_CLIENT_PORT),
                       m_ip_version(None),
                       m_socket(SOCKET_NOT_SET)
{
    // empty
}

EnvoyTcp::EnvoyTcp(
    string addr,
    int port)
:   m_addr_str(addr),
    m_port(port),
    m_ip_version(None),
    m_socket(SOCKET_NOT_SET)
{
    // empty
}

EnvoyTcp::~EnvoyTcp()
{
    CleanSocket();
}

void
EnvoyTcp::CleanSocket()
{
    if (m_socket != SOCKET_NOT_SET)
    {
        close(m_socket);
        m_socket = SOCKET_NOT_SET;
    }
}

bool
EnvoyTcp::SetInetAddr()
{
    // remove spaces in address string
    m_addr_str.erase(
        remove_if(m_addr_str.begin(), m_addr_str.end(), ::isspace),
        m_addr_str.end());
    
    if (inet_pton(AF_INET,
                  m_addr_str.c_str(),
                  &(m_addr_inet.addr_in4.sin_addr)) == 1)
    {
        m_ip_version = IPv4;
        m_addr_inet.addr_in4.sin_family = AF_INET;
        m_addr_inet.addr_in4.sin_port = htons(m_port);
        return true;
    }
    else if (inet_pton(AF_INET6,
                       m_addr_str.c_str(),
                       &(m_addr_inet.addr_in6.sin6_addr)) == 1)
    {
        m_ip_version = IPv6;
        m_addr_inet.addr_in6.sin6_family = AF_INET6;
        m_addr_inet.addr_in6.sin6_port = htons(m_port);
        return true;
    }
    else
    {
        return false;
    }    
}

bool
EnvoyTcp::MakeNonBlocking(int socket)
{
    int flags = fcntl(socket, F_GETFL, 0);
    if (fcntl(socket, F_SETFL, flags | O_NONBLOCK) != 0)
    {
        return false;
    }

    return true;
}

bool
EnvoyTcp::Init()
{
    if (!SetInetAddr())
    {
        // Inet address set failed.
        return false;
    }

    m_socket = socket(m_addr_inet.addrRaw.sa_family,
                      SOCK_STREAM,
                      0);
    if (m_socket == -1)
    {
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
        CleanSocket();
        return false;
    }

    // todo - set recv/snd buffer?

    if (!MakeNonBlocking(m_socket))
    {
        CleanSocket();
        return false;
    }

    if (connect(m_socket,
                &m_addr_inet.addrRaw,
                sizeof(&m_addr_inet.addrRaw)) != 0)
    {
        if (errno != EINPROGRESS)
        {
            CleanSocket();
            return false;
        }
        else
        {
            // epoll part
        }
    }
}