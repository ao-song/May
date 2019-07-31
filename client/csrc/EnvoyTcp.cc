#include <algorithm>
#include <ctype.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>

#include "EnvoyTcp.h"

using namespace std;
using namespace May;

EnvoyTcp::EnvoyTcp(EventHandlerTable* table)
: m_addr_str(DEFAULT_CLIENT_ADDR),
  m_port(DEFAULT_CLIENT_PORT),
  m_table(table)
{
    m_tcp_client = make_unique<TcpClient>(new TcpClient(
        DEFAULT_CLIENT_ADDR,
        DEFAULT_CLIENT_PORT,
        table,
        this));

    m_tcp_client->Init();
}

EnvoyTcp::EnvoyTcp(
    string addr,
    int port,
    EventHandlerTable* table)
:   m_addr_str(addr),
    m_port(port),
    m_table(table)
{
    m_tcp_client = make_unique<TcpClient>(new TcpClient(
        DEFAULT_CLIENT_ADDR,
        DEFAULT_CLIENT_PORT,
        table,
        this));

    m_tcp_client->Init();
}

EnvoyTcp::~EnvoyTcp()
{
    // empty
}

void 
EnvoyTcp::SetCallback(
    Event event,
    function<void(unsigned char*)> callback)
{
    m_callbacks[event] = callback;
}

void
EnvoyTcp::HandleEventErr(TcpClient* client)
{
    client->Close();
}

void
EnvoyTcp::HandleReceivedData()
{
    unique_ptr<unsigned char> data =
        make_unique<unsigned char>(new unsigned char[m_recv_bytes]);
    
    size_t bytes_left = m_recv_bytes;
    size_t position = 0;

    for (auto i : m_recv_buffer)
    {
        size_t sz = (bytes_left > i.GetSize()) ? i.GetSize() : bytes_left;
        memcpy(data.get() + position, i.GetData(), sz);
        position += sz;
        bytes_left -= sz;
    }

    json result_in_json = json::parse(data.get());
}

void
EnvoyTcp::HandleEventResult(
    TcpClient* client,
    EventType  events)
{
    if (events & EPOLLIN)
    {
        // read
        if (!m_recv_buffer.empty())
        {
            m_recv_buffer.clear();
        }        
        m_recv_bytes = 0;

        TcpClient::Action res = client->Receive(
            &m_recv_buffer, m_recv_bytes);
        switch (res)
        {
            case TcpClient::JobDone:
            {
                HandleReceivedData();
                break;                
            }
            case TcpClient::WaitForEvent:
            {
                break;
            }
            case TcpClient::RemoveConnection:
            {
                client->Close();
                break;
            }
            default:
                break;
        }
    }
    else if (events & EPOLLOUT)
    {
        // write
        if (!m_send_buffer.empty())
        {
            // resend buffered data
            for (auto b : m_send_buffer)
            {
                Send(&b);
                m_send_buffer.pop_front();
            }
        }
    }
}

EnvoyTcp::Action
EnvoyTcp::Send(Buffer* buff)
{
    switch (m_tcp_client->Send(buff->GetData(), buff->GetSize))
    {
        case TcpClient::CallAgain: case TcpClient::WaitForEvent:
        {
            m_send_buffer.push_back(*buff);
            return DoItLater;
        }
        case TcpClient::RemoveConnection:
        {
            m_tcp_client->Close();
            return ConnectionRemoved;
        }
        case TcpClient::JobDone:
        {
            return JobDone;
        }
        default:
            return JobDone;            
    }
}

EnvoyTcp::Action
EnvoyTcp::Register(
    Service* service,
    function<void(unsigned char*)> callback)
{
    this->SetCallback(REG, callback);
    Register(service);
}

EnvoyTcp::Action
EnvoyTcp::Register(Service* service)
{
    service->SetValue("action", "REG");
    string service_str = service->GetService();
    Buffer buff(service_str);
    Send(&buff);      
}

EnvoyTcp::Action
EnvoyTcp::Deregister(
    string* service_id,
    function<void(unsigned char*)> callback)
{
    this->SetCallback(DEREG, callback);
    Deregister(service_id);
}

EnvoyTcp::Action
EnvoyTcp::Deregister(string* service_id)
{
    unique_ptr<Service> service = make_unique<Service>(new Service);
    service->SetValue("action", "DEREG");
    service->SetValue("id", *service_id);
    
    string service_str = service->GetService();
    Buffer buff(service_str);    
    Send(&buff);       
}