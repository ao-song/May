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
    m_tcp_client = make_unique<EventHandler>(new TcpClient(
        DEFAULT_CLIENT_ADDR,
        DEFAULT_CLIENT_PORT,
        table,
        this));
}

EnvoyTcp::EnvoyTcp(
    string addr,
    int port,
    EventHandlerTable* table)
:   m_addr_str(addr),
    m_port(port),
    m_table(table)
{
    m_tcp_client = make_unique<EventHandler>(new TcpClient(
        DEFAULT_CLIENT_ADDR,
        DEFAULT_CLIENT_PORT,
        table,
        this));
}

EnvoyTcp::~EnvoyTcp()
{
    // empty
}

void
EnvoyTcp::HandleEventErr(TcpClient* client)
{
    client->Close();
}

void
EnvoyTcp::HandleEventResult(
    TcpClient* client,
    EventType  events)
{
    if (events & EPOLLIN)
    {
        // read
        size_t recv_bytes = 0;
        TcpClient::Action res = client->Receive(&m_buffer_list, recv_bytes);
        switch (res)
        {
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
    }
}

int
EnvoyTcp::Register(Service* service)
{
    vector<uint8_t> bin_vec = service->GetServiceJsonBinary();
    Buffer buff(bin_vec);
    size_t len = buff.GetSize();        
}