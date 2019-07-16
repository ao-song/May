#include <algorithm>
#include <ctype.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>

#include "EnvoyTcp.h"

using namespace std;
using namespace May;

EnvoyTcp::EnvoyTcp() : m_addr_str(DEFAULT_CLIENT_ADDR),
                       m_port(DEFAULT_CLIENT_PORT)
{
    // empty
}

EnvoyTcp::EnvoyTcp(
    string addr,
    int port)
:   m_addr_str(addr),
    m_port(port)
{
    // empty
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