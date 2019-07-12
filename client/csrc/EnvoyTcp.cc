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
        // TcpClient::Action res = client->Receive();
    }
    else if (events & EPOLLOUT)
    {
        // write
    }
}