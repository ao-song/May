// Copyright 2019 Ao Song <ao.song@outlook.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <algorithm>
#include <ctype.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>

#include "EnvoyTcp.h"

using namespace May;

EnvoyTcp::EnvoyTcp(EventHandlerTable* table)
: m_addr_str(DEFAULT_CLIENT_ADDR),
  m_port(DEFAULT_CLIENT_PORT),
  m_table(table)
{
    m_tcp_client = unique_ptr<TcpClient>(new TcpClient(
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
    m_tcp_client = unique_ptr<TcpClient>(new TcpClient(
        addr,
        port,
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
    function<void(unsigned char*)> callback)
{
    m_callback = callback;
}

void
EnvoyTcp::HandleEventErr(EventHandler* client)
{
    client->Close();
}

void
EnvoyTcp::HandleReceivedData()
{
    unique_ptr<unsigned char> data =
        unique_ptr<unsigned char>(new unsigned char[m_recv_bytes]);
    
    size_t bytes_left = m_recv_bytes;
    size_t position = 0;

    for (auto i : m_recv_buffer)
    {
        size_t sz = (bytes_left > i.GetSize()) ? i.GetSize() : bytes_left;
        memcpy(data.get() + position, i.GetData(), sz);
        position += sz;
        bytes_left -= sz;
    }

    m_callback(data.get());
}

void
EnvoyTcp::HandleEventResult(
    EventHandler* client,
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

Envoy::Action
EnvoyTcp::Send(Buffer* buff)
{
    switch (m_tcp_client->Send(buff->GetData(), buff->GetSize()))
    {
        case TcpClient::CallAgain: case TcpClient::WaitForEvent:
        {
            LOG_WARN("Socket not ready to send now, cache the buffer.");
            m_send_buffer.push_back(*buff);
            return DoItLater;
        }
        case TcpClient::RemoveConnection:
        {
            LOG_ERR("Send failed with action remove connection, TCP closed.");
            m_tcp_client->Close();
            return ConnectionRemoved;
        }
        case TcpClient::JobDone:
        {
            return JobDone;
        }
        default:
            return DoItLater;            
    }
}

Envoy::Action
EnvoyTcp::Register(
    Service* service,
    function<void(unsigned char*)> callback)
{
    this->SetCallback(callback);
    auto ret = this->Register(service);
    return ret;
}

Envoy::Action
EnvoyTcp::Register(Service* service)
{
    service->SetValue("action", "REG");
    string service_str = service->GetService();
    
    Buffer buff(service_str);
    auto ret = this->Send(&buff);     
    return ret; 
}

Envoy::Action
EnvoyTcp::Deregister(
    string* service_id,
    function<void(unsigned char*)> callback)
{
    this->SetCallback(callback);
    auto ret = this->Deregister(service_id);
    return ret;
}

Envoy::Action
EnvoyTcp::Deregister(string* service_id)
{
    unique_ptr<Service> service = unique_ptr<Service>(new Service());
    service->SetValue("action", "DEREG");
    service->SetValue("id", *service_id);
    
    string service_str = service->GetService();
    Buffer buff(service_str);    
    auto ret = this->Send(&buff);  
    return ret;     
}

Envoy::Action
EnvoyTcp::Get(string* service_name)
{
    unique_ptr<Service> service = unique_ptr<Service>(new Service());
    service->SetValue("action", "GET");
    service->SetValue("name", *service_name);
    
    string service_str = service->GetService();
    Buffer buff(service_str);    
    auto ret = this->Send(&buff);  
    return ret; 
}
        
Envoy::Action
EnvoyTcp::Get(
    string* service_name,
    function<void(unsigned char*)> callback)
{
    this->SetCallback(callback);
    auto ret = this->Deregister(service_name);
    return ret;
}

Envoy::Action
EnvoyTcp::Subscribe(
    Service* service,
    function<void(unsigned char*)> callback)
{
    this->SetCallback(callback);
    auto ret = this->Subscribe(service);
    return ret;
}

Envoy::Action
EnvoyTcp::Subscribe(Service* service)
{
    service->SetValue("action", "SUBSCRIBE");
    string service_str = service->GetService();
    Buffer buff(service_str);
    auto ret = this->Send(&buff);   
    return ret;   
}

Envoy::Action 
EnvoyTcp::Unsubscribe(const int& subscribe_id)
{
    unique_ptr<Service> service = unique_ptr<Service>(new Service());
    service->SetValue("action", "CANCELSUBSCRIBE");
    service->SetValue("id", subscribe_id);
    
    string service_str = service->GetService();
    Buffer buff(service_str);    
    auto ret = this->Send(&buff);
    return ret;
}

Envoy::Action
EnvoyTcp::Unsubscribe(
    const int& subscribe_id,
    function<void(unsigned char*)> callback)
{
    this->SetCallback(callback);
    auto ret = this->Unsubscribe(subscribe_id);
    return ret;
}