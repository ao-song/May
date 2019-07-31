#ifndef ENVOY_TCP_H
#define ENVOY_TCP_H

#include <string>
#include <list>
#include <deque>
#include <memory>
#include <unordered_map>
#include <functional>

#include <sys/socket.h>
#include <netinet/in.h>

#include "Envoy.h"
#include "TcpClient.h"
#include "TcpClientOwner.h"
#include "Buffer.h"

using namespace std;

#define DEFAULT_CLIENT_ADDR "127.0.0.1"
#define DEFAULT_CLIENT_PORT 8080
#define SOCKET_NOT_SET -1

namespace May
{
    class EnvoyTcp: public Envoy, public TcpClientOwner
    {
    public:
        EnvoyTcp(EventHandlerTable* table);
        EnvoyTcp(
            string addr,
            int port,
            EventHandlerTable* table);                  
        ~EnvoyTcp();        

        Action Register(Service* service);
        Action Register(
            Service* service,
            function<void(unsigned char*)> callback);
        Action Deregister(string* service_id);
        Action Deregister(
            string* service_id,
            function<void(unsigned char*)> callback);
        Action Watch(Service* service);
        Action Watch(
            Service* service,
            function<void(unsigned char*)> callback);

        void SetCallback(
            Event event,
            function<void(unsigned char*)> callback);

        virtual
        void HandleEventErr(TcpClient* client);
        // read/write event coming
        virtual
        void HandleEventResult(
            TcpClient* client,
            EventType  events);
    
    private:
        void HandleReceivedData();
        Action Send(Buffer* buff);

    private:
        string m_addr_str;
        int m_port;
        list<Buffer> m_recv_buffer;
        deque<Buffer> m_send_buffer;
        size_t m_recv_bytes;
        EventHandlerTable* m_table;
        unique_ptr<TcpClient> m_tcp_client;
        unordered_map<Event, function<void(unsigned char*)>> m_callbacks;
    };
}

#endif