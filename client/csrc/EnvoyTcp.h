#ifndef ENVOY_TCP_H
#define ENVOY_TCP_H

#include <string>
#include <list>
#include <memory>

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

        int Register(Service* service);
        int Deregister(const string* service_id);
        int Watch(Service* service);

        virtual
        void HandleEventErr(TcpClient* client);
        // read/write event coming
        virtual
        void HandleEventResult(
            TcpClient* client,
            EventType  events);

    private:
        string m_addr_str;
        int m_port;
        list<Buffer> m_buffer_list;
        EventHandlerTable* m_table;
        unique_ptr<TcpClient> m_tcp_client;
    };
}

#endif