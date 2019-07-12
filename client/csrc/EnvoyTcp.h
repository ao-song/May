#ifndef ENVOY_TCP_H
#define ENVOY_TCP_H

#include <string>

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
        EnvoyTcp();
        EnvoyTcp(
            string addr,
            int port);                  
        ~EnvoyTcp();

        int Register();
        int Deregister();
        int Watch();

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
    };
}

#endif