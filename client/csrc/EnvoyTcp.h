#ifndef ENVOY_TCP_H
#define ENVOY_TCP_H

#include <string>

#include <sys/socket.h>
#include <netinet/in.h>

#include "Envoy.h"

using namespace std;

#define DEFAULT_CLIENT_ADDR "127.0.0.1"
#define DEFAULT_CLIENT_PORT 8080
#define SOCKET_NOT_SET -1

namespace May
{
    class EnvoyTcp: public Envoy
    {
    public:
        EnvoyTcp();
        EnvoyTcp(
            string addr,
            int port);                  
        ~EnvoyTcp();

        bool Init();

        int Register();
        int Watch();
    private:
        string m_addr_str;
        int m_port;

        union Address
        {
            struct sockaddr_in6 addr_in6;
            struct sockaddr_in  addr_in4;            
            struct sockaddr     addrRaw;
        } m_addr_inet;

        enum {None, IPv4, IPv6} m_ip_version;
        int m_socket;

        bool
        SetInetAddr();
        bool
        MakeNonBlocking(int socket);
        void
        CleanSocket();
    };
}

#endif