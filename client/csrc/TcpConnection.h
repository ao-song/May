#ifndef TCP_CONNECTION_H
#define TCP_CONNECTION_H

#include <string>

#include <sys/socket.h>
#include <netinet/in.h>

#include "EventHandler.h"

using namespace std;

#define SOCKET_NOT_SET -1

namespace May
{
    class TcpConnection : public EventHandler
    {
    public:
        TcpConnection(
            string addr,
            int port);                  
        ~TcpConnection();

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