#ifndef TCP_CONNECTION_H
#define TCP_CONNECTION_H

#include <string>
#include <cstdint>
#include <list>
#include <memory>
#include <cassert>

#include <sys/socket.h>
#include <netinet/in.h>

#include "EventHandlerTable.h" // for event data definition
#include "EventHandler.h"
#include "TcpClientOwner.h"
#include "Buffer.h"

using namespace std;

#define SOCKET_NOT_SET -1
#define BUFFER_SIZE 1024

typedef uint32_t EVENT_TYPE;

namespace May
{
    class TcpClientOwner;
    
    class TcpClient : public EventHandler
    {
    public:
        typedef enum
        {
            Idle,
            Connecting,
            Established,
            Listen // not used in client
        } State;        

        TcpClient(
            string ip,
            int port,
            EventHandlerTable* table,
            TcpClientOwner* owner);                  
        ~TcpClient();

        bool Init();

        virtual Action Send(
            const void* data,
            size_t length);
        virtual Action Receive(
            list<Buffer>* buffer,
            size_t& recvlen);
        virtual void Close();

        virtual void HandleEvent(
            EventType events,
            int fd);
        
    private:
        string m_srv_addr_str;
        int m_srv_port;

        union Address
        {
            struct sockaddr_in6 addr_in6;
            struct sockaddr_in  addr_in4;            
            struct sockaddr     addrRaw;
        } m_srv_addr_inet;

        enum {None, IPv4, IPv6} m_ip_version;
        int m_socket;
        struct epoll_event m_event;
        State m_state;
        bool m_flag_set_event;
        TcpClientOwner* m_owner;

        bool
        SetInetAddr();
        bool
        MakeNonBlocking(int socket);
        void
        SetEvent(EVENT_TYPE events);
        void
        ResetEvent();
        bool
        IsConnected();
    };
}

#endif