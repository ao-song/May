#ifndef ENVOY_TCP_H
#define ENVOY_TCP_H

#include <string>
#include <list>
#include <deque>
#include <memory>

#include <sys/socket.h>
#include <netinet/in.h>

#include "Envoy.h"
#include "TcpClient.h"
#include "TcpClientOwner.h"
#include "Buffer.h"
#include "EventHandler.h"

using namespace std;

#define DEFAULT_CLIENT_ADDR "127.0.0.1"
#define DEFAULT_CLIENT_PORT 8080
#define SOCKET_NOT_SET -1

namespace May
{
    class EventHandlerTable;
    class EventHandler;
    class Buffer;
    class TcpClient;

    /*
    // make_unique c++14 impl
    template<typename T, typename... Args>
    std::unique_ptr<T> make_unique(Args&&... args)
    {
        return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
    }
    */

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
        Action Get(string* service_name);
        Action Get(
            string* service_name,
            function<void(unsigned char*)> callback);
        Action Watch(Service* service);
        Action Watch(
            Service* service,
            function<void(unsigned char*)> callback);
        Action CancelWatch(const string& watch_id);
        Action CancelWatch(
            const string& watch_id,
            function<void(unsigned char*)> callback);

        void SetCallback(
            function<void(unsigned char*)> callback);

        virtual
        void HandleEventErr(EventHandler* client);
        // read/write event coming
        virtual
        void HandleEventResult(
            EventHandler* client,
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
        unique_ptr<EventHandler> m_tcp_client;
        function<void(unsigned char*)> m_callback;
    };
}

#endif