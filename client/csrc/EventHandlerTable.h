#ifndef EVENTHANDLERTABLE_H
#define EVENTHANDLERTABLE_H

#include <unistd.h>
#include <cstring>
#include <sys/epoll.h>

#define FD_NOT_SET    -1
// any number bigger than 0 works, use epoll_create rather than epoll_create1
// just because I don't like the name. ^_^
#define EPOLL_SIZE       5000
#define MAX_EPOLL_EVENTS 5 // currently enough..
#define EPOLL_TIMEOUT    0

namespace May
{
    class EventHandlerTable
    {
    public:
        EventHandlerTable()
        : m_epfd(FD_NOT_SET)
        {
            // empty
        }

        virtual ~EventHandlerTable();

        bool Init();
        bool AddEvent(struct epoll_event* event);
        bool ModifyEvent(struct epoll_event* event);
        bool DeleteEvent(struct epoll_event* event);

    private:
        int m_epfd;
        struct epoll_event* m_events;
    };
}

#endif