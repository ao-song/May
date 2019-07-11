#ifndef EVENTHANDLER_H
#define EVENTHANDLER_H

#include <unistd.h>
#include <sys/epoll.h>
#include "EventHandlerOwner.h"

#define FD_NOT_SET    -1
// any number bigger than 0 works, use epoll_create rather than epoll_create1
// just because I don't like the name. ^_^
#define EPOLL_SIZE    7
#define EPOLL_TIMEOUT 0

namespace May
{
    class EventHandlerOwner;

    class EventHandler
    {
    public:
        EventHandler(EventHandlerOwner* owner)
        : m_fd(FD_NOT_SET),
          m_owner(owner)
        {
            // empty
        }

        virtual ~EventHandler();

        bool Init();

    private:
        int m_fd;
        EventHandlerOwner* m_owner;
    };
}

#endif