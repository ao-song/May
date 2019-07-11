#include "EventHandler.h"

using namespace May;

EventHandlerTable::~EventHandlerTable()
{
    if (m_epfd != FD_NOT_SET)
    {
        close(m_epfd);
        m_epfd = FD_NOT_SET;
    }

    if (m_events)
    {
        delete [] m_events;
    }
}

bool
EventHandlerTable::Init()
{
    m_epfd = epoll_create(EPOLL_SIZE);

    if (m_epfd == FD_NOT_SET)
    {
        return false;
    }

    m_events = new struct epoll_event[MAX_EPOLL_EVENTS];
    memset(m_events, 0, sizeof(struct epoll_event[MAX_EPOLL_EVENTS]));

    return true;
}

bool
EventHandlerTable::AddEvent(struct epoll_event* event)
{
    if (epoll_ctl(m_epfd, EPOLL_CTL_ADD, event->data.fd, event) == 0)
    {
        return true;
    }
    else
    {
        return false;
    } 
}

bool
EventHandlerTable::ModifyEvent(struct epoll_event* event)
{
    if (epoll_ctl(m_epfd, EPOLL_CTL_MOD, event->data.fd, event) == 0)
    {
        return true;
    }
    else
    {
        return false;
    }
}


bool
EventHandlerTable::DeleteEvent(struct epoll_event* event)
{
    if (epoll_ctl(m_epfd, EPOLL_CTL_DEL, event->data.fd, event) == 0)
    {
        return true;
    }
    else
    {
        return false;
    }
}

void
EventHandlerTable::HandleEvents()
{
    int num = epoll_wait(m_epfd,
                         m_events,
                         MAX_EPOLL_EVENTS,
                         EPOLL_TIMEOUT);

    for (int i = 0; i < num; ++i)
    {
        EventHandler* handler = 
            static_cast<EventHandler*>(m_events[i].data.ptr);
        int fd = m_events[i].data.fd;

        assert(handler != 0);
        if (handler->HandleEvent(m_events[i].events,
                                 fd) == false)
        {
            handler->Close();
        }
   }   
}