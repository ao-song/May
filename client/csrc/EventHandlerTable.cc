#include "EventHandler.h"

#include <iostream> // debug use

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
    if (epoll_ctl(m_epfd, EPOLL_CTL_ADD, static_cast<EpollData*>(event->data.ptr)->fd, event) == 0)
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
    if (epoll_ctl(m_epfd, EPOLL_CTL_MOD, static_cast<EpollData*>(event->data.ptr)->fd, event) == 0)
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
    if (epoll_ctl(m_epfd, EPOLL_CTL_DEL, static_cast<EpollData*>(event->data.ptr)->fd, event) == 0)
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
            static_cast<EventHandler*>(static_cast<EpollData*>(m_events[i].data.ptr)->ptr);
        int fd = static_cast<EpollData*>(m_events[i].data.ptr)->fd;

        assert(handler != 0);

        handler->HandleEvent(m_events[i].events, fd);
    }   
}