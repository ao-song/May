// Copyright 2019 Ao Song <ao.song@outlook.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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