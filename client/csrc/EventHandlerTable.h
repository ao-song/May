// Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
// Distributed under the Apache License, Version 2.0 (https://www.apache.org/licenses/LICENSE-2.0)

#ifndef EVENTHANDLERTABLE_H
#define EVENTHANDLERTABLE_H

#include <unistd.h>
#include <cstring>
#include <cassert>
#include <sys/epoll.h>

#define FD_NOT_SET    -1
// any number bigger than 0 works, use epoll_create rather than epoll_create1
// just because I don't like the name. ^_^
#define EPOLL_SIZE       5000
#define MAX_EPOLL_EVENTS 5 // currently enough..
#define EPOLL_TIMEOUT    0

typedef struct EpollData
{
    void* ptr;
    int   fd;
} EpollData;


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

        void HandleEvents();

    private:
        int m_epfd;
        struct epoll_event* m_events;
    };
}

#endif