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
        EventHandlerTable(const EventHandlerTable& other) = delete;
        EventHandlerTable& operator=(const EventHandlerTable& other) = delete;

    private:
        int m_epfd;
        struct epoll_event* m_events;
    };
}

#endif