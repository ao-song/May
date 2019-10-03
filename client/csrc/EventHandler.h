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

#ifndef EVENTHANDLER_H
#define EVENTHANDLER_H

#include <list>

#include "EventHandlerTable.h"
#include "Buffer.h"

typedef uint32_t EventType;

namespace May
{
    class EventHandlerTable;
    
    class EventHandler
    {
    public:
        EventHandler(EventHandlerTable* table)
        : m_table(table)
        {
            // empty
        }

        typedef enum
        {
            JobDone,
            CallAgain,
            WaitForEvent,
            RemoveConnection
        } Action;

        virtual ~EventHandler();

        virtual void HandleEvent(
            EventType events,
            int fd) = 0;
        virtual void Close() = 0;

        virtual bool Init() = 0;

        virtual Action Send(
            const void* data,
            size_t length) = 0;
        virtual Action Receive(
            list<Buffer>* buffer,
            size_t& recvlen) = 0;

        EventHandlerTable* GetTable();

    private:
        EventHandler(const EventHandler& other);
        EventHandler& operator=(const EventHandler& other);

        EventHandlerTable*  m_table;
    };

    inline
    EventHandlerTable* EventHandler::GetTable()
    {
        return m_table;
    }
}

#endif