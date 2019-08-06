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
        EventHandlerTable*  m_table;
    };

    inline
    EventHandlerTable* EventHandler::GetTable()
    {
        return m_table;
    }
}

#endif