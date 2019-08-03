#ifndef EVENTHANDLER_H
#define EVENTHANDLER_H

#include "EventHandlerTable.h"

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

        virtual ~EventHandler();

        virtual void HandleEvent(
            EventType events,
            int fd) = 0;
        virtual void Close() = 0;

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