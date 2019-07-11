#ifndef EVENTHANDLER_H
#define EVENTHANDLER_H

#include "EventHandlerTable.h"

typedef uint32_t EventType;

namespace May
{
    class EventHandler
    {
    public:
        EventHandler(EventHandlerTable* table)
        : m_table(table)
        {
            // empty
        }

        virtual ~EventHandler();

        virtual bool HandleEvent(
            EventType events,
            int fd) = 0;
        virtual void Close() = 0;

    private:
        EventHandlerTable*  m_table;
    };
}

#endif