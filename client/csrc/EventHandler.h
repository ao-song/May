#ifndef EVENTHANDLER_H
#define EVENTHANDLER_H

#include "EventHandlerTable.h"

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

        virtual bool HandleEvent() = 0;

    private:
        EventHandlerTable*  m_table;
    };
}

#endif