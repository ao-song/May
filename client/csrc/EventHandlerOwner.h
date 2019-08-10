// Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
// Distributed under the Apache License, Version 2.0 (https://www.apache.org/licenses/LICENSE-2.0)

#ifndef EVENTHANDLEROWNER_H
#define EVENTHANDLEROWNER_H

namespace May
{
    class EventHandlerOwner
    {
    public:

        virtual ~EventHandlerOwner()
        {
            // empty
        }
    };
}

#endif