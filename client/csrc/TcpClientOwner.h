// Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
// Distributed under the Apache License, Version 2.0 (https://www.apache.org/licenses/LICENSE-2.0)

#ifndef TCPCLIENTOWNER_H
#define TCPCLIENTOWNER_H

#include "TcpClient.h"
#include "EventHandler.h"

namespace May
{
    class TcpClient;
    class EventHandler;
    
    class TcpClientOwner
    {
    public:
        TcpClientOwner()
        {
            // empty
        }

        virtual ~TcpClientOwner()
        {
            // empty
        }

        virtual
        void HandleEventErr(EventHandler* client) = 0;
        // read/write event coming
        virtual
        void HandleEventResult(
            EventHandler* client,
            EventType  events) = 0;
    };
}

#endif