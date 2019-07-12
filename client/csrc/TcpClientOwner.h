#ifndef TCPCLIENTOWNER_H
#define TCPCLIENTOWNER_H

#include "TcpClient.h"

namespace May
{
    class TcpClientOwner
    {
    public:
        virtual ~TcpClientOwner();

        virtual
        void HandleEventErr(TcpClient* client) = 0;
        // read/write event coming
        virtual
        void HandleEventResult(
            TcpClient* client,
            EventType  events) = 0;
    };
}

#endif