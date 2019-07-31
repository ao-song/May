#ifndef ENVOY_H
#define ENVOY_H

#include "Service.h"

namespace May
{
    class Envoy
    {
    public:
        Envoy(){}
                  
        virtual ~Envoy()
        {
            // empty
        }

        typedef enum
        {
            DoItLater,
            ConnectionRemoved,
            JobDone
        } Action;

        typedef enum
        {
            REG,
            DEREG,
            WATCH
        } Event;

        virtual Action Register(Service* service) = 0;
        virtual Action Deregister(const string* service_id) = 0;
        virtual Action Watch(Service* service) = 0;
    };
}

#endif