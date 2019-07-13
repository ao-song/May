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

        virtual int Register(Service* service) = 0;
        virtual int Deregister(const string* service_id) = 0;
        virtual int Watch(Service* service) = 0;
    };
}

#endif