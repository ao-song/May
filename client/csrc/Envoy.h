#ifndef ENVOY_H
#define ENVOY_H

#include <functional>
#include "Service.h"

namespace May
{
    class Service;
    
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

        virtual Action Register(Service* service) = 0;
        virtual Action Register(
            Service* service,
            function<void(unsigned char*)> callback) = 0;
        virtual Action Deregister(string* service_id) = 0;
        virtual Action Deregister(
            string* service_id,
            function<void(unsigned char*)> callback) = 0;
        virtual Action Watch(Service* service) = 0;
        virtual Action Watch(
            Service* service,
            function<void(unsigned char*)> callback) = 0;
        virtual Action CancelWatch(const int& watch_id) = 0;
        virtual Action CancelWatch(
            const int& watch_id,
            function<void(unsigned char*)> callback) = 0;
    };
}

#endif