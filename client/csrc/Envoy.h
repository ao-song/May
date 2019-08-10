// Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
// Distributed under the Apache License, Version 2.0 (https://www.apache.org/licenses/LICENSE-2.0)

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