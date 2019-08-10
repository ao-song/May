// Copyright 2019 Ao Song <ao.song@outlook.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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