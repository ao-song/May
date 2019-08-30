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

#include <memory>
#include <cassert>
#include <thread>
#include <mutex>
#include <chrono>
#include <iostream>

#include "unistd.h" // for sleep

#include "EventHandlerTable.h"
#include "EnvoyTcp.h"
#include "Service.h"
#include "json.hpp"

using namespace std;

int subscribe_id;

void subscribe_cb(unsigned char* data)
{
    cout << "Call subscribe callback with data: " << data << endl;
    json response = json::parse(data);
    cout << "Json subscribe cb: " << response[0]["response"] << endl;
    if (response[0]["response"] == "subscribed" || response[0]["response"] == "subscribe_updated")
    {
        subscribe_id = response[1]["subscribeID"];
        
        cout << response[0]["response"] << ", subscribe ID is: " << subscribe_id << endl;
    }
    else
    {
        cout << "The response for subscriber is: " << data << endl;
    }
    
}

void doer_cb(unsigned char* data)
{
    cout << "The response for doer is: " << data << endl;
}

int main()
{
    May::EventHandlerTable* table = new May::EventHandlerTable();
    assert(table->Init() == true);

    May::EnvoyTcp* envoy_subscriber = new May::EnvoyTcp(table);
    May::EnvoyTcp* envoy_doer = new May::EnvoyTcp(table);

    this_thread::sleep_for(chrono::seconds(1));

    
    cout << "Start subscriber!" << endl;

    May::Service service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});

    envoy_subscriber->Subscribe(&service, subscribe_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    cout << "Start doing something!" << endl;

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});
    envoy_doer->Register(&service, doer_cb);    

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2", "c=3"});
    envoy_doer->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=0", "b=1"});
    envoy_doer->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    service = May::Service("id_001", "test0", "127.0.0.1", 8080, {"a=1", "b=2", "c=4"});
    envoy_doer->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2", "c=5"});
    envoy_doer->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2", "c=6"});
    envoy_doer->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1"});
    envoy_doer->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();

    envoy_subscriber->Unsubscribe(subscribe_id, subscribe_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();  

    delete table;   
    delete envoy_subscriber;
    delete envoy_doer;

    return 0;
}