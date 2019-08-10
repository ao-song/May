// Copyright(c) 2019-present, Ao Song <ao.song@outlook.com>
// Distributed under the Apache License, Version 2.0 (https://www.apache.org/licenses/LICENSE-2.0)

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

int watch_id;

void watch_cb(unsigned char* data)
{
    cout << "Call watch callback with data: " << data << endl;
    json response = json::parse(data);
    cout << "Json watch cb: " << response[0]["response"] << endl;
    if (response[0]["response"] == "watched" || response[0]["response"] == "watch_updated")
    {
        watch_id = response[1]["watchID"];
        
        cout << response[0]["response"] << ", watch ID is: " << watch_id << endl;
    }
    else
    {
        cout << "The response for watcher is: " << data << endl;
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

    May::EnvoyTcp* envoy_watcher = new May::EnvoyTcp(table);
    May::EnvoyTcp* envoy_doer = new May::EnvoyTcp(table);

    this_thread::sleep_for(chrono::seconds(1));

    
    cout << "Start watcher!" << endl;

    May::Service service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});

    envoy_watcher->Watch(&service, watch_cb);

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

    envoy_watcher->CancelWatch(watch_id, watch_cb);

    this_thread::sleep_for(chrono::seconds(1));
    table->HandleEvents();  

    delete table;   
    delete envoy_watcher;
    delete envoy_doer;

    return 0;
}