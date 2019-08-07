#include <memory>
#include <cassert>
#include <thread>
#include <chrono>
#include <iostream>

#include "unistd.h" // for sleep

#include "EventHandlerTable.h"
#include "EnvoyTcp.h"
#include "Service.h"
#include "json.hpp"

using namespace std;

string watch_id;

void watch_cb(unsigned char* data)
{
    json response = json::parse(data);
    if (response["response"] == "watched" || response["response"] == "watch_updated")
    {
        watch_id = response["watchID"];
        cout << response["response"] << ", watch ID is: " << watch_id << endl;
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

void watcher(May::EventHandlerTable* table)
{
    unique_ptr<May::EnvoyTcp> envoy = unique_ptr<May::EnvoyTcp>(new May::EnvoyTcp(table));

    this_thread::sleep_for(chrono::seconds(1));

    May::Service service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});

    envoy->Watch(&service, watch_cb);

    this_thread::sleep_for(chrono::seconds(6));

    envoy->CancelWatch(watch_id, watch_cb);
}

void doer(May::EventHandlerTable* table)
{
    unique_ptr<May::EnvoyTcp> envoy = unique_ptr<May::EnvoyTcp>(new May::EnvoyTcp(table));

    this_thread::sleep_for(chrono::seconds(1));

    May::Service service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});
    envoy->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2", "c=3"});
    envoy->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=0", "b=1"});
    envoy->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));

    service = May::Service("id_001", "test0", "127.0.0.1", 8080, {"a=1", "b=2", "c=4"});
    envoy->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2", "c=5"});
    envoy->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2", "c=6"});
    envoy->Register(&service, doer_cb);

    this_thread::sleep_for(chrono::seconds(1));

    service = May::Service("id_001", "test", "127.0.0.1", 8080, {"a=1"});
    envoy->Register(&service, doer_cb);
}

int main()
{
    May::EventHandlerTable* table = new May::EventHandlerTable();
    assert(table->Init() == true);

    auto t_watcher = thread(watcher, table);
    auto t_doer = thread(doer, table);

    int c = 0;

    while (c < 10)
    {
        table->HandleEvents();
        this_thread::sleep_for(chrono::microseconds(200));
        c += 1;
    }

    t_doer.join();
    t_watcher.join();

    delete table;   

    return 0;
}