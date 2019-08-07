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

string watch_id;

mutex mtx;

void watch_cb(unsigned char* data)
{
    cout << "Call watch callback with data: " << data << endl;
    json response = json::parse(data);
    if (response["response"].dump() == "watched" || response["response"].dump() == "watch_updated")
    {
        mtx.lock();
        watch_id = response["watchID"].dump();        
        mtx.unlock();
        
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

void watcher(May::EnvoyTcp* envoy)
{
    cout << "Start watcher!" << endl;

    May::Service service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});

    envoy->Watch(&service, watch_cb);

    /*
    mtx.lock();
    envoy->CancelWatch(watch_id, watch_cb);
    mtx.unlock();
    */
}

void doer(May::EnvoyTcp* envoy)
{
    cout << "Start doer!" << endl;

    this_thread::sleep_for(chrono::seconds(3));

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

    May::EnvoyTcp* envoy_watcher = new May::EnvoyTcp(table);
    May::EnvoyTcp* envoy_doer = new May::EnvoyTcp(table);

    mtx.lock();
    auto t_watcher = thread(watcher, envoy_watcher);
    mtx.unlock();
    mtx.lock();
    auto t_doer = thread(doer, envoy_doer);
    mtx.unlock();

    int c = 0;

    while (c < 20)
    {
        table->HandleEvents();
        this_thread::sleep_for(chrono::microseconds(1000));
        c += 1;
    }

    t_doer.join();
    t_watcher.join();

    
    mtx.lock();
    envoy_watcher->CancelWatch(watch_id, watch_cb);
    mtx.unlock();    

    delete table;   
    delete envoy_watcher;
    delete envoy_doer;

    return 0;
}