#include <memory>
#include <cassert>
#include <thread>
#include <chrono>
#include <iostream>

#include "EventHandlerTable.h"
#include "EnvoyTcp.h"
#include "Service.h"
#include "TcpClientOwner.h"

using namespace std;

void event_loop(May::EventHandlerTable* table)
{
    while (true)
    {
        table->HandleEvents();
    }
}

void callback(unsigned char* data)
{
    cout << data << endl;
}

int main()
{
    unique_ptr<May::EventHandlerTable> table = unique_ptr<May::EventHandlerTable>(new May::EventHandlerTable());
    assert(table->Init() == true);

    unique_ptr<May::EnvoyTcp> envoy = unique_ptr<May::EnvoyTcp>(new May::EnvoyTcp(table.get()));

    thread t(event_loop, table.get());

    May::Service service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});
    envoy->Register(&service, callback);

    this_thread::sleep_for(chrono::seconds(1));

    string name = "test";
    envoy->Get(&name);

    string id = "id_001";
    envoy->Deregister(&id);

    this_thread::sleep_for(chrono::seconds(1));

    envoy->Get(&name);

    t.join();

    return 0;
}