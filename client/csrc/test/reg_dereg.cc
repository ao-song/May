#include <memory>
#include <cassert>
// #include <thread>
// #include <chrono>
#include <iostream>

#include "unistd.h" // for sleep

#include "EventHandlerTable.h"
#include "EnvoyTcp.h"
#include "Service.h"

using namespace std;

void callback(unsigned char* data)
{
    cout << "The response is: " << data << endl;
}

int main()
{
    unique_ptr<May::EventHandlerTable> table = unique_ptr<May::EventHandlerTable>(new May::EventHandlerTable());
    assert(table->Init() == true);

    // this_thread::sleep_for(chrono::seconds(1));
    sleep(1);

    table->HandleEvents();

    unique_ptr<May::EnvoyTcp> envoy = unique_ptr<May::EnvoyTcp>(new May::EnvoyTcp(table.get()));

    // this_thread::sleep_for(chrono::seconds(1));
    sleep(1);

    table->HandleEvents();

    May::Service service("id_001", "test", "127.0.0.1", 8080, {"a=1", "b=2"});
    cout << "Service is generated: " << service.GetService() << endl;

    envoy->Register(&service, callback);

    // this_thread::sleep_for(chrono::seconds(1));
    sleep(1);

    table->HandleEvents();

    string name = "test";
    envoy->Get(&name);

    // this_thread::sleep_for(chrono::seconds(1));
    sleep(1);

    table->HandleEvents();

    string id = "id_001";
    envoy->Deregister(&id);

    // this_thread::sleep_for(chrono::seconds(1));
    sleep(1);

    table->HandleEvents();

    envoy->Get(&name);

    // this_thread::sleep_for(chrono::seconds(1));
    sleep(1);

    table->HandleEvents();

    return 0;
}