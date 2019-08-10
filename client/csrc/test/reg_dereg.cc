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

    // unique_ptr<May::EnvoyTcp> envoy = unique_ptr<May::EnvoyTcp>(new May::EnvoyTcp(table.get()));
    May::EnvoyTcp* envoy = new May::EnvoyTcp(table.get());

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

    if (envoy)
        delete envoy;

    return 0;
}