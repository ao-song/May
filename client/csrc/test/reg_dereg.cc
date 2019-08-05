#include <memory>
#include <cassert>
#include <thread>

#include "EventHandlerTable.h"
#include "EnvoyTcp.h"

using namespace std;

class EventHandlerTable;

int main()
{
    unique_ptr<May::EventHandlerTable> table = unique_ptr<May::EventHandlerTable>(new May::EventHandlerTable());
    assert(table->Init() == true);

    unique_ptr<May::EnvoyTcp> envoy = unique_ptr<May::EnvoyTcp>(new May::EnvoyTcp(table.get()));
}