#include "Service.h"

using namespace May;

Service::Service(
    const string& id,
    const string& name,
    const string& address,
    const int& port,
    const vector<string>& tags)
:   idM(id),
    nameM(name),
    addressM(address),
    portM(port),
    tagsM(tags)
{
    // empty
}

const bert_data_t*
Service::get_bert()
{
    
}
