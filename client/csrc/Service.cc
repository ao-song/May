#include "Service.h"

using namespace May;

Service::Service(
    const string& id,
    const string& name,
    const string& address,
    const int& port,
    const vector<string>& tags)
:   m_id(id),
    m_name(name),
    m_address(address),
    m_port(port),
    m_tags(tags)
{
    // empty
}

const bert_byte_t*
Service::GetBert()
{
    
}
