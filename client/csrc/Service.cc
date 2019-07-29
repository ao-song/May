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
    m_json["id"] = id;
    m_json["name"] = name;
    m_json["address"] = address;
    m_json["port"] = port;
    m_json["tags"] = json::parse(tags);
}

Service::Service(vector<uint8_t> bson)
{
    m_json = json::from_bson(bson);

    m_id = m_json["id"];
    m_address = m_json["address"];
    m_port = m_json["port"];
    m_name = m_json["name"];
    
    if (m_json["tags"].is_array())
    {
        m_tags.clear();
        for (auto i : m_json["tags"])
        {
            m_tags.emplace_back(i);
        }
    }
}

void
Service::SetValue(string key, string value)
{
    const char* key_str = key.c_str();
    m_json[key_str] = value.c_str();
}

string
Service::GetService()
{
    return m_json.dump();
}

vector<uint8_t>
Service::GetServiceBson()
{
    return json::to_bson(m_json);
}
