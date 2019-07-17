#ifndef SERVICE_H
#define SERVICE_H

#include <string>
#include <vector>

#include "json.hpp"

using namespace std;
using namespace nlohmann;

namespace May
{
    class Service
    {
    public:
        Service(
            const string& id,
            const string& name,
            const string& address,
            const int& port,
            const vector<string>& tags);
        Service(vector<uint8_t> bson);
        ~Service();

        const string
        GetID();
        const string
        GetName();
        const string
        GetAddress();
        const int
        GetPort();
        const vector<string>
        GetTags();

        vector<uint8_t>
        GetServiceJsonBinary();

    private:
        string m_id;
        string m_name;
        string m_address;
        int m_port;
        vector<string> m_tags;
        json m_json;
    };

    inline
    Service::~Service()
    {
        // empty
    }

    inline
    const string
    Service::GetID()
    {
        return m_id;
    }

    inline
    const string
    Service::GetName()
    {
        return m_name;
    }

    inline
    const string
    Service::GetAddress()
    {
        return m_address;
    }

    inline
    const int
    Service::GetPort()
    {
        return m_port;
    }

    inline
    const vector<string>
    Service::GetTags()
    {
        return m_tags;
    }

}

#endif