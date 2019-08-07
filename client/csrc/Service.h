#ifndef SERVICE_H
#define SERVICE_H

#include <string>
#include <vector>

#include "json.hpp"

using namespace std;
using json = nlohmann::json;

namespace May
{
    class Service
    {
    public:
        Service();
        Service(
            const string& id,
            const string& name,
            const string& address,
            const int& port,
            const vector<string>& tags);
        Service(vector<uint8_t> bson);
        Service& operator = (const Service &other)
        {
            if (&other == this)
            {
                return *this;
            }

            m_id = other.m_id;
            m_name = other.m_name;
            m_address = other.m_address;
            m_port = other.m_port;
            m_tags = other.m_tags;
            m_json = other.m_json;
            return *this;
        }
        ~Service();

        const string
        GetID();
        const string
        GetName();
        const string
        GetAddress();
        int
        GetPort();
        const vector<string>
        GetTags();

        void
        SetValue(string key, int value)
        {
            const char* key_str = key.c_str();
            m_json[key_str] = value;
        }

        void
        SetValue(string key, string value)
        {
            const char* key_str = key.c_str();
            m_json[key_str] = value.c_str();
        }

        // Get service in json string
        string
        GetService();

        vector<uint8_t>
        GetServiceBson();

    private:
        string m_id;
        string m_name;
        string m_address;
        int m_port;
        vector<string> m_tags;
        json m_json;
    };

    inline
    Service::Service()
    {
        // empty
    }

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
    int
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