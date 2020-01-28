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