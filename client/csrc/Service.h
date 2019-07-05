#ifndef SERVICE_H
#define SERVICE_H

#include<string>
#include<vector>

#include "bert.h"

using namespace std;

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
        ~Service();

        const string
        get_id();
        const string
        get_name();
        const string
        get_address();
        const int
        get_port();
        const vector<string>
        get_tags();

        const bert_data_t*
        get_bert();

    private:
        string idM;
        string nameM;
        string addressM;
        int portM;
        vector<string> tagsM;
        bert_data_t* bertM;
    };

    inline
    Service::Service()
    {
        bertM = nullptr;
    }

    inline
    Service::~Service()
    {
        // empty
    }

    inline
    const string
    Service::get_id()
    {
        return idM;
    }

    inline
    const string
    Service::get_name()
    {
        return nameM;
    }

    inline
    const string
    Service::get_address()
    {
        return addressM;
    }

    inline
    const int
    Service::get_port()
    {
        return portM;
    }

    inline
    const vector<string>
    Service::get_tags()
    {
        return tagsM;
    }

}

#endif