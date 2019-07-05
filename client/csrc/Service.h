#ifndef SERVICE_H
#define SERVICE_H

#include<string>
#include<vector>

using namespace std;

namespace May
{
    class Service
    {
    public:
        Service();
        ~Service();

    private:
        string idM;
        string nameM;
        string addressM;
        int portM;
        vector<string> tagsM;
    };
}

#endif