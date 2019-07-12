#ifndef ENVOY_H
#define ENVOY_H

namespace May
{
    class Envoy
    {
    public:
        Envoy(){}
                  
        virtual ~Envoy()
        {
            // empty
        }

        virtual int Register() = 0;
        virtual int Deregister() = 0;
        virtual int Watch() = 0;
    };
}

#endif