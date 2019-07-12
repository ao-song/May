#ifndef BUFFER_H
#define BUFFER_H

#include <cstring>

using namespace std;

namespace May
{
    class Buffer
    {        
    public:
        Buffer(size_t size);
        Buffer(
            unsigned char* data,
            size_t size);
        ~Buffer();
        size_t
        BufferSize()
        {
            return m_size;
        }
    private:
        unsigned char* m_data;
        size_t m_size;     
    };
}

#endif