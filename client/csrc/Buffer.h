#ifndef BUFFER_H
#define BUFFER_H

#include <cstring>
#include <vector>
#include <cstdint>
#include <string>
#include <memory>

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
        Buffer(string& str);
        ~Buffer();
        size_t
        GetSize()
        {
            return m_size;
        }
        unsigned char*
        GetData()
        {
            return m_data.get();
        }
    private:
        shared_ptr<unsigned char> m_data;
        size_t m_size;     
    };
}

#endif