#include "Buffer.h"

using namespace May;

Buffer::Buffer(size_t size)
: m_size(size)  
{
    m_data = shared_ptr<unsigned char>(new unsigned char[size],
        [](unsigned char* data)
        {
            delete[] data;
        }); 
}

Buffer::Buffer(
    unsigned char* data,
    size_t size)
: m_size(size) 
{
    m_data = shared_ptr<unsigned char>(new unsigned char[size],
        [](unsigned char* data)
        {
            delete[] data;
        });
    memcpy(m_data.get(), data, size);
}

Buffer::Buffer(string& str)
{
    m_size = str.size();
    m_data = shared_ptr<unsigned char>(new unsigned char[m_size],
        [](unsigned char* data)
        {
            delete[] data;
        });
    memcpy(m_data.get(), str.c_str(), m_size);
}

Buffer::~Buffer()
{
    // empty
}