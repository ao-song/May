#include "Buffer.h"

using namespace May;

Buffer::Buffer(size_t size)
: m_size(size),
  m_data(nullptr)
{
    m_data = new unsigned char[size];
    memset(m_data, 0, size);
}

Buffer::Buffer(
    unsigned char* data,
    size_t size)
: m_size(size),
  m_data(nullptr)
{
    m_data = new unsigned char[size];
    memcpy(m_data, data, size);
}

Buffer::Buffer(string& str)
{
    m_size = str.size();
    m_data = new unsigned char[m_size];
    memcpy(m_data, str.c_str(), m_size);
}

Buffer::~Buffer()
{
    if (m_data)
    {
        delete [] m_data;
        m_data = nullptr;
    }
}