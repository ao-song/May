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

#include "Buffer.h"

using namespace May;

Buffer::Buffer(size_t size)
: m_size(size)  
{
    LOG_DEBUG("Init an empty buffer with size: {}", size);
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
    LOG_DEBUG("Init a buffer with data: {} and size: {}", data, size);
    m_data = shared_ptr<unsigned char>(new unsigned char[size],
        [](unsigned char* data)
        {
            delete[] data;
        });
    memcpy(m_data.get(), data, size);
}

Buffer::Buffer(string& str)
{
    LOG_DEBUG("Init a buffer with a string: {}", str);
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