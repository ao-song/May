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

#ifndef BUFFER_H
#define BUFFER_H

#include <cstring>
#include <vector>
#include <cstdint>
#include <string>
#include <memory>

#include "Logger.h"

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

        Buffer(const Buffer&) = default;
        Buffer& operator=(const Buffer&) = default;
        Buffer(Buffer&&) = default;
        Buffer& operator=(Buffer&&) = default;

        ~Buffer();

        size_t
        GetSize() const
        {
            return m_size;
        }

        unsigned char*
        GetData() const
        {
            return m_data.get();
        }
    private:
        shared_ptr<unsigned char> m_data;
        size_t m_size;     
    };
}

#endif