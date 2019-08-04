# Set compiler
set(CMAKE_CXX_COMPILER /usr/bin/g++)
set(CMAKE_CXX_STANDARD 11)

# Turn on warnings
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -mcrc32 -Wno-literal-suffix")

set(MAY_SOURCES
   Buffer.cc
   EnvoyTcp.cc
   EventHandler.cc
   EventHandlerTable.cc
   Service.cc
   TcpClient.cc
)

add_library(may SHARED ${MAY_SOURCES})
target_include_directories(may PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../3pp/json/)