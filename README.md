# May

Client/Server module for service discovery management. Service is defined as tuple {id, name, address, port, properties} with which can help different nodes find each other and monitor the status change.

Compile tool:
-----
The server and the client erlang part code are organized with rebar3, client C++ part is with cmake.

## Server
Server is implemented in erlang, accepts TCP connections for handling requests and send notifications.

## Client
Client is divided into two parts, which are implemented in C++ and erlang. These two parts can be run on either same node or different nodes which is flexible.

### Client erlang part
This is a client which work as a proxy between C++ part client and server. Both sides are TCP connections. It can handle more than one C++ clients and one connection to the server.

### Client C++ part
This is a client which should work with applications. It provided several interfaces for Service Discovery management.

#### Interfaces
    - Register, register service to server and provide a callback to handle the response;
    - Deregister, de-register service from server and provide a callback to handle the response;
    - Get, get the services which match the request and provide a callback to handle the response;
    - Watch, create a watch on services change with match condition and provide a callback to handle the response;
    - CancelWatch, cancel the created watch and provide a callback to handle the response;

Todo:
-----
    - tls
    - error handling
    - logging
    - HA server
    - Watch notice protocol check