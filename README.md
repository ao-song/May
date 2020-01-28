# May

Client/Server module for service discovery management. Service is defined as tuple ***{id, name, address, port, properties}*** with which can help different nodes find each other and monitor the status change. All TCP connections are with non-blocking IO, the socket fds in client C++ part are monitored by *epoll*.

The Client and Server can be configured to talk in TLS.

![Here is how May looks like](https://github.com/ao-song/May/blob/master/img/may.png)

How to compile:
*rebar3* and *cmake* are used to compile.

## Server
Server is implemented in erlang, accepts TCP connections for handling requests and send notifications.

## Client
Client is divided into two parts, which are implemented in C++ (Envoy) and erlang (Agent). These two parts can be run on either same node or different nodes which is flexible.

### Agent
This is a client which work as a proxy between C++ part client and server. Both sides are TCP connections. It can handle more than one C++ clients and one connection to the server.

### Envoy
This is a client which should work with applications. It provided several interfaces for Service Discovery management.

#### Interfaces
    - Register, register service to server and provide a callback to handle the response;
    - Deregister, de-register service from server and provide a callback to handle the response;
    - Get, get the services which match the request and provide a callback to handle the response;
    - Subscribe, create a subscribe on services change with match condition and provide a callback to handle the response;
    - Unsubscribe, cancel the created subscribe and provide a callback to handle the response;

Todo:
-----    
    - HA server
    - tls between Client erlang node and Envoy
    - Considering more general Service structure definition rather than the existing id, name blabla?
    - Now it is only perfect match maybe a more loose match can be supported?
