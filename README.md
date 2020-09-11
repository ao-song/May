# May

Client/Server module for service discovery management. Service is defined as tuple ***{id, name, address, port, properties}***. All TCP connections are with non-blocking IO.
The Client and Server can be configured to TLS.

![Here is how May looks like](https://github.com/ao-song/May/blob/master/img/may.png)

How to compile:
*rebar3* and *cmake* are used to compile erlang and c++ code.

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
