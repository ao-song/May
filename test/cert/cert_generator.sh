#! /usr/bin/env bash

# Create the CA key and cert for signing server/client certs.
openssl genrsa -out ca.key 4096
openssl req -new -x509 -days 365 -key ca.key -out cacert.pem

# Create the server key and csr.
openssl genrsa -out server.key 4096
openssl req -new -key server.key -out server.csr

# Self sign server cert.
openssl x509 -req -days 365 -in server.csr -CA cacert.pem -CAkey ca.key -out server.pem

# Create the client key and csr.
openssl genrsa -out client.key 4096
openssl req -new -key client.key -out client.csr

# Self sign client cert.
openssl x509 -req -days 365 -in client.csr -CA cacert.pem -CAkey ca.key -out client.pem

# Verify Server Certificate
openssl verify -purpose sslserver -CAfile cacert.pem server.pem

# Verify Client Certificate
openssl verify -purpose sslclient -CAfile cacert.pem client.pem
