# Generate a self signed certificate for the CA along with a key.
mkdir -p ca/private
chmod 700 ca/private
# NOTE: using -nodes, this means that once anybody gets
# their hands on this particular certificate they can become this CA.
openssl req \
    -x509 \
    -nodes \
    -days 365 \
    -newkey rsa:4096 \
    -keyout ca/private/ca.key \
    -out ca/ca.pem \
    -subj "/C=SE/ST=Stockholm/L=Danderyd/O=Ao/CN=ao.com"

# Create server private key and certificate request
mkdir -p server/private
chmod 700 ca/private
openssl genrsa -out server/private/server.key 4096
openssl req -new \
    -key server/private/server.key \
    -out server/server.csr \
    -subj "/C=SE/ST=Stockholm/L=Danderyd/O=Ao/CN=server.ao.com"

# Create client private key and certificate request
mkdir -p client/private
chmod 700 client/private
openssl genrsa -out client/private/client.key 4096
openssl req -new \
    -key client/private/client.key \
    -out client/client.csr \
    -subj "/C=SE/ST=Stockholm/L=Danderyd/O=Ao/CN=client.ao.com"

# Generate certificates
openssl x509 -req -days 365 -in server/server.csr \
    -CA ca/ca.pem -CAkey ca/private/ca.key \
    -CAcreateserial -out server/server.pem
openssl x509 -req -days 365 -in client/client.csr \
    -CA ca/ca.pem -CAkey ca/private/ca.key \
    -CAcreateserial -out client/client.pem

# Now test both the server and the client
# On one shell, run the following
openssl s_server -CAfile ca/ca.pem -cert server/server.pem -key server/private/server.key -Verify 1
# On another shell, run the following
openssl s_client -CAfile ca/ca.pem -cert client/client.pem -key client/private/client.key
# Once the negotiation is complete, any line you type is sent over to the other side.
# By line, I mean some text followed by a keyboar return press.