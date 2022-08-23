#!/bin/bash

HOST=$1
PASSPHRASE=foo
KEYSIZE=4096
DEFAULT_SUBJ="/C=/ST=/L=/O=/CN="

#https://docs.docker.com/engine/security/protect-access/
openssl genrsa -aes256 -out ca-key.pem --passout pass:$PASSPHRASE $KEYSIZE
openssl req -new -x509 -subj $DEFAULT_SUBJ -days 365 -key ca-key.pem -sha256 -out ca.pem --passin pass:$PASSPHRASE -noenc

openssl genrsa -out server-key.pem  --passout pass:$PASSPHRASE $KEYSIZE
openssl req -subj "/CN=$HOST" -sha256 -new -key server-key.pem -out server.csr --passin pass:$PASSPHRASE

echo subjectAltName = DNS:$HOST,IP:10.10.10.20,IP:127.0.0.1 >> extfile.cnf
echo extendedKeyUsage = serverAuth >> extfile.cnf


openssl x509 -req -days 365 -sha256 -in server.csr -CA ca.pem -CAkey ca-key.pem \
  -CAcreateserial -out server-cert.pem -extfile extfile.cnf  --passin pass:$PASSPHRASE

openssl genrsa -out key.pem --passout pass:$PASSPHRASE $KEYSIZE
openssl req -subj '/CN=client' -new -key key.pem -out client.csr  --passin pass:$PASSPHRASE

echo extendedKeyUsage = clientAuth > extfile-client.cnf

openssl x509 -req -days 365 -sha256 -in client.csr -CA ca.pem -CAkey ca-key.pem \
  -CAcreateserial -out cert.pem -extfile extfile-client.cnf  --passin pass:$PASSPHRASE

rm -v client.csr server.csr extfile.cnf extfile-client.cnf
chmod -v 0400 ca-key.pem key.pem server-key.pem
chmod -v 0444 ca.pem server-cert.pem cert.pem