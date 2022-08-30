setup:
	ansible-playbook --connection=ssh --timeout=30 --ask-become-pass --inventory-file=inventory.cfg infra.yml

generate:
	cd certs && rm -f *.pem && ./generate.sh dev1.laurentprosperi.info

test:
	docker --tlsverify --tlscacert=certs/ca.pem --tlscert=certs/cert.pem --tlskey=certs/key.pem -H=dev.laurentprosperi.info:2376 version
	docker --tlsverify --tlscacert=certs/ca.pem --tlscert=certs/cert.pem --tlskey=certs/key.pem -H=dev1.laurentprosperi.info:2376 version