RUN_TIMEOUT = 30 # s


#setup remote
# on host: add ssg_user in docker group:  ``sudo usermod -aG docker ssh_user`` + activate change group: ``newgrp docker`` 
#ssh://severus@dev1.laurentprosperi.info

# debug with cli:
# docker context create --docker host=ssh://severus@dev1.laurentprosperi.info dev1
# docker context use dev1
# docker info
#DEFAULT_DOCKER_REMOTE = None
DEFAULT_DOCKER_REMOTE = "tcp://severus@dev1.laurentprosperi.info:2376"

if DEFAULT_DOCKER_REMOTE:
    import os
    os.environ["DOCKER_TLS_VERIFY"] = "1"
    os.environ["DOCKER_CERT_PATH"] = "benchmarks/infra/certs"
