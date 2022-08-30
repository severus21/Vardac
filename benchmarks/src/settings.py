import os
from pathlib import Path
from execo import Host

RUN_TIMEOUT = 30 # s


#setup remote
# on host: add ssg_user in docker group:  ``sudo usermod -aG docker ssh_user`` + activate change group: ``newgrp docker`` 
#ssh://severus@dev1.laurentprosperi.info

# debug with cli:
# docker context create --docker host=ssh://severus@dev1.laurentprosperi.info dev1
# docker context use dev1
# docker info
DEFAULT_DOCKER_REMOTE = None
#DEFAULT_DOCKER_REMOTE = "tcp://severus@dev.laurentprosperi.info:2376"
#DEFAULT_HOST = Host("dev.laurentprosperi.info", user="severus",
#                    port=22, keyfile="~/.ssh/id_rsa.pub")
#DEFAULT_DOCKER_REMOTE = "tcp://severus@dev1.laurentprosperi.info:2376"
#DEFAULT_HOST = Host("dev1.laurentprosperi.info", user="severus",
#                    port=2221, keyfile="~/.ssh/id_rsa.pub")


DOCKER_CERT_PATH = Path(Path(__file__).parent.parent/"infra"/"certs")

if DEFAULT_DOCKER_REMOTE:
    import os
    os.environ["DOCKER_TLS_VERIFY"] = "1"
    os.environ["DOCKER_CERT_PATH"] = str(DOCKER_CERT_PATH)

CLOC_DEFINITIONS = Path(__file__).parent.parent / "cloc_varda_definitions.txt"
COUNTSDIR = Path(__file__).parent.parent /'counts'

assert(os.path.isfile(CLOC_DEFINITIONS))
assert(os.path.isdir(COUNTSDIR))
