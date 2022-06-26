vardac, a compiler for Varda 
=====================================================================

[![pipeline status](https://gitlab.lip6.fr/lprosperi/Lg4DC/badges/master/pipeline.svg)](https://gitlab.lip6.fr/lprosperi/Lg4DC/-/commits/master)
[![coverage report](https://gitlab.lip6.fr/lprosperi/Lg4DC/badges/master/coverage.svg)](https://gitlab.lip6.fr/lprosperi/Lg4DC/-/commits/master)


[User Manual](XXXX)
-------------


Installation via Opam
---------------------
TODO

Compilation from the sources
----------------------------

You can get the sources using `git` as follows:
```bash
git clone git@gitlab.lip6.fr:lprosperi/compiler.git
```

Dependencies are described in `vardac.opam`. For building the source
code documentation, one needs
[odoc](https://github.com/ocaml/odoc). For building the User Manual,
see `docs/README.md`.

Using Opam, a suitable OCaml environment can be setup as follows (automated script ``requirements.sh``):
```bash
opam switch 4.12.0
./requirements.sh
```

To compile vardac, just run the command `make` in the source directory.
This produces the `_build/install/default/bin/vardac` binary.
Use the `--help` option for more information. Other make targets are:

```bash
make                        # Build vardac 
#make doc                    # Build the user documentation (avalaible on readthedocs)
make odoc                   # Build the developer documentation
make install                # Install vardac 
#make vscode                 # Install vscode extension
make tests
```

**Note:** you can run `vardac` without installing it with `dune exec -- vardac`.

**Note:** the starting file of the source code html documentation is
`_build/default/_doc/_html/vardac/index.html`.

The following commands can be used to clean up the repository:
```bash
make clean     # Removes files generated by OCaml.
make distclean # Same as clean, but also removes library checking files.
```
### Usage (without installing) 

* message-passing with simple varda (only port and continuation) 
```bash
make run -- compile --places examples/00-ping-pong-simpl-varda/places.yml --targets examples/00-ping-pong-simpl-varda/targets.yml --filename examples/00-ping-pong-simpl-varda/pingpong.varch --impl examples/00-ping-pong-simpl-varda/pingpong.vimpl --provenance 0
```
* message-passing encoding
```bash
make run -- compile --places examples/0-ping-pong/places.yml --targets examples/0-ping-pong/targets.yml --filename examples/0-ping-pong/pingpong.varch --impl examples/0-ping-pong/pingpong.vimpl --provenance 0
```
* message-passing with interception 
```bash
make run -- compile --places examples/2-ping-pong-intercept/places.yml --targets examples/2-ping-pong-intercept/targets.yml --filename examples/2-ping-pong-intercept/pingpong.varch --impl examples/2-ping-pong-intercept/pingpong.vimpl --provenance 0
```
* counter
```bash
make run -- compile --places examples/1-counter/places.yml --targets examples/1-counter/targets.yml --filename examples/1-counter/counter.varch --impl examples/1-counter/counter.vimpl --provenance 0
```
* minimal-kv
```bash
make run -- compile --places examples/3-minimal-simple-kv/places.yml --targets examples/3-minimal-simple-kv/targets.yml --filename examples/3-minimal-simple-kv/kv.varch --impl examples/3-minimal-simple-kv/kv.vimpl --provenance 0
```
* s-kv
```bash
make run -- compile --places examples/4-sharded-kv/places.yml --targets examples/4-sharded-kv/targets.yml --filename examples/4-sharded-kv/kv.varch --impl examples/4-sharded-kv/kv.vimpl --provenance 0
```
* kvs
```bash
make run -- compile --places examples/kvs/places.yml --targets examples/kvs/targets.yml --filename examples/kvs/kvs.varch --impl examples/kvs/kvs.vimpl --provenance 0
```

```
make run -- info
```

Compile the akka source code generated by the Lg4DC compiler
```
    cd compiler-build/akka && gradle jarMain
    java -enableassertions -jar build/libs/main.jar
```
```
docker build .
```

Start a compiler with the compiled jar
```
docker-compose up -d
```

## Editors integration
We provide two plugins for vim and vscode.

Manual installation of vim plugin for the current user
```bash
    cd editors/vim && make install
```

Manual installation of vscode plugin for the current user
```bash
    cd editors/vscode

    # generate the package from sources
    vsce package
    code --install-extension aspeclg-0.0.1.vsix
```

# Docker and Docker-Compose

A little tweak/hack is currently needed for the console application to work from outside a docker context.
Add the following to /etc/hosts: `127.0.0.1 host.docker.internal kvs-server-seed`


## build
```docker-compose build```

## run
start all services
```docker-compose up -d```

start a shell in a container that is on the same network as the kvs services 
```docker run -it --rm --network kvs_default kvs-server```

## Vizualisation tools

* Static logic topology
after compilation (that generate the .dot file) run ``make sltopology``

## Akka target

### Requirements
```
------------------------------------------------------------
Gradle 7.1.1
------------------------------------------------------------

Build time:   2021-07-02 12:16:43 UTC
Revision:     774525a055494e0ece39f522ac7ad17498ce032c

Kotlin:       1.4.31
Groovy:       3.0.7
Ant:          Apache Ant(TM) version 1.10.9 compiled on September 27 2020
JVM:          11.0.12 (Debian 11.0.12+7-post-Debian-2)
OS:           Linux 5.10.0-6-amd64 amd64
```

## Compile docs

```
make odoc
open _build/default/_doc/_html/index.html

add private lib
dune build @doc-private
```

## Tests

```
make tests
```

To run a subset of tests e.g. 0:Parser
```
make && dune exec --profile release -- tests/main.exe -list-test
make && dune exec --profile release -- tests/main.exe -only-test 0:Parser
```

### Fuzzing-based tests

1. Setup
    ```bash
    opam switch create 4.12.0+afl --package=ocaml-variants.4.12.0+options,ocaml-option-afl
    opam switch 4.12.0+afl
    eval $(opam env)
    bash requirements.sh
    apt install afl++
    opam install crowbar bun
    ```
2. Run tests
    ```bash
        make fuzz 
        or
        make ci-fuzz or make ci-fuzz-timeout (to find more than one bug at at time)
    ```
    replay crashes
    ```bash
        make replay-fuzz /tmp/findings/XX/crashes/YYY
    ```
    


More about fuzzing

* https://blog.regehr.org/archives/1687
* https://somerandomidiot.com/blog/2017/04/26/crowbar-dhcp/
* https://tarides.com/blog/2019-09-04-an-introduction-to-fuzzing-ocaml-with-afl-crowbar-and-bun/

## Packaging for OPAM
```
    Update the CHANGES.md add new version X.Y.Z
    dune-release tag vX.Y.Z
    make opam-release
    dune-release publish
```

## Register a runner for Gitlab (using docker compose)
```bash
    #https://techoverflow.net/2021/01/12/how-to-install-gitlab-runner-using-docker-compose/
    # executor: docker
    # default image: alpine:3.14
    # tags: empty
```

```yaml
    version: '3'
    services:
        minio:
            image: minio/minio:latest
            expose:
                - "9000"
            # environment:
                # MINIO_ROOT_USER: minioadmin
                # MINIO_ROOT_PASSWORD: minioadmin
            healthcheck:
                test: ["CMD", "curl", "-f", "http://localhost:9000/minio/health/live"]
                interval: 30s
                timeout: 20s
                retries: 3

            volumes:
            - ./minio:/root/.minio
            - ./minio/export:/export
            restart: unless-stopped
            command: server --address 0.0.0.0:9000 /export
        gitlab-runner:
            image: 'gitlab/gitlab-runner:latest'
            volumes:
            - /var/run/docker.sock:/var/run/docker.sock
            - ./config:/etc/gitlab-runner
            restart: unless-stopped
            depends_on: 
            - minio
```

config.toml
```toml
concurrent = 1
check_interval = 0

[session_server]
  session_timeout = 1800

[[runners]]
  name = "PIMS"
  url = "https://gitlab.lip6.fr/"
  token = "<TOKEN>"
  executor = "docker"
  [runners.custom_build_dir]
  [runners.cache]
    Type = "s3"
    Path = "mycustom-s3"
    Shared = true  
    [runners.cache.s3]
      ServerAddress = "minio:9005"
      AccessKey = "minioadmin"
      SecretKey = "minioadmin"
      BucketName = "runner"
      Insecure = true
  [runners.docker]
    tls_verify = false
    image = "alpine:3.14"
    privileged = false
    disable_entrypoint_overwrite = false
    oom_kill_disable = false
    disable_cache = false
    volumes = ["/cache"]
    shm_size = 0
```