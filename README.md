Compspec, a compiler for Varda 
=====================================================================

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

Dependencies are described in `compspec.opam`. For building the source
code documentation, one needs
[odoc](https://github.com/ocaml/odoc). For building the User Manual,
see `docs/README.md`.

Using Opam, a suitable OCaml environment can be setup as follows (automated script ``requirements.sh``):
```bash
opam switch 4.12.0
opam install yaml ounit2 process rresult pprint ppx_deriving menhir bos fileutils
```

To compile Compspec, just run the command `make` in the source directory.
This produces the `_build/install/default/bin/compspec` binary.
Use the `--help` option for more information. Other make targets are:

```bash
make                        # Build compspec 
#make doc                    # Build the user documentation (avalaible on readthedocs)
make odoc                   # Build the developer documentation
make install                # Install compspec 
#make vscode                 # Install vscode extension
make tests
```

**Note:** you can run `compspec` without installing it with `dune exec -- compspec`.

**Note:** the starting file of the source code html documentation is
`_build/default/_doc/_html/compspec/index.html`.

The following commands can be used to clean up the repository:
```bash
make clean     # Removes files generated by OCaml.
make distclean # Same as clean, but also removes library checking files.
```
### Usage (without installing) 

* message-passing with simple varda (only port and continuation) 
```bash
make run -- compile --places examples/00-ping-pong-simpl-varda/places.yml --targets examples/00-ping-pong-simpl-varda/targets.yml --filename examples/00-ping-pong-simpl-varda/pingpong.spec --impl examples/00-ping-pong-simpl-varda/pingpong.impl --provenance 0
```
* message-passing encoding
```bash
make run -- compile --places examples/0-ping-pong/places.yml --targets examples/0-ping-pong/targets.yml --filename examples/0-ping-pong/pingpong.spec --impl examples/0-ping-pong/pingpong.impl --provenance 0
```
* message-passing with interception 
```bash
make run -- compile --places examples/2-ping-pong-intercept/places.yml --targets examples/2-ping-pong-intercept/targets.yml --filename examples/2-ping-pong-intercept/pingpong.spec --impl examples/2-ping-pong-intercept/pingpong.impl --provenance 0
```
* counter
```bash
make run -- compile --places examples/1-counter/places.yml --targets examples/1-counter/targets.yml --filename examples/1-counter/counter.spec --impl examples/1-counter/counter.impl --provenance 0
```
* minimal-kv
```bash
make run -- compile --places examples/3-minimal-simple-kv/places.yml --targets examples/3-minimal-simple-kv/targets.yml --filename examples/3-minimal-simple-kv/kv.spec --impl examples/3-minimal-simple-kv/kv.impl --provenance 0
```
* s-kv
```bash
make run -- compile --places examples/4-sharded-kv/places.yml --targets examples/4-sharded-kv/targets.yml --filename examples/4-sharded-kv/kv.spec --impl examples/4-sharded-kv/kv.impl --provenance 0
```
* kvs
```bash
make run -- compile --places examples/kvs/places.yml --targets examples/kvs/targets.yml --filename examples/kvs/kvs.spec --impl examples/kvs/kvs.impl --provenance 0
```

```
make run -- info
```

Compile the akka source code generated by the Lg4DC compiler
```
    cd compiler-build/akka && gradle jarMain
    java -jar build/libs/main.jar
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
make && dune exec --profile release -- tests/main.exe -only-test 0:Parser
```

## Packaging for OPAM
```
    Update the CHANGES.md add new version X.Y.Z
    dune-release tag vX.Y.Z
    make opam-release
    dune-release publish
```
