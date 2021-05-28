#TODO incorporate Makefile from https://github.com/Deducteam/lambdapi
NAME = compspec

#### Compilation (binary, library and documentation) #########################
.PHONY: all 
all: bin

.PHONY: bin
bin:
	#dune build @all --profile release
	dune build -p $(NAME)

.PHONY: odoc
odoc: 
	dune build -p $(NAME) @doc

#TODO non developper doc

.PHONY: plugins
plugins: bin
	echo "TODO"

#### Unit tests and sanity check #############################################

#### XXX tests ###########################################################

.PHONY: tests
tests: bin
	@dune runtest --profile release
	#dune test -p $(NAME) 
	#@printf "## Decision tree tests ##\n"
	#@dune exec -p $(NAME) --  

#### Localy running targets ########################################################

# If the first argument is "run"...
ifeq (run,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

.PHONY: run
run: bin 
	@dune exec --profile release -- compspec $(RUN_ARGS)

#### Cleaning targets ########################################################

.PHONY: clean
clean:
	@dune clean

.PHONY: distclean
distclean: clean
	@find . -type f -name "*~" -exec rm {} \;

#### Installation and release targets ########################################

.PHONY: install
install: bin
	@dune install

.PHONY: uninstall
uninstall:
	@dune uninstall

opam-release:
	dune-release distrib
	dune-release opam pkg

# TODO
#OPAM_REPO=/home/egallego/external/coq/opam-deducteam
#OPAM_LP_VER=$(shell dune-release log -t)
## Prior to build:
## - dune-release log edit && dune-release tag commit [or edit by yourself]
## - dune-release tag                                 [or git tag]
#repos_release:
#	rm -rf _build
#	dune-release distrib
#	dune-release publish distrib
#	dune-release opam pkg -p lambdapi
#	cp -a _build/lambdapi.$(OPAM_LP_VER) $(OPAM_REPO)/packages/lambdapi/
#	cd $(OPAM_REPO) && git add -A && git commit -a -m "[lambdapi] new version $(OPAM_LP_VER)"

#### Setup reproductible and isolated developpement environment	##############
builddocker:
	docker build -t dsl_image .

vagrant:
	vagrant up && vagrant ssh