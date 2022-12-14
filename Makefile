#TODO incorporate Makefile from https://github.com/Deducteam/lambdapi
NAME = vardac

#### Compilation (binary, library and documentation) #########################
.PHONY: all 
all: bin

.PHONY: bin
bin: generatedune
	#dune build @all --profile release
	dune build -p $(NAME)

.PHONY: odoc
odoc: 
	dune build -p $(NAME) @doc

#TODO non developper doc

.PHONY: plugins
plugins: bin
	echo "TODO"

.PHONY:generatedune


PLUGINS= akka

generatedune: 
	@find templates -type f | grep -v "dune.j2" | grep -v "dune" | sed -e "s/templates\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o templates/dune -f json -d - templates/dune.j2
	@find stdlib -type f | grep -v "dune.j2" | grep -v "dune" | sed -e "s/stdlib\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o stdlib/dune -f json -d - stdlib/dune.j2
	@find externals -type f | grep -v "dune.j2" | grep -v "dune"  | sed -e "s/externals\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o externals/dune -f json -d - externals/dune.j2
	@find examples -type f | grep -v "dune.j2" | grep -v "dune"  | sed -e "s/examples\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o examples/dune -f json -d - examples/dune.j2
	@cd tests && find data -type f | grep -v "dune.j2"  | grep -v "dune" | sed -e "s/data\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o data/dune -f json -d - data/dune.j2
	for plg in $(PLUGINS) ; do \
		echo $$plg;\
		cd src/codegen/plugins/$$plg/interfaces && find externals -type f | grep -v "dune.j2"  | grep -v "dune" | sed -e "s/externals\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o externals/dune -f json -d - externals/dune.j2 \
		&& find templates -type f | grep -v "dune.j2"  | grep -v "dune" | sed -e "s/templates\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o templates/dune -f json -d - templates/dune.j2 \
		&& cd .. && find stdlib -type f | grep -v "dune.j2"  | grep -v "dune" | sed -e "s/stdlib\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o stdlib/dune -f json -d - stdlib/dune.j2 \
		;\
	done
#### Unit tests and sanity check #############################################

#### XXX tests ###########################################################

.PHONY: tests testcoverage
tests: generatedune
	@dune runtest --profile release
	@rm -rf tests/examples

testcoverage: generatedune
	@dune runtest --profil release --instrument-with bisect_ppx --force
	@rm -rf tests/examples
	@bisect-ppx-report html
	echo $(bisect-ppx-report summary)

.PHONY: fuzz fuzz-ci fuzz-ci-timeout
fuzz: 
	rm -rf /tmp/findings && mkdir -p /tmp/findings 
	AFL_SKIP_CPUFREQ=1 dune build --profile release @fuzz --no-buffer

fuzz-ci: 
	rm -rf /tmp/findings && mkdir -p /tmp/findings 
	AFL_SKIP_CPUFREQ=1 dune build --profile release @bun-fuzz --no-buffer

fuzz-ci-timeout: 
	rm -rf /tmp/findings && mkdir -p /tmp/findings 
	AFL_SKIP_CPUFREQ=1 dune build --profile release @bun-fuzz-no-kill --no-buffer

replay-fuzz:
	./_build/default/fuzz/fuzz_me.exe $(RUN_ARGS)


#### Localy running targets ########################################################

# If the first argument is "run"...
ifeq (run,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

# If the first argument is "debugbox"...
ifeq (debugbox,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "debugbox"
  DEBUGBOX_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(DEBUGBOX_ARGS):;@:)
endif

.PHONY: run 
.PHONY: debugbox

run: generatedune bin 
	@dune exec --profile release -- vardac $(RUN_ARGS)

debugbox: generatedune bin
	@dune exec --profile release -- vardad $(DEBUGBOX_ARGS) 


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


#### Vizualisation tools
render:
	dot -Tpng compiler-build/sltopology_ir1.dot -o compiler-build/sltopology_ir1.png
	dot -Tpng compiler-build/sltopology_ir2.dot -o compiler-build/sltopology_ir2.png
	dot -Tpng compiler-build/sltopology_ir3.dot -o compiler-build/sltopology_ir3.png

# eog - gnome default
sltopology: render
	eog compiler-build/sltopology.png