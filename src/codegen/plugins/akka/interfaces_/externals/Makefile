.PHONY: dune
dune: dune.j2
	@find . -type f | grep -v "dune.j2" | grep -v "dune" | grep -v "Makefile" |sed -e "s/.\///" | jc --ls | sed -e 's/\(.*\)/{"locations":\1}/g' | jinja -o dune -f json -d - dune.j2