PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')

all: install

test:
	Rscript -e 'library(methods); devtools::test()'

roxygen:
	@mkdir -p man
	Rscript -e "library(methods); devtools::document()"

install:
	R CMD INSTALL --no-test-load .

build:
	R CMD build .

check: build
	R CMD check --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

# No real targets!
.PHONY: all test document install
