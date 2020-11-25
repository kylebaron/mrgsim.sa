SHELL := /bin/bash
LIBDIR=/Users/kyleb/Rlibs/lib
PACKAGE=mrgsim.sa
VERSION=$(shell grep Version DESCRIPTION | awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.

test: 
	Rscript -e "testthat::test_dir('tests/testthat')"

covr:
	Rscript inst/covr/covr.R

cran:
	make doc
	make build
	R CMD CHECK --as-cran ${TARBALL} 

all:
	make doc
	make build
	make install

.PHONY: doc
doc:
	Rscript -e 'devtools::document("${PKGDIR}")'

build:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD CHECK ${TARBALL} 

readme:
	Rscript -e "rmarkdown::render('README.Rmd')"

pkgdown:
	Rscript -e "pkgdown::build_site()"

spelling:
	Rscript -e "spelling::spell_check_package()"
