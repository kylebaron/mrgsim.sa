SHELL := /bin/bash
LIBDIR=/Users/kyleb/Rlibs/lib
PACKAGE=parseq
VERSION=$(shell grep Version pkg/DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.


cran:
	make doc
	make build
	R CMD CHECK --as-cran ${TARBALL} -o ${CHKDIR}

all:
	make doc
	make build
	make install

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document("${PKGDIR}")'

build:
	R CMD build --md5 $(PKGDIR)


install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD CHECK ${TARBALL} -o ${CHKDIR}

