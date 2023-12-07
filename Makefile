SHELL := /bin/bash
LIBDIR=/Users/kyleb/Rlibs/lib
PACKAGE=mrgsim.sa
VERSION=$(shell grep Version DESCRIPTION | awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.

bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)

test: 
	Rscript -e "testthat::test_dir('tests/testthat')"

covr:
	Rscript inst/covr/covr.R

cran:
	make doc
	make build
	R CMD CHECK --as-cran ${TARBALL} 

package:
	make doc
	make build-vignettes
	R CMD CHECK --as-cran ${TARBALL} 
	
submit:
	make doc
	make build-vignettes
	

all:
	make doc
	make build
	make install

.PHONY: doc
doc:
	Rscript -e 'devtools::document("${PKGDIR}")'

build:
	R CMD build --md5 $(PKGDIR) --no-build-vignettes

build-vignettes:
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
	rm -rf DOCS
	Rscript -e "pkgdown::build_site('.', override = list(destination = 'DOCS'))"

spelling:
	Rscript -e "spelling::spell_check_package()"

rhub:
	Rscript -e 'rhub::check_for_cran(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false", R_COMPILE_AND_INSTALL_PACKAGES = "always"))'

check-win:
	Rscript -e 'devtools::check_win_devel()'

clean: 
	rm -f README.html
	rm -rf DOCS
	rm -rf mrgsim.sa.Rcheck

	