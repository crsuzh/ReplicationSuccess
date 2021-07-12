include Makefile.defs

PACKAGE = ReplicationSuccess
VERSION = 1.0.0
TAR = $(PACKAGE)_$(VERSION).tar.gz



.PHONY: test-package update-src lib check-cran check covr manual \
	winbuild winbuild-devel webpage clean


update-src:
	sed -i -r -- 's/^Version:.*/Version: '$(VERSION)'/g' DESCRIPTION ;      
	sed -i -r -- 's/^Date:.*/Date: '`date +'%F'`'/g' DESCRIPTION ;
	$(RSCRIPT) -e "roxygen2::roxygenize(\".\")"

lib: update-src
	mkdir -p lib
	$(R) CMD INSTALL -l lib . --no-lock

test-package:
	$(RSCRIPT) -e "devtools::test('.')"

$(TAR): update-src
	$(R) CMD build . --compact-vignettes="both"

check-cran: $(TAR)
	$(R) CMD check --as-cran $(TAR) 

check: $(TAR)
	$(R) CMD check $(TAR) 

covr: 
	$(RSCRIPT) -e "covr::package_coverage()"

manual: update-src
	$(R) -e 'devtools::build_manual(pkg = ".", path = ".")'


winbuild: update-src
	$(RSCRIPT) -e "devtools::check_win_release()"

winbuild-devel: update-src
	$(RSCRIPT) -e "devtools::check_win_devel()"

webpage: update-src
	$(RSCRIPT) -e 'pkgdown::build_site()'

clean:
	rm -rf lib ReplicationSuccess.Rcheck *.tar.gz

