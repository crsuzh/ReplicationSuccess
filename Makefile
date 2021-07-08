include Makefile.defs

PACKAGE = ReplicationSuccess
VERSION = 0.2
TAR = $(PACKAGE)_$(VERSION).tar.gz



.PHONY: test-package update-src lib rm check-cran check covr


update-src:
	sed -i -r -- 's/^Version:.*/Version: '$(VERSION)'/g' DESCRIPTION ;      
	sed -i -r -- 's/^Date:.*/Date: '`date +'%F'`'/g' DESCRIPTION ;
	$(RSCRIPT) -e "roxygen2::roxygenize(\".\")"

lib: update-src
	mkdir -p lib
	$(R) CMD INSTALL -l lib .

rm:
	rm -rf lib

test-package:
	$(RSCRIPT) -e "devtools::test('.')"

$(TAR): update-src
	$(R) CMD build .

check-cran: $(TAR)
	$(R) CMD check --as-cran $(TAR) 

check: $(TAR)
	$(R) CMD check $(TAR)

covr: 
	$(RSCRIPT) -e "covr::package_coverage()"

manual: update-src
	$(R) -e 'devtools::build_manual(pkg = ".", path = ".")'
