include Makefile.defs

VERSION := 0.2
TAR = optimParallel_$(VERSION).tar.gz

.PHONY: test-package update-src


update-src:
	sed -i -r -- 's/^Version:.*/Version: '$(VERSION)'/g' DESCRIPTION ;      
	sed -i -r -- 's/^Date:.*/Date: '`date +'%F'`'/g' DESCRIPTION ;
	$(RSCRIPT) -e "roxygen2::roxygenize(\".\")"


test-package:
	$(RSCRIPT) -e "devtools::test('.')"
