TAR = gtar

RELEASE = $(shell date -I)
DIRNAME = $(shell basename `pwd`)
ARCHIVE = $(DIRNAME)-$(RELEASE)

WWW = $(HOME)/public_html/rx

all :
	cd src; $(MAKE) all
	cd doc; $(MAKE) all

check : 
	cd src; $(MAKE) all
	cd examples; $(MAKE) check

clean :
	rm -f *~
	cd src; $(MAKE) clean
	cd doc; $(MAKE) clean
	cd examples; $(MAKE) clean

tgz :
	cd ..; $(TAR) cvvfz $(ARCHIVE).tar.gz \
		$(DIRNAME)/*.html \
		$(DIRNAME)/TODO \
		$(DIRNAME)/CHANGELOG \
		$(DIRNAME)/Makefile \
		$(DIRNAME)/src/Makefile* \
		$(DIRNAME)/src/*.hs \
		$(DIRNAME)/doc/Makefile \
		$(DIRNAME)/doc/*.lit \
		$(DIRNAME)/doc/*.dvi.gz \
		$(DIRNAME)/doc/*.ps.gz \
		$(DIRNAME)/examples/Makefile \
		$(DIRNAME)/examples/*.lit



www : tgz
	$(TAR) xvvfz ../$(ARCHIVE).tar.gz
	rm -rf $(WWW)/src $(WWW)/doc $(WWW)/examples
	mv $(DIRNAME)/* $(WWW)/
	cp ../$(ARCHIVE).tar.gz $(WWW)/release/
# 	rm -rf $(DIRNAME)


