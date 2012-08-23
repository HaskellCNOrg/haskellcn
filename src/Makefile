
PROG  = ./hakyll
SOURCES = hakyll.hs
HC=ghc
CABAL_CONF=./cabal-dev/packages-7.4.1.conf

SITE=_site
CACHE=_cache

.PHONY: slides

default: build

$(PROG) : $(SOURCES)
	$(HC) -package-conf $(CABAL_CONF) --make $< -o $@

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)
	rm -rf $(SITE) $(CACHE)
	rm -rf $(GAE)/$(SITE)

build: $(PROG) slides
	$(PROG) build

rebuild: $(PROG) slides
	$(PROG) rebuild

preview:
	$(PROG) preview 9900

deploy: build
	cd $(SITE) && ../gzall.pl 
	$(PROG) deploy

p: preview

bp: build preview

rp: rebuild preview
