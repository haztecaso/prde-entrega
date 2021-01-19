VERSION=$$(awk '/^version:/ { print $$2 }' package.yaml)
PANDOC_OPTIONS=--to html5 -s --highlight-style tango -M title="Meta tres en raya"

PANDOC_HTML_OPTIONS=-H header.html

all: build readme docs

build:
	stack build

readme:
	pandoc -o readme.html -i readme.md $(PANDOC_OPTIONS) 
	ln -srfT readme.html index.html

docs:
	stack haddock
	cp -r .stack-work/dist/x86_64-linux-nix/Cabal-3.0.1.0/doc/html/mttt docs

publish: docs readme tarball
	rsync -aPvzz ./docs hhh:/srv/http/mttt/ --delete
	rsync -vzz ./mttt-v${VERSION}.tar.gz hhh:/srv/http/mttt/
	rsync -aPv ./*.html hhh:/srv/http/mttt/

push: clean
	git push

tarball: docs readme
	tar -zcvf /tmp/mttt-v${VERSION}.tar.gz --exclude .git --exclude .stack-work .
	mv /tmp/mttt-v${VERSION}.tar.gz .

version:
	echo ${VERSION}

clean:
	rm -rf .stack-work docs readme.html index.html *.tar.gz

