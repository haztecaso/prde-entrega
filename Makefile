VERSION=$$(awk '/^version:/ { print $$2 }' package.yaml)
PANDOC_OPTIONS=--to html5 -s --highlight-style tango -H header.html --metadata\
			   title="Meta tres en raya"

all: build install readme docs publish

build:
	stack build

install: build
	stack install

readme:
	pandoc -o readme.html -i readme.md $(PANDOC_OPTIONS) 

docs:
	stack haddock
	cp -r .stack-work/dist/x86_64-linux-nix/Cabal-3.0.1.0/doc/html/mttt docs

publish: docs readme tarball
	rsync -aPvzz ./docs/ hhh:/srv/http/mttt/docs/ --delete
	rsync -vzz ./mttt-v${VERSION}.tar.gz hhh:/srv/http/mttt/releases/
	rsync -aPv ./captura.png hhh:/srv/http/mttt/
	rsync -aPv ./readme.html hhh:/srv/http/mttt/index.html

tarball: docs
	tar -zcvf /tmp/mttt-v${VERSION}.tar.gz\
		--exclude .git\
		--exclude .stack-work\
		--exclude .gitignore\
		--exclude *.tar.gz\
		.
	mv /tmp/mttt-v${VERSION}.tar.gz .

version:
	echo ${VERSION}

clean:
	rm -rf .stack-work\
		docs\
		readme.html\
		*.tar.gz
