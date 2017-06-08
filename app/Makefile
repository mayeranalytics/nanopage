all: ../bin/nanopage

../bin/nanopage: src/*.hs src/Internal/*.hs src/Partials/*.hs
	stack --local-bin-path ../bin install

ghcid:
	stack exec ghcid -- -c'stack ghci --main-is=nanopage'

atom:
	atom .

clean:
	rm -f ../bin/nanopage; stack clean

.PHONY: atom build clean ghcid
