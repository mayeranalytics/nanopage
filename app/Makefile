all: ../bin/nanopage

../bin/nanopage:
	stack --local-bin-path ../bin install

ghcid:
	stack exec ghcid -- -c'stack ghci --main-is=nanopage'

atom:
	atom .

clean:
	rm -f ../bin/nanopage; stack clean

.PHONY: atom build clean ghcid ../bin/nanopage
