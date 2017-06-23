all: ../bin/nanopage

../bin/nanopage:
	stack --local-bin-path ../bin install nanopage:exe:nanopage

ghcid:
	stack exec ghcid -- -c'stack ghci --main-is=nanopage'

src/Partials.hs: mkPartialsHs
	stack exec mkPartialsHs > $@

mkPartialsHs: src/MkPartialsHs.hs
	stack build nanopage:exe:mkPartialsHs

clean:
	rm -f ../bin/nanopage; stack clean

.PHONY: atom build clean ghcid ../bin/nanopage mkPartialsHs
