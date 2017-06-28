all:

src/Partials.hs: mkPartialsHs
	stack exec mkPartialsHs > $@

mkPartialsHs: src/MkPartialsHs.hs
	stack build nanopage:exe:mkPartialsHs

clean:
	rm -f ../bin/nanopage; stack clean

.PHONY: clean mkPartialsHs
