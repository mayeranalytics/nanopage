all: bin/nanopage
	ls -las bin/nanopage
	strip bin/nanopage
	ls -las bin/nanopage

run: bin/nanopage
	bin/nanopage

admin: bin/nanopage
	cd content; ../bin/nanopage -mADMIN

bin/nanopage:
	make -C app

ghcid:
	make -C app ghcid

clean:
	make -C app clean

.PHONY: clean run admin
