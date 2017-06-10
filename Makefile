all: bin/nanopage
	ls -las bin/nanopage
	strip bin/nanopage
	ls -las bin/nanopage

run: bin/nanopage
	bin/nanopage

admin: bin/nanopage
	cd content; ../bin/nanopage -mADMIN

bin/nanopage:
	mkdir -p bin
	make -C app

ghcid:
	make -C app ghcid

compress:
	upx -9 bin/nanopage

clean:
	make -C app clean

.PHONY: clean run admin compress bin/nanopage
