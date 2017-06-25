all: bin/nanopage
	ls -las bin/nanopage
	strip bin/nanopage
	ls -las bin/nanopage

run: bin/nanopage
	bin/nanopage

admin:
	cd content; ../bin/nanopage -mADMIN

bin/nanopage:
	mkdir -p bin
	make -C nanopage

ghcid:
	make -C nanopage ghcid

compress:
	upx -9 bin/nanopage

clean:
	make -C nanopage clean

.PHONY: clean run admin compress bin/nanopage Demo

Demo:
	APPDIR=$$PWD/$@ stack  --local-bin-path $@/bin/ install
	strip Demo/bin/*
