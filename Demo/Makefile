EXE=demo-app

run: bin/$(EXE)
	$<

admin: bin/$(EXE)
	$< -m ADMIN -C content

bin/$(EXE): Demo

Demo:
	cd ..;\
	APPDIR=$$PWD/$@ stack --local-bin-path $@/bin/ install;\
	strip Demo/bin/*

compress: bin/$(EXE)
	upx -9 $<

clean:
	rm -f bin/*

.PHONY: Demo clean run admin
