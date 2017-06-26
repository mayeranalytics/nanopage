EXE=demo-app

bin/$(EXE): Demo

run: bin/$(EXE)
	$<

admin: bin/$(EXE)
	$< -m ADMIN -C content

Demo:
	cd ..;\
	APPDIR=$$PWD/$@ stack --local-bin-path $@/bin/ install;\
	strip Demo/bin/*

compress: bin/$(EXE)
	upx -9 $<

clean:
	rm -f bin/*

.PHONY: Demo clean run admin
