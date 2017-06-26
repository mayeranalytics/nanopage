all: Demo

Demo:
	make -C Demo

clean:
	make -C Demo clean

.PHONY: Demo clean
