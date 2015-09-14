all: compile install clean

compile: 
	ghc -o heather main.hs

install:
	mv -f heather /usr/local/bin

clean:
	rm Heather.{o,hi} main.{hi,o}
