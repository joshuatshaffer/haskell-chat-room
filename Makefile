
all: *.hs
	ghc -lpthread Chat.hs

clean:
	rm -rf Chat *.o *.hi
