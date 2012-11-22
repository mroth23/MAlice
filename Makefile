all:
	cabal install --only-dependencies
	cabal configure
	cabal build
	rm -f ./compile
	ln -s ./dist/build/MAlice/MAlice compile

clean:
	cabal clean
