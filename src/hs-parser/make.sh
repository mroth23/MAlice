echo "~/.cabal/bin/alex -g Scanner.x"
~/.cabal/bin/alex -g Scanner.x
echo "happy -a -c -g Parser.y"
happy -a -c -g Parser.y
echo "ghc -fglasgow-exts {...} -o MAlice Scanner.hs Parser.hs"
ghc -O3 -fllvm -optlo-O3 -o MAlice Scanner.hs Parser.hs
