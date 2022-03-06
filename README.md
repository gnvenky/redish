# redish
REDIS in Haskell

# cabal install stm

Taking inspiration from https://honza.pokorny.ca/2015/09/building-a-redis-clone-in-haskell
and making changes

Added 'ping' support

% ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
% cabal --version
cabal-install version 3.6.2.0
compiled using version 3.6.2.0 of the Cabal library 

-- Stress test
$ redis-cli -p 7777 -r 1000 get venky
