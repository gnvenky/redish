# redish
REDIS in Haskell

$ cabal install stm

Taking inspiration from https://honza.pokorny.ca/2015/09/building-a-redis-clone-in-haskell
and making changes

Added 'ping' support

% ghc --version <br>
The Glorious Glasgow Haskell Compilation System, version 8.10.7
% cabal --version <br>
cabal-install version 3.6.2.0
compiled using version 3.6.2.0 of the Cabal library 

-- Stress test <br>
$ redis-cli -p 7777 -r 1000 get venky

<ul>
  <li> get and set operations </li>
  <li> Multi-threaded</li>
   <li> Atomic </li>
  <li>Redis compatible (implement the Redis protocol)</li>
</ul>
