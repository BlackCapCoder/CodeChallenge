# Welcome to BlackCap's code challenge!

Right now there are no challenge, I have just implemented the level system. Come back later!

[Go to the challenge](https://blackcapcoder.github.io/CodeChallenge/index.html)

The challenge is divided into levels, and you need the solution to the previous level to move on to the next. You also have a total of 2 skips, so if you find a particular level especially difficult you can choose to skip it. Skips can be earned back by revisiting previous levels that you haven't already solved.

That is, at any given time you can view a total of 3 levels that you haven't solved yet.

## About the implementation

I am cheap so I don't want to pay for a server, I also don't want to implement the leveling system in javascript as, that requires storing both the answers and the levels in plain text. It would also be trivial to bypass in the browser terminal.

Therefor, each level is encrypted with AES CRT 256- the key of course being the solution to the previous level. This ensures that the user cannot cheat and move on to later levels.

Skips are handled by the `skipfile` file. This file contains one key for every possible skip/beat history for every level. For instance, if the user is at level 4 and their history is `Beat,Skip,Beat` - that is, they skipped level 2 -, then there exist a key for level 4 in the skipfile that is encrypted with the level 1 and the level 3 keys.
The skipfile only contains histories with at most 2 skips in total, so it grows relatively slowly (`32n(n²+5)/6` bytes where n is number of levels).

If you are interested the `Deploy` folder contains the Haskell program that makes the skipfile and encrypts the levels.
