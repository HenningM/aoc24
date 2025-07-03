# Advent of Code: 25 Programming Languages in 25 Days

## Introduction
Advent of Code 2024 marks the ten year anniversary of AoC. To celebrate this,
I've decided that I will use a slightly different approach than the previous
years. The main difference being that I plan on actually solving all the
puzzles. The other, and maybe more interesting, difference being that instead of
sticking to one programming language, I will be working my way through the
alphabet, starting with Ada on December 1st, ending with YAL (Yet Another Lisp)
on the 25th.

## Ground rules
1. I will be aiming to solve every puzzle without the use of wrappers written in
other languages, but some exceptions to this rule might be necessary.
2. The input will be read from `stdin`, while the output will be written to
`stdout` as two lines, giving the answer to part one and part two, respectively.
3. Use of GenAI is only permitted while setting up the basic framework for each solution. AI assistance is not permitted while solving the puzzle.
    * Basic framework = read from stdin, write to stdout
4. No LSP or IDE support

### Example
```sh
% ./day01 < input
<Solution to part one>
<Solution to part two>
```

## Languages
### [Day 1](https://adventofcode.com/2024/day/1): [Ada](https://ada-lang.io/)
#### Why Ada?
I considered assembly, but decided I didn't want to break my own spirit this
early. ActionScript and AWK were also in the running for this spot, but I
decided to use Ada because it seems relatively well documented, while also
being different enough from the languages I normally use.

#### Puzzle
Today's puzzle was a very gentle start to AoC 24. The main challenge today was
to figure out how to parse the input (into `Vector`s) using `Find_Token()` and
extracting substrings using ranges. Sorting the `Vectors` was handled
by`Generic_Sorting` from the Ada standard library.

#### Experience
A good and smooth start. The code I produced won't impress anyone, but using the
language to solve today's puzzle(s) was doable without too much effort. I/O
operations were quite painless, and the syntax was OK.


### [Day 2](https://adventofcode.com/2024/day/2): [BASIC](https://www.freebasic.net/)
#### Why BASIC?
I was born slightly too late to experience the golden age of BASIC. However,
based on the nostalgic tone people use when they describe their experience with
the language, better late than never.

#### Puzzle
Another relatively straightforward puzzle. Getting to grips with the language
was slightly more challenging. I especially struggled with parsing the input
and understanding how to correctly re-size and use arrays.

#### Experience
Getting to grips with the language was slightly challenging. I especially
struggled with parsing the input and understanding how to correctly re-size and
use arrays. I was also slightly annoyed that I couldn't find anything in the
standard library to help me split strings around a delimiter, but I realize
that this is more likely to be a problem stemming from my inexperience with the
language rather than the language itself.


### [Day 3](https://adventofcode.com/2024/day/3): [C](https://en.wikipedia.org/wiki/C_(programming_language))
#### Why C?
COBOL, Clojure and Coq were all in the running for this spot, but I ended up picking C because:
1. It's a very widespread language that I have embarrasingly little experience using
2. I don't want to burn out before the 25th and C should at least feel _somewhat_ familiar

#### Puzzle
The puzzle was not too difficult, although I did manage to shoot myself in the
foot a few times by messing up string termination. The code is one delightfully
regexy `main()` function for you to admire and adore.

#### Experience
This is the first language I have a tiny bit of previous experience with. I'm
quite certain you wouldn't be able to derive this fact from the quality of the
code, but it did have an impact on my experience. Less googling, smoother flow.

I did manage to get myself into some unnecessary off-by-one errors related to
string termination, but this was a very nice experience overall. `clang` spits
out much better warning/error messages than I expected.


### [Day 4](https://adventofcode.com/2024/day/4): [Dart](https://dart.dev/)
#### Why Dart?
Dart is a _relatively_ new and modern language that still maintains at least
_some_ traction over a decade after its introduction. I've never used it, so I
figured this is a golden opportunity to change that.

#### Puzzle
Classic AoC puzzle. Parse and search a 2D-board. Not too difficult, but maybe
slightly more difficult than the previous days? Today's solution includes a
beautiful triple-nested for-loop for part one.

#### Experience
This language feels very similar to Java, Kotlin, TypeScript etc., but maybe
that's just because the language allows me to stick to what I know from these
other languages. Idiomatic Dart might look very different from what I've
produced today.


### [Day 5](https://adventofcode.com/2024/day/5): [Erlang](https://www.erlang.org/)
#### Why Erlang?
Erlang (and Elixir) have been mentioned/recommended to me so many times that I
had to resist my urge to pick Elm this year. I might give Elm a go next time...

#### Puzzle
Very fun puzzle. You get a list of ordering rules and a list of "updates", and
you need to figure out which of the updates are valid according to the rules.
For part two you need to fix the invalid ones, which in hindsight sounds very
doable, but I spent a _lot_ of time getting this right.

#### Experience
Erlang, being a functional language, was quite different from all the previous
days. I've had much worse experiences with other functional languages, but this
was no walk in the park. I think this is the best day so far when it comes to
learning outcome, but it was also a day filled with frustration (and endless
googling).


### [Day 6](https://adventofcode.com/2024/day/6): [Fortran](https://fortran-lang.org/)
#### Why Fortran?
Who would _not_ include Fortran here? It simply had to be on the list. Some
might argue that F# would have been a more practical/useful choice, but let's
be honest: F is for Fortran.

#### Puzzle
Another 2D-puzzle. A guard is patrolling a mapped area, and you need to figure
out how many positions in the area he visits before leaving. Fun puzzle, and
solving it with Fortran makes it fun².

For part two you need to force the guard into an endless loop. You achieve this
by blocking his path at certain points. The output for part two is the number
of points where you're able to force an endless loop.

#### Experience
Near endless fun. I spent nearly a full day working on this, should-be
relatively doable, puzzle. Most of the time was spent getting used to how
simple I/O operations work, how arrays work, when to use functions and
subroutines etc. This was _at least_ as exhausting as the experience with
Erlang yesterday, but unlike yesterday it didn't feel like time and effort well
spent. I'm _probably_ not touching Fortran again for at least a year.


### [Day 7](https://adventofcode.com/2024/day/7): [Go](https://go.dev/)
#### Why Go?
I had a very strong suspicion that I would need a "rest day" after Erlang and
Fortran the two preceding days. I was right. I don't have _a lot_ of experience
with Go, but I'm confident that I have used it enough to make this a more
pleasant experience than the one I had yesterday.

#### Puzzle
You're helping a group of engineers calibrate a rope bridge. To calibrate the
bridge you need to insert some operators (add, multiply) to some "calibration
equations". You get a list where each line holds a test value and a list of
numbers that should amount to the test value by inserting operators between
them.

For part two there's one additional operator available for insertion (concatenation).

#### Experience
This this did indeed end up feeling like a rest day. Not because it required
_no effort_, but because I was somewhat familiar with the language already and
the puzzle was brute-forceable. My solution is not elegant nor efficient, but
it works.


### [Day 8](https://adventofcode.com/2024/day/8): [Haskell](https://www.haskell.org/)
#### Why Haskell?
I've already made several (unsuccessful) attempts to make myself comfortable
with purely functional programming. I'm at the point where I can see and
appreciate many of the benefits of functional programming, but I struggle when
losing my imperative training wheels completely.

Solving one AoC puzzle using Haskell will hardly make me an expert of
functional programming nor [category
theory](https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_),
but I hope I will walk away a _little bit_ wiser.

#### Puzzle
Another 2D grid puzzle. This time you're looking at a map of antennas
broadcasting on various frequencies. The first part of the puzzle is to figure
out where these antennas create "antinodes". Antinodes are formed when two
antennas broadcasting the same frequency are placed close to each other (see
AoC for a more precise description). The output should be the total antinode count.

Part two expands on the puzzle from part one by specifying that adjacent
antennas should not just create one antinode, but many nodes following the same
pattern all the way to the map boundaries.

#### Experience
Fun puzzle that I made an absolute mess of. But at least a different kind of
mess than those I've made earlier this year. This time I completely messed up
my approach to the problem.

Instead of finding antenna pairs and generating antinodes from those, I chose
to go the opposite way. In other words: I scan through the map checking for
each position whether it should contain an antinode. I probably don't have to
tell you why this is a bad idea. It's obviously slow as molasses.

On the bright side: the code is relatively compact, even with a lot of
unnecessary duplication. Working with Haskell was also more comfortable than
expected.


### [Day 9](https://adventofcode.com/2024/day/9): [Io](https://iolanguage.org/)
#### Why Io?
Good question. Primarily because there aren't a lot of nonesoteric programming
languages starting with the letter "I".
[Icon](https://en.wikipedia.org/wiki/Icon_(programming_language)) would have
been interesting, but I wasn't confident that I would be able to set up a
usable environment for it in a short time frame.

Io seems quite well documented and easy to get started with, so I'm giving it a go.

#### Puzzle
This day's puzzle is titled "Disk Fragmenter", which of course takes me back to
[the good ol' days of Windows
9x](https://en.wikipedia.org/wiki/Defragmentation). The goal of the puzzle is
pretty much the straight opposite of the goals of the "Defrag" tool in Win 9x.
While Defrag tries to ensure that each file is represented on the disk using
the smallest number of contiguous regions/blocks possible, the aim of this
puzzle is to cram file fragments into the first available block on the disk.

#### Experience
This _could_ have been the most pleasant surprise so far. I/O in Io feels
effortless, and many of the built-in methods for lists are very convenient. But
unfortunately today's puzzle requires calculation with some pretty big numbers,
and as far as I could tell, this would have required compiling and setting up
an [extra module](https://github.com/IoLanguage/BigNum).

I ended up invoking the exception to rule #1 for the first time here, and wrote
a tiny shell script to do the final calculation for both parts of the puzzle.


### [Day 10](https://adventofcode.com/2024/day/10): [Java](https://dev.java/)
#### Why Java?
Mainly for the same reason I chose Go for day 7. Java is one of the languages I
have most experience with professionally, so I'm hoping that this will reduce
the effort required to solve the task. Rule #4 might throw a spanner in the
works here.

#### Puzzle
Very cool puzzle, and another 2D grid. Today we're finding the longest evenly
gradual uphill slope on a topographic map in ASCII form. We're scoring all the
possible trailheads based on how many peaks they allow us to visit.

Part two asks for the "rating" of a trailhead. The rating is based on the
number of _distinct_ trails that start at a given trailhead.

#### Experience
A humbling experience. I was already aware that I'm quite reliant on IDEA
when writing Java and Kotlin, but apparently I had underestimated exactly how
much I rely on it. I still ended up googling less than the previous days, and
it was _relatively_ comfortable, but I struggled a bit more than I had anticipated.


### [Day 11](https://adventofcode.com/2024/day/11): [Kotlin](https://kotlinlang.org/)
#### Why Kotlin?
Pretty much the same as for Java. I figured I could use a small "vacation" when
I'm nearing the halfway mark.

#### Puzzle
We're dealing with some strange stones that change every time we blink. Our input is a list of stones associated with values. The output should be the stone count after blinking X times.

Part 1: X = 25
Part 2: X = 75

#### Experience
This was a very pleasant experience. The fact that the puzzle was easy probably
helped, but writing Kotlin without an IDE was not too bad either. A simple
recursive function with a light sprinkle of memoization (for part 2) did the
trick.


### [Day 12](https://adventofcode.com/2024/day/12): [Lua](https://www.lua.org/)
#### Why Lua?
I wrote a bit of Lua when I was using [Awesome](https://awesomewm.org/), but I
haven't really touched it after switching away from it around ten years ago.
Lua is still relevant today, and it's embedded in everything from [game
engines](https://defold.com/) to [text editors](https://neovim.io/).

#### Puzzle
Today we're calculating fencing prices for garden plots. In other words:
another 2D grid to digest. For part one we're calculating the price based on
the total area and perimeter of the plots.

For part two we have to include a discount, which involves counting the sides
of a plot instead of just using its perimeter. This sounds like an
insignificant difference from part one. It really isn't.

#### Experience
I ended up writing quite a lot of code here. A lot of it is related to various
conversions between string representations and Lua tables. The overall
experience wasn't too bad, but part two was a bit of a struggle.


### [Day 13](https://adventofcode.com/2024/day/13): [MATLAB/Octave](https://octave.org/)
#### Why MATLAB?
For fun? I've never really used MATLAB or Octave outside of maybe a few times
in university, and I'll likely never find a better excuse than this.

#### Puzzle
I can't believe my luck. Today we're essentially solving a system of equations...

We're operating a set of claw machines, and we're tasked with finding the
cheapest combination of button presses to win as many prizes as possible. Each
button has an associated cost and movement of the claw.

#### Experience
I was initially hoping that I would be able to solve the equations using
Octave, and while that probably _is_ possible, I was not able to do it. Instead
I resorted to using [WolframAlpha](https://www.wolframalpha.com/) for that
part. The rest of the puzzle was relatively straightforward. There's probably
still some room for improvement here, since dealing with floats and rounding
seems like it should not be necessary.


### [Day 14](https://adventofcode.com/2024/day/14): [Nim](http://nim-lang.org/)
#### Why Nim?
Primarily because of the hype on [HackerNews](https://news.ycombinator.com/)
and various other forums I frequent.

#### Puzzle
Today we're counting robots on a 2D grid. For part one we're finding the
"safety factor" by counting robots within each quadrant of the grid.

In part two we're supposed to find an easter egg. Specifically we're told that
the robots will, at some point, arrange themselves in the shape of a christmas
tree. Our output should be the number of ticks/seconds that elapse before this
happens.

#### Experience
I enjoyed this a lot. The puzzle was fun, and the language feels very easy to
pick up. The syntax feels similar to Python, and the documentation is good and
helpful with plenty of examples.

I'm not entirely convinced that my solution for part two is correct for all
inputs. My approach is to find the first "frame" where more than half of the
robots are centered.


### [Day 15](https://adventofcode.com/2024/day/15): [OCaml](https://ocaml.org/)
#### Why OCaml?
I keep seeing references to OCaml here and there, and the web site does a good
job of selling the language. The documentation seems good and up-to-date, and
the smae goes for the tooling.

#### Puzzle
We're predicting the movements of a robot (visualized as `@`) and boxes
(visualized as `O`) it's pushing around in a warehouse. The warehouse is
(obviously) presented to us as a 2D grid. The input also contains a list of
movements for the robot in the warehouse.

Part two is more of the same, but here we're dealing with a warehouse twice as
wide, meaning that the boxes from part one (`O`) are now replaced with `[]`.

#### Experience
Part one was relatively easy, but my code quickly devolved into a hot mess for
part two. Lots of duplication that could be eliminated, and the overall
approach could probably also be simplified a bit.

Working with OCaml was a pleasant experience, but slightly more so for part one
than for part two. The code ended up quite messy after solving part two of the
puzzle. Partly because of duplicated code, partly because I didn't exactly
optimize for readability.


### [Day 16](https://adventofcode.com/2024/day/16): [Python](https://www.python.org/)
#### Why Python?
I'm already familiar with the language, and the puzzles are usually quite
difficult by this point in the calendar. Realistically I can probably use a
"rest day" here.

#### Puzzle
We're finding the shortest path a reindeer can travel between two points in a
"reindeer maze" (aka. 2D grid).

For part two we're finding possible spectator spots along _any_ shortest path
in the maze. The important distinction from part one is that it's no longer
enough to find one optimal path, you need to find all of them.

#### Experience
1. Parse the input to find the start point and end point
2. Implement [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)

Not the most ambitious nor novel approach, but it works.


### [Day 17](https://adventofcode.com/2024/day/17): [Q#](https://quantum.microsoft.com/en-us/insights/education/concepts/qsharp)
#### Why Q#?
I challenge you to find a more suitable alternative starting with the letter
"q". Initially I had plans to use
[Q](https://en.wikipedia.org/wiki/Q_(programming_language_from_Kx_Systems)),
but I wasn't able to find an implementation of the language that I was
satisfied with. I was able to get Q# working without too much hassle. I don't
plan on venturing into quantum computing for this year's edition of AoC, so I'm
hoping that the language can work well enough as a general-purpose programming
language.

#### Puzzle
We're emulating a 3-bit computer, with the end goal being to determine what the
output produced by a given set of instructions.


#### Experience


### [Day 18](https://adventofcode.com/2024/day/18): [Rust](https://www.rust-lang.org/)

### [Day 19](https://adventofcode.com/2024/day/19): [Smalltalk](https://www.gnu.org/software/smalltalk/)

### [Day 20](https://adventofcode.com/2024/day/20): [TypeScript](https://www.typescriptlang.org/)

### [Day 21](https://adventofcode.com/2024/day/21): [Umple](https://cruise.umple.org/umple/)

### [Day 22](https://adventofcode.com/2024/day/22): [V](https://vlang.io/)

### [Day 23](https://adventofcode.com/2024/day/23): [Wren](https://wren.io/)

### [Day 24](https://adventofcode.com/2024/day/24): [X10](http://x10-lang.org/)

### [Day 25](https://adventofcode.com/2024/day/25): [YAL (Yet Another Lisp)](https://github.com/skx/yal)
