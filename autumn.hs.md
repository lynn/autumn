# autumn.hs

I wrote a game in a strange style of Haskell where every definition fits on one line. I'm not sure why I did this, but it looks kind of pretty.

Can code be split into "prose" and "poetry"? It's not exactly code golf: I can think of many ways to make it smaller. But I'd like to think of this program as a poem, and this restriction as its meter. I'd never write a "real" program in this style, but only for the same reason that my documentation isn't made of rhyming iambic verse.

Here's how it works.

## Preamble

Once upon a time...

```hs
import System.Environment
import System.IO
import Data.Char
import Data.Time.Clock
import Data.List
```

I didn't want this program to have any imports beyond "base", so I will deal with pseudorandom number generation and terminal graphics by hand.

The cards are divided into four suits. Rather than define an `enum` for them, I take the shortcut of just using the [terminal color numbers](https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit) as the representation of the suits. I give them all three-letter names because that's pretty. But they are simply green, blue, orange, and red.

```hs
-- Colors
ivy = 64
sky = 69
sun = 208
red = 162
```

Here's just a single utility function: it applies `f` to the `i`-th element of `xs`. It's not very efficient, but it gets the job done.

```hs
at i f xs = [if i == j then f x else x | (j,x) <- zip [0..] xs]
```

## Cards

Autumn Leaves is a solitaire card game, and solitaire card games are made up of cards. A card can be face-down (`Hidden`) or face-up (`Shown`); if it is face-up, it may further be "held" (targeted by the current move / rendered with inverted color), which is really more of a UI concern than part of the game logic, but here I've intertwined them a little, so that the UI state and the tableau state are one and the same.

The "face" of a card is just a `Char`. Face-down cards are shown as `?` and face-up cards are shown as their face.

```hs
-- Cards
data Card = Hidden Char | Shown Bool Char deriving (Eq)
ch (Hidden _) = '?'
ch (Shown _ c) = c
held (Hidden _) = False
held (Shown h _) = h
```

The faces are the ASCII alphabet characters, divided into four suits of 13 cards. `suit` tells us which suit number/color a face character belongs to.

```hs
suit c | c >= 'n' = ivy | c >= 'a' = sky | c >= 'N' = sun | otherwise = red
```

We define some relations on card-faces. They are operators because I like how that looks.

* `c ?< d` is the "precedes" relation, meaning `c < d` and they have the same suit (thus `c` may be moved onto `d`).
* `c !< d` is the "precedes immediately" relation, meaning they meld together into a run.
* `c !<= d` is the "precedes immediately or equals" relation. This is used to print `????` together when displaying runs.

```hs
c ?< d = suit c == suit d && c < d
c !< d = suit c == suit d && succ c == d
c !<= d = c !< d || c == d
```

## Shuffling the deck 

To shuffle the deck, we'll need random numbers. A simple [linear congruence generator](https://en.wikipedia.org/wiki/Linear_congruential_generator) is fine. `rng` makes a stream of random integers modulo something huge, starting from a seed value. Then `shuffle` shuffles a list by sorting it using the random stream as keys.

```hs
rng n = tail $ iterate (\x -> (1103515245 * x + 12345) `mod` 1073741824) n
shuffle ns = map snd . sort . zip ns
```

The deck consists of these four suits stacked together. We could just write `deck = ['A'..'Z'] ++ ['a'..'z']`, but we'll use `suits` later.

```hs
suits = [['A'..'M'], ['N'..'Z'], ['a'..'m'], ['n'..'z']]
deck = concat suits
```

## Initializing the game

These functions build the initial layout, consisting of piles of 4-4-3-3-4-4 with the top card of each pile revealed.

`game n` returns the layout of the game with seed `n`, by shuffling the deck with that seed and dealing out the piles. It returns a `([Card], [[Card]])`: the "stock" (rest of the deck) paired with the "tableau" (list of six piles).

```hs
build (c:cs) = Shown False c : map Hidden cs
layout (n:ns) d = (build (take n d):) <$> layout ns (drop n d)
layout [] d = (d, [])
game n = layout [4,4,3,3,4,4] $ shuffle (rng n) deck
```

## Selecting cards

Here the game logic begins. We say that a face char `c` is "atop" a pile if it is in the first run of that pile, and thus can be grabbed. For example, `'e'` is atop `"cdefi"` (runs: `cdef` `i`), but it is not atop `"cQefi"` (runs: `c` `Q` `ef` `i`).

```hs
c `atop` [] = False
c `atop` cs@(x:_) = x <= c && [x..c] `isPrefixOf` cs
```

`dim` makes cards un-held, and `lit` makes them held. These names refer to the cards "lighting up" when held and "dimming" when put back down. These functions ignore face-down cards.

```hs
dim (Shown _ c) = Shown False c
dim c = c
lit (Shown _ c) = Shown True c
lit c = c
```

To `reach` at a character `c` in a pile of cards `x:xs` is to light up the card with that character and all the cards before it. Its implementation recurses until it finds `c`:

```hs
reach c [] = []
reach c (x:xs) = lit x : if ch x == c then map dim xs else reach c xs
```

To `grab` a character `c` from a pile `p` is to reach it _if that is a valid move_, and otherwise to dim the whole pile.

```hs
grab c p = if c `atop` map ch p then reach c p else map dim p
```

## Moving held cards

We'll say that a held run of cards `cs` "likes" a pile `p` if it can be moved there.

Moving an empty run doesn't make sense, but anything can be moved to an empty pile. Otherwise, the bottom card of the held run (`last cs`) must precede the top card of the pile (`head p`), via the `?<` relation, which, let's remember, verifies that they are also of the same suit.

```hs
[] `likes` _ = False
_ `likes` [] = True
cs `likes` p = ch (last cs) ?< ch (head p)
```

The following functions apply to the whole of the tableau `t`, which is a list of piles of cards.

The `hand` is all the cards that are held. In practice only one pile `p` will contain held cards at a time.

```hs
hand t = [c | p <- t, c@(Shown True _) <- p]
```

To `move i` is to clear all held cards from everywhere in the tableau, and to dump them all onto the `i`-th pile, dimmed. In this process, we `peek` at every pile to reveal new cards.

```hs
move i t = at i (map dim (hand t) <>) $ peek . filter (not.held) <$> t
```

The function `legal i` asks of a tableau if it is legal to perform a `move i`, using our `likes` and `hand` functions. Finally, `try i` performs `move i`, but only if it's legal.

```hs
legal i t = i < length t && hand t `likes` (t!!i)
try i t = if legal i t then move i t else t
```


## Dealing more cards

One of the moves the player has in the game is to deal more cards from the stock. `deal` transforms a stock-and-tableau by moving the top 6 cards of the stock onto the tableau.

```hs
deal (s,t) = (drop 6 s, zipWith (:) (Shown False <$> take 6 s) t)
```

## Revealing cards

As mentioned earlier, face-down cards get revealed when they end up at the top of a stack during a move. `turn` and `peek` are straightforward functions to accomplish this.

```hs
turn (Hidden c) = Shown False c
turn c = c
peek (c:cs) = turn c : cs
peek [] = []
```

## End of game

The game is `won` when all suits are `cleared`, which means that the entire suit exists as one long run in the tableau.

```hs
cleared t suit = or [suit `isInfixOf` map ch p | p <- t]
won t = all (cleared t) suits
```

## Output

We will use the following ANSI escape codes:

* `\ESC[2J`: clear the entire screen; `\ESC[H`: move to the top-left corner.
* `\ESC[2m`: dim; darken the foreground color. Used to color face-down cards (`?`).
* `\ESC[7m`: invert; swap foreground and background colors. This is used to mark held cards.
* `\ESC[38;5;♥m`: set the foreground color to ♥. Used to render suit colors.

```hs
clear = "\ESC[2J\ESC[H\n    autumn leaves\n"
paint c s = "\ESC[" <> c <> "m" <> s <> "\ESC[0m"
mark b = if b then ";7" else ""
sprite (Hidden c) = paint "2" "?"
sprite (Shown h c) = paint ("38;5;" <> show (suit c) <> mark h) [c]
runs [] = [[]]
runs (c:cs) = case runs cs of { (x:y):z | ch c !<= ch x -> (c:x:y):z; r -> [c]:r }
row name p = "    " <> name <> " " <> unwords (concatMap sprite <$> runs p)
header i t = paint (mark $ legal i t) (show $ i+1)
tableau t = unlines [row (header i t) r | (i,r) <- zip [0..] t]
stock s = "    ~ deal (" <> show (length s `div` 6) <> " left)"
```

This code is mostly unremarkable. `clear`, `tableau t`, and `stock s` are all strings that can be printed to the terminal to show parts of the game state. `runs` is a somewhat tricky function that splits `"abcefl"` into `["abc","ef","l"]`.

## Input

The game state is a stock-and-tableau pair `(s,t)`. We will manage a whole list of these to support "undo". The pattern `(s,t):h` matches the current stock, current tableau, and history.

The `act` function discriminates on various keys the player might press and updates the game state accordingly. Digits perform moves, pushing the result of `try` to the history. Letters select cards, overwriting the top state in the history. Space unselects cards. Tilde deals out cards from the stock to the tableau if possible. Backslash is undo. All other keys leave the game state unchanged.

```hs
act ((s,t):h) k | isDigit k, k > '0' = (s, try (digitToInt k - 1) t) : (s,t):h
act ((s,t):h) k | isAlpha k = (s, grab k <$> t):h
act ((s,t):h) ' ' = (s, map dim <$> t):h
act ((s,t):h) '~' | s /= [] = deal (s, map dim <$> t) : (s,t):h
act ((s,t):h) '\\' | h /= [] = h
act ((s,t):h) _ = (s,t):h
```

## Game loop

Let's define `view` to show the game UI, and `yay` to print a win message:

```hs
view s t = putStr $ unlines [clear, tableau t, stock s, "    \\ undo"]
yay h = putStrLn ("Completed in " <> show (length h - 1) <> " moves.")
```

Then the game loop consists of showing the UI, printing the win message if we won, else reading a key, interpreting it as an action, and looping.

```hs
play h@((s,t):_) = view d t >> if won t then yay h else (getChar >>= play . act s)
```

## Main

To set up the game, we disable buffering on standard input, so that the player can press a key on the keyboard and get immediate feedback, rather than "line buffering" — the normal terminal input mode where the terminal holds on to these inputs until the user presses Enter.

```hs
setup = hSetBuffering stdin NoBuffering
```

We read a seed from the command line, so that the player can retry specific shufflings of the deck. If there are no command-line arguments, we use the current Unix time in picoseconds as the seed.

```hs
seed [] = diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
seed [x] = pure (read x)
```

Let the games begin!

```hs
begin n = let (s,t) = game n in play [(s,t)]
main = setup >> getArgs >>= seed >>= begin
```

_Written on my balcony in Antwerp, on May 18, 2025._
