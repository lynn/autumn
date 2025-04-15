import System.Environment
import System.IO
import Data.Char
import Data.Time.Clock
import Data.List

-- Colors
ivy = "38;5;64"
sky = "38;5;69"
sun = "38;5;208"
red = "38;5;162"

-- Utilities
at i f xs = [if i == j then f x else x | (j,x) <- zip [0..] xs]
plural n word = show n <> " " <> word <> ['s' | n /= 1]

-- Cards
data Card = Hidden Char | Shown Bool Char deriving (Eq)
suit c | c>='n'=ivy | c>='a'=sky | c>='N'=sun | otherwise=red
ch (Hidden _) = '?'
ch (Shown _ c) = c
held (Hidden _) = False
held (Shown h _) = h
c ?< d = suit c == suit d && c < d
c !< d = suit c == suit d && succ c == d
c !<= d = c !< d || c == d

-- Shuffling the deck
toss n = (1103515245 * n + 12345) `mod` 1073741824
rng n = tail $ iterate toss n
shuffle rnds = map snd . sort . zip rnds
suits = [['A'..'M'], ['N'..'Z'], ['a'..'m'], ['n'..'z']]
deck = concat suits

-- Initializing the game
build (c:cs) = Shown False c : (Hidden <$> cs)
layout (n:ns) deck = (build (take n deck):) <$> layout ns (drop n deck)
layout [] deck = (deck, [])
game n = layout [4,4,3,3,4,4] $ shuffle (rng n) deck

-- Selecting cards
c `atop` [] = False
c `atop` cs@(x:_) = x <= c && [x..c] `isPrefixOf` cs
dim (Shown _ c) = Shown False c
dim c = c
lit (Shown _ c) = Shown True c
lit c = c
reach c [] = []
reach c (x:xs) = lit x : if ch x == c then map dim xs else reach c xs
grab c p = if c `atop` map ch p then reach c p else map dim p

-- Moving held cards
[] `likes` _ = False
_ `likes` [] = True
cs `likes` p = ch (last cs) ?< ch (head p)
hand t = [c | p <- t, c@(Shown True _) <- p]
move i t = at i (map dim (hand t) <>) $ peek . filter (not.held) <$> t
legal i t = i < length t && hand t `likes` (t!!i)
try i t = if legal i t then move i t else t

-- Dealing more cards
deal (d,t,n) | (p,q) <- splitAt 6 d = (q, zipWith (:) (Shown False <$> p) t, n+1)

-- Revealing cards
turn (Hidden c) = Shown False c
turn c = c
peek (c:cs) = turn c : cs
peek [] = []

-- End of game
cleared t suit = any (suit `isInfixOf`) (map (map ch) t)
won t = all (cleared t) suits

-- Output
clear = "\ESC[2J\ESC[H\n    autumn leaves\n"
paint c s = "\ESC[" <> c <> "m" <> s <> "\ESC[0m"
mark b = if b then ";7" else ""
sprite (Hidden c) = paint "2" "?"
sprite (Shown h c) = paint (suit c <> mark h) [c]
runs [] = [[]]
runs (c:cs) = case runs cs of { (x:y):z | ch c !<= ch x -> (c:x:y):z; r -> [c]:r }
row name p = "    " <> name <> " " <> unwords (concatMap sprite <$> runs p)
header i t = paint (mark $ legal i t) (show $ i+1)
tableau t = unlines [row (header i t) r | (i,r) <- zip [0..] t]
talon d = "    ~ " <> (plural (length d `div` 6) "deal" <> " left")

-- Input
act (d,t,n) k | isDigit k, k > '0' = (d, try (digitToInt k - 1) t, n+1)
act (d,t,n) k | isAlpha k = (d, grab k <$> t, n+1)
act (d,t,n) ' ' = (d, map dim <$> t, n)
act (d,t,n) '~' | d /= [] = deal (d, map dim <$> t, n)
act (d,t,n) _ = (d,t,n)

-- Game loop
view d t = putStrLn (clear <> "\n" <> tableau t <> "\n" <> talon d)
yay n = putStrLn ("Completed in " <> show n <> " turns.")
play s@(d,t,n) = view d t >> if won t then yay n else (getChar >>= play . act s)

-- Main
setup = hSetBuffering stdin NoBuffering
begin n = let (d,t) = game n in play (d,t,0)
seed [] = diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
seed [x] = pure (read x)
main = setup >> getArgs >>= seed >>= begin
