# autumn

A console version of the solitaire card game [Autumn Leaves](https://www.amirrorclear.net/flowers/game/autumn-leaves/) by Toby Ord.

## Building

Compile like `ghc autumn.hs` on Unix and `ghc -rtsopts -with-rtsopts="--io-manager=native" autumn.hs` on Windows.

Then, run `./autumn` to start a random game or `./autumn 123` to play a fixed seed.

https://github.com/user-attachments/assets/a68a7939-4007-464d-b11b-a7a05f042682

## How to play

* There are four suits of cards: `ABCDEFGHIJKLM` (red), `NOPQRSTUVWXYZ` (orange), `abcdefghijklm` (blue), and `nopqrstuvwxyz` (green).
* You can move single cards, or runs of consecutive cards, onto higher cards of the same suit or onto empty piles.
* `?` cards are revealed when they are at the top of a pile.
* At any time, you can deal six cards from the deck by pressing `~`.
* To win, form four complete piles in the tableau.

| Key | Action |
| --- | --- |
| `A-Za-z` | Grab cards (to grab `cde`, press `e`) |
| `1-6` | Move cards to pile |
| `~` | Deal cards from deck |
| Ctrl-C | Quit game |
