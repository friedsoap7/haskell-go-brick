# haskell-go-brick

# Go, written in Haskell using Brick!
### A project for UCSD's CSE 230 in Fall 2022.

We plan to implement the Chinese abstract strategy board game, Go, using the Haskell brick library. While the game has been implemented in Haskell before, these are largely GUI-based designs and are often outdated and/or incomplete. Our version of the game will be entirely terminal-based, played locally by two players. 

These two players will take alternating turns to input moves via either coordinate inputs (e.g. E5, H7, etc.) or a cursor controlled by the keyboard’s arrow keys. We plan to experiment with each of these input methods, and depending on their ease of implementation and performance, we may use one or the other, or leave both in the game, with the ability to toggle between the two via the game’s settings. 

Moves will be validated in real-time, preventing players from making any illegal moves. As a stretch goal, we may want to implement real-time graphical feedback for where the player is attempting to place their stones (Go playing pieces). That is, in the case of an illegal move (space already occupied, Ko rule, etc.), we display a red “X” at the player’s proposed move location, and in the case of a legal move, we display a green check mark or other visual cue.

~~We plan to implement real-time scoring for the game, with permanent displays for each of the two players’ scores somewhere in the game’s terminal window. Go has two different scoring systems (area or territory), determined by the players before the game. Just as with the input methods, we will decide on one or the other (or both, toggleable) depending on ease of implementation and quality.~~ scoring Go ain't easy :([^1]

[^1]: https://webdocs.cs.ualberta.ca/~mmueller/ps/goeval.pdf

Finally, we hope to allow players to choose different board sizes (9x9, 13x13, 19x19, etc.) provided this does not significantly complicate the game’s implementation.

