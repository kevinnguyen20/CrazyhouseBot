# CrazyhouseBot

Just a simple chess bot listing all possible moves concerning the chess variant [Crazyhouse](https://en.wikipedia.org/wiki/Crazyhouse). The notation is akin to the [Forsyth-Edwards Notation](https://en.wikipedia.org/wiki/Forsyth-Edwards_Notation).

## How to use it

1. Open the terminal.

2. Start ```ghci``` in the same directory where the file ```CrazyhouseBot.hs``` is located.

3. Execute ```:l CrazyhouseBot.hs```.

4. Enter a game situation (obviously it should be correct in terms of chess rules of Crazyhouse).

5. Enjoy the set of possible moves!

## Samples

### Initial situation

- ```listMoves "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/w"```

### Others

- ```listMoves "rnbQ2Q1/pppp3p/6k1/8/1P6/8/Pn1pPKPP/RNB2BNR/BPQRppq b"```
