# Clojure Chess

## Introduction
A simple chess ui for playing chess written using reagent and electron. By simple I mean it contains no chess engine, all the engine work must be performed on the server. Clojure Chess is writtent to work with [cl-chess](https://github.com/DevonKS/cl-chess).

<img src="https://raw.githubusercontent.com/DevonKS/clj-chess/main/docs/img/board.png" alt="Chess Board" title="Screenshot of the Clojure Chess board. Highlighting features."/>

## Features
 - Highlight squares in 4 different colors.
 - Draw Arrows on the board.
 - Valid move hints.
 - Drag and Drop as well as click to move.

## Development

### Development mode
```
npm install
npm run dev
electron .
```
start a ClojureScript REPL
```
clj -M:shadow-cljs browser-repl
```
### Building for production

```
npm run build
npx electron-packager . clj-chess
```
### Debugging build issues
If the release build doesn't work then you can compile a debug version and view it in the browser to get nice error messages.

1. Compile a debug version
```
clj -M:shadow-cljs release main app --debug
```
2. Open the public/index.html file in a browser and open the console to see errors

