
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

