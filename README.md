# tf2-web-server-manager
A Team Fortress 2 server manager that runs inside your browser.

To run, start the program "tf2-server-manager" and visit "localhost:3000" on your browser. There, you can log in to your server and run any command as if you were on the server window.
It is possible to override the port by passing it as the first parameter to the program i.e. "tf2-server-manager 80" to run it on port 80.

## Compiling
To compile, install GHC, Cabal and Elm, then run `cabal build --flags=debug`, if you wish to compile with optimizations turned on, then run `cabal build` on the main directory. The build output will be printed on the terminal. Copy this executable (you don't need any of the other files) to a folder (let's call it "out"). Inside this "out" folder, create a folder called "frontend". Back to the root directory, switch to the "frontend" folder (not the one you just created), then run `elm make src/Server.elm --output=outfolder/frontend/server.html`, replacing "outfolder" with the folder the executable is located. To compile with optimizations on, use `elm make src/Server.elm --output=outfolder/frontend/server.html --optimize`. Now run `elm make src/Login.elm --output=outfolder/frontend/login.html` without changing directory, again replacing "outfolder" with the folder the executable is in. To compile with opimizations, use `elm make src/Login.elm --output=outfolder/frontend/login.html --optimize`. Now run the server executable from the same directory it is in.
