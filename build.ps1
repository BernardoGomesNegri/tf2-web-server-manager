cabal build --builddir=.\out --flags="debug"
Move-Item ".\out\build\x86_64-windows\ghc-8.10.7\tf2-server-manager-1.0.0.0\x\tf2-server-manager\build\tf2-server-manager\tf2-server-manager.exe" ".\out\tf2-server-manager.exe" -Force
Remove-Item ".\out\build" -Recurse
Remove-Item ".\out\cache" -Recurse
Remove-Item ".\out\packagedb" -Recurse
Remove-Item ".\out\tmp" -Recurse
