if (($args[0]) -eq "final")
{
    Write-Host "With optimizations ON"
    cabal build --builddir=./out
    $extraelmflags = "--optimize"
}
else
{
    cabal build --builddir=./out --flags="debug"
    $extraelmflags = ""
}
$ghcver = Invoke-Expression -Command "ghc --numeric-version"
$version = "1.2.1.0"
New-Item -ItemType Directory -Force -Path out
New-Item -ItemType Directory -Force -Path out/frontend
if ($IsLinux)
{
    $system = "x86_64-linux"
    $exe = "tf2-server-manager"
}
if ($IsWindows)
{
    $system = "x86_64-windows"
    $exe = "tf2-server-manager.exe"
}
$exe_path = "./out/build/" + $system + "/ghc-" + $ghcver + "/tf2-server-manager-" + $version + "/x/tf2-server-manager/build/tf2-server-manager/" + $exe
$out_exe = "./out/" + $exe
Write-Host ("Copying: " + $exe_path)
Move-Item $exe_path $out_exe -Force
Remove-Item "./out/build" -Recurse
Remove-Item "./out/cache" -Recurse
Remove-Item "./out/packagedb" -Recurse
Remove-Item "./out/tmp" -Recurse
cd frontend
Get-ChildItem static | Copy-Item -Destination ../out/frontend -Recurse
elm make src/Login.elm --output=../out/frontend/login.js $extraelmflags
elm make src/Server.elm --output=../out/frontend/server.js $extraelmflags
cd ..