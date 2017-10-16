@echo off
rem Copyright (c) 2017 Ben Zimmer. All rights reserved.

rem Package a distribution archive.

pushd %~dp0

del dist\secondary.jar
rmdir /s /q dist\doc

copy /b /y target\scala-2.10\secondary-assembly-*.jar dist\secondary.jar
robocopy doc\web dist\doc /s /e

rem TODO: figure out the best way to create an archive.

popd
