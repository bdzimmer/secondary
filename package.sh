#!/bin/bash
# Copyright (c) 2015 Ben Zimmer. All rights reserved.

# Package a distribution archive.
# This isn't the best; contains a lot of  cheap workarounds for use with
# Cygwin.

chmod -R 755 dist
rm dist/secondary.jar
rm -rf dist/doc

# TODO: figure out how to run sbt from Cygwin.
# sbt assembly $@

cp target/scala-2.10/secondary-assembly-2015.11.13.jar dist/secondary.jar
cp -R doc/web dist/doc

chmod -R 755 dist
cp -R dist secondary

chmod -R 755 secondary
rm -rf secondary.tar.gz
tar czf secondary.tar.gz secondary

rm -rf secondary
