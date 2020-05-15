@echo off
rem Copyright (c) 2015 Ben Zimmer. All rights reserved.
rem Batch file for running Secondary on Windows.

java -cp %~dp0\secondary.jar bdzimmer.secondary.export.controller.Driver %*