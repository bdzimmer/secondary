# Secondary [![Build Status](https://travis-ci.org/bdzimmer/secondary.svg)](https://travis-ci.org/bdzimmer/secondary) [![Codacy Badge](https://api.codacy.com/project/badge/grade/d862db25644045359ba166636cbb2392)](https://www.codacy.com/app/bdzimmer/secondary)

Secondary is a tool for creating fictional worlds. Describe a world consisting of complex hierarchies of characters, locations, events, and other concepts using Markdown in text files which are straightforward to read and edit. Use special tags to track relationships between concepts and mark tasks and thoughts. Compile your world into a static web site, browse and review tasks, then edit and compile again.

Copyright &copy; 2018 Ben Zimmer. All rights reserved.

### Setup

You'll need sbt and JDK 7 or greater.

Clone the repositories for Secondary and its dependencies.

    git clone https://github.com/bdzimmer/secondary.git
    git clone https://github.com/bdzimmer/util-scala.git
    git clone https://github.com/bdzimmer/pixel-editor.git
    git clone https://github.com/bdzimmer/orbits.git

Run tests, build documentation, and build the JAR.

    cd secondary
    sbt test it:test
    sbt assembly
    sh package.sh

Add `secondary/dist` to your path, or extract the generated `secondary.tar.gz` archive to the location of your choice and add that location to your path. Documentation--generated using Secondary from a project in `secondary/doc`--is in `secondary/dist/doc`.

### License

This code is currently published under the 3-clause BSD license. See [LICENSE](LICENSE) for further information.
