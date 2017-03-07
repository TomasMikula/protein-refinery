# protein-refinery
Reasoning about protein-protein interactions, using propagators

## Try it out

To run a protein-refinery session, you will need:
 - Java Runtime Environment (JRE) version 8u102 or later. ([Download](http://www.oracle.com/technetwork/java/javase/downloads/index.html))
 - sbt (Scala Build Tool), version 0.13 or later. ([Download](http://www.scala-sbt.org/))
 - [Git](https://git-scm.com/)
 
Once you have the above tools installed, run
 
```sh
git clone https://github.com/TomasMikula/protein-refinery.git
cd protein-refinery
sbt session
```

to start the interactive session. Note that the first time it will take longer, because it will download the dependencies and compile the sources.

On subsequent runs, just do

```sh
git pull    # optional, to ensure you have the most up-to-date version
sbt session
```
