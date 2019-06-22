# ![](https://github.com/aimacode/aima-java/blob/gh-pages/aima3e/images/aima3e.jpg)aima-scala [![Build Status](https://travis-ci.org/aimacode/aima-scala.svg?branch=master)](https://travis-ci.org/aimacode/aima-scala) [![Gitter](https://badges.gitter.im/aima-scala/community.svg)](https://gitter.im/aima-scala/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
Scala implementation of algorithms from [Russell](http://www.cs.berkeley.edu/~russell/) And [Norvig's](http://www.norvig.com/) [Artificial Intelligence - A Modern Approach 3rd Edition](http://aima.cs.berkeley.edu/). You can use this in conjunction with a course on AI, or for study on your own.

## Formating the code
```bash
sbt scalafmt
```

## Test Coverage
```
sbt ";clean;test"
sbt coverageReport
```

reports are generated in the target/scoverage-report folder of the sub-projects
