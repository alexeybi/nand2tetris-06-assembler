## Assembler implementation in Scala

Assembler implementation using [Scala Standard Parser Combinator
Library](https://github.com/scala/scala-parser-combinators) for nand2tetris [Project 6](https://www.nand2tetris.org/project06).
Jack language is translated into Hack assembly based on the project requirements.
Correctness is verified with the Hack Assembler provided with the [project tools](https://www.nand2tetris.org/software).

#### To translate Hack asm file:
```
sbt "run <ASM FILE>"
```

#### To run existing tests:
```
sbt test
```

