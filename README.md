# String-Solver
Scala version of Flash-Fill for Excel 2013 by Gulwani et Al. See http://rise4fun.com/QuickCode/dates

## Table of contents

- [Table of contents](#table-of-contents)
- [Key features](#key-features)
- [Usage](#usage)
    - [Compile it](#compile-it)
    - [Link library for SBT](#link-library-for-sbt)
    - [Link library for Maven](#link-library-for-maven)
- [Automated Bash commands](#automated-bash-commands)
    - [Bash and Cygwin](#bash-and-cygwin)
    - [Semi-automated Renaming](#semi-automated-renaming)
        - [Windows Shell extension for renaming files (requires python)](#windows-shell-extension-for-renaming-files-requires-python)
        - [Bash version](#bash-version)
    - [Semi-automated bash commands](#semi-automated-bash-commands)
        - [Implicit file names and file content](#implicit-file-names-and-file-content)
    - [Semi-automated partition commands](#semi-automated-partition-commands)
    - [Semi-automated filter commands](#semi-automated-filter-commands)
    - [Semi-automated file content mapping commands](#semi-automated-file-content-mapping-commands)
- [API](#api)
    - [Providing input/output examples](#providing-inputoutput-examples)
    - [Solving new input](#solving-new-input)
    - [Options](#options)

Build using sbt 0.13 and scala 2.10.3.

## Key features:

- Supports incremental numbering.
- "..." continues an expression if there is a loop.
- Semi-automated file renaming commands
- Semi-automated file processing commands
- Semi-automated file filter and partition commands

## Usage

### Compile it

- Install SBT http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html
- Make sure to have PATH making sbt accessible
- `git clone https://github.com/MikaelMayer/StringSolver.git`
- `sbt compile`

Now it should work. To test it, run `sbt test' but this might take a while.

### Link library for SBT

Add the following two lines separated by a blank line in your main `build.sbt`:

    resolvers += "Sonatype.org" at "https://oss.sonatype.org/content/groups/staging"

    libraryDependencies += "ch.epfl.lara" %% "stringsolver" % "1.1"

Then you can use it in your code, for example in a file in `src/main/scala/Custom.scala`:

```Scala
package com.example
import ch.epfl.lara.synthesis.stringsolver._
object Custom {
  val c = StringSolver()
  def main(a: Array[String]): Unit = {
    c.add(a(0).split("\\|").toList, a(1))
	c.solve() match {
	  case Some(a) => println(Printer(a))
	  case None => println("No program found")
    }
  }
}
```

Then if in your `build.sbt` you have `mainClass in (Compile, run) := Some("com.example.Custom")`, you can then run:

    sbt
	run "tEsT|inPuT1" TESTinput001

and it should output:

    the first input uppercase + the lowercase second input until the end of the first word
	+ a 3-digit number from the second input starting at the first number

### Link library for Maven

    <dependency>
      <groupId>ch.epfl.lara</groupId>
      <artifactId>stringsolver_2.10</artifactId>
      <version>1.1</version>
    </dependency>

## Automated Bash commands

StringSolver includes a nice automatic renaming tool and an automated command generalizer.

Installation:

### Bash and Cygwin

- Build the project using `sbt one-jar` or download it from sonatype (see URL [above](#link-library-for-sbt))
- Use the following alias to rename file using the tool (e.g. in your `.bashrc` file:)
```
export STRINGSOLVERPATH = [/path/to/StringSolver/target/scala/]
alias mv='java -jar "$STRINGSOLVERPATH/stringsolver_2.10-1.1-one-jar.jar" move'
alias auto='java -jar "$STRINGSOLVERPATH/stringsolver_2.10-1.1-one-jar.jar" auto'
alias partition='java -jar "$STRINGSOLVERPATH/stringsolver_2.10-1.1-one-jar.jar" partition'
alias filter='java -jar "$STRINGSOLVERPATH/stringsolver_2.10-1.1-one-jar.jar" filter'
```

### Semi-automated Renaming

#### Windows Shell extension for renaming files (requires python)

[![ScreenShot](http://i1.ytimg.com/vi/rbhAv3uBFqw/mqdefault.jpg)](http://youtu.be/rbhAv3uBFqw)

This windows shell extension recursively monitors folders for renaming and suggests renamings.

- Install python https://www.python.org/downloads/
- Make sure that python can be invoked from the command line.
- Edit the first line of `Monitor.ps1` to indicate which root folder to monitor (it could be c:\)
- Run PowerShell and navigate to the repository
- Inside Powershell, run `PowerShell -Sta` to start single threaded mode.
- Start:

    .\Monitor.ps1

- Now navigate to the `StringSolver` repository
- Type `sbt`
- Type `run server`

Now the service is launched.

Now, within the explorer, rename two files or two folders in the same folder.
A balloon should appear with the suggestion. Click it and the renaming is automatically done.

To stop the service, either close the command line utility, or on another shell at the `StringSolver` repository, write:

- echo "stop" | Send-TcpRequest localhost 12345;

#### Bash version

The semi-automatic rename tool overrides the actual one:

```
mv file1 file2
```

However, when a mapping is detected, the algorithm displays it and you can then use `mv` to trigger it for all other files.

```
mv
```

This is equivalent to perform the global transformation in a single line using `-a` or `--auto` modifier, when you trust enough the system:

```
mv -a file1 file2
```

There are other commands after a renaming is done:

* `mv -e` or `--explain` provides a high-level english explanation of the transformation
* `mv -t` or `--test` provides a visualization of the transformation if applied.

Adding options `-e` or `-t` along with filenames helps the system to refine the request without modifying any files.

* `-p or --properties` provides owner name and last modification date.

If you experience trouble with the `mv` command, you can always use the `-c` or `--clear` option to clear the history stored in a temporary file.

To produce an equivalent bash script which would produce the result, add the option `-b` or `--bash`

### Semi-automated bash commands

[![ScreenShot](http://i1.ytimg.com/vi/yaNr-JDc8tA/mqdefault.jpg)](http://youtu.be/yaNr-JDc8tA)

[![ScreenShot](http://i1.ytimg.com/vi/SRFC-Hi08-I/mqdefault.jpg)](http://youtu.be/SRFC-Hi08-I)

The standard way to run bash commands is the following:

```
auto filename "my unix command depending on filename"
```

For example, to convert a like `Algebra2Week5.txt` to `Week5/Algebra2.pdf` and remove the original file, you can do the following:

```
auto Algebra2Week5.txt "convert Algebra2Week5.txt Week5/Algebra2.pdf;rm Algebra2Week5.txt"
```

To perform this transformation for all files, just type again

```
auto
```

The last two commands could be combined in a single command if you trust the system enough by using the `-a` or `--auto` command.

```
auto -a Algebra2Week5.txt "convert Algebra2Week5.txt Week5/Algebra2.pdf;rm Algebra2Week5.txt"
```

To produce an equivalent bash script which would produce the result, add the option `-b` or `--bash` (to come soon)

#### Implicit file names and file content

The previous command can also be abbreviated by letting the program infer what is the file the command depends on. So writing this would be equivalent to the previous command:

```
auto -a "convert Algebra2Week5.txt Week5/Algebra2.pdf;rm Algebra2Week5.txt"
```

To directs the system to use the file content line by line instead of just the filename, use the flag `-l` or `--lines`.

If you do not trust the system, you can run `auto -e` before your command, or after any command having used `auto` to check what the global transformation would be. In the other hand, `auto -t` visualizes the transformation.

For example, running the following command will not trigger it, but will display a list of command that would be executed if the user runs `auto`

```
auto -t "convert Algebra2Week5.txt Week5/Algebra2.pdf;rm Algebra2Week5.txt"
```

If you experience trouble with the `auto` command, you can always run `auto --clear` to clear the history stored in a temporary file.

### Semi-automated partition commands

[![ScreenShot](http://i1.ytimg.com/vi/NNFY9azdblQ/mqdefault.jpg)](http://youtu.be/NNFY9azdblQ)

If you have a set of files, by providing at least two files of each partition for at least two partitions, you can split the files into as many folders as there are partitions.
For example:

```
partition --test myphoto1.jpg images otherpicture2.jpg images mytextfile.txt text otherdoc.txt text
partition
```

It will move all files ending with `.jpg` to images, all files ending with `.txt` to text, and if there are files ending `.pdf`, they will be moved in a folder named `.pdf`, etc.

Provided partition names can be unrelated constants (red, blue, etc.), numbers (1, 2, 3...), strings transformation from the common substring of each partition (set-TXT,set-JPG,....) or any combination of these three.

### Semi-automated file content mapping commands

StringSolver provides a way to change the content of an entire file based on examples. Given an even number of inputs, `map` will start to compute the transformation from inputs to outputs.
If it is given one input, it will apply the current transformation to the lines of the file.

```
map '  case France => "0033"' '  case France => "+33"'
map CountryMapping.scala
```

The `--filter` flag can be used to apply the transformation on a subset of lines. See below.

### Semi-automated filter commands

`filter` is slightly different from the previous commands. It is similar to partition, in the sense that it can move files to different folder. The difference is that it separates files given a property, and is lazy to move files, so it can be used for the other commands.
It has the --test option by default and can effectively move files only when using the `filter` alone or if the modifier `--auto` is set.

For example:

```
filter myphoto1.jpg images otherpicture2.jpg images mytextfile.txt . otherdoc.pdf .
filter
```

The first line considers that the `images` is the tag for accepted files. It will find out that accepted files end with `.jpg`.
The second line asks to move accepted files to a folder `images` and keep the others in the current directory `.`

`mv`, `auto` and `partition` also accept the `--filter` modifier. If set, it will perform the last `filter --test` and apply their transformation only on filtered files and not the others.

To use the `filter` command with the content of a file and the `map` command, you have to provide the `--lines` flag to `filter` and then the `--filter` flag to `map`. For example:

```
filter --lines "element {01}" ok "element {02}" ok "Title" notok
map "element {01}" "element {1,el}" "content {10}" "content {10,co}"
map --filter mycontent.json
```

## API

```Scala
import ch.epfl.lara.synthesis.stringsolver._

object Test {
   val c = StringSolver()
   c.add("file499.pdf -> 01file.pdf")
   c.add("report_761.pdf -> 02report.pdf")
   c.add("credits##.pdf -> 03credits.pdf")
   
   /* Prints:
	* a 2-digit counter incrementing starting at 1
	* + the first input until the end of the first lowercase word
	* + the first input starting at the first '.'
	*/ 
   println(Printer(c.solve().get))
   
   /* Prints:
	* 04input.pdf
	**/
   println(c.solve("input%^$.pdf"))
}

object Test2 {
   val c = StringSolver()
   c.add(List("Alg1.txt"), "convert Alg1.txt Alg001.pdf")
   
   /* Prints:
	* the constant string 'convert ' + the first input
	  + the constant string ' ' + the first input until the end of the first word
	  + a 3-digit number from the first number in previous output
	  + the constant string '.pdf'
	*/ 
   println(Printer(c.solve().get))
   
   /* Prints:
	* "convert Math2.txt Math002.pdf"
	*/ 
   println(c.solve("Math2.txt"))

   
   val c2 = StringSolver()
   c2.add(List("Alg001.pdf", "Alg002.pdf", "Alg003.pdf"), "convert Alg001.pdf Alg002.pdf... AlgBook.pdf")
   
   /* Prints:
	* the constant string 'convert ' + concatenates all inputs separated by ' '
	* + the constant string ' ' + the first input until the end of the first word
	* + the constant string 'Book' + the first input starting at the last non-number
	*/
   println(Printer(c2.solve().get))
   
   /* Prints:
	* convert Math1.pdf Math2.pdf Math3.pdf Math4.pdf MathBook.pdf
	*/
   println(c2.solve("Math1.pdf | Math2.pdf | Math3.pdf | Math4.pdf"))
   // prints "
}
```

### Providing input/output examples

The other ways to add input/output examples in StringSolver are the following, given that `c` is a StringSolver instance.

```Scala
// Exactly one input and one output
c.add("input1 -> output1")
// c.add returns a set of solution programs. You can .takeBest on it

// Three inputs and two outputs
c.add("input1 | input2 | input3 | output1 | output2", 3)

// Three inputs and two outputs
c.add(List("input1", "input2", "input3"), List("output1", "output2"))

// Three inputs and one output
c.add(List("input1", "input2", "input3"), "output1)

// Three inputs and two outputs two times
c.add("""input1 | input2 | input3 | output1 | output2
input4 | input5 | input6 | output3 | output4""", 2)
```

### Solving new input

To solve and print an existing StringSolver instance, do the following:
```Scala
val c = StringSolver()
c.add(List("a","b"),List("ab","ba"))

// If one output was provided (equivalent to c.solve(0))
c.solve() match {    // The first input + the second input
  case Some(prog) => println(Printer(prog))
  case None =>
}

// Retrieve the second output program (0-index based)
c.solve(1) match {   // The second input + the first input
  case Some(prog) => println(Printer(prog)) 
  case None =>
}

// Returns "cd | dc"
c.solve("c | d")

// Returns List("cd", "dc")
c.solve(List("c", "d"))
```

To solve a problem from scratch, you can also do the following:
```Scala
val c = StringSolver()
c.add(List("a","b"),List("ab","ba"))

// Return: "a | b | ab | ba
//         c | d | cd | dc"

c.solve("""
a | b  | ab | ba
c | d""", 2)
````

### Options

Most of the StringSolver usage is fully automated, and does not require to change the following options.
For some cases, they can be useful to trigger on/off.

```Scala
val c = StringSolver()

/**
 * Use numbering from previous input option
 */
c.setUseNumbers(b: Boolean) = {ff.numbering = b; this}

/**
 * Loop level. 0 will not look for loops
 */
c.setLoopLevel(i: Int) = {ff.DEFAULT_REC_LOOP_LEVEL = i; this}

/**
 * Timeout in seconds to add a new input/output example.
 * This is approximate. Default is 15s
 */
c.setTimeout(seconds: Int) = {ff.TIMEOUT_SECONDS = seconds; this}

/**
 * If looking for loops, what could be the maximum separator length
 */
c.setMaxSeparatorLength(length: Int) = {ff.MAX_SEPARATOR_LENGTH = length; this}

/**
 * If only interesting positions (aka word, special chars and digit separators)
 * are considered when looking for loops
 */
c.setOnlyInterestingPositions(b: Boolean) = {ff.onlyInterestingPositions = b; this}

/**
 * Outputs programs steps. Useful for debugging an other.
 */
c.setVerbose(b: Boolean) = {ff.verbose = b; this}
c.isVerbose = ff.verbose

/**
 * Allows to iterate over inputs.
 */
c.setIterateInput(b: Boolean) = ff.iterateInput = b

/**
 * Allows to use the example index for positions
 */
c.setUseIndexForPosition(b: Boolean) = ff.useIndexForPosition = b

/**
 * Retrieves statistics
 */
c.getStatistics(): String = ff.statistics()

/**
 * Advanced stats.
 */
c.setAdvancedStats(b: Boolean) = ff.advanced_stats = b

/**
 * Extra time to merge as a proportion of timeout
 */
c.setExtraTimeToMerge(f: Float) = extra_time_to_merge = f
```
