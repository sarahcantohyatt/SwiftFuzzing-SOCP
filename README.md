# SwiftFuzzing-SOCP

Installation instructions:

MacOS Requirements: 
-Java installation: https://www.oracle.com/java/technologies/downloads/#jdk24-mac
	or brew install java if using homebrew
-Swift installation: https://www.swift.org/install/macos/
	or brew install swift if using homebrew
-coreutils: Can be installed through the package manager homebrew, (download homebrew here: https://brew.sh/). Run the command `brew install coreutils` and follow the instructions to adjust the PATH
	
Linux Requirements:
-Java installation: https://www.oracle.com/java/technologies/downloads/#jdk24-linux
-Swift installation: https://www.swift.org/install/linux/

Note: Whichever swift compiler version that is installed is the one that the fuzzer will be running the generated tests against

Clone this github repository by running this command: git clone https://github.com/sarahcantohyatt/SwiftFuzzing-SOCP.git


Usage:

1. Loading seed swift files
Navigate to where the repository was cloned on your computer.
Navigate to SwiftFuzzing-SOCP/swift-programs
This is where you are going to place all .swift files that you want to use as seed programs for the fuzzer.
There are already 2 .swift files located in this directory named example.swift and example_1.swift for example purposes. Those can be removed when running your own fuzzing campaign.

2. Running the fuzzer

Recommended: Pre-configured fuzzing campaign
Navigate to SwiftFuzzing-SOCP/scripts and run `./run_fully_automated.sh`
There will be console output that displays which seed file is currently being processed and at which level.

What is this doing?
This option will take each seed file in SwiftFuzzing-SOCP/swift-programs and run it end to end through each level of the fuzzer.
It will mutate the seed file with each level of the fuzzer(1, 2, and 3) and subseqeuntly run each levels mutants against the installed swiftc.
This mode has timeouts for the mutation and running of mutants against the compiler that we found most appropriate. The timeouts are 5 seconds, 10 seconds, and 15 seconds for each mutation level respectively, and 1 minute for each batch of mutants running against the compiler.

When the fuzing campaign is finished all of the triggers that were found (mutants that did not compile) will be located in the SwiftFuzzing-SOCP/triggers directory.
This directory will have a sub-directory that is named after the seed file that created a problematic mutant, inside that sub-directory the triggers will be divided by fuzzer level for easier inspection.
Each mutant that did not compiler will have it's source file saved along with its accompanying compiler errors (in the .output file) or its accompanying compiler crash output (in the .crash file).
The mode will also create a csv file named results.csv that is located in SwiftFuzzing-SOCP/scripts that contains information about how many mutants were created per program and how many of those were tested.

Advanced: Custom configuration
Navigate to SwiftFuzzing-SOCP/scripts.
From here you can run `timeout -s [TIME] ./run_fuzzer_get_inputs.sh -[LEVEL]`
Replace [TIME] with an integer representing the number of seconds until timeout and replacing [LEVEL] with your choice of fuzzer level, options are 1, 2, or 3.

This will take each seed file in SwiftFuzing-SOCP/swift-programs and run it through the desired level of mutation for the desired amount of time.
After completion of this command we will have all generated mutants available in a .zip folder named output.zip that is located in the SwiftFuzzing-SOCP/scripts directory.

To run the generated mutants against swiftc you will need to run `./run_zipfile.sh`
Run timeout -s [TIME] ./run_zipfile.sh if a timeout is desired replacing [TIME] with the desired time in seconds.
This will run each mutant program against swiftc and if there is a mutant that does not compile it will be left in the SwiftFuzzing-SOCP/scripts directory along with its accompanying compiler errors (in the .output file) or its accompanying compiler crash output (in the .crash file).


Developer Instructions:

Additional requirements:
Scala 2 installation: https://www.scala-lang.org/download/2.12.20.html for macOS or Linux
sbt installation: https://www.scala-sbt.org/download/ for macOS or Linux
Java JDK 11: https://www.oracle.com/java/technologies/downloads/#java11 for macOS or Linux

All source code is located in the SwiftFuzzing-SOCP/src directory.
After making changes to the source code you must generate a new .jar file.
This can be done by running `sbt assembly` from the top level of the directory.