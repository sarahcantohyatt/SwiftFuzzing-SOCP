package FuzzingSwift.fuzzer

import FuzzingSwift.unification._
import FuzzingSwift.unification.UIterator._
import FuzzingSwift.tokenizer._
import FuzzingSwift.parser._
import FuzzingSwift.toast._
import FuzzingSwift.typechecker._
import FuzzingSwift.generator._
import FuzzingSwift.printer._

import scala.io.Source

object Main {
	
	def main(args: Array[String]) = {
		//get the contents from the specified input file
		val fileName: String = args(0)
		val file = Source.fromFile(fileName)
		val fileContents: String = file.getLines.mkString("\n")
		file.close
		
		args(1) match {
			case "-c" => { //exp node counting
				val ast1 = Toast(fileName, fileContents)
				val expCounts: (Double, Double) = Typechecker(ast1)
				//println("expCount: " + expCounts._1)
				//println("expReplaceCount: " + expCounts._2)
				if (expCounts._1 == 0) {
					println("Nil")
				} else {
					println(expCounts._2/expCounts._1)
				}
			}
			case "-f" => { //run fuzzer
				val ast = Toast(fileName, fileContents)
				Typechecker(ast)
				Generator(ast, args(2)).reify(new UnificationEnvironment, Unit).map(_._3).foreach(x => println(PrettyPrinter(x) + "\n-----"))
			}
			case "-p" => { //parsing only
				val tokens = Tokenizer(fileName, fileContents)
				val ast = Parser(fileName, Parser.program, tokens.toList)
				println(ast)
			}
			case _ => throw new Exception("invalid flag")
		}
		
		//can individually do tokenizer/parser for debugging:
		//val tokens = Tokenizer(fileName, fileContents)
		//println(tokens)
		//val ast1 = Parser(fileName, Parser.program, tokens.toList)
		//println(ast1)
		
		//call toast with the input file contents to get back the AST
		//val ast = Toast(fileName, fileContents)
		//println(ast)
		
		//call typechecker with the above result to annotate the AST
		//Typechecker(ast)
		
		//println(ast)
		//println(PrettyPrinter(ast))
		
		//below is without cutting
		//Generator(ast).reify(new UnificationEnvironment, Unit).map(_._3).foreach(x => println(PrettyPrinter(x) + "\n-----"))
		
		//with cutting below (this is kind of problematic...)
		//Now we never backtrack on success. Meaning for things like replacing variables and functions:
		//We only ever replace them with the first thing in the map that succeeds to unify
		//gen(ast, 100).foreach(x => println(PrettyPrinter(x) + "\n-----"))
	}
	
	//workflow:
	//takes in the ast
	//generates the uiterator
	//takes the first element
	//discards the rest
	//continues doing that for the specified amount of times
	// def gen(seed: Program, numFiles: Int): List[Program] = {
	//   if (numFiles <= 0) {
	//     List()
	//   } else {
	//     val it: Iterator[Program] = Generator(seed).reify(new UnificationEnvironment, Unit).map(_._3)
	//     if (it.hasNext) {
	//       it.next :: gen(seed, numFiles - 1)
	//     } else {
	//       List()
	//     }
	//   }
	// }
	//this is a fix for the enormous state space that is created when exhaustively generating mutants for programs
	//while still ensuring program variability
	//what the generator does:
	//essentially creates a tree of decision points and the first program is representative of a dfs for a random path
	//down the tree.
	//then it backtracks one decision and explores the other decisions that could have been made there.
	//what the above forces:
	//since we are just taking the first program in each uiterator, we are always getting a different random dfs path
	//down the tree
}
