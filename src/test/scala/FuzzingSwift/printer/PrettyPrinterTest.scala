package FuzzingSwift.printer

import org.scalatest.FlatSpec

class PrettyPrinterTest extends FlatSpec {
	import FuzzingSwift.parser._
	
	"prettyprinter" should "handle a string literal exp" in {
		val input = Program(List(ExpressionStmt(SingleLineStaticStringLiteralExp("A"))))
		val expected = "(\"A\")" + ";"
		assertResult(expected) { PrettyPrinter(input) }
	}
}