package FuzzingSwift.toast

import org.scalatest.FlatSpec

class ToastTest extends FlatSpec {
	import FuzzingSwift.tokenizer._
	import FuzzingSwift.parser._
	import FuzzingSwift.toast._
	
	"transform_exp" should "handle 1 + 2" in {
		val exp = NilExp()
		val list = List(ExpFunctionCallArgument(exp))
		val after = SimpleFunctionCall(list)
		val postfixFunctionCallExp = PostfixFunctionCallExp(exp, after)
		val expected = postfixFunctionCallExp
		assertResult(expected) { Toast.traverseExp(postfixFunctionCallExp) }
	}
	
	"toast" should "handle `var`" in {
		val input = "`var`"
		val expected = Program(List(ExpressionStmt(VariableExp("`var`"))))
		assertResult(expected) { Toast("test", input) }
	}

	"toast" should "handle 5 + 1" in {
		val input = "5 + 1"
		val prefix = DecimalIntegerLiteralExp("5")
		val op = Operator("+")
		val exp = DecimalIntegerLiteralExp("1")
		val expected = Program(List(ExpressionStmt(TrueInfixExp(prefix, op, exp))))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle this complex test" in {
		val input = "func factorial(n: Int) -> Int"	
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("factorial")
		val localParamName = LocalParamName("n")
		val typ = NormalTypeIdentifier(TypeName("Int"))
		val typeAnno = TypeAnnotation(None, None, typ)
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcResult = FunctionResult(None, typ)
		val funcSig = ThrowsFunctionSig(paramClause, None, None, Some(funcResult))
		val stmt = DeclarationStmt(FunctionDeclaration(funcHead, funcName, None, funcSig, None, None))
		val expected = Program(List(stmt))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle this less complex test" in {
		val input = "func factorial(n: Int) -> Int {}"
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("factorial")
		val localParamName = LocalParamName("n")
		val typ = NormalTypeIdentifier(TypeName("Int"))
		val typeAnno = TypeAnnotation(None, None, typ)
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcResult = FunctionResult(None, typ)
		val funcSig = ThrowsFunctionSig(paramClause, None, None, Some(funcResult))
		val prefix = VariableExp("n")
		val op = Operator("<=")
		val exp = DecimalIntegerLiteralExp("1")
		val conditionList = List(ExpressionCondition(TrueInfixExp(prefix, op, exp)))
		val returnN = ReturnStmt(Some(VariableExp("n")))
		val codeBlock = CodeBlock(Some(List(returnN)))
		val ifStmt = IfStmt(conditionList, codeBlock, None)
		val identifier = FunctionCallArgName("n")
		val otherExp = TrueInfixExp(VariableExp("n"), Operator("-"), DecimalIntegerLiteralExp("1"))
		val functionCallArg = IdentifierColonExpFunctionCallArgument(identifier, otherExp)
		val simpleFunctionCall = SimpleFunctionCall(List(functionCallArg))
		val functionCall = PostfixFunctionCallExp(VariableExp("factorial"), simpleFunctionCall)
		val nTimesFactorial = TrueInfixExp(VariableExp("n"), Operator("*"), functionCall)
		val returnExpStmt = ReturnStmt(Some(nTimesFactorial))
		val stmtsInBlock = List(ifStmt, returnExpStmt)
		val funcBody = CodeBlock(None)
		val stmt = DeclarationStmt(FunctionDeclaration(funcHead, funcName, None, funcSig, None, Some(funcBody)))
		val expected = Program(List(stmt))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle this even more complex test" in {
		val input = "func factorial(n: Int) -> Int { if n <= 1 { return n } return n * factorial(n: n - 1) }"
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("factorial")
		val localParamName = LocalParamName("n")
		val typ = NormalTypeIdentifier(TypeName("Int"))
		val typeAnno = TypeAnnotation(None, None, typ)
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcResult = FunctionResult(None, typ)
		val funcSig = ThrowsFunctionSig(paramClause, None, None, Some(funcResult))
		val prefix = VariableExp("n")
		val op = Operator("<=")
		val exp = DecimalIntegerLiteralExp("1")
		val conditionList = List(ExpressionCondition(TrueInfixExp(prefix, op, exp)))
		val returnN = ReturnStmt(Some(VariableExp("n")))
		val codeBlock = CodeBlock(Some(List(returnN)))
		val ifStmt = IfStmt(conditionList, codeBlock, None)
		val identifier = FunctionCallArgName("n")
		val otherExp = TrueInfixExp(VariableExp("n"), Operator("-"), DecimalIntegerLiteralExp("1"))
		val functionCallArg = IdentifierColonExpFunctionCallArgument(identifier, otherExp)
		val simpleFunctionCall = SimpleFunctionCall(List(functionCallArg))
		val functionCall = PostfixFunctionCallExp(VariableExp("factorial"), simpleFunctionCall)
		val nTimesFactorial = TrueInfixExp(VariableExp("n"), Operator("*"), functionCall)
		val returnExpStmt = ReturnStmt(Some(nTimesFactorial))
		val stmtsInBlock = List(ifStmt, returnExpStmt)
		val funcBody = CodeBlock(Some(stmtsInBlock))
		val stmt = DeclarationStmt(FunctionDeclaration(funcHead, funcName, None, funcSig, None, Some(funcBody)))
		val expected = Program(List(stmt))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle an if exp stmt" in {
		val input = "if true {}"
		val conditionList = List(ExpressionCondition(TrueLiteralExp()))
		val codeBlock = CodeBlock(None)
		val expected = Program(List(IfStmt(conditionList, codeBlock, None)))
		assertResult(expected) { Toast("test", input) }
	}
	
	"precedenceLevel" should "handle Operator(+)" in {
		val input = Operator("+")
		assertResult(3) { Toast.precedenceLevel(input) }
	}
	
	"precedenceLevel" should "handle Operator(*)" in {
		val input = Operator("*")
		assertResult(2) { Toast.precedenceLevel(input) }
	}
	
	"toListLike" should "handle 1 + 2 * 3" in {
		val left = DecimalIntegerLiteralExp("1")
		val op = Operator("+")
		val right = TrueInfixExp(DecimalIntegerLiteralExp("2"), Operator("*"), DecimalIntegerLiteralExp("3"))
		val resultExp = DecimalIntegerLiteralExp("1")
		val resultList = List((Operator("+"), DecimalIntegerLiteralExp("2")), (Operator("*"), DecimalIntegerLiteralExp("3")))
		assertResult((resultExp, resultList)) { Toast.toListLike(left, op, right) }
	}
	
	"toListLike" should "handle 3 * 1 + 2" in {
		val left = DecimalIntegerLiteralExp("3")
		val op = Operator("*")
		val right = TrueInfixExp(DecimalIntegerLiteralExp("1"), Operator("+"), DecimalIntegerLiteralExp("2"))
		val resultExp = DecimalIntegerLiteralExp("3")
		val resultList = List((Operator("*"), DecimalIntegerLiteralExp("1")), (Operator("+"), DecimalIntegerLiteralExp("2")))
		assertResult((resultExp, resultList)) { Toast.toListLike(left, op, right) }
	}
	
	"handlePrecedence" should "handle 1 + 2 * 3" in {
		val paramExp = DecimalIntegerLiteralExp("1")
		val paramList = List((Operator("+"), DecimalIntegerLiteralExp("2")), (Operator("*"), DecimalIntegerLiteralExp("3")))
		val multiplication = TrueInfixExp(DecimalIntegerLiteralExp("2"), Operator("*"), DecimalIntegerLiteralExp("3"))
		val expected = TrueInfixExp(DecimalIntegerLiteralExp("1"), Operator("+"), multiplication)
		assertResult(expected) { Toast.handlePrecedence(paramExp, paramList) }
	}
	
	"handlePrecedence" should "handle 3 * 1 + 2" in {
		val paramExp = DecimalIntegerLiteralExp("3")
		val paramList = List((Operator("*"), DecimalIntegerLiteralExp("1")), (Operator("+"), DecimalIntegerLiteralExp("2")))
		val multiplication = TrueInfixExp(DecimalIntegerLiteralExp("3"), Operator("*"), DecimalIntegerLiteralExp("1"))
		val expected = TrueInfixExp(multiplication, Operator("+"), DecimalIntegerLiteralExp("2"))
		assertResult(expected) { Toast.handlePrecedence(paramExp, paramList) }
	}
	
	"toast" should "handle 3 * 1 + 2" in {
		val input = "3 * 1 + 2"
		val multiplication = TrueInfixExp(DecimalIntegerLiteralExp("3"), Operator("*"), DecimalIntegerLiteralExp("1"))
		val expected = Program(List(ExpressionStmt(TrueInfixExp(multiplication, Operator("+"), DecimalIntegerLiteralExp("2")))))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle 1 + 2 * 3" in {
		val input = "1 + 2 * 3"
		val exp = TrueInfixExp(DecimalIntegerLiteralExp("2"), Operator("*"), DecimalIntegerLiteralExp("3"))
		val expected = Program(List(ExpressionStmt(TrueInfixExp(DecimalIntegerLiteralExp("1"), Operator("+"), exp))))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle 1 + 2 * 3 - 4" in {
		val input = "1 + 2 * 3 - 4"
		val multiplication = TrueInfixExp(DecimalIntegerLiteralExp("2"), Operator("*"), DecimalIntegerLiteralExp("3"))
		val overallLeft = TrueInfixExp(DecimalIntegerLiteralExp("1"), Operator("+"), multiplication)
		val overall = TrueInfixExp(overallLeft, Operator("-"), DecimalIntegerLiteralExp("4"))
		val expected = Program(List(ExpressionStmt(overall)))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle a custom operator declaration" in {
		val input = "infix operator +-: AdditionPrecedence"
		val opDecl = InfixOperatorDeclaration(Operator("+-"), Some(PrecedenceGroupName("AdditionPrecedence")))
		val expected = Program(List(DeclarationStmt(opDecl)))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle a custom operator declaration followed by an expression stmt that uses that operator" in {
		val input = "infix operator +-: AdditionPrecedence; 1 * 2 +- 3"
		val opDecl = InfixOperatorDeclaration(Operator("+-"), Some(PrecedenceGroupName("AdditionPrecedence")))
		val multiplication = TrueInfixExp(DecimalIntegerLiteralExp("1"), Operator("*"), DecimalIntegerLiteralExp("2"))
		val theExp = TrueInfixExp(multiplication, Operator("+-"), DecimalIntegerLiteralExp("3"))
		val expected = Program(List(DeclarationStmt(opDecl), ExpressionStmt(theExp)))
		assertResult(expected) { Toast("test", input) }
	}
	
	"toast" should "handle a precedence group declaration" in {
		val input = "precedencegroup myGroup {}"
		val expected = Program(List(DeclarationStmt(PrecedenceGroupDeclaration(PrecedenceGroupName("myGroup"), None))))
		assertResult(expected) { Toast("test", input) }
	}
}