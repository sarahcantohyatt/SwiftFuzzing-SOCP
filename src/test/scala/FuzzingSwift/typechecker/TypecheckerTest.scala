package FuzzingSwift.typechecker

import org.scalatest.FlatSpec

class TypecheckerTest extends FlatSpec {
	import FuzzingSwift.toast._
	import FuzzingSwift.parser._
	
	val env: Map[String, (Typ, Mutability)] = Map()
	val funcs: Map[String, (Typ, Option[Throwing], List[Parameter])] = Map()
	
	//func myFunc() -> Int
	//x
	"the typechecker" should "add a basic function declaration to the function environment" in {
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val funcDecl = FunctionDeclaration(FunctionHead(None, None), IdentifierFunctionName("myFunc"), None,
						ThrowsFunctionSig(List(), None, None, Some(FunctionResult(None, theType))), None, None)
		val exp1 = VariableExp("x")
		val input = Program(List(DeclarationStmt(funcDecl), ExpressionStmt(exp1)))
		Typechecker(input)
		assert(theType.resolvedTyp == Some(IntType))
		assert(exp1.functions == Some(Map("myFunc" -> (IntType, None, List()))))
	}
	
	//func myFunc() throws -> Int
	//x
	"the typechecker" should "add a basic throwing function declaration to the function environment" in {
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val funcDecl = FunctionDeclaration(FunctionHead(None, None), IdentifierFunctionName("myFunc"), None,
						ThrowsFunctionSig(List(), None, Some(ThrowsMod()), Some(FunctionResult(None, theType))), None, None)
		val exp1 = VariableExp("x")
		val input = Program(List(DeclarationStmt(funcDecl), ExpressionStmt(exp1)))
		Typechecker(input)
		assert(theType.resolvedTyp == Some(IntType))
		assert(exp1.functions == Some(Map("myFunc" -> (IntType, Some(Throws), List()))))
	}
	
	//func myFunc() -> Int
	//myFunc()
	"the typechecker" should "annotate the function call exp with the correct type" in {
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val funcDecl = FunctionDeclaration(FunctionHead(None, None), IdentifierFunctionName("myFunc"), None,
						ThrowsFunctionSig(List(), None, None, Some(FunctionResult(None, theType))), None, None)
		val exp1 = PostfixFunctionCallExp(VariableExp("myFunc"), SimpleFunctionCall(List()))
		val input = Program(List(DeclarationStmt(funcDecl), ExpressionStmt(exp1)))
		Typechecker(input)
		assert(theType.resolvedTyp == Some(IntType))
		assert(exp1.functions == Some(Map("myFunc" -> (IntType, None, List()))))
		assert(exp1.expType == Some(IntType))
		assert(exp1.typeEnv == Some(Map()))
	}
	
	//func myFunc(x: Int) -> Int
	//myFunc(x: 1)
	"the typechecker" should "annotate the function call that has a parameter exp with the correct type" in {
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val paramList = List(OptDefaultArgClauseParameter(None, LocalParamName("x"), TypeAnnotation(None, None, theType), None))
		val funcDecl = FunctionDeclaration(FunctionHead(None, None), IdentifierFunctionName("myFunc"), None,
						ThrowsFunctionSig(paramList, None, None, Some(FunctionResult(None, theType))), None, None)
		val funcArgList = List(IdentifierColonExpFunctionCallArgument(FunctionCallArgName("x"), DecimalIntegerLiteralExp("1")))
		val exp1 = PostfixFunctionCallExp(VariableExp("myFunc"), SimpleFunctionCall(funcArgList))
		val input = Program(List(DeclarationStmt(funcDecl), ExpressionStmt(exp1)))
		Typechecker(input)
		assert(theType.resolvedTyp == Some(IntType))
		assert(exp1.functions == Some(Map("myFunc" -> (IntType, None, paramList))))
		assert(exp1.expType == Some(IntType))
		assert(exp1.typeEnv == Some(Map()))
	}
	
	//func myFunc() -> Int
	//func YESITWORKS() -> Int
	//myFunc()
	"the typechecker" should "correctly handle this example" in {
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val funcDecl = FunctionDeclaration(FunctionHead(None, None), IdentifierFunctionName("myFunc"), None,
						ThrowsFunctionSig(List(), None, None, Some(FunctionResult(None, theType))), None, None)
		val funcDecl1 = FunctionDeclaration(FunctionHead(None, None), IdentifierFunctionName("YESITWORKS"), None,
						ThrowsFunctionSig(List(), None, None, Some(FunctionResult(None, theType))), None, None)
		val exp = PostfixFunctionCallExp(VariableExp("myFunc"), SimpleFunctionCall(List()))
		val input = Program(List(DeclarationStmt(funcDecl), DeclarationStmt(funcDecl1), ExpressionStmt(exp)))
		Typechecker(input)
		assert(exp.expType == Some(IntType))
		assert(exp.typeEnv == Some(Map()))
		assert(exp.functions == Some(Map("myFunc" -> (IntType, None, List()), "YESITWORKS" -> (IntType, None, List()))))
	}
	
	//let x:Int = 5
	//let y:Int = 5
	//x
	//y
	"the typechecker" should "handle a constant with a type annotation" in {
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val typeAnno = TypeAnnotation(None, None, theType)
		val thePattern = IdentifierPattern(PatternName("x"), Some(typeAnno))
		val patternInitList: List[PatternInitializer] = List(PatternInitializer(thePattern, Some(DecimalIntegerLiteralExp("5"))))
		val decl = ConstantDeclaration(None, None, patternInitList)
		val thePattern2 = IdentifierPattern(PatternName("y"), Some(typeAnno))
		val patternInitList2: List[PatternInitializer] = List(PatternInitializer(thePattern2, Some(DecimalIntegerLiteralExp("5"))))
		val decl2 = ConstantDeclaration(None, None, patternInitList2)
		val exp = VariableExp("x")
		val exp2 = VariableExp("y")
		val input = Program(List(DeclarationStmt(decl), DeclarationStmt(decl2), ExpressionStmt(exp), ExpressionStmt(exp2)))
		Typechecker(input)
		assert(theType.resolvedTyp == Some(IntType))
		assert(exp.expType == Some(IntType))
		assert(exp.typeEnv == Some(Map("x" -> (IntType, Immutable), "y" -> (IntType, Immutable))))
		assert(exp2.expType == Some(IntType))
		assert(exp2.typeEnv == Some(Map(("x" -> (IntType, Immutable)), ("y" -> (IntType, Immutable)))))

	}

	"the typechecker" should "handle a program with a single decimal integer" in {
		val exp = DecimalIntegerLiteralExp("10")
		val input = Program(List(ExpressionStmt(exp)))
		Typechecker(input)
		assert(exp.expType == Some(IntType))
	}

	"typeOfExp" should "handle a BooleanLiteralExp" in {
		val input = TrueLiteralExp()
		val expected = Some(BoolType)
		assertResult(expected) { Typechecker.typeOfExp(input, env, funcs) }
	}
	
	 "the typechecker" should "handle an TrueInfixExp: 1 + 2" in {
		val exp1 = DecimalIntegerLiteralExp("1")
		val exp2 = DecimalIntegerLiteralExp("2")
		val infixExp = TrueInfixExp(exp1, Operator("+"), exp2)
		val input = Program(List(ExpressionStmt(infixExp)))
		Typechecker(input)
		assert(exp1.expType == Some(IntType))
		assert(exp2.expType == Some(IntType))
		assert(infixExp.expType == Some(IntType))
	}
	
	 	"typecheckExp" should "handle an TrueInfixExp: 1 + 2" in {
		val exp1 = DecimalIntegerLiteralExp("1")
		val exp2 = DecimalIntegerLiteralExp("2")
		val input = TrueInfixExp(exp1, Operator("+"), exp2)
		Typechecker.typecheckExp(input, env, funcs)
		assert(exp1.expType == Some(IntType))
		assert(exp2.expType == Some(IntType))
		assert(input.expType == Some(IntType))
	}
	
	"typeOfBinopExp" should "handle + Int Int" in {
		val expected = Some(IntType)
		assertResult(expected) { Typechecker.typeOfBinopExp(Operator("+"), Some(IntType), Some(IntType)) }
	}

	"annotateExp" should "handle a TrueInfixExp: 1 + 2" in {
		val exp1 = DecimalIntegerLiteralExp("1")
		exp1.expType = Some(IntType)
		val exp2 = DecimalIntegerLiteralExp("2")
		exp2.expType = Some(IntType)
		val input = TrueInfixExp(exp1, Operator("+"), exp2)
		Typechecker.annotateExp(input, env, funcs)
		assert(input.expType == Some(IntType))
	}

	"typeOfExp" should "handle a TrueInfixExp: 1 + 2" in {
		val exp1 = DecimalIntegerLiteralExp("1")
		exp1.expType = Some(IntType)
		val exp2 = DecimalIntegerLiteralExp("2")
		exp2.expType = Some(IntType)
		val input = TrueInfixExp(exp1, Operator("+"), exp2)
		val expected = Some(IntType)
		assertResult(expected) {Typechecker.typeOfExp(input, env, funcs)}
	}

	"the typechecker" should "handle an TrueInfixExp: 1 + 2 - 3" in {
		val exp1 = DecimalIntegerLiteralExp("1")
		val subExp1 = DecimalIntegerLiteralExp("2")
		val subExp2 = DecimalIntegerLiteralExp("3")
		val exp2 = TrueInfixExp(subExp1, Operator("-"), subExp2)
		val input = Program(List(ExpressionStmt(TrueInfixExp(exp1, Operator("+"), exp2))))
		val expected = Some(IntType)
		Typechecker(input)
		assert(exp1.expType == Some(IntType))
		assert(subExp1.expType == Some(IntType))
		assert(subExp2. expType == Some(IntType))
		assert(exp2.expType == Some(IntType))
	}

	"annotateExpType" should "handle a BooleanLiteralExp" in {
		val input = TrueLiteralExp()
		Typechecker.annotateExp(input, env, funcs)
		assert(input.expType == Some(BoolType))
	}

	"annotateExpType" should "handle an expression we haven't added yet" in {
		val input = NilExp()
		Typechecker.annotateExp(input, env, funcs)
		assert(input.expType == None)
	}

	"typecheckExp" should "handle a BooleanLiteralExp" in {
		val input = TrueLiteralExp()
		Typechecker.typecheckExp(input, env, funcs)
		assert(input.expType == Some(BoolType))
	}

	"typecheckExp" should "handle a TryExp that contains BooleanLiteralExp" in {
		val input = TryExp(RegTryModifier, TrueLiteralExp())
		Typechecker.typecheckExp(input, env, funcs)
		input match {
			case TryExp(mod, exp) => assert(exp.expType == Some(BoolType))
			case _ => //nothing
		}
	}
}