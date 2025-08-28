package FuzzingSwift.parser

import org.scalatest.FlatSpec

class ParserTest extends FlatSpec {
	import FuzzingSwift.tokenizer._
	import FuzzingSwift.parser._
	
	//statement
	"statement" should "handle 5 + 1" in { //(expression_stmt)
		val input = List(DecimalIntegerLiteralToken("5"), InfixOperatorLiteralToken("+"), DecimalIntegerLiteralToken("1"))
		val prefix = DecimalIntegerLiteralExp("5")
		val op = Operator("+")
		val exp = DecimalIntegerLiteralExp("1")
		val expected = Stream(ExpressionStmt(TrueInfixExp(prefix, op, exp)))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle import name" in { //(declaration_stmt)
		val input = List(ImportToken, VariableToken("name"))
		val expected = Stream(DeclarationStmt(ImportDeclaration(None, None, RegularPath(ImportPathName("name")))))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle for _ in 5 { 0 }" in { //(loop_stmt)
		val input = List(ForToken, UnderscoreToken, InToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("5")
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(ForInStmt(None, pattern, exp, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle if 5 { 0 }" in {	//(branch_stmt)
		val input = List(IfToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(IfStmt(conditionList, codeBlock, None))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle labelName: switch 0 { }" in { //(labeled_stmt)
		val input = List(VariableToken("labelName"), ColonToken, SwitchToken, DecimalIntegerLiteralToken("0"), LeftCurlyToken, RightCurlyToken)
		val labeledStmt = SwitchStmt(DecimalIntegerLiteralExp("0"), None)
		val name = LabelName("labelName")
		val expected = Stream(LabeledStmt(name, labeledStmt))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle break" in {	//(control_transfer_stmt)
		val input = List(BreakToken)
		val expected = Stream(BreakStmt(None))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle defer { 0 }" in {	//(defer_stmt)
		val input = List(DeferToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(DeferStmt(codeBlock))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle do { 0 }" in { //(do_stmt)
		val input = List(DoToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(DoStmt(codeBlock, None))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	"statement" should "handle do { 0 } catch { 0 }" in {
		val input = List(DoToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, CatchToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val catchClauses = List(CatchClause(None, codeBlock))
		val expected = Stream(DoStmt(codeBlock, Some(catchClauses)))
		assertResult(expected) { Parser("test", Parser.statement, input) }
	}

	//loop_stmt
	//(for_in_stmt) thru loop_stmt
	"loop" should "handle for _ in 5 { 0 }" in {
		val input = List(ForToken, UnderscoreToken, InToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("5")
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(ForInStmt(None, pattern, exp, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.loop_stmt, input) }
	}

	//(while_stmt) thru loop_stmt
	"loop" should "handle while 5 { 0 }" in {
		val input = List(WhileToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(WhileStmt(conditionList, codeBlock))
		assertResult(expected) { Parser("test", Parser.loop_stmt, input) }
	}

	//(repeat_while_stmt) thru loop_stmt
	"loop" should "handle repeat { 0 } while 5" in {
		val input = List(RepeatToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, WhileToken, DecimalIntegerLiteralToken("5"))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val exp = DecimalIntegerLiteralExp("5")
		val expected = Stream(RepeatWhileStmt(codeBlock, exp))
		assertResult(expected) { Parser("test", Parser.loop_stmt, input) }
	}

	//for_in_stmt
	"for_in_stmt" should "handle for _ in 5 { 0 }" in {
		val input = List(ForToken, UnderscoreToken, InToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("5")
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(ForInStmt(None, pattern, exp, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.for_in_stmt, input) }
	}

	"for_in_stmt" should "handle for case _ in 5 { 0 }" in {
		val input = List(ForToken, CaseToken, UnderscoreToken, InToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("5")
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(ForInStmt(Some(CaseMod()), pattern, exp, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.for_in_stmt, input) }
	}

	"for_in_stmt" should "handle for case _ in 5 where 1 { 0 }" in {
		val input = List(ForToken, CaseToken, UnderscoreToken, InToken, DecimalIntegerLiteralToken("5"), WhereToken, DecimalIntegerLiteralToken("1"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("5")
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val whereClause = DecimalIntegerLiteralExp("1")
		val expected = Stream(ForInStmt(Some(CaseMod()), pattern, exp, Some(whereClause), codeBlock))
		assertResult(expected) { Parser("test", Parser.for_in_stmt, input) }
	}

	//while_stmt
	"while_stmt" should "handle while 5 { 0 }" in {
		val input = List(WhileToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(WhileStmt(conditionList, codeBlock))
		assertResult(expected) { Parser("test", Parser.while_stmt, input) }
	}

	"while_stmt" should "handle while 5, true { 0 }" in {
		val input = List(WhileToken, DecimalIntegerLiteralToken("5"), CommaToken, TrueToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")), ExpressionCondition(TrueLiteralExp()))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(WhileStmt(conditionList, codeBlock))
		assertResult(expected) { Parser("test", Parser.while_stmt, input) }
	}

	//condition
	"condition" should "handle 5" in {
		val input = List(DecimalIntegerLiteralToken("5"))
		val expected = Stream(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.condition, input) }
	}

	"condition" should "handle case _ = 0" in {
		val input = List(CaseToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("0"))
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("0")
		val expected = CaseCondition(pattern, exp)
		val result = Parser("test", Parser.condition, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.condition, input) }
	}

	"condition" should "handle let _" in {
		val input = List(LetToken, UnderscoreToken)
		val pattern = WildcardPattern(None)
		val expected = OptionalBindingConditionLet(pattern, None)
		val result = Parser("test", Parser.condition, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.condition, input) }
	}

	"condition" should "handle let _ = 0" in {
		val input = List(LetToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("0"))
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("0")
		val expected = OptionalBindingConditionLet(pattern, Some(exp))
		val result = Parser("test", Parser.condition, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.condition, input) }
	}

	"condition" should "handle var _" in {
		val input = List(VarToken, UnderscoreToken)
		val pattern = WildcardPattern(None)
		val expected = OptionalBindingConditionVar(pattern, None)
		val result = Parser("test", Parser.condition, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.condition, input) }
	}

	"condition" should "handle var _ = 0" in {
		val input = List(VarToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("0"))
		val pattern = WildcardPattern(None)
		val exp = DecimalIntegerLiteralExp("0")
		val expected = OptionalBindingConditionVar(pattern, Some(exp))
		val result = Parser("test", Parser.condition, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.condition, input) }
	}

	//repeat_while_stmt
	"repeat_while_stmt" should "handle repeat { 0 } while 5" in {
		val input = List(RepeatToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, WhileToken, DecimalIntegerLiteralToken("5"))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val exp = DecimalIntegerLiteralExp("5")
		val expected = Stream(RepeatWhileStmt(codeBlock, exp))
		assertResult(expected) { Parser("test", Parser.repeat_while_stmt, input) }
	}

	//branch
	//(if_stmt) thru branch
	"branch" should "handle if 5 { 0 }" in {
		val input = List(IfToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(IfStmt(conditionList, codeBlock, None))
		assertResult(expected) { Parser("test", Parser.branch_stmt, input) }
	}

	//(guard_stmt) thru branch
	"branch" should "handle guard 5 else { 0 }" in {
		val input = List(GuardToken, DecimalIntegerLiteralToken("5"), ElseToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(GuardStmt(conditionList, codeBlock))
		assertResult(expected) { Parser("test", Parser.branch_stmt, input) }
	}

	//(switch_stmt) thru branch
	"branch" should "handle switch 0 { }" in {
		val input = List(SwitchToken, DecimalIntegerLiteralToken("0"), LeftCurlyToken, RightCurlyToken)
		val expected = Stream(SwitchStmt(DecimalIntegerLiteralExp("0"), None))
		assertResult(expected) { Parser("test", Parser.branch_stmt, input) }
	}

	//if_stmt
	"if_stmt" should "handle if 5 { 0 }" in {
		val input = List(IfToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(IfStmt(conditionList, codeBlock, None))
		assertResult(expected) { Parser("test", Parser.if_stmt, input) }
	}

	"if_stmt" should "handle if 5 { 0 } else { 0 }" in {
		val input = List(IfToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, ElseToken,
						LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val elseClause = ElseCodeBlock(codeBlock)
		val expected = Stream(IfStmt(conditionList, codeBlock, Some(elseClause)))
		assertResult(expected) { Parser("test", Parser.if_stmt, input) }
	}

	//else_clause
	"else_clause" should "handle else { 0 }" in {
		val input = List(ElseToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(ElseCodeBlock(codeBlock))
		assertResult(expected) { Parser("test", Parser.else_clause, input) }
	}

	"else_clause" should "handle else if 5 { 0 }" in {
		val input = List(ElseToken, IfToken, DecimalIntegerLiteralToken("5"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val ifStmt = IfStmt(conditionList, codeBlock, None)
		val expected = Stream(ElseIfStmt(ifStmt))
		assertResult(expected) { Parser("test", Parser.else_clause, input) }
	}

	//guard_stmt
	"guard_stmt" should "handle guard 5 else { 0 }" in {
		val input = List(GuardToken, DecimalIntegerLiteralToken("5"), ElseToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val conditionList = List(ExpressionCondition(DecimalIntegerLiteralExp("5")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(GuardStmt(conditionList, codeBlock))
		assertResult(expected) { Parser("test", Parser.guard_stmt, input) }
	}

	//switch_stmt
	"switch_stmt" should "handle switch 0 { }" in {
		val input = List(SwitchToken, DecimalIntegerLiteralToken("0"), LeftCurlyToken, RightCurlyToken)
		val expected = Stream(SwitchStmt(DecimalIntegerLiteralExp("0"), None))
		assertResult(expected) { Parser("test", Parser.switch_stmt, input) }
	}

	"switch_stmt" should "handle switch 0 { case _ : 5 }" in {
		val input = List(SwitchToken, DecimalIntegerLiteralToken("0"), LeftCurlyToken, CaseToken, UnderscoreToken, ColonToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val caseItemList = List(CaseItem(WildcardPattern(None), None))
		val caseLabel = CaseLabel(None, caseItemList)
		val stmts = List(ExpressionStmt(DecimalIntegerLiteralExp("5")))
		val switchCases = List(CaseLabelStmts(caseLabel, stmts))
		val expected = Stream(SwitchStmt(DecimalIntegerLiteralExp("0"), Some(switchCases)))
		assertResult(expected) { Parser("test", Parser.switch_stmt, input) }
	}

	//switch_cases
	"switch_cases" should "handle case _ : 5 default : 5" in {
		val input = List(CaseToken, UnderscoreToken, ColonToken, DecimalIntegerLiteralToken("5"), DefaultToken, ColonToken, DecimalIntegerLiteralToken("5"))
		val caseItemList = List(CaseItem(WildcardPattern(None), None))
		val caseLabel = CaseLabel(None, caseItemList)
		val stmts = List(ExpressionStmt(DecimalIntegerLiteralExp("5")))
		val stmts1 = List(ExpressionStmt(DecimalIntegerLiteralExp("5")))
		val secondCase = DefaultLabelStmts(None, stmts1)
		val firstCase = CaseLabelStmts(caseLabel, stmts)
		val expected = Stream(List(firstCase, secondCase))
		assertResult(expected) { Parser("test", Parser.switch_cases, input) }
	}

	//switch_case
	"switch_case" should "handle case _ : 5" in {
		val input = List(CaseToken, UnderscoreToken, ColonToken, DecimalIntegerLiteralToken("5"))
		val caseItemList = List(CaseItem(WildcardPattern(None), None))
		val caseLabel = CaseLabel(None, caseItemList)
		val stmts = List(ExpressionStmt(DecimalIntegerLiteralExp("5")))
		val expected = Stream(CaseLabelStmts(caseLabel, stmts))
		assertResult(expected) { Parser("test", Parser.switch_case, input) }
	}

	"switch_case" should "handle default : 5" in {
		val input = List(DefaultToken, ColonToken, DecimalIntegerLiteralToken("5"))
		val stmts = List(ExpressionStmt(DecimalIntegerLiteralExp("5")))
		val expected = Stream(DefaultLabelStmts(None, stmts))
		assertResult(expected) { Parser("test", Parser.switch_case, input) }
	}

	"switch_case" should "handle @name default : 5" in {
		val input = List(AtToken, VariableToken("name"), DefaultToken, ColonToken, DecimalIntegerLiteralToken("5"))
		val stmts = List(ExpressionStmt(DecimalIntegerLiteralExp("5")))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(DefaultLabelStmts(Some(attributes), stmts))
		assertResult(expected) { Parser("test", Parser.switch_case, input) }
	}

	//case_label
	"case_label" should "handle case _ :" in {
		val input = List(CaseToken, UnderscoreToken, ColonToken)
		val caseItemList = List(CaseItem(WildcardPattern(None), None))
		val expected = Stream(CaseLabel(None, caseItemList))
		assertResult(expected) { Parser("test", Parser.case_label, input) }
	}

	"case_label" should "handle @name case _ :" in {
		val input = List(AtToken, VariableToken("name"), CaseToken, UnderscoreToken, ColonToken)
		val caseItemList = List(CaseItem(WildcardPattern(None), None))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(CaseLabel(Some(attributes), caseItemList))
		assertResult(expected) { Parser("test", Parser.case_label, input) }
	}

	//case_item_list
	"case_item_list" should "handle _ where 5, _" in {
		val input = List(UnderscoreToken, WhereToken, DecimalIntegerLiteralToken("5"), CommaToken, UnderscoreToken)
		val expected = Stream(List(CaseItem(WildcardPattern(None), Some(DecimalIntegerLiteralExp("5"))), CaseItem(WildcardPattern(None), None)))
		assertResult(expected) { Parser("test", Parser.case_item_list, input) }
	}

	//case_item
	"case_item" should "handle _" in {
		val input = List(UnderscoreToken)
		val expected = CaseItem(WildcardPattern(None), None)
		val result = Parser("test", Parser.case_item, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.case_item, input) }
	}

	"case_item" should "handle _ where 5" in {
		val input = List(UnderscoreToken, WhereToken, DecimalIntegerLiteralToken("5"))
		val expected = CaseItem(WildcardPattern(None), Some(DecimalIntegerLiteralExp("5")))
		val result = Parser("test", Parser.case_item, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.case_item, input) }
	}

	//control_stmt
	"control_stmt" should "handle break" in {
		val input = List(BreakToken)
		val expected = Stream(BreakStmt(None))
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	"control_stmt" should "handle break labelName" in {
		val input = List(BreakToken, VariableToken("labelName"))
		val labelName = LabelName("labelName")
		val expected = Stream(BreakStmt(Some(labelName)))
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	"control_stmt" should "handle continue" in {
		val input = List(ContinueToken)
		val expected = Stream(ContinueStmt(None))
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	"control_stmt" should "handle continue labelName" in {
		val input = List(ContinueToken, VariableToken("labelName"))
		val labelName = LabelName("labelName")
		val expected = Stream(ContinueStmt(Some(labelName)))
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	"control_stmt" should "handle fallthrough" in {
		val input = List(FallthroughToken)
		val expected = Stream(FallthroughStmt)
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	"control_stmt" should "handle return" in {
		val input = List(ReturnToken)
		val expected = Stream(ReturnStmt(None))
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	"control_stmt" should "handle return 5" in {
		val input = List(ReturnToken, DecimalIntegerLiteralToken("5"))
		val expected = Stream(ReturnStmt(Some(DecimalIntegerLiteralExp("5"))))
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	"control_stmt" should "handle throw 5" in {
		val input = List(ThrowToken, DecimalIntegerLiteralToken("5"))
		val expected = Stream(ThrowStmt(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.control_stmt, input) }
	}

	//catch_clause
	"catch_clause" should "handle catch { 0 }" in {
		val input = List(CatchToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = CatchClause(None, codeBlock)
		val result = Parser("test", Parser.catch_clause, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.catch_clause, input) }
	}

	"catch_clause" should "handle catch _ { 0 }" in {
		val input = List(CatchToken, UnderscoreToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val thePattern = WildcardPattern(None)
		val patternList = List(CatchPattern(thePattern, None))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(CatchClause(Some(patternList), codeBlock))
		assertResult(expected) { Parser("test", Parser.catch_clause, input) }
	}

	//catch_pattern
	"catch_pattern" should "handle _" in {
		val input = List(UnderscoreToken)
		val thePattern = WildcardPattern(None)
		val expected = CatchPattern(thePattern, None)
		val result = Parser("test", Parser.catch_pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.catch_pattern, input) }
	}

	"catch_pattern" should "handle _ where 5" in {
		val input = List(UnderscoreToken, WhereToken, DecimalIntegerLiteralToken("5"))
		val thePattern = WildcardPattern(None)
		val expected = CatchPattern(thePattern, Some(DecimalIntegerLiteralExp("5")))
		val result = Parser("test", Parser.catch_pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.catch_pattern, input) }
	}
	//end testing for statements

	//expression
	"expression" should "handle 5 + 1" in {
		val input = List(DecimalIntegerLiteralToken("5"), InfixOperatorLiteralToken("+"), DecimalIntegerLiteralToken("1"))
		val prefix = DecimalIntegerLiteralExp("5")
		val op = Operator("+")
		val exp = DecimalIntegerLiteralExp("1")
		val expected = Stream(TrueInfixExp(prefix, op, exp))
		assertResult(expected) { Parser("test", Parser.expression, input) }
	}

	"expression" should "handle 5 + 1 - 2" in {
		val input = List(DecimalIntegerLiteralToken("5"), InfixOperatorLiteralToken("+"), DecimalIntegerLiteralToken("1"), InfixOperatorLiteralToken("-"), DecimalIntegerLiteralToken("2"))
		val prefix = DecimalIntegerLiteralExp("5")
		val op = Operator("+")
		val exp = TrueInfixExp(DecimalIntegerLiteralExp("1"), Operator("-"), DecimalIntegerLiteralExp("2"))
		val expected = Stream(TrueInfixExp(prefix, op, exp))
		assertResult(expected) { Parser("test", Parser.expression, input) }
	}

	//in_out_expression
	"in_out_expression" should "handle &name" in {
		val input = List(PrefixOperatorLiteralToken("&"), VariableToken("name"))
		val expected = Stream(InOutExp(VariableExp("name")))
		assertResult(expected) { Parser("test", Parser.in_out_expression, input) }

	}

	//postfix_expression
	//(primary_expression) thru postfix_expression
	"postfix_expression" should "handle a custom op postfix expression: name?!" in {
		val input = List(VariableToken("name"), PostfixOperatorLiteralToken("?!"))
		val expected = Stream(PostfixWithOpExp(VariableExp("name"), Operator("?!")))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle a VariableExp identifier" in {
		val input = List(VariableToken("variableName"))
		assertResult(Stream(VariableExp("variableName"))) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle a forced value expression: name!" in {
		val input = List(VariableToken("name"), PostfixOperatorLiteralToken("!"))
		val expected = PostfixForcedValueExp(VariableExp("name"))
		val result = Parser("test", Parser.postfix_expression, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an optional chaining expression: name?" in {
		val input = List(VariableToken("name"), PostfixOperatorLiteralToken("?"))
		val expected = PostfixOptionalChainingExp(VariableExp("name"))
		val result = Parser("test", Parser.postfix_expression, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle a function call exp: name(5)" in {
		val input = List(VariableToken("name"), LeftParenToken, DecimalIntegerLiteralToken("5"), RightParenToken)
		val list: List[FunctionCallArgument] = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("5")))
		val call = SimpleFunctionCall(list)
		val expected = Stream(PostfixFunctionCallExp(VariableExp("name"), call))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle a subscript exp: name[5]" in {
		val input = List(VariableToken("name"), LeftBracketToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list: List[FunctionCallArgument] = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("5")))
		val expected = Stream(PostfixSubscriptExp(VariableExp("name"), list))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an initializer exp: name.init" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), InitToken)
		val expected = Stream(PostfixInitializerExp(VariableExp("name"), None))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an initializer exp: name.init(x:)" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), InitToken, LeftParenToken, VariableToken("x"), ColonToken, RightParenToken)
		val list = List(ArgumentName("x"))
		val expected = Stream(PostfixInitializerExp(VariableExp("name"), Some(list)))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an initializer exp: name.init(x: y:)" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), InitToken, LeftParenToken, VariableToken("x"), ColonToken, VariableToken("y"), ColonToken, RightParenToken)
		val list = List(ArgumentName("x"), ArgumentName("y"))
		val expected = Stream(PostfixInitializerExp(VariableExp("name"), Some(list)))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle a self exp: name.self" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), SelfToken)
		val expected = Stream(PostfixSelfExp(VariableExp("name")))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an explicit member expression: name.2" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), DecimalIntegerLiteralToken("2"))
		val member = ExplicitMemberDecimalDigits("2")
		val expected = Stream(PostfixExplicitMemberExp(VariableExp("name"), member))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an explicit member expression: name.identifier" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), VariableToken("identifier"))
		val member = ExplicitMemberIdentifierOptGeneric(ExplicitMemberName("identifier"), None)
		val expected = Stream(PostfixExplicitMemberExp(VariableExp("name"), member))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an explicit member expression: name.identifier<Int>" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), VariableToken("identifier"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"))
		val clause = List(NormalTypeIdentifier(TypeName("Int")))
		val member = ExplicitMemberIdentifierOptGeneric(ExplicitMemberName("identifier"), Some(clause))
		val expected = Stream(PostfixExplicitMemberExp(VariableExp("name"), member))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"postfix_expression" should "handle an explicit member expression: name.identifier(x:)" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), VariableToken("identifier"), LeftParenToken, VariableToken("x"), ColonToken, RightParenToken)
		val args = List(ArgumentName("x"))
		val member = ExplicitMemberIdentifierArgs(ExplicitMemberName("identifier"), args)
		val expected = Stream(PostfixExplicitMemberExp(VariableExp("name"), member))
		assertResult(expected) { Parser("test", Parser.postfix_expression, input) }
	}

	"after_postfix_function_call" should "handle: a function arg clause: (5)" in {
		val input = List(LeftParenToken, DecimalIntegerLiteralToken("5"), RightParenToken)
		val list: List[FunctionCallArgument] = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("5")))
		val expected = Stream(SimpleFunctionCall(list))
		assertResult(expected) { Parser("test", Parser.after_postfix_function_call, input) }
	}

 	"after_postfix_function_call" should "handle: a function arg clause: (5) {} x: {}" in {
		val input = List(LeftParenToken, DecimalIntegerLiteralToken("5"), RightParenToken, LeftCurlyToken, RightCurlyToken, VariableToken("x"), ColonToken, LeftCurlyToken, RightCurlyToken)
		val list: List[FunctionCallArgument] = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("5")))
		val list1: List[LabeledTrailingClosure] = List(LabeledTrailingClosure(TrailingClosureLabel("x"), ClosureExp(None, None, None)))
		val trailing = TrailingClosure(ClosureExp(None, None, None), Some(list1))
		val expected = Stream(ComplexFunctionCall(Some(list), trailing))
		assertResult(expected) { Parser("test", Parser.after_postfix_function_call, input) }
	}

 	"after_postfix_function_call" should "handle {} x: {}" in {
		val input = List(LeftCurlyToken, RightCurlyToken, VariableToken("x"), ColonToken, LeftCurlyToken, RightCurlyToken)
		val list: List[LabeledTrailingClosure] = List(LabeledTrailingClosure(TrailingClosureLabel("x"), ClosureExp(None, None, None)))
		val trailing = TrailingClosure(ClosureExp(None, None, None), Some(list))
		val expected = Stream(ComplexFunctionCall(None, trailing))
		assertResult(expected) { Parser("test", Parser.after_postfix_function_call, input) }
	}

	"trailing closure" should "handle {} x: {}" in {
		val input = List(LeftCurlyToken, RightCurlyToken, VariableToken("x"), ColonToken, LeftCurlyToken, RightCurlyToken)
		val list: List[LabeledTrailingClosure] = List(LabeledTrailingClosure(TrailingClosureLabel("x"), ClosureExp(None, None, None)))
		val expected = Stream(TrailingClosure(ClosureExp(None, None, None), Some(list)))
		assertResult(expected) { Parser("test", Parser.trailing_closures, input) }
	}

	"labeled_trailing_closure" should "handle x : {}" in {
		val input = List(VariableToken("x"), ColonToken, LeftCurlyToken, RightCurlyToken)
		val expected = Stream(LabeledTrailingClosure(TrailingClosureLabel("x"), ClosureExp(None, None, None)))
		assertResult(expected) { Parser("test", Parser.labeled_trailing_closure, input) }
	}



	//primary_expression
	"primary_expression" should "handle an identifier followed by a list of generic types: name<Int>" in {
		val input = List(VariableToken("name"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"))
		val typeList = List(NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(GenericExp(GenericName("name"), typeList))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	"primary_expression" should "handle an identifier followed by a list of generic types: name<Int, String>" in {
		val input = List(VariableToken("name"), InfixOperatorLiteralToken("<"), VariableToken("Int"), CommaToken, VariableToken("String"), PostfixOperatorLiteralToken(">"))
		val typeList = List(NormalTypeIdentifier(TypeName("Int")), NormalTypeIdentifier(TypeName("String")))
		val expected = Stream(GenericExp(GenericName("name"), typeList))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	//(identifier) thru primary_expression
	"primary_expression" should "handle a VariableExp identifier" in {
		val input = List(VariableToken("variableName"))
		assertResult(Stream(VariableExp("variableName"))) { Parser("test", Parser.primary_expression, input) }
	}

	//(literal_expression) thru primary_expression
	"primary_expression" should "handle an unsigned decimal integer literal token: 23" in {
		val input = List(DecimalIntegerLiteralToken("23"))
		assertResult(Stream(DecimalIntegerLiteralExp("23"))) { Parser("test", Parser.primary_expression, input) }
	}

	//(self_expression) thru primary_expression
	"primary_expression" should "handle a single self expression" in {
		assertResult(Stream(SoloSelfExp())) { Parser("test", Parser.primary_expression, List(SelfToken)) }
	}


	//(superclass_expression) thru primary_expression
	"primary_expression" should "handle a super method expression: super.hello" in {
		val input = List(SuperToken, InfixDotOperatorLiteralToken("."), VariableToken("hello"))
		val expected = Stream(MethodSuperExp(SuperMethodName("hello")))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	//(closure_expression) thru primary_expression
	"primary_expression" should "handle {}" in {
		val input = List(LeftCurlyToken, RightCurlyToken)
		val expected = Stream(ClosureExp(None, None, None))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	//(parenthesized_expression) thru primary_expression
	"primary_expression" should "handle: (5)" in {
		val input = List(LeftParenToken, DecimalIntegerLiteralToken("5"), RightParenToken)
		val expected = Stream(ParenthesizedExp(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	//(tuple_expression) thru primary_expression
	"primary_expression" should "handle ()" in {
		val input = List(LeftParenToken, RightParenToken)
		val tupleList: List[TupleElement] = List()
		val expected = Stream(TupleExp(tupleList))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}


	//(implicit_member_expression) thru primary expression
	"primary_expression" should "handle .x" in {
		val input = List(PrefixDotOperatorLiteralToken("."), VariableToken("x"))
		val expected = Stream(IdentifierImplicitMemberExp(ImplicitMemberName("x")))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	//(wilcard_expression) thru primary_expression
	"primary_expression" should "handle _" in {
		assertResult(Stream(WildcardExp())) { Parser("test", Parser.primary_expression, List(UnderscoreToken)) }
	}

	//(key_path_expression) thru primary_expression
	"primary_expression" should "handle \\.name" in {
		val input = List(BackSlashToken, InfixDotOperatorLiteralToken("."), VariableToken("name"))
		val list: List[KeyPathComponent] = List(IdentifierThenOptPostfixesKPC(KeyPathComponentName("name"), None))
		val expected = Stream(KeyPathExp(None, list))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	//(selctor_expression) thru primary_expression
	"primary_expression" should "handle #selector(1)" in {
		val input = List(HashSelectorToken, LeftParenToken, DecimalIntegerLiteralToken("1"), RightParenToken)
		val expected = Stream(SelectorExp(DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}


	//(key_path_string_expression) thru primary expression
	"primary_expression" should "handle #keyPath(1)" in {
		val input = List(HashKeyPathToken, LeftParenToken, DecimalIntegerLiteralToken("1"), RightParenToken)
		val expected = Stream(KeyPathStringExp(DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}


	//identifier
	"identifier" should "handle a VariableExp identifier" in {
		val input = List(VariableToken("variableName"))
		assertResult(Stream(VariableExp("variableName"))) { Parser("test", Parser.identifier, input) }
	}

	"identifier" should "handle an implicit parameter OR property wrapper projction identifier: $4" in {
		val input = List(ImplicitParameterOrPropertyWrapperProjectionToken("$4"))
		val expected = Stream(ImplicitParameterExpOrPWP("$4"))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	"identifier" should "handle a property wrapper projection identifier: $abc" in {
		val input = List(PropertyWrapperProjectionToken("$abc"))
		val expected = Stream(PropertyWrapperProjectionExp("$abc"))
		assertResult(expected) { Parser("test", Parser.primary_expression, input) }
	}

	//literal_expression
	//(literal) thru literal_expression
	"literal_expression" should "handle an unsigned decimal integer literal token: 23" in {
		val input = List(DecimalIntegerLiteralToken("23"))
		assertResult(Stream(DecimalIntegerLiteralExp("23"))) { Parser("test", Parser.literal_expression, input) }
	}

	//(array_literal) thru literal_expression
	"literal_expression" should "handle an array literal []" in {
		val input = List(LeftBracketToken, RightBracketToken)
		val emptyList: List[Exp] = List()
		val expected = Stream(ArrayLiteralExp(emptyList))
		assertResult(Stream(ArrayLiteralExp(emptyList))) { Parser("test", Parser.literal_expression, input) }
	}


	//(dictionary_literal) thru literal_expression
	"literal_expression" should "handle an empty dictionary literal: [:]" in {
		val input = List(LeftBracketToken, ColonToken, RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List()
		val expected = Stream(DictionaryLiteralExp(dictionaryList))
		assertResult(expected) { Parser("test", Parser.literal_expression, input) }
	}


	//(playground_literal) thrue literal_expression
	"literal_expression" should "handle a colorLiteral playground literal" in {
		val input = List(HashColorLiteralToken, LeftParenToken, RedToken, ColonToken, DecimalIntegerLiteralToken("0"), CommaToken, GreenToken, ColonToken, DecimalIntegerLiteralToken("1"), CommaToken, BlueToken, ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, AlphaToken, ColonToken, DecimalIntegerLiteralToken("3"), RightParenToken)
		val expected = Stream(ColorPlaygroundLiteralExp(DecimalIntegerLiteralExp("0"), DecimalIntegerLiteralExp("1"), DecimalIntegerLiteralExp("2"), DecimalIntegerLiteralExp("3")))
		assertResult(expected) { Parser("test", Parser.literal_expression, input) }
	}

	//all the other parsers inside of literal_expression
/* 	"literal_expression" should "handle a HashFileToken and return a HashFileExp" in {
		assertResult(HashFileExp) { Parser("test", Parser.literal_expression, Seq(HashFileToken)) }
	}

	"literal_expression" should "handle a HashFileIDToken and return a HashFileIDExp" in {
		assertResult(HashFileIDExp) { Parser("test", Parser.literal_expression, Seq(HashFileIDToken)) }
	}

	"literal_expression" should "handle a HashFilePathToken and return a HashFilePathExp" in {
		assertResult(HashFilePathExp) { Parser("test", Parser.literal_expression, Seq(HashFilePathToken)) }
	}

	"literal_expression" should "handle a HashLineToken and return a HashLineExp" in {
		assertResult(HashLineExp) { Parser("test", Parser.literal_expression, Seq(HashLineToken)) }
	}

	"literal_expression" should "handle a HashColumnToken and return a HashColumnExp" in {
		assertResult(HashColumnExp) { Parser("test", Parser.literal_expression, Seq(HashColumnToken)) }
	}

	"literal_expression" should "handle a HashFunctionToken and return a HashFunctionExp" in {
		assertResult(HashFunctionExp) { Parser("test", Parser.literal_expression, Seq(HashFunctionToken)) }
	}

	"literal_expression" should "handle a HashDSOHandleToken and return a HashDSOHandleExp" in {
		assertResult(HashDSOHandleExp) { Parser("test", Parser.literal_expression, Seq(HashDSOHandleToken)) }
	} */

	//self_expression
	"self_expression" should "handle a single self expression" in {
		assertResult(Stream(SoloSelfExp())) { Parser("test", Parser.self_expression, List(SelfToken)) }
	}

	"self_expression" should "handle a self method expression: self.hello" in {
		val input = List(SelfToken, InfixDotOperatorLiteralToken("."), VariableToken("hello"))
		val expected = Stream(MethodSelfExp(SelfMethodName("hello")))
		assertResult(expected) { Parser("test", Parser.self_expression, input) }
	}

	"self_expression" should "handle a self subscript expression: self [2]" in {
		val input = List(SelfToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("2")))
		val expected = Stream(SubscriptSelfExp(list))
		assertResult(expected) { Parser("test", Parser.self_expression, input) }
	}

	"self_expression" should "handle a self subscript expression: self [name : 5]" in {
		val input = List(SelfToken, LeftBracketToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(IdentifierColonExpFunctionCallArgument(FunctionCallArgName("name"), DecimalIntegerLiteralExp("5")))
		val expected = Stream(SubscriptSelfExp(list))
		assertResult(expected) { Parser("test", Parser.self_expression, input) }
	}

	"self_expression" should "handle a self subscript expression: self [2, name : 5]" in {
		val input = List(SelfToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), CommaToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("2")) , IdentifierColonExpFunctionCallArgument(FunctionCallArgName("name"), DecimalIntegerLiteralExp("5")))
		val expected = Stream(SubscriptSelfExp(list))
		assertResult(expected) { Parser("test", Parser.self_expression, input) }
	}

	"self_expression" should "handle a self init expression" in {
		assertResult(Stream(InitSelfExp())) { Parser("test", Parser.self_expression, List(SelfToken, InfixDotOperatorLiteralToken("."), InitToken)) }
	}

	//superclass_expression
	"superclass_expression" should "handle a super method expression: super.hello" in {
		val input = List(SuperToken, InfixDotOperatorLiteralToken("."), VariableToken("hello"))
		val expected = Stream(MethodSuperExp(SuperMethodName("hello")))
		assertResult(expected) { Parser("test", Parser.superclass_expression, input) }
	}

	"superclass_expression" should "handle a super init expression" in {
		assertResult(Stream(InitSuperExp())) { Parser("test", Parser.superclass_expression, List(SuperToken, InfixDotOperatorLiteralToken("."), InitToken)) }
	}

	"superclass_expression" should "handle a super subscript expression: super [2, name : 5]" in {
		val input = List(SuperToken, LeftBracketToken, DecimalIntegerLiteralToken("2"), CommaToken, VariableToken("name"), ColonToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("2")) , IdentifierColonExpFunctionCallArgument(FunctionCallArgName("name"), DecimalIntegerLiteralExp("5")))
		val expected = Stream(SubscriptSuperExp(list))
		assertResult(expected) { Parser("test", Parser.superclass_expression, input) }
	}

	//closure_expression
	"closure_expression" should "handle {}" in {
		val input = List(LeftCurlyToken, RightCurlyToken)
		val expected = Stream(ClosureExp(None, None, None))
		assertResult(expected) { Parser("test", Parser.closure_expression, input) }
	}

	"closure_expression" should "handle { @name }" in {
		val input = List(LeftCurlyToken, AtToken, VariableToken("name"), RightCurlyToken)
		//val emptyList: List[BalancedToken] = List()
		val attributeList = List(Attribute(AttributeName("name"), None))
		val expected = Stream(ClosureExp(Some(attributeList), None, None))
		assertResult(expected) { Parser("test", Parser.closure_expression, input) }
	}

	"closure_expression" should "handle {  }" in {
		val input = List(LeftCurlyToken, RightCurlyToken)
		val expected = Stream(ClosureExp(None, None, None))
		assertResult(expected) { Parser("test", Parser.closure_expression, input) }
	}

	"closure_expression" should "handle { attributes }" in {
		val input = List(LeftCurlyToken, AtToken, VariableToken("name"), RightCurlyToken)
		//val emptyBTList: List[BalancedToken] = List()
		val theAttribute = Attribute(AttributeName("name"), None)
		val expected = Stream(ClosureExp(Some(List(theAttribute)), None, None))
		assertResult(expected) { Parser("test", Parser.closure_expression, input) }
	}

	"closure_expression" should "handle { closure-signature } which is { () in }" in {
		val input = List(LeftCurlyToken, LeftParenToken, RightParenToken, InToken, RightCurlyToken)
		val emptyList: List[ClosureParameter] = List()
		val cpc = CPCClosureParameterList(emptyList)
		val closureSig = (ClosureSignatureComplex(None, cpc, None, None, None))
		val expected = Stream(ClosureExp(None, Some(closureSig), None))
		assertResult(expected) { Parser("test", Parser.closure_expression, input) }
	}

	"closure_expression" should "handle { statements }" in {
		val input = List(LeftCurlyToken, DecimalIntegerLiteralToken("1"), RightCurlyToken)
		val stmt = ExpressionStmt(DecimalIntegerLiteralExp("1"))
		val expected = Stream(ClosureExp(None, None, Some(List(stmt))))
		assertResult(expected) { Parser("test", Parser.closure_expression, input) }
	}

 	"closure_expression" should "handle the combination of the above tests into { attributes closure-signature statements }" in {
		val input = List(LeftCurlyToken, AtToken, VariableToken("name"), LeftParenToken, RightParenToken, InToken, DecimalIntegerLiteralToken("1"), RightCurlyToken)
		val theAttribute = Attribute(AttributeName("name"), None)
		val emptyList: List[ClosureParameter] = List()
		val cpc = CPCClosureParameterList(emptyList)
		val closureSig = (ClosureSignatureComplex(None, cpc, None, None, None))
		val stmt = ExpressionStmt(DecimalIntegerLiteralExp("1"))
		val expected = Stream(ClosureExp(Some(List(theAttribute)), Some(closureSig), Some(List(stmt))))
		assertResult(expected) { Parser("test", Parser.closure_expression, input) }
	}

	//helper: closure_signature
	"closure_signature" should "handle the complex closure signature -> closure_parameter_clause in" in {
		val input = List(LeftParenToken, RightParenToken, InToken)
		val emptyList: List[ClosureParameter] = List()
		val cpc = CPCClosureParameterList(emptyList)
		val expected = Stream((ClosureSignatureComplex(None, cpc, None, None, None)))
		assertResult(expected) { Parser("test", Parser.closure_signature, input) }
	}

	"closure_signature" should "handle the complex closure signature -> capture-list closure_parameter_clause in" in {
		val input = List(LeftBracketToken, VariableToken("identifierName"), RightBracketToken, LeftParenToken, RightParenToken, InToken)
		val emptyList: List[ClosureParameter] = List()
		val cpc = CPCClosureParameterList(emptyList)
		val list: List[CaptureListItem] = List(CaptureListItemIdentifier(None, CaptureListItemName("identifierName")))
		val expected = Stream((ClosureSignatureComplex(Some(list), cpc, None, None, None)))
		assertResult(expected) { Parser("test", Parser.closure_signature, input) }
	}

	"closure_signature" should "handle the complex closure signature -> capture-list closure_parameter_clause async in" in {
		val input = List(LeftBracketToken, VariableToken("identifierName"), RightBracketToken, LeftParenToken, RightParenToken, AsyncToken, InToken)
		val emptyList: List[ClosureParameter] = List()
		val cpc = CPCClosureParameterList(emptyList)
		val list: List[CaptureListItem] = List(CaptureListItemIdentifier(None, CaptureListItemName("identifierName")))
		val expected = Stream((ClosureSignatureComplex(Some(list), cpc, Some(AsyncMod()), None, None)))
		assertResult(expected) { Parser("test", Parser.closure_signature, input) }
	}

	"closure_signature" should "handle the complex closure signature -> capture-list closure_parameter_clause async throws in" in {
		val input = List(LeftBracketToken, VariableToken("identifierName"), RightBracketToken, LeftParenToken, RightParenToken, AsyncToken, ThrowsToken, InToken)
		val emptyList: List[ClosureParameter] = List()
		val cpc = CPCClosureParameterList(emptyList)
		val list: List[CaptureListItem] = List(CaptureListItemIdentifier(None, CaptureListItemName("identifierName")))
		val expected = Stream((ClosureSignatureComplex(Some(list), cpc, Some(AsyncMod()), Some(ThrowsMod()), None)))
		assertResult(expected) { Parser("test", Parser.closure_signature, input) }
	}

	"closure_signature" should "handle the complex closure signature -> capture-list closure_parameter_clause async throws function-result in" in {
		val input = List(LeftBracketToken, VariableToken("identifierName"), RightBracketToken, LeftParenToken, RightParenToken, AsyncToken, ThrowsToken, InfixOperatorLiteralToken("->"), VariableToken("Int"), InToken)
		val emptyList: List[ClosureParameter] = List()
		val cpc = CPCClosureParameterList(emptyList)
		val list: List[CaptureListItem] = List(CaptureListItemIdentifier(None, CaptureListItemName("identifierName")))
		val funcResult = FunctionResult(None, NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream((ClosureSignatureComplex(Some(list), cpc, Some(AsyncMod()), Some(ThrowsMod()), Some(funcResult))))
		assertResult(expected) { Parser("test", Parser.closure_signature, input) }
	}

	"closure-signature" should "handle the simple closure signature -> capture-list in" in {
		val input = List(LeftBracketToken, VariableToken("identifierName"), RightBracketToken, InToken)
		val list: List[CaptureListItem] = List(CaptureListItemIdentifier(None, CaptureListItemName("identifierName")))
		val expected = Stream(ClosureSignatureSimple(list))
		assertResult(expected) { Parser("test", Parser.closure_signature, input) }
	}

	//helper: capture_list
	"capture_list" should "handle [identifierName]" in {
		val input = List(LeftBracketToken, VariableToken("identifierName"), RightBracketToken)
		val expected = Stream(List(CaptureListItemIdentifier(None, CaptureListItemName("identifierName"))))
		assertResult(expected) { Parser("test", Parser.capture_list, input) }
	}

	"capture_list" should "handle [identifierName, weak self]" in {
		val input = List(LeftBracketToken, VariableToken("identifierName"), CommaToken, WeakToken, SelfToken, RightBracketToken)
		val expected = Stream(List(CaptureListItemIdentifier(None, CaptureListItemName("identifierName")),
											   CaptureListItemSelf(Some(WeakCaptureSpecifier), SoloSelfExp())))
		assertResult(expected) { Parser("test", Parser.capture_list, input) }
	}

	//helper: capture_list_item
	"capture_list_item" should "handle -> identifier" in {
		val input = List(VariableToken("identifier"))
		val expected = Stream(CaptureListItemIdentifier(None, CaptureListItemName("identifier")))
		assertResult(expected) { Parser("test", Parser.capture_list_item, input) }
	}

	"capture_list_item" should "handle -> capture-specifier identifier" in {
		val input = List(WeakToken, VariableToken("identifier"))
		val expected = Stream(CaptureListItemIdentifier(Some(WeakCaptureSpecifier), CaptureListItemName("identifier")))
		assertResult(expected) { Parser("test", Parser.capture_list_item, input) }
	}

	"capture_list_item" should "handle -> identifier = expression" in {
		val input = List(VariableToken("identifier"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("1"))
		val expected = Stream(CaptureListItemAssignment(None, CaptureListItemName("identifier"), DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.capture_list_item, input) }
	}

	"capture_list_item" should "handle -> capture-specifier identifier = expression" in {
		val input = List(WeakToken, VariableToken("identifier"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("1"))
		val expected = Stream(CaptureListItemAssignment(Some(WeakCaptureSpecifier), CaptureListItemName("identifier"), DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.capture_list_item, input) }
	}

	"capture_list_item" should "handle -> self-expression" in {
		val input = List(SelfToken)
		val expected = Stream(CaptureListItemSelf(None, SoloSelfExp()))
		assertResult(expected) { Parser("test", Parser.capture_list_item, input) }
	}

	"capture_list_item" should "handle -> capture-specifier self-expression" in {
		val input = List(WeakToken, SelfToken)
		val expected = Stream(CaptureListItemSelf(Some(WeakCaptureSpecifier), SoloSelfExp()))
		assertResult(expected) { Parser("test", Parser.capture_list_item, input) }
	}


	//helper: capture_specifier
	"capture_specifier" should "handle: weak" in {
		assertResult(Stream(WeakCaptureSpecifier)) { Parser("test", Parser.capture_specifier, List(WeakToken)) }
	}

	"capture_specifier" should "handle: unowned" in {
		assertResult(Stream(UnownedCaptureSpecifier)) { Parser("test", Parser.capture_specifier, List(UnownedToken)) }
	}

	"capture_specifier" should "handle: unowned(safe)" in {
		val input = List(UnownedToken, LeftParenToken, SafeToken, RightParenToken)
		assertResult(Stream(UnownedSafeCaptureSpecifier)) { Parser("test", Parser.capture_specifier, input) }
	}

	"capture_specifier" should "handle: unowned(unsafe)" in {
		val input = List(UnownedToken, LeftParenToken, UnsafeToken, RightParenToken)
		assertResult(Stream(UnownedUnsafeCaptureSpecifier)) { Parser("test", Parser.capture_specifier, input) }
	}


	//helper: closure_parameter_clause
	"closure_parameter_clause" should "handle: ()" in {
		val input = List(LeftParenToken, RightParenToken)
		val emptyList: List[ClosureParameter] = List()
		val expected = Stream(CPCClosureParameterList(emptyList))
		assertResult(expected) { Parser("test", Parser.closure_parameter_clause, input) }
	}

	"closure_parameter_clause" should "handle: (name)" in {
		val input = List(LeftParenToken, VariableToken("name"), RightParenToken)
		val list: List[ClosureParameter] = List(ClosureParameterReg(ClosureParamName("name"), None))
		val expected = Stream(CPCClosureParameterList(list))
		assertResult(expected) { Parser("test", Parser.closure_parameter_clause, input) }
	}

	"closure_parameter_clause" should "handle: (name, name: String)" in {
		val input = List(LeftParenToken, VariableToken("name"), CommaToken, VariableToken("name"), ColonToken, VariableToken("String"), RightParenToken)
		val list: List[ClosureParameter] = List(ClosureParameterReg(ClosureParamName("name"), None),
												ClosureParameterReg(ClosureParamName("name"),
												Some(TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String"))))))
		val expected = Stream(CPCClosureParameterList(list))
		assertResult(expected) { Parser("test", Parser.closure_parameter_clause, input) }
	}

	"closure_parameter_clause" should "handle an identifier list: name, theName" in {
		val input = List(VariableToken("name"), CommaToken, VariableToken("theName"))
		val idList = List("name", "theName")
		val expected = Stream(CPCIdentifierList(idList))
		assertResult(expected) { Parser("test", Parser.closure_parameter_clause, input) }
	}

	//helper: closure_parameter
	"closure_parameter" should "handle -> identifier: name" in {
		val input = List(VariableToken("name"))
		val expected = Stream(ClosureParameterReg(ClosureParamName("name"), None))
		assertResult(expected) { Parser("test", Parser.closure_parameter, input) }
	}

	"closure_parameter" should "handle name: String" in {
		val input = List(VariableToken("name"), ColonToken, VariableToken("String"))
		val expected = Stream(ClosureParameterReg(ClosureParamName("name"),
										   Some(TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String"))))))
		assertResult(expected) { Parser("test", Parser.closure_parameter, input) }
	}

	"closure_parameter" should "handle name: String..." in {
		val input = List(VariableToken("name"), ColonToken, VariableToken("String"), PostfixDotOperatorLiteralToken("..."))
		val expected = Stream(ClosureParameterElipses(ClosureParamName("name"),
											   TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String")))))
		assertResult(expected) { Parser("test", Parser.closure_parameter, input) }
	}

	//parenthesized_expression
	"parenthesized_expression" should "handle: (5)" in {
		val input = List(LeftParenToken, DecimalIntegerLiteralToken("5"), RightParenToken)
		val expected = Stream(ParenthesizedExp(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.parenthesized_expression, input) }
	}

	//tuple_expression
	"tuple_expression" should "handle ()" in {
		val input = List(LeftParenToken, RightParenToken)
		val tupleList: List[TupleElement] = List()
		val expected = Stream(TupleExp(tupleList))
		assertResult(expected) { Parser("test", Parser.tuple_expression, input) }
	}

	"tuple_expression" should "handle (expression, identifier:expression) which is (5, x:3)" in {
		val input = List(LeftParenToken, DecimalIntegerLiteralToken("5"), CommaToken, VariableToken("x"), ColonToken, DecimalIntegerLiteralToken("3"), RightParenToken)
		val tupleList: List[TupleElement] = List(ExpTuple(DecimalIntegerLiteralExp("5")),
												 IdentifierColonExpTuple(TupleElementName("x"), DecimalIntegerLiteralExp("3")))
		val expected = Stream(TupleExp(tupleList))
		assertResult(expected) { Parser("test", Parser.tuple_expression, input) }
	}

	//helper: tuple_element
	"tuple_element" should "handle -> expression which is 5" in {
		val input = List(DecimalIntegerLiteralToken("5"))
		val expected = Stream(ExpTuple(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.tuple_element, input) }
	}

	"tuple_element" should "handle -> expression which is x:3" in {
		val input = List(VariableToken("x"), ColonToken, DecimalIntegerLiteralToken("3"))
		val expected = Stream(IdentifierColonExpTuple(TupleElementName("x"), DecimalIntegerLiteralExp("3")))
		assertResult(expected) { Parser("test", Parser.tuple_element, input) }
	}

	//implicit_member_expression
	"implicit_member_expression" should "handle .x" in {
		val input = List(PrefixDotOperatorLiteralToken("."), VariableToken("x"))
		val expected = Stream(IdentifierImplicitMemberExp(ImplicitMemberName("x")))
		assertResult(expected) { Parser("test", Parser.implicit_member_expression, input) }
	}

	"implicit_member_expression" should "handle .x.5" in {
		val input = List(PrefixDotOperatorLiteralToken("."), VariableToken("x"), InfixDotOperatorLiteralToken("."), DecimalIntegerLiteralToken("5"))
		val expected = Stream(IdentifierDotPostfixImplicitMemberExp(ImplicitMemberName("x"),
										 DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.implicit_member_expression, input) }
	}

	//wildcard_expression
	"wildcard_expression" should "handle _" in {
		assertResult(Stream(WildcardExp())) { Parser("test", Parser.wildcard_expression, List(UnderscoreToken)) }
	}

	//key_path_expression
	"key_path_expression" should "handle \\.name" in {
		val input = List(BackSlashToken, InfixDotOperatorLiteralToken("."), VariableToken("name"))
		val list: List[KeyPathComponent] = List(IdentifierThenOptPostfixesKPC(KeyPathComponentName("name"), None))
		val expected = Stream(KeyPathExp(None, list))
		assertResult(expected) { Parser("test", Parser.key_path_expression, input) }
	}

	"key_path_expression" should "handle \\.name?" in {
		val input = List(BackSlashToken, InfixDotOperatorLiteralToken("."), VariableToken("name"), PostfixOperatorLiteralToken("?"))
		val list: List[KeyPathComponent] = List(IdentifierThenOptPostfixesKPC(KeyPathComponentName("name"), Some(List(QuestionKPP))))
		val expected = Stream(KeyPathExp(None, list))
		assertResult(expected) { Parser("test", Parser.key_path_expression, input) }
	}

	//helper: key_path_component
	"key_path_component" should "handle name" in {
		val input = List(VariableToken("name"))
		val expected = Stream(IdentifierThenOptPostfixesKPC(KeyPathComponentName("name"), None))
		assertResult(expected) { Parser("test", Parser.key_path_component, input) }
	}

	//UHHHHHHHHHHHHHHHHHHHHHHHHHHH
/* 	"key_path_component" should "handle name ? !" in {
		val input = List(VariableToken("name"), InfixOperatorLiteralToken("?"), PostfixOperatorLiteralToken("!"))
		val expected = IdentifierThenOptPostfixesKPC(VariableExp("name"), Some(List(QuestionKPP, ExclamationKPP)))
		assertResult(expected) { Parser("test", Parser.key_path_component, input) }
	} */

/* 	"key_path_component" should "handle ? !" in {
		val input = List(OperatorLiteralToken("?"), OperatorLiteralToken("!"))
		val expected = PostfixesKPC(List(QuestionKPP, ExclamationKPP))
		assertResult(expected) { Parser("test", Parser.key_path_component, input) }
	} */

	//helper: key_path_postfixes
/* 	"key_path_postfixes" should "handle ? !" in {
		val input = List(OperatorLiteralToken("?"), OperatorLiteralToken("!"))
		val expected: List[KeyPathPostfix] = List(QuestionKPP, ExclamationKPP)
		assertResult(expected) { Parser("test", Parser.key_path_postfixes, input) }
	} */

	//helper: key_path_postfix
	"key_path_postfix" should "handle ?" in {
		assertResult(Stream(QuestionKPP)) { Parser("test", Parser.key_path_postfix, List(InfixOperatorLiteralToken("?"))) }
	}

	"key_path_postfix" should "handle !" in {
		assertResult(Stream(ExclamationKPP)) { Parser("test", Parser.key_path_postfix, List(InfixOperatorLiteralToken("!"))) }
	}

	"key_path_postfix" should "handle self" in {
		assertResult(Stream(SelfKPP)) { Parser("test", Parser.key_path_postfix, List(SelfToken)) }
	}

	"key_path_postfix" should "handle a function call argument list: [ 5 ]" in {
		val input = List(LeftBracketToken, DecimalIntegerLiteralToken("5"), RightBracketToken)
		val list: List[FunctionCallArgument] = List(ExpFunctionCallArgument(DecimalIntegerLiteralExp("5")))
		val expected = Stream(FuncCallArgListKPP(list))
		assertResult(expected) { Parser("test", Parser.key_path_postfix, input) }
	}

	//selector_expression
	"selector_expression" should "handle #selector(1)" in {
		val input = List(HashSelectorToken, LeftParenToken, DecimalIntegerLiteralToken("1"), RightParenToken)
		val expected = Stream(SelectorExp(DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.selector_expression, input) }
	}

	"selector_expression" should "handle #selector(getter: 1)" in {
		val input = List(HashSelectorToken, LeftParenToken, GetterToken, ColonToken, DecimalIntegerLiteralToken("1"), RightParenToken)
		val expected = Stream(SelectorGetterExp(DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.selector_expression, input) }
	}

	"selector_expression" should "handle #selector(setter: 1)" in {
		val input = List(HashSelectorToken, LeftParenToken, SetterToken, ColonToken, DecimalIntegerLiteralToken("1"), RightParenToken)
		val expected = Stream(SelectorSetterExp(DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.selector_expression, input) }
	}

	//key_path_string_expression
	"key_path_string_expression" should "handle #keyPath(1)" in {
		val input = List(HashKeyPathToken, LeftParenToken, DecimalIntegerLiteralToken("1"), RightParenToken)
		val expected = Stream(KeyPathStringExp(DecimalIntegerLiteralExp("1")))
		assertResult(expected) { Parser("test", Parser.key_path_string_expression, input) }
	}


	//literal
	//(numeric_literal) thru literal
	"literal" should "handle an unsigned decimal integer literal token: 23" in {
		val input = List(DecimalIntegerLiteralToken("23"))
		assertResult(Stream(DecimalIntegerLiteralExp("23"))) { Parser("test", Parser.literal, input) }
	}

	//(string_literal) thru literal
	"literal" should "handle a single line string literal token: hello" in {
		val input = List(SingleLineStringLiteralToken("hello"))
		assertResult(Stream(SingleLineStaticStringLiteralExp("hello"))) { Parser("test", Parser.literal, input) }
	}

	//(boolean_literal) thru literal
	"literal" should "handle a boolean literal true token" in {
		val input = List(TrueToken)
		assertResult(Stream(TrueLiteralExp())) { Parser("test", Parser.literal, input) }
	}

	//(nil_literal) thru literal
	"literal" should "handle a nil token" in {
		val input = List(NilToken)
		assertResult(Stream(NilExp())) { Parser("test", Parser.literal, input) }
	}


	//array_literal
	"array_literal" should "handle an array literal []" in {
		val input = List(LeftBracketToken, RightBracketToken)
		val emptyList: List[Exp] = List()
		val expected = Stream(ArrayLiteralExp(emptyList))
		assertResult(Stream(ArrayLiteralExp(emptyList))) { Parser("test", Parser.array_literal, input) }
	}

	"array_literal" should "handle an array literal [1]" in {
		val input = List(LeftBracketToken, DecimalIntegerLiteralToken("1"), RightBracketToken)
		val expected = Stream(ArrayLiteralExp(List(DecimalIntegerLiteralExp("1"))))
		assertResult(expected) { Parser("test", Parser.array_literal, input) }
	}

	"array_literal" should "handle an array literal [1,]" in {
		val input = List(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, RightBracketToken)
		val expected = Stream(ArrayLiteralExp(List(DecimalIntegerLiteralExp("1"))))
		assertResult(expected) { Parser("test", Parser.array_literal, input) }
	}

	"array_literal" should "handle an array literal [1,2]" in {
		val input = List(LeftBracketToken, DecimalIntegerLiteralToken("1"), CommaToken, DecimalIntegerLiteralToken("2"), RightBracketToken)
		val expected = Stream(ArrayLiteralExp(List(DecimalIntegerLiteralExp("1"), DecimalIntegerLiteralExp("2"))))
		assertResult(expected) { Parser("test", Parser.array_literal, input) }
	}

	//helper: comma_sep_exps
	"comma_sep_exps" should "handle an empty list" in {
		val input: List[Token] = List()
		val emptyReturnList: List[Exp] = List()
		assertResult(Stream(emptyReturnList)) { Parser("test", Parser.comma_sep_exps, input) }
	}

	"comma_sep_exps" should "handle a list with one element" in {
		val input = List(DecimalIntegerLiteralToken("1"))
		assertResult(Stream(List(DecimalIntegerLiteralExp("1")))) { Parser("test", Parser.comma_sep_exps, input) }
	}

	"comma_sep_exps" should "handle a list with two elements" in {
		val input = List(DecimalIntegerLiteralToken("1"), CommaToken, DecimalIntegerLiteralToken("2"))
		val expected = Stream(List(DecimalIntegerLiteralExp("1"), DecimalIntegerLiteralExp("2")))
		assertResult(expected) { Parser("test", Parser.comma_sep_exps, input) }
	}

	//dictionary_literal
	"dictionary_literal" should "handle an empty dictionary literal: [:]" in {
		val input = List(LeftBracketToken, ColonToken, RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List()
		val expected = Stream(DictionaryLiteralExp(dictionaryList))
		assertResult(expected) { Parser("test", Parser.dictionary_literal, input) }
	}

 	"dictionary_literal" should "handle a dictionary literal: [1:2]" in {
		val input = List(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((DecimalIntegerLiteralExp("1"), DecimalIntegerLiteralExp("2")))
		val expected = Stream(DictionaryLiteralExp(dictionaryList))
		assertResult(expected) { Parser("test", Parser.dictionary_literal, input) }
	}


 	"dictionary_literal" should "handle an dictionary literal: [1:2,]" in {
		val input = List(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((DecimalIntegerLiteralExp("1"), DecimalIntegerLiteralExp("2")))
		val expected = Stream(DictionaryLiteralExp(dictionaryList))
		assertResult(expected) { Parser("test", Parser.dictionary_literal, input) }
	}

 	"dictionary_literal" should "handle an dictionary literal: [1:2, 3:4]" in {
		val input = List(LeftBracketToken, DecimalIntegerLiteralToken("1"), ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, DecimalIntegerLiteralToken("3"), ColonToken, DecimalIntegerLiteralToken("4"), RightBracketToken)
		val dictionaryList: List[(Exp, Exp)] = List((DecimalIntegerLiteralExp("1"), DecimalIntegerLiteralExp("2")), (DecimalIntegerLiteralExp("3"), DecimalIntegerLiteralExp("4")))
		val expected = Stream(DictionaryLiteralExp(dictionaryList))
		assertResult(expected) { Parser("test", Parser.dictionary_literal, input) }
	}

	//playground_literal
	"playground_literal" should "handle a colorLiteral playground literal" in {
		val input = List(HashColorLiteralToken, LeftParenToken, RedToken, ColonToken, DecimalIntegerLiteralToken("0"), CommaToken, GreenToken, ColonToken, DecimalIntegerLiteralToken("1"), CommaToken, BlueToken, ColonToken, DecimalIntegerLiteralToken("2"), CommaToken, AlphaToken, ColonToken, DecimalIntegerLiteralToken("3"), RightParenToken)
		val expected = Stream(ColorPlaygroundLiteralExp(DecimalIntegerLiteralExp("0"), DecimalIntegerLiteralExp("1"), DecimalIntegerLiteralExp("2"), DecimalIntegerLiteralExp("3")))
		assertResult(expected) { Parser("test", Parser.playground_literal, input) }
	}

	"playground_literal" should "handle a fileLiteral playground literal" in {
		val input = List(HashFileLiteralToken, LeftParenToken, ResourceNameToken, ColonToken, DecimalIntegerLiteralToken("0"), RightParenToken)
		val expected = Stream(FilePlaygroundLiteralExp(DecimalIntegerLiteralExp("0")))
		assertResult(expected) { Parser("test", Parser.playground_literal, input) }
	}

	"playground_literal" should "handle an imageLiteral playground literal" in {
		val input = List(HashImageLiteralToken, LeftParenToken, ResourceNameToken, ColonToken, DecimalIntegerLiteralToken("0"), RightParenToken)
		val expected = Stream(ImagePlaygroundLiteralExp(DecimalIntegerLiteralExp("0")))
		assertResult(expected) { Parser("test", Parser.playground_literal, input) }
	}

	//numeric_literal
	"numeric_literal" should "handle an unsigned decimal integer literal token: 23" in {
		val input = List(DecimalIntegerLiteralToken("23"))
		assertResult(Stream(DecimalIntegerLiteralExp("23"))) { Parser("test", Parser.numeric_literal, input) }
	}

	"numeric_literal" should "handle a negative decimal integer literal token: -23" in {
		val input = List(PrefixOperatorLiteralToken("-"), DecimalIntegerLiteralToken("23"))
		assertResult(Stream(PrefixExp(Operator("-"), DecimalIntegerLiteralExp("23")))) { Parser("test", Parser.numeric_literal, input) }
	}

	"numeric_literal" should "handle an unsigned decimal float literal token: 2.3" in {
		val input = List(FloatDecimalLiteralToken("2.3"))
		assertResult(Stream(DecimalFloatLiteralExp("2.3"))) { Parser("test", Parser.numeric_literal, input) }
	}

	"numeric_literal" should "handle a negative decimal float literal token: -2.3" in {
		val input = List(PrefixOperatorLiteralToken("-"), FloatDecimalLiteralToken("2.3"))
		assertResult(Stream(PrefixExp(Operator("-"), DecimalFloatLiteralExp("2.3")))) { Parser("test", Parser.numeric_literal, input) }
	}

	//integer_literal
	//(decimal_integer) thru integer_literal
	"integer_literal" should "handle a decimal integer literal token: 23" in {
		val input = List(DecimalIntegerLiteralToken("23"))
		assertResult(Stream(DecimalIntegerLiteralExp("23"))) { Parser("test", Parser.integer_literal, input) }
	}

	//(binary_integer) thru integer_literal
	"integer_literal" should "handle a binary integer literal token: 0b0101" in {
		val input = List(BinaryIntegerLiteralToken("0b0101"))
		assertResult(Stream(BinaryIntegerLiteralExp("0b0101"))) { Parser("test", Parser.integer_literal, input) }
	}

	//(octal_integer) thru integer_literal
	"integer_literal" should "handle an octal integer literal token: 0o734" in {
		val input = List(OctalIntegerLiteralToken("0o734"))
		assertResult(Stream(OctalIntegerLiteralExp("0o734"))) { Parser("test", Parser.integer_literal, input) }
	}

	//(hex_integer) thru integer_literal
	"integer_literal" should "handle a hex integer literal token: 0xA43B" in {
		val input = List(HexIntegerLiteralToken("0xA43B"))
		assertResult(Stream(HexIntegerLiteralExp("0xA43B"))) { Parser("test", Parser.integer_literal, input) }
	}

	//float_literal
	//(decimal_float) thru float_literal
	"float_literal" should "handle a decimal float literal token: 34.5" in {
		val input = List(FloatDecimalLiteralToken("34.5"))
		assertResult(Stream(DecimalFloatLiteralExp("34.5"))) { Parser("test", Parser.float_literal, input) }
	}

	//(hex_float) thru float_literal
	"float_literal" should "handle a hex float literal token: 0xA34.B5" in {
		val input = List(FloatHexLiteralToken("0xA34.B5"))
		assertResult(Stream(HexFloatLiteralExp("0xA34.B5"))) { Parser("test", Parser.float_literal, input) }
	}

	//string_literal
	"string_literal" should "handle a single line string literal token: hello" in {
		val input = List(SingleLineStringLiteralToken("hello"))
		assertResult(Stream(SingleLineStaticStringLiteralExp("hello"))) { Parser("test", Parser.string_literal, input) }
	}

	"string_literal" should "handle a multi line string literal token: hello\nthere" in {
		val input = List(MultiLineStringLiteralToken("hello\nthere"))
		assertResult(Stream(MultiLineStaticStringLiteralExp("hello\nthere"))) { Parser("test", Parser.string_literal, input) }
	}

	//boolean_literal
	"boolean_literal" should "handle a boolean literal true token" in {
		val input = List(TrueToken)
		assertResult(Stream(TrueLiteralExp())) { Parser("test", Parser.boolean_literal, input) }
	}

	"boolean_literal" should "handle a boolean literal false token" in {
		val input = List(FalseToken)
		assertResult(Stream(FalseLiteralExp())) { Parser("test", Parser.boolean_literal, input) }
	}

	//nil_literal
	"nil_literal" should "handle a nil token" in {
		val input = List(NilToken)
		assertResult(Stream(NilExp())) { Parser("test", Parser.nil_literal, input) }
	}
	//end testing for expressions

	//testing for declarations
	//declaration
	//(import_declaration) thru declaration
	"declaration" should "handle import name" in {
		val input = List(ImportToken, VariableToken("name"))
		val expected = Stream(ImportDeclaration(None, None, RegularPath(ImportPathName("name"))))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(constant_declaration) thru declaration
	"declaration" should "handle: let _ = 5" in {
		val input = List(LetToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val list = List(PatternInitializer(WildcardPattern(None), Some(DecimalIntegerLiteralExp("5"))))
		val expected = ConstantDeclaration(None, None, list)
		val result = Parser("test", Parser.declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(variable_declaration) thru declaration
	"declaration" should "handle var x = 5" in {
		val input = List(VarToken, VariableToken("x"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val thePattern = IdentifierPattern(PatternName("x"), None)
		val patternInitList = List(PatternInitializer(thePattern, Some(DecimalIntegerLiteralExp("5"))))
		val expected = VariableDeclaration1(None, None, patternInitList)
		val result = Parser("test", Parser.declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(typealias_declaration) thru declaration
	"declaration" should "handle typealias theType = Int" in {
		val input = List(TypeAliasToken, VariableToken("theType"), InfixOperatorLiteralToken("="), VariableToken("Int"))
		val expected = Stream(TypeAliasDeclaration(None, None, TypeAliasName("theType"), None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(function_declaration) thru declaration
	"declaration" should "handle func sum (localParamName: Int)" in {
		val input = List(FuncToken, VariableToken("sum"), LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcSig = ThrowsFunctionSig(paramClause, None, None, None)
		val expected = Stream(FunctionDeclaration(funcHead, funcName, None, funcSig, None, None))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(enum_declaration) thru declaration
	"declaration" should "handle enum enumName { }" in {
		val input = List(EnumToken, VariableToken("enumName"), LeftCurlyToken, RightCurlyToken)
		val enumName = EnumName("enumName")
		val expected = Stream(UnionStyleEnumDeclaration(None, None, None, enumName, None, None, None, None))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(struct_declaration) thru declaration
	"declaration" should "handle struct structName { }" in {
		val input = List(StructToken, VariableToken("structName"), LeftCurlyToken, RightCurlyToken)
		val structName = Structname("structName")
		val expected = Stream(StructDeclaration(None, None, structName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(class_declaration) thru declaration
	"declaration" should "handle final class className { }" in { //(forced final)
		val input = List(FinalToken, ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val expected = ForcedFinalClassDeclaration(None, None, className, None, None, None, List())
		val result = Parser("test", Parser.declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(actor_declaration) thru declaration
	"declaration" should "handle actor actorName { }" in {
		val input = List(ActorToken, VariableToken("actorName"), LeftCurlyToken, RightCurlyToken)
		val actorName = ActorName("actorName")
		val expected = Stream(ActorDeclaration(None, None, actorName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(protocol_declaration) thru declaration
	"declaration" should "handle protocol protocolName { var variableName: Int { get } }" in {
		val input = List(ProtocolToken, VariableToken("protocolName"), LeftCurlyToken, VarToken, VariableToken("variableName"), ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken, RightCurlyToken)
		val protocolName = ProtocolName("protocolName")
		val name = VariableName("variableName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val protocolBody = List(ProtocolPropertyDeclaration(None, None, name, typeAnno, block))
		val expected = Stream(ProtocolDeclaration(None, None, protocolName, None, None, protocolBody))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(initializer_declaration) thru declaration
	"declaration" should "handle init () { 5 }" in {
		val input = List(InitToken, LeftParenToken, RightParenToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val expected = Stream(ThrowsInitializerDeclaration(initHead, None, List(), None, None, None, block))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(deinitializer_declaration) thru declaration
	"declaration" should "handle deinit { 5 }" in {
		val input = List(DeinitToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val expected = Stream(DeinitializerDeclaration(None, block))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(extension_declaraition) thru declaration
	"declaration" should "handle extension Int { import name }" in {
		val input = List(ExtensionToken, VariableToken("Int"), LeftCurlyToken, ImportToken, VariableToken("name"), RightCurlyToken)
		val typeID = NormalTypeIdentifier(TypeName("Int"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val body = List(DeclarationExtensionMember(decl))
		val expected = Stream(ExtensionDeclaration(None, None, typeID, None, None, body))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(subscript_declaration) thru declaration
	"declaration" should "handle subscript () -> Int { 0 }" in {
		val input = List(SubscriptToken, LeftParenToken, RightParenToken, PrefixOperatorLiteralToken("->"), VariableToken("Int"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val head = SubscriptHead(None, None, None, List())
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val result = SubscriptResult(None, theType)
		val block = AllowableGetterSetterBlock(CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0"))))))
		val expected = Stream(SubscriptDeclaration(head, result, None, block))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(operator_declaration) thru declaration
	"declaration" should "handle prefix operator +-" in {
		val input = List(PrefixToken, OperatorToken, InfixOperatorLiteralToken("+-"))
		val expected = Stream(PrefixOperatorDeclaration(Operator("+-")))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}

	//(precedence_declaration) thru declaration
	"declaration" should "handle precedencegroup groupName { }" in {
		val input = List(PrecedenceGroupToken, VariableToken("groupName"), LeftCurlyToken, RightCurlyToken)
		val name = PrecedenceGroupName("groupName")
		val expected = Stream(PrecedenceGroupDeclaration(name, None))
		assertResult(expected) { Parser("test", Parser.declaration, input) }
	}
	//end of declaration

	//import_declaration
	"import_declaration" should "handle import name" in {
		val input = List(ImportToken, VariableToken("name"))
		val expected = Stream(ImportDeclaration(None, None, RegularPath(ImportPathName("name"))))
		assertResult(expected) { Parser("test", Parser.import_declaration, input) }
	}

	"import_declaration" should "handle @attributeName import name" in {
		val input = List(AtToken, VariableToken("attributeName"), ImportToken, VariableToken("name"))
		val theAttributes = List(Attribute(AttributeName("attributeName"), None))
		val expected = Stream(ImportDeclaration(Some(theAttributes), None, RegularPath(ImportPathName("name"))))
		assertResult(expected) { Parser("test", Parser.import_declaration, input) }
	}

	"import_declaration" should "handle import typealias name" in {
		val input = List(ImportToken, TypeAliasToken, VariableToken("name"))
		val expected = Stream(ImportDeclaration(None, Some(TypeAliasKind), RegularPath(ImportPathName("name"))))
		assertResult(expected) { Parser("test", Parser.import_declaration, input) }
	}

	"import_declaration" should "handle @attributeName import typealias name" in {
		val input = List(AtToken, VariableToken("attributeName"), ImportToken, TypeAliasToken, VariableToken("name"))
		val theAttributes = List(Attribute(AttributeName("attributeName"), None))
		val expected = Stream(ImportDeclaration(Some(theAttributes), Some(TypeAliasKind), RegularPath(ImportPathName("name"))))
		assertResult(expected) { Parser("test", Parser.import_declaration, input) }
	}

	"import_kind" should "handle typealias" in {
		assertResult(Stream(TypeAliasKind)) { Parser("test", Parser.import_kind, List(TypeAliasToken)) }
	}

	"import_kind" should "handle struct" in {
		assertResult(Stream(StructKind)) { Parser("test", Parser.import_kind, List(StructToken)) }
	}

	"import_kind" should "handle class" in {
		assertResult(Stream(ClassKind)) { Parser("test", Parser.import_kind, List(ClassToken)) }
	}

	"import_kind" should "handle enum" in {
		assertResult(Stream(EnumKind)) { Parser("test", Parser.import_kind, List(EnumToken)) }
	}

	"import_kind" should "handle protocol" in {
		assertResult(Stream(ProtocolKind)) { Parser("test", Parser.import_kind, List(ProtocolToken)) }
	}

	"import_kind" should "handle let" in {
		assertResult(Stream(LetKind)) { Parser("test", Parser.import_kind, List(LetToken)) }
	}

	"import_kind" should "handle var" in {
		assertResult(Stream(VarKind)) { Parser("test", Parser.import_kind, List(VarToken)) }
	}

	"import_kind" should "handle func" in {
		assertResult(Stream(FuncKind)) { Parser("test", Parser.import_kind, List(FuncToken)) }
	}

	"import_path" should "handle name" in {
		val input = List(VariableToken("name"))
		val expected = Stream(RegularPath(ImportPathName("name")))
		assertResult(expected) { Parser("test", Parser.import_path, input) }
	}

	"import_path" should "handle name.place" in {
		val input = List(VariableToken("name"), InfixDotOperatorLiteralToken("."), VariableToken("place"))
		val expected = Stream(NestedPath(ImportPathName("name"), RegularPath(ImportPathName("place"))))
		assertResult(expected) { Parser("test", Parser.import_path, input) }
	}

	"constant_declaration" should "handle: let _ = 5" in {
		val input = List(LetToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val list = List(PatternInitializer(WildcardPattern(None), Some(DecimalIntegerLiteralExp("5"))))
		val expected = ConstantDeclaration(None, None, list)
		val result = Parser("test", Parser.constant_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.constant_declaration, input) }
	}

	"constant_declaration" should "handle: let _ = 5, x = 0" in {
		val input = List(LetToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"), CommaToken, VariableToken("x"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("0"))
		val list = List(PatternInitializer(WildcardPattern(None), Some(DecimalIntegerLiteralExp("5"))), PatternInitializer(IdentifierPattern(PatternName("x"), None), Some(DecimalIntegerLiteralExp("0"))))
		val expected = ConstantDeclaration(None, None, list)
		val result = Parser("test", Parser.constant_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.constant_declaration, input) }
	}

	"constant_declaration" should "handle: @name let _ = 5" in {
		val input = List(AtToken, VariableToken("name"), LetToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val list = List(PatternInitializer(WildcardPattern(None), Some(DecimalIntegerLiteralExp("5"))))
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = ConstantDeclaration(Some(theAttributes), None, list)
		val result = Parser("test", Parser.constant_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.constant_declaration, input) }
	}

	"constant_declaration" should "handle: @name final let _ = 5" in {
		val input = List(AtToken, VariableToken("name"), FinalToken, LetToken, UnderscoreToken, InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val list = List(PatternInitializer(WildcardPattern(None), Some(DecimalIntegerLiteralExp("5"))))
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = ConstantDeclaration(Some(theAttributes), Some(List(FinalModifier)), list)
		val result = Parser("test", Parser.constant_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.constant_declaration, input) }
	}

	"variable_declaration" should "handle var x = 5" in {
		val input = List(VarToken, VariableToken("x"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val thePattern = IdentifierPattern(PatternName("x"), None)
		val patternInitList = List(PatternInitializer(thePattern, Some(DecimalIntegerLiteralExp("5"))))
		val expected = VariableDeclaration1(None, None, patternInitList)
		val result = Parser("test", Parser.variable_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.variable_declaration, input) }
	}

	"variable_declaration" should "handle @name static var x = 5" in {
		val input = List(AtToken, VariableToken("name"), StaticToken, VarToken, VariableToken("x"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val modifiers = List(StaticModifier)
		val thePattern = IdentifierPattern(PatternName("x"), None)
		val patternInitList = List(PatternInitializer(thePattern, Some(DecimalIntegerLiteralExp("5"))))
		val expected = VariableDeclaration1(Some(theAttributes), Some(modifiers), patternInitList)
		val result = Parser("test", Parser.variable_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.variable_declaration, input) }
	}

	"variable_declaration" should "handle var x: Int { 0 }" in {
		val input = List(VarToken, VariableToken("x"), ColonToken, VariableToken("Int"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val name = VariableName("x")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(VariableDeclaration23(None, None, name, typeAnno, block))
		assertResult(expected) { Parser("test", Parser.variable_declaration, input) }
	}

	"variable_declaration" should "handle var x = 5 { willSet { 0 } }" in {
		val input = List(VarToken, VariableToken("x"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"), LeftCurlyToken, WillSetToken, LeftCurlyToken,
						DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val name = VariableName("x")
		val initializer = DecimalIntegerLiteralExp("5")
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val block = WillDidSetBlock(WillSetClause(None, None, None, codeBlock), None)
		val expected = VariableDeclaration5(None, None, name, initializer, block)
		val result = Parser("test", Parser.variable_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.variable_declaration, input) }
	}

	"variable_declaration" should "handle var x: Int { willSet { 0 } }" in {
		val input = List(VarToken, VariableToken("x"), ColonToken, VariableToken("Int"), LeftCurlyToken, WillSetToken, LeftCurlyToken,
						DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val name = VariableName("x")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val block = WillDidSetBlock(WillSetClause(None, None, None, codeBlock), None)
		val expected = VariableDeclaration6(None, None, name, typeAnno, None, block)
		val result = Parser("test", Parser.variable_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.variable_declaration, input) }
	}

	//typealias_declaration
	"typealias_declaration" should "handle typealias theType = Int" in {
		val input = List(TypeAliasToken, VariableToken("theType"), InfixOperatorLiteralToken("="), VariableToken("Int"))
		val expected = Stream(TypeAliasDeclaration(None, None, TypeAliasName("theType"), None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typealias_declaration, input) }
	}

	"typealias_declaration" should "handle @name typealias theType = Int" in {
		val input = List(AtToken, VariableToken("name"), TypeAliasToken, VariableToken("theType"), InfixOperatorLiteralToken("="), VariableToken("Int"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(TypeAliasDeclaration(Some(attributes), None, TypeAliasName("theType"), None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typealias_declaration, input) }
	}

	"typealias_declaration" should "handle @name private typealias theType = Int" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, TypeAliasToken, VariableToken("theType"), InfixOperatorLiteralToken("="), VariableToken("Int"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(TypeAliasDeclaration(Some(attributes), Some(PrivateModifier), TypeAliasName("theType"), None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typealias_declaration, input) }
	}

	"typealias_declaration" should "handle @name private typealias theType<Int> = Int" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, TypeAliasToken, VariableToken("theType"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), InfixOperatorLiteralToken("="), VariableToken("Int"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(TypeAliasDeclaration(Some(attributes), Some(PrivateModifier), TypeAliasName("theType"), Some(genericParams), NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typealias_declaration, input) }
	}

	//getter_setter_block
	//(code_block) thru getter_setter_block
	"getter_setter_block" should "handle { 0 }" in {
		val input = List(LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val expected = Stream(CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0"))))))
		assertResult(expected) { Parser("test", Parser.getter_setter_block, input) }
	}

	"getter_setter_block" should "handle { get {0} }" in {
		val input = List(LeftCurlyToken, GetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val getter = GetterClause(None, None, codeBlock)
		val expected = GetterSetterClauseBlock(getter, None)
		val result = Parser("test", Parser.getter_setter_block, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.getter_setter_block, input) }
	}

	"getter_setter_block" should "handle { get {0} set {0} }" in {
		val input = List(LeftCurlyToken, GetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, SetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val getter = GetterClause(None, None, codeBlock)
		val setter = SetterClause(None, None, None, codeBlock)
		val expected = GetterSetterClauseBlock(getter, Some(setter))
		val result = Parser("test", Parser.getter_setter_block, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.getter_setter_block, input) }
	}

	"getter_setter_block" should "handle { set {0} get {0} }" in {
		val input = List(LeftCurlyToken, SetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, GetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val getter = GetterClause(None, None, codeBlock)
		val setter = SetterClause(None, None, None, codeBlock)
		val expected = SetterGetterClauseBlock(setter, getter)
		val result = Parser("test", Parser.getter_setter_block, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.getter_setter_block, input) }
	}

	"getter_setter_keyword_block" should "handle { get }" in {
		val input = List(LeftCurlyToken, GetToken, RightCurlyToken)
		val getterKeyword = GetterKeywordClause(None, None)
		val expected = Stream(GetterSetterKeywordBlock(getterKeyword, None))
		assertResult(expected) { Parser("test", Parser.getter_setter_keyword_block, input) }
	}

	"getter_setter_keyword_block" should "handle { get set }" in {
		val input = List(LeftCurlyToken, GetToken, SetToken, RightCurlyToken)
		val getterKeyword = GetterKeywordClause(None, None)
		val setterKeyword = SetterKeywordClause(None, None)
		val expected = Stream(GetterSetterKeywordBlock(getterKeyword, Some(setterKeyword)))
		assertResult(expected) { Parser("test", Parser.getter_setter_keyword_block, input) }
	}

	"getter_setter_keyword_block" should "handle { set get }" in {
		val input = List(LeftCurlyToken, SetToken, GetToken, RightCurlyToken)
		val getterKeyword = GetterKeywordClause(None, None)
		val setterKeyword = SetterKeywordClause(None, None)
		val expected = Stream(SetterGetterKeywordBlock(setterKeyword, getterKeyword))
		assertResult(expected) { Parser("test", Parser.getter_setter_keyword_block, input) }
	}

	//code_block
	"code_block" should "handle { 0 }" in {
		val input = List(LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val expected = Stream(CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0"))))))
		assertResult(expected) { Parser("test", Parser.code_block, input) }
	}

	"code_block" should "handle { 0 ; 5}" in {
		val input = List(LeftCurlyToken, DecimalIntegerLiteralToken("0"), SemicolonToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val expected = Stream(CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")), ExpressionStmt(DecimalIntegerLiteralExp("5"))))))
		assertResult(expected) { Parser("test", Parser.code_block, input) }
	}

	//set_block
	"set_block" should "handle { willSet { 0 } }" in {
		val input = List(LeftCurlyToken, WillSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(WillDidSetBlock(WillSetClause(None, None, None, codeBlock), None))
		assertResult(expected) { Parser("test", Parser.set_block, input) }
	}

	"set_block" should "handle { willSet { 0 } didSet {0} }" in {
		val input = List(LeftCurlyToken, WillSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, DidSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val codeBlock1 = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(WillDidSetBlock(WillSetClause(None, None, None, codeBlock), Some(DidSetClause(None, None, None, codeBlock))))
		assertResult(expected) { Parser("test", Parser.set_block, input) }
	}

	"set_block" should "handle { didSet { 0 } }" in {
		val input = List(LeftCurlyToken, DidSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(DidWillSetBlock(DidSetClause(None, None, None, codeBlock), None))
		assertResult(expected) { Parser("test", Parser.set_block, input) }
	}

	"set_block" should "handle { didSet { 0 } willSet {0} }" in {
		val input = List(LeftCurlyToken, DidSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, WillSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(DidWillSetBlock(DidSetClause(None, None, None, codeBlock), Some(WillSetClause(None, None, None, codeBlock))))
		assertResult(expected) { Parser("test", Parser.set_block, input) }
	}

	//getter_clause
	"getter_clause" should "handle get {0}" in {
		val input = List(GetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(GetterClause(None, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.getter_clause, input) }
	}

	"getter_clause" should "handle @name get {0}" in {
		val input = List(AtToken, VariableToken("name"), GetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(GetterClause(Some(theAttribtutes), None, codeBlock))
		assertResult(expected) { Parser("test", Parser.getter_clause, input) }
	}

	"getter_clause" should "handle @name nonmutating get {0}" in {
		val input = List(AtToken, VariableToken("name"), NonmutatingToken, GetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(GetterClause(Some(theAttribtutes), Some(NonMutatingModifier), codeBlock))
		assertResult(expected) { Parser("test", Parser.getter_clause, input) }
	}

	//setter_clause
	"setter_clause" should "handle set {0}" in {
		val input = List(SetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(SetterClause(None, None, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.setter_clause, input) }
	}

	"setter_clause" should "handle @name set {0}" in {
		val input = List(AtToken, VariableToken("name"), SetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(SetterClause(Some(theAttribtutes), None, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.setter_clause, input) }
	}

	"setter_clause" should "handle @name nonmutating set {0}" in {
		val input = List(AtToken, VariableToken("name"), NonmutatingToken, SetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(SetterClause(Some(theAttribtutes), Some(NonMutatingModifier), None, codeBlock))
		assertResult(expected) { Parser("test", Parser.setter_clause, input) }
	}

	"setter_clause" should "handle @name nonmutating set (hello) {0}" in {
		val input = List(AtToken, VariableToken("name"), NonmutatingToken, SetToken, LeftParenToken, VariableToken("hello"), RightParenToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(SetterClause(Some(theAttribtutes), Some(NonMutatingModifier), Some(SetterName("hello")), codeBlock))
		assertResult(expected) { Parser("test", Parser.setter_clause, input) }
	}

	//getter_keyword_clause
	"getter_keyword_clause" should "handle get" in {
		val input = List(GetToken)
		val expected = Stream(GetterKeywordClause(None, None))
		assertResult(expected) { Parser("test", Parser.getter_keyword_clause, input) }
	}

	"getter_keyword_clause" should "handle @name get" in {
		val input = List(AtToken, VariableToken("name"), GetToken)
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(GetterKeywordClause(Some(theAttributes), None))
		assertResult(expected) { Parser("test", Parser.getter_keyword_clause, input) }
	}

	"getter_keyword_clause" should "handle @name mutating get" in {
		val input = List(AtToken, VariableToken("name"), MutatingToken, GetToken)
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(GetterKeywordClause(Some(theAttributes), Some(MutatingModifier)))
		assertResult(expected) { Parser("test", Parser.getter_keyword_clause, input) }
	}

	//setter_keyword_clause
	"setter_keyword_clause" should "handle set" in {
		val input = List(SetToken)
		val expected = Stream(SetterKeywordClause(None, None))
		assertResult(expected) { Parser("test", Parser.setter_keyword_clause, input) }
	}

	"setter_keyword_clause" should "handle @name set" in {
		val input = List(AtToken, VariableToken("name"), SetToken)
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(SetterKeywordClause(Some(theAttributes), None))
		assertResult(expected) { Parser("test", Parser.setter_keyword_clause, input) }
	}

	"setter_keyword_clause" should "handle @name mutating set" in {
		val input = List(AtToken, VariableToken("name"), MutatingToken, SetToken)
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(SetterKeywordClause(Some(theAttributes), Some(MutatingModifier)))
		assertResult(expected) { Parser("test", Parser.setter_keyword_clause, input) }
	}

	//will_set_clause
	"will_set_clause" should "handle willSet { 0 }" in {
		val input = List(WillSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(WillSetClause(None, None, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.will_set_clause, input) }
	}

	"will_set_clause" should "handle @name willSet { 0 }" in {
		val input = List(AtToken, VariableToken("name"), WillSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(WillSetClause(Some(theAttribtutes), None, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.will_set_clause, input) }
	}

	"will_set_clause" should "handle @name willSet (setterName) { 0 }" in {
		val input = List(AtToken, VariableToken("name"), WillSetToken, LeftParenToken, VariableToken("setterName"), RightParenToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val setterName = SetterName("setterName")
		val expected = Stream(WillSetClause(Some(theAttribtutes), None, Some(setterName), codeBlock))
		assertResult(expected) { Parser("test", Parser.will_set_clause, input) }
	}

	//did_set_clause
	"did_set_clause" should "handle didSet { 0 }" in {
		val input = List(DidSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(DidSetClause(None, None, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.did_set_clause, input) }
	}

	"did_set_clause" should "handle @name didSet { 0 }" in {
		val input = List(AtToken, VariableToken("name"), DidSetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(DidSetClause(Some(theAttribtutes), None, None, codeBlock))
		assertResult(expected) { Parser("test", Parser.did_set_clause, input) }
	}

	"did_set_clause" should "handle @name didSet (setterName) { 0 }" in {
		val input = List(AtToken, VariableToken("name"), DidSetToken, LeftParenToken, VariableToken("setterName"), RightParenToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val theAttribtutes = List(Attribute(AttributeName("name"), None))
		val setterName = SetterName("setterName")
		val expected = Stream(DidSetClause(Some(theAttribtutes), None, Some(setterName), codeBlock))
		assertResult(expected) { Parser("test", Parser.did_set_clause, input) }
	}

	//function_declaration
	"function_declaration" should "handle func sum (localParamName: Int)" in {
		val input = List(FuncToken, VariableToken("sum"), LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcSig = ThrowsFunctionSig(paramClause, None, None, None)
		val expected = Stream(FunctionDeclaration(funcHead, funcName, None, funcSig, None, None))
		assertResult(expected) { Parser("test", Parser.function_declaration, input) }
	}

	//add in generic param clause
	"function_declaration" should "handle func sum<Int> (localParamName: Int)" in {
		val input = List(FuncToken, VariableToken("sum"), PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcSig = ThrowsFunctionSig(paramClause, None, None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(FunctionDeclaration(funcHead, funcName, Some(genericParams), funcSig, None, None))
		assertResult(expected) { Parser("test", Parser.function_declaration, input) }
	}

	//add in generic where clause
	"function_declaration" should "handle func sum<Int> (localParamName: Int) where Int: Int" in {
		val input = List(FuncToken, VariableToken("sum"), PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken,
						WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"))
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcSig = ThrowsFunctionSig(paramClause, None, None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(FunctionDeclaration(funcHead, funcName, Some(genericParams), funcSig, Some(genericWhereClause), None))
		assertResult(expected) { Parser("test", Parser.function_declaration, input) }
	}

	//add in code block
	"function_declaration" should "handle func sum<Int> (localParamName: Int) where Int: Int { 0 }" in {
		val input = List(FuncToken, VariableToken("sum"), PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken,
						WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val funcHead = FunctionHead(None, None)
		val funcName = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcSig = ThrowsFunctionSig(paramClause, None, None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val expected = Stream(FunctionDeclaration(funcHead, funcName, Some(genericParams), funcSig, Some(genericWhereClause), Some(codeBlock)))
		assertResult(expected) { Parser("test", Parser.function_declaration, input) }
	}

	//function_head
	"function_head" should "handle func" in {
		val input = List(FuncToken)
		val expected = Stream(FunctionHead(None, None))
		assertResult(expected) { Parser("test", Parser.function_head, input) }
	}

	"function_head" should "handle @name func" in {
		val input = List(AtToken, VariableToken("name"), FuncToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(FunctionHead(Some(attributes), None))
		assertResult(expected) { Parser("test", Parser.function_head, input) }
	}

	"function_head" should "handle @name private func" in {
		val input = List(AtToken, VariableToken("name"), LazyToken, FuncToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val declMods: List[DeclarationModifier] = List(LazyModifier)
		val expected = Stream(FunctionHead(Some(attributes), Some(declMods)))
		assertResult(expected) { Parser("test", Parser.function_head, input) }
	}


	//function_name
	"function_name" should "handle sum" in {
		val input = List(VariableToken("sum"))
		val expected = Stream(IdentifierFunctionName("sum"))
		assertResult(expected) { Parser("test", Parser.function_name, input) }
	}

	"function_name" should "handle ++" in {
		val input = List(InfixOperatorLiteralToken("++"))
		val expected = Stream(OperatorFunctionName(Operator("++")))
		assertResult(expected) { Parser("test", Parser.function_name, input) }
	}

	//function_signature
	"function_signature" should "handle (localParamName: Int)" in {
		val input = List(LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val expected = Stream(ThrowsFunctionSig(paramClause, None, None, None))
		assertResult(expected) { Parser("test", Parser.function_signature, input) }
	}

	"function_signature" should "handle (localParamName: Int) async" in {
		val input = List(LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken, AsyncToken)
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val expected = Stream(ThrowsFunctionSig(paramClause, Some(AsyncMod()), None, None))
		assertResult(expected) { Parser("test", Parser.function_signature, input) }
	}

	"function_signature" should "handle (localParamName: Int) async throws" in {
		val input = List(LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken, AsyncToken, ThrowsToken)
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val expected = Stream(ThrowsFunctionSig(paramClause, Some(AsyncMod()), Some(ThrowsMod()), None))
		assertResult(expected) { Parser("test", Parser.function_signature, input) }
	}

	"function_signature" should "handle (localParamName: Int) async throws -> Int" in {
		val input = List(LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken, AsyncToken, ThrowsToken, InfixOperatorLiteralToken("->"), VariableToken("Int"))
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val funcResult = FunctionResult(None, NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(ThrowsFunctionSig(paramClause, Some(AsyncMod()), Some(ThrowsMod()), Some(funcResult)))
		assertResult(expected) { Parser("test", Parser.function_signature, input) }
	}

	//function_result
	"function_result" should "handle -> Int" in {
		val input = List(PrefixOperatorLiteralToken("->"), VariableToken("Int"))
		val expected = Stream(FunctionResult(None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.function_result, input) }
	}

	"function_result" should "handle -> @name Int" in {
		val input = List(InfixOperatorLiteralToken("->"), AtToken, VariableToken("name"), VariableToken("Int"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(FunctionResult(Some(attributes), NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.function_result, input) }
	}

	//parameter_clause
	"parameter_clause" should "handle (localParamName: Int)" in {
		val input = List(LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None)))
		assertResult(expected) { Parser("test", Parser.parameter_clause, input) }
	}

	"parameter_clause" should "handle (localParamName: Int, localParamName: Int)" in {
		val input = List(LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), CommaToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None), OptDefaultArgClauseParameter(None, localParamName, typeAnno, None)))
		assertResult(expected) { Parser("test", Parser.parameter_clause, input) }
	}

	//parameter
	"parameter" should "handle localParamName: Int" in {
		val input = List(VariableToken("localParamName"), ColonToken, VariableToken("Int"))
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		assertResult(expected) { Parser("test", Parser.parameter, input) }
	}

	"parameter" should "handle localParamName: Int = 5" in {
		val input = List(VariableToken("localParamName"), ColonToken, VariableToken("Int"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val defaultArgClause = DecimalIntegerLiteralExp("5")
		val expected = Stream(OptDefaultArgClauseParameter(None, localParamName, typeAnno, Some(defaultArgClause)))
		assertResult(expected) { Parser("test", Parser.parameter, input) }
	}

	"parameter" should "handle localParamName: Int ..." in {
		val input = List(VariableToken("localParamName"), ColonToken, VariableToken("Int"), InfixDotOperatorLiteralToken("..."))
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(ElipsesParameter(None, localParamName, typeAnno))
		assertResult(expected) { Parser("test", Parser.parameter, input) }
	}

	//generic_parameter_clause
	"generic_parameter_clause" should "handle <Int>" in {
		val input = List(PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"))
		val expected = Stream(List(SimpleGenericParameter(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.generic_parameter_clause, input) }
	}

	"generic_parameter_clause" should "handle <Int, Bool>" in {
		val input = List(PrefixOperatorLiteralToken("<"), VariableToken("Int"), CommaToken, VariableToken("Bool"), PostfixOperatorLiteralToken(">"))
		val expected = Stream(List(SimpleGenericParameter(TypeName("Int")), SimpleGenericParameter(TypeName("Bool"))))
		assertResult(expected) { Parser("test", Parser.generic_parameter_clause, input) }
	}

	//generic_parameter
	"generic_parameter" should "handle Int" in {
		val input = List(VariableToken("Int"))
		val expected = Stream(SimpleGenericParameter(TypeName("Int")))
		assertResult(expected) { Parser("test", Parser.generic_parameter, input) }
	}

	"generic_parameter" should "handle Number: Int" in {
		val input = List(VariableToken("Number"), ColonToken, VariableToken("Int"))
		val typeID = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(AnnotatedGenericParameter(TypeName("Number"), typeID))
		assertResult(expected) { Parser("test", Parser.generic_parameter, input) }
	}

	"generic_parameter" should "handle Number: Int & Bool" in {
		val input = List(VariableToken("Number"), ColonToken, VariableToken("Int"), InfixOperatorLiteralToken("&"), VariableToken("Bool"))
		val list = List(NormalTypeIdentifier(TypeName("Int")),
				   NormalTypeIdentifier(TypeName("Bool")))
		val protocolCompType = ProtocolCompositionType(list)
		val expected = Stream(ProtocolCompGenericParameter(TypeName("Number"), protocolCompType))
		assertResult(expected) { Parser("test", Parser.generic_parameter, input) }
	}

	//generic_where_clause
	"generic_where_clause" should "handle where Int: Int" in {
		val input = List(WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(List(ConformanceRequirementDoubleTypeID(typeID1, typeID2)))
		assertResult(expected) { Parser("test", Parser.generic_where_clause, input) }
	}

	"generic_where_clause" should "handle where Int: Int, Int: Int" in {
		val input = List(WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), CommaToken, VariableToken("Int"), ColonToken, VariableToken("Int"))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(List(ConformanceRequirementDoubleTypeID(typeID1, typeID2), ConformanceRequirementDoubleTypeID(typeID1, typeID2)))
		assertResult(expected) { Parser("test", Parser.generic_where_clause, input) }
	}

	//requirement
	"requirement" should "handle Int: Int" in {
		val input = List(VariableToken("Int"), ColonToken, VariableToken("Int"))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		assertResult(expected) { Parser("test", Parser.requirement, input) }
	}

	"requirement" should "handle Int: Int & Bool" in {
		val input = List(VariableToken("Int"), ColonToken, VariableToken("Int"), InfixOperatorLiteralToken("&"), VariableToken("Bool"))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeIDList = List(NormalTypeIdentifier(TypeName("Int")), NormalTypeIdentifier(TypeName("Bool")))
		val protocolCompType = ProtocolCompositionType(typeIDList)
		val expected = Stream(ConformanceRequirementTypeIDProtocolCompType(typeID1, protocolCompType))
		assertResult(expected) { Parser("test", Parser.requirement, input) }
	}

	"requirement" should "handle Int = Int" in {
		val input = List(VariableToken("Int"), InfixOperatorLiteralToken("=="), VariableToken("Int"))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(SameTypeRequirement(typeID1, theType))
		assertResult(expected) { Parser("test", Parser.requirement, input) }
	}

	//enum_declaration
	"enum_declaration" should "handle enum enumName { }" in {
		val input = List(EnumToken, VariableToken("enumName"), LeftCurlyToken, RightCurlyToken)
		val enumName = EnumName("enumName")
		val expected = Stream(UnionStyleEnumDeclaration(None, None, None, enumName, None, None, None, None))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	"enum_declaration" should "handle @name enum enumName { }" in {
		val input = List(AtToken, VariableToken("name"), EnumToken, VariableToken("enumName"), LeftCurlyToken, RightCurlyToken)
		val enumName = EnumName("enumName")
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(UnionStyleEnumDeclaration(Some(attributes), None, None, enumName, None, None, None, None))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	"enum_declaration" should "handle @name private enum enumName { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, EnumToken, VariableToken("enumName"), LeftCurlyToken, RightCurlyToken)
		val enumName = EnumName("enumName")
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(UnionStyleEnumDeclaration(Some(attributes), Some(PrivateModifier), None, enumName, None, None, None, None))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	"enum_declaration" should "handle enum enumName: Int { }" in {
		val input = List(EnumToken, VariableToken("enumName"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val enumName = EnumName("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val expected = Stream(UnionStyleEnumDeclaration(None, None, None, enumName, None, Some(typeInheritanceClause), None, None))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	//this one is actually ambiguous, could be either one. So it's going to assing it to whichever is first.
	//wtf is assing? lmao
	"enum_declaration" should "handle enum enumName :Int { case enumCaseName }" in {
		val input = List(EnumToken, VariableToken("enumName"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), RightCurlyToken)
		val enumName = EnumName("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val enumMembers = List(EnumCaseClauseUSEnumMember(None, None, caseList))
		val expected = UnionStyleEnumDeclaration(None, None, None, enumName, None, Some(typeInheritanceClause), None, Some(enumMembers))
		val result = Parser("test", Parser.enum_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	"enum_declaration" should "handle indirect enum enumName :Int { case enumCaseName }" in {
		val input = List(IndirectToken, EnumToken, VariableToken("enumName"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), RightCurlyToken)
		val enumName = EnumName("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val enumMembers = List(EnumCaseClauseUSEnumMember(None, None, caseList))
		val expected = Stream(UnionStyleEnumDeclaration(None, None, Some(IndirectMod()), enumName, None, Some(typeInheritanceClause), None, Some(enumMembers)))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	"enum_declaration" should "handle indirect enum enumName<Int> :Int { case enumCaseName }" in {
		val input = List(IndirectToken, EnumToken, VariableToken("enumName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), RightCurlyToken)
		val enumName = EnumName("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val enumMembers = List(EnumCaseClauseUSEnumMember(None, None, caseList))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(UnionStyleEnumDeclaration(None, None, Some(IndirectMod()), enumName, Some(genericParams), Some(typeInheritanceClause), None, Some(enumMembers)))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	"enum_declaration" should "handle indirect enum enumName<Int> :Int where Int : Int { case enumCaseName }" in {
		val input = List(IndirectToken, EnumToken, VariableToken("enumName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"),
						WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), RightCurlyToken)
		val enumName = EnumName("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val enumMembers = List(EnumCaseClauseUSEnumMember(None, None, caseList))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(UnionStyleEnumDeclaration(None, None, Some(IndirectMod()), enumName, Some(genericParams), Some(typeInheritanceClause), Some(genericWhereClause), Some(enumMembers)))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

/* 	"enum" should "handle enum enumName :Int { case enumCaseName } (as a raw value enum)" in {
		val input = List(EnumToken, VariableToken("enumName"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), RightCurlyToken)
		val enumName = VariableExp("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(RawValueStyleEnumCase(VariableExp("enumCaseName"), None))
		val enumMembers = List(EnumCaseClauseRVSEnumMember(RawValueStyleEnumCaseClause(None, caseList)))
		val expected = RawValueStyleEnum(enumName, None, typeInheritanceClause, None, enumMembers)
		assertResult(expected) { Parser("test", Parser.enum, input) }
	} */

/*  	"enum" should "handle enum enumName<Int> :Int { case enumCaseName } (as a raw value enum)" in {
		val input = List(EnumToken, VariableToken("enumName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), RightCurlyToken)
		val enumName = VariableExp("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(RawValueStyleEnumCase(VariableExp("enumCaseName"), None))
		val enumMembers = List(EnumCaseClauseRVSEnumMember(RawValueStyleEnumCaseClause(None, caseList)))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(VariableExp("Int")))
		val expected = RawValueStyleEnum(enumName, Some(genericParams), typeInheritanceClause, None, enumMembers)
		assertResult(expected) { Parser("test", Parser.enum, input) }
	} */

/*   	"enum" should "handle enum enumName<Int> :Int where Int: Int { case enumCaseName } (as a raw value enum)" in {
		val input = List(EnumToken, VariableToken("enumName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"),
						WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), RightCurlyToken)
		val enumName = VariableExp("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(RawValueStyleEnumCase(VariableExp("enumCaseName"), None))
		val enumMembers = List(EnumCaseClauseRVSEnumMember(RawValueStyleEnumCaseClause(None, caseList)))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(VariableExp("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = RawValueStyleEnum(enumName, Some(genericParams), typeInheritanceClause, Some(genericWhereClause), enumMembers)
		assertResult(expected) { Parser("test", Parser.enum, input) }
	} */

		"enum_declaration" should "handle enum enumName<Int> :Int where Int: Int { case enumCaseName = 5 } (as a raw value enum)" in {
		val input = List(EnumToken, VariableToken("enumName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"),
						WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, CaseToken, VariableToken("enumCaseName"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val enumName = EnumName("enumName")
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val caseList = List(RawValueStyleEnumCase(EnumCaseName("enumCaseName"), Some(NumericLiteralRawValue("5"))))
		val enumMembers = List(EnumCaseClauseRVSEnumMember(None, caseList))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(RawValueStyleEnumDeclaration(None, None, enumName, Some(genericParams), typeInheritanceClause, Some(genericWhereClause), enumMembers))
		assertResult(expected) { Parser("test", Parser.enum_declaration, input) }
	}

	//type_inheritance_clause
	"type_inheritance_clause" should "handle :Int" in {
		val input = List(ColonToken, VariableToken("Int"))
		val expected = Stream(List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int")))))
		assertResult(expected) { Parser("test", Parser.type_inheritance_clause, input) }
	}

	"type_inheritance_clause" should "handle :Int, Bool" in {
		val input = List(ColonToken, VariableToken("Int"), CommaToken, VariableToken("Bool"))
		val expected = Stream(List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))),
							TypeInheritance(None, NormalTypeIdentifier(TypeName("Bool")))))
		assertResult(expected) { Parser("test", Parser.type_inheritance_clause, input) }
	}

	//type_inheritance
	"type_inheritance" should "handle Int" in {
		val input = List(VariableToken("Int"))
		val expected = Stream(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.type_inheritance, input) }
	}

	"type_inheritance" should "handle @name Int" in {
		val input = List(AtToken, VariableToken("name"), VariableToken("Int"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(TypeInheritance(Some(attributes), NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.type_inheritance, input) }
	}

	//raw_value_style_enum_member
	"raw_value_style_enum_member" should "handle import name" in { //(declaration)
		val input = List(ImportToken, VariableToken("name"))
		val expected = Stream(DeclarationRVSEnumMember(ImportDeclaration(None, None, RegularPath(ImportPathName("name")))))
		assertResult(expected) { Parser("test", Parser.raw_value_style_enum_member, input) }
	}

	"raw_value_style_enum_member" should "handle case enumCaseName" in {
		val input = List(CaseToken, VariableToken("enumCaseName"))
		val caseList = List(RawValueStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val expected = Stream(EnumCaseClauseRVSEnumMember(None, caseList))
		assertResult(expected) { Parser("test", Parser.raw_value_style_enum_member, input) }
	}

	"raw_value_style_enum_member" should "handle case enumCaseName, enumCaseName2" in {
		val input = List(CaseToken, VariableToken("enumCaseName"), CommaToken, VariableToken("enumCaseName2"))
		val caseList = List(RawValueStyleEnumCase(EnumCaseName("enumCaseName"), None), RawValueStyleEnumCase(EnumCaseName("enumCaseName2"), None))
		val expected = Stream(EnumCaseClauseRVSEnumMember(None, caseList))
		assertResult(expected) { Parser("test", Parser.raw_value_style_enum_member, input) }
	}

	//raw_value_style_enum_case
	"raw_value_style_enum_case" should "handle enumCaseName" in {
		val input = List(VariableToken("enumCaseName"))
		val expected = Stream(RawValueStyleEnumCase(EnumCaseName("enumCaseName"), None))
		assertResult(expected) { Parser("test", Parser.raw_value_style_enum_case, input) }
	}

	"raw_value_style_enum_case" should "handle enumCaseName = 5" in {
		val input = List(VariableToken("enumCaseName"), InfixOperatorLiteralToken("="), DecimalIntegerLiteralToken("5"))
		val expected = Stream(RawValueStyleEnumCase(EnumCaseName("enumCaseName"), Some(NumericLiteralRawValue("5"))))
		assertResult(expected) { Parser("test", Parser.raw_value_style_enum_case, input) }
	}

	//raw_value_literal
	"raw_value_literal" should "handle 5" in {
		val input = List(DecimalIntegerLiteralToken("5"))
		val expected = Stream(NumericLiteralRawValue("5"))
		assertResult(expected) { Parser("test", Parser.raw_value_literal, input) }
	}

	"raw_value_literal" should "handle hi (as a string literal)" in {
		val input = List(SingleLineStringLiteralToken("hi"))
		val expected = Stream(StaticStringLiteralRawValue("hi"))
		assertResult(expected) { Parser("test", Parser.raw_value_literal, input) }
	}

	"raw_value_literal" should "handle true" in {
		val input = List(TrueToken)
		val expected = Stream(BooleanTrueLiteralRawValue)
		assertResult(expected) { Parser("test", Parser.raw_value_literal, input) }
	}


	//union_style_enum_member
	"union_style_enum_member" should "handle import name" in { //(declaration)
		val input = List(ImportToken, VariableToken("name"))
		val expected = Stream(DeclarationUSEnumMember(ImportDeclaration(None, None, RegularPath(ImportPathName("name")))))
		assertResult(expected) { Parser("test", Parser.union_style_enum_member, input) }
	}

	"union_style_enum_member" should "handle case enumCaseName " in { //(enum case class)
		val input = List(CaseToken, VariableToken("enumCaseName"))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val expected = Stream(EnumCaseClauseUSEnumMember(None, None, caseList))
		assertResult(expected) { Parser("test", Parser.union_style_enum_member, input) }
	}

	"union_style_enum_member" should "handle case enumCaseName, enumCaseName2 " in {
		val input = List(CaseToken, VariableToken("enumCaseName"), CommaToken, VariableToken("enumCaseName2"))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None), UnionStyleEnumCase(EnumCaseName("enumCaseName2"), None))
		val expected = Stream(EnumCaseClauseUSEnumMember(None, None, caseList))
		assertResult(expected) { Parser("test", Parser.union_style_enum_member, input) }
	}

	"union_style_enum_member" should "handle @name case enumCaseName" in {
		val input = List(AtToken, VariableToken("name"), CaseToken, VariableToken("enumCaseName"))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(EnumCaseClauseUSEnumMember(Some(attributes), None, caseList))
		assertResult(expected) { Parser("test", Parser.union_style_enum_member, input) }
	}

	"union_style_enum_member" should "handle @name indirect case enumCaseName" in {
		val input = List(AtToken, VariableToken("name"), IndirectToken, CaseToken, VariableToken("enumCaseName"))
		val caseList = List(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(EnumCaseClauseUSEnumMember(Some(attributes), Some(IndirectMod()), caseList))
		assertResult(expected) { Parser("test", Parser.union_style_enum_member, input) }
	}

	//union_style_enum_case
	"union_style_enum_case" should "handle enumCaseName" in {
		val input = List(VariableToken("enumCaseName"))
		val expected = Stream(UnionStyleEnumCase(EnumCaseName("enumCaseName"), None))
		assertResult(expected) { Parser("test", Parser.union_style_enum_case, input) }
	}

	"union_style_enum_case" should "handle enumCaseName ()" in {
		val input = List(VariableToken("enumCaseName"), LeftParenToken, RightParenToken)
		val expected = Stream(UnionStyleEnumCase(EnumCaseName("enumCaseName"), Some(TupleType(List()))))
		assertResult(expected) { Parser("test", Parser.union_style_enum_case, input) }
	}

	//struct_declaration
	"struct_declaration" should "handle struct structName { }" in {
		val input = List(StructToken, VariableToken("structName"), LeftCurlyToken, RightCurlyToken)
		val structName = Structname("structName")
		val expected = Stream(StructDeclaration(None, None, structName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.struct_declaration, input) }
	}

	"struct_declaration" should "handle @name struct structName { }" in {
		val input = List(AtToken, VariableToken("name"), StructToken, VariableToken("structName"), LeftCurlyToken, RightCurlyToken)
		val structName = Structname("structName")
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(StructDeclaration(Some(theAttributes), None, structName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.struct_declaration, input) }
	}

	"struct_declaration" should "handle @name private struct structName { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, StructToken, VariableToken("structName"), LeftCurlyToken, RightCurlyToken)
		val structName = Structname("structName")
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(StructDeclaration(Some(theAttributes), Some(PrivateModifier), structName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.struct_declaration, input) }
	}

	"struct_declaration" should "handle @name private struct structName<Int> { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, StructToken, VariableToken("structName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftCurlyToken, RightCurlyToken)
		val structName = Structname("structName")
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(StructDeclaration(Some(theAttributes), Some(PrivateModifier), structName, Some(genericParams), None, None, List()))
		assertResult(expected) { Parser("test", Parser.struct_declaration, input) }
	}

	"struct_declaration" should "handle @name private struct structName<Int> : Int { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, StructToken, VariableToken("structName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"),
						ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val structName = Structname("structName")
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val expected = Stream(StructDeclaration(Some(theAttributes), Some(PrivateModifier), structName, Some(genericParams), Some(typeInheritanceClause), None, List()))
		assertResult(expected) { Parser("test", Parser.struct_declaration, input) }
	}

	"struct_declaration" should "handle @name private struct structName<Int> : Int where Int: Int { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, StructToken, VariableToken("structName"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"),
						ColonToken, VariableToken("Int"), WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val structName = Structname("structName")
		val theAttributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(StructDeclaration(Some(theAttributes), Some(PrivateModifier), structName, Some(genericParams), Some(typeInheritanceClause), Some(genericWhereClause), List()))
		assertResult(expected) { Parser("test", Parser.struct_declaration, input) }
	}

	//struct_body
	"struct_member" should "handle { import name }" in {
		val input = List(LeftCurlyToken, ImportToken, VariableToken("name"), RightCurlyToken)
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(List(DeclarationStructMember(decl)))
		assertResult(expected) { Parser("test", Parser.struct_body, input) }
	}

	"struct_member" should "handle { import name import name }" in {
		val input = List(LeftCurlyToken, ImportToken, VariableToken("name"), ImportToken, VariableToken("name"), RightCurlyToken)
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(List(DeclarationStructMember(decl), DeclarationStructMember(decl)))
		assertResult(expected) { Parser("test", Parser.struct_body, input) }
	}

	//struct_member
	"struct_member" should "handle import name" in {
		val input = List(ImportToken, VariableToken("name"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(DeclarationStructMember(decl))
		assertResult(expected) { Parser("test", Parser.struct_member, input) }
	}


	//class_declaration
	"class_declaration" should "handle final class className { }" in { //(forced final)
		val input = List(FinalToken, ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val expected = ForcedFinalClassDeclaration(None, None, className, None, None, None, List())
		val result = Parser("test", Parser.class_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name final class className { }" in { //(forced final)
		val input = List(AtToken, VariableToken("name"), FinalToken, ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = ForcedFinalClassDeclaration(Some(attributes), None, className, None, None, None, List())
		val result = Parser("test", Parser.class_declaration, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name final private class className { }" in { //(forced final)
		val input = List(AtToken, VariableToken("name"), FinalToken, PrivateToken, ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(ForcedFinalClassDeclaration(Some(attributes), Some(PrivateModifier), className, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name final private class className<Int> { }" in { //(forced final)
		val input = List(AtToken, VariableToken("name"), FinalToken, PrivateToken, ClassToken, VariableToken("className"), InfixOperatorLiteralToken("<"),
						VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val attributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(ForcedFinalClassDeclaration(Some(attributes), Some(PrivateModifier), className, Some(genericParams), None, None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name final private class className<Int> :Int { }" in { //(forced final)
		val input = List(AtToken, VariableToken("name"), FinalToken, PrivateToken, ClassToken, VariableToken("className"), InfixOperatorLiteralToken("<"),
						VariableToken("Int"), PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val attributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val expected = Stream(ForcedFinalClassDeclaration(Some(attributes), Some(PrivateModifier), className, Some(genericParams), Some(typeInheritanceClause), None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name final private class className<Int> :Int where Int:Int { }" in { //(forced final)
		val input = List(AtToken, VariableToken("name"), FinalToken, PrivateToken, ClassToken, VariableToken("className"), InfixOperatorLiteralToken("<"),
						VariableToken("Int"), PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val attributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ForcedFinalClassDeclaration(Some(attributes), Some(PrivateModifier), className, Some(genericParams), Some(typeInheritanceClause), Some(genericWhereClause), List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle class className { }" in {	//(reg)
		val input = List(ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val className = Classname("className")
		val expected = Stream(RegClassDeclaration(None, None, None, className, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name class className { }" in {	//(reg)
		val input = List(AtToken, VariableToken("name"), ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val className = Classname("className")
		val expected = Stream(RegClassDeclaration(Some(attributes), None, None, className, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name private class className { }" in {	//(reg)
		val input = List(AtToken, VariableToken("name"), PrivateToken, ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val className = Classname("className")
		val expected = Stream(RegClassDeclaration(Some(attributes), Some(PrivateModifier), None, className, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name private final class className { }" in {	//(reg)
		val input = List(AtToken, VariableToken("name"), PrivateToken, FinalToken, ClassToken, VariableToken("className"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val className = Classname("className")
		val expected = Stream(RegClassDeclaration(Some(attributes), Some(PrivateModifier), Some(FinalMod()), className, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name private final class className<Int> { }" in {	//(reg)
		val input = List(AtToken, VariableToken("name"), PrivateToken, FinalToken, ClassToken, VariableToken("className"), InfixOperatorLiteralToken("<"), VariableToken("Int"),
						PostfixOperatorLiteralToken(">"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val className = Classname("className")
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(RegClassDeclaration(Some(attributes), Some(PrivateModifier), Some(FinalMod()), className, Some(genericParams), None, None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name private final class className<Int>: Int { }" in {	//(reg)
		val input = List(AtToken, VariableToken("name"), PrivateToken, FinalToken, ClassToken, VariableToken("className"), InfixOperatorLiteralToken("<"), VariableToken("Int"),
						PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val className = Classname("className")
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val expected = Stream(RegClassDeclaration(Some(attributes), Some(PrivateModifier), Some(FinalMod()), className, Some(genericParams), Some(typeInheritanceClause), None, List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	"class_declaration" should "handle @name private final class className<Int>: Int where Int:Int }" in {	//(reg)
		val input = List(AtToken, VariableToken("name"), PrivateToken, FinalToken, ClassToken, VariableToken("className"), InfixOperatorLiteralToken("<"), VariableToken("Int"),
						PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val className = Classname("className")
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(RegClassDeclaration(Some(attributes), Some(PrivateModifier), Some(FinalMod()), className, Some(genericParams), Some(typeInheritanceClause), Some(genericWhereClause), List()))
		assertResult(expected) { Parser("test", Parser.class_declaration, input) }
	}

	//class_body
	"class_body" should "handle { import name }" in {
		val input = List(LeftCurlyToken, ImportToken, VariableToken("name"), RightCurlyToken)
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(List(DeclarationClassMember(decl)))
		assertResult(expected) { Parser("test", Parser.class_body, input) }
	}

	"class_body" should "handle { import name import name }" in {
		val input = List(LeftCurlyToken, ImportToken, VariableToken("name"), ImportToken, VariableToken("name"), RightCurlyToken)
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(List(DeclarationClassMember(decl), DeclarationClassMember(decl)))
		assertResult(expected) { Parser("test", Parser.class_body, input) }
	}

	//class_member
	"class_member" should "handle import name" in {
		val input = List(ImportToken, VariableToken("name"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(DeclarationClassMember(decl))
		assertResult(expected) { Parser("test", Parser.class_member, input) }
	}

	//actor_declaration
	"actor_declaration" should "handle actor actorName { }" in {
		val input = List(ActorToken, VariableToken("actorName"), LeftCurlyToken, RightCurlyToken)
		val actorName = ActorName("actorName")
		val expected = Stream(ActorDeclaration(None, None, actorName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.actor_declaration, input) }
	}

	"actor_declaration" should "handle @name actor actorName { }" in {
		val input = List(AtToken, VariableToken("name"), ActorToken, VariableToken("actorName"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val actorName = ActorName("actorName")
		val expected = Stream(ActorDeclaration(Some(attributes), None, actorName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.actor_declaration, input) }
	}

	"actor_declaration" should "handle @name private actor actorName { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ActorToken, VariableToken("actorName"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val actorName = ActorName("actorName")
		val expected = Stream(ActorDeclaration(Some(attributes), Some(AccessLevelModifier(PrivateModifier)), actorName, None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.actor_declaration, input) }
	}

	"actor_declaration" should "handle @name private actor actorName<Int> { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ActorToken, VariableToken("actorName"), InfixOperatorLiteralToken("<"), VariableToken("Int"),
						PostfixOperatorLiteralToken(">"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val actorName = ActorName("actorName")
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(ActorDeclaration(Some(attributes), Some(AccessLevelModifier(PrivateModifier)), actorName, Some(genericParams), None, None, List()))
		assertResult(expected) { Parser("test", Parser.actor_declaration, input) }
	}

	"actor_declaration" should "handle @name private actor actorName<Int>: Int { }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ActorToken, VariableToken("actorName"), InfixOperatorLiteralToken("<"), VariableToken("Int"),
						PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val actorName = ActorName("actorName")
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val expected = Stream(ActorDeclaration(Some(attributes), Some(AccessLevelModifier(PrivateModifier)), actorName, Some(genericParams), Some(typeInheritanceClause), None, List()))
		assertResult(expected) { Parser("test", Parser.actor_declaration, input) }
	}

	"actor_declaration" should "handle @name private actor actorName<Int>: Int where Int:Int{ }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ActorToken, VariableToken("actorName"), InfixOperatorLiteralToken("<"), VariableToken("Int"),
						PostfixOperatorLiteralToken(">"), ColonToken, VariableToken("Int"), WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, RightCurlyToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val actorName = ActorName("actorName")
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ActorDeclaration(Some(attributes), Some(AccessLevelModifier(PrivateModifier)), actorName, Some(genericParams), Some(typeInheritanceClause), Some(genericWhereClause), List()))
		assertResult(expected) { Parser("test", Parser.actor_declaration, input) }
	}

	//actor_body
	"actor_body" should "handle { import name import name }" in {
		val input = List(LeftCurlyToken, ImportToken, VariableToken("name"), ImportToken, VariableToken("name"), RightCurlyToken)
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(List(DeclarationActorMember(decl), DeclarationActorMember(decl)))
		assertResult(expected) { Parser("test", Parser.actor_body, input) }
	}

	//actor_member
	"actor_member" should "handle import name" in {
		val input = List(ImportToken, VariableToken("name"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(DeclarationActorMember(decl))
		assertResult(expected) { Parser("test", Parser.actor_member, input) }
	}

	//protocol_declaration
	"protocol_declaration" should "handle protocol protocolName { var variableName: Int { get } }" in {
		val input = List(ProtocolToken, VariableToken("protocolName"), LeftCurlyToken, VarToken, VariableToken("variableName"), ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken, RightCurlyToken)
		val protocolName = ProtocolName("protocolName")
		val name = VariableName("variableName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val protocolBody = List(ProtocolPropertyDeclaration(None, None, name, typeAnno, block))
		val expected = Stream(ProtocolDeclaration(None, None, protocolName, None, None, protocolBody))
		assertResult(expected) { Parser("test", Parser.protocol_declaration, input) }
	}

	"protocol_declaration" should "handle @name private protocol protocolName { var variableName: Int { get } }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ProtocolToken, VariableToken("protocolName"), LeftCurlyToken, VarToken, VariableToken("variableName"), ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken, RightCurlyToken)
		val protocolName = ProtocolName("protocolName")
		val name = VariableName("variableName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val protocolBody = List(ProtocolPropertyDeclaration(None, None, name, typeAnno, block))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(ProtocolDeclaration(Some(attributes), Some(PrivateModifier), protocolName, None, None, protocolBody))
		assertResult(expected) { Parser("test", Parser.protocol_declaration, input) }
	}

	"protocol_declaration" should "handle @name private protocol protocolName: Int where Int:Int { var variableName: Int { get } }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ProtocolToken, VariableToken("protocolName"), ColonToken, VariableToken("Int"),
						WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, VarToken, VariableToken("variableName"), ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken, RightCurlyToken)
		val protocolName = ProtocolName("protocolName")
		val name = VariableName("variableName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val protocolBody = List(ProtocolPropertyDeclaration(None, None, name, typeAnno, block))
		val attributes = List(Attribute(AttributeName("name"), None))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ProtocolDeclaration(Some(attributes), Some(PrivateModifier), protocolName, Some(typeInheritanceClause), Some(genericWhereClause), protocolBody))
		assertResult(expected) { Parser("test", Parser.protocol_declaration, input) }
	}

	//protocol_body
	"protocol_body" should "handle { var variableName: Int { get } } " in {
		val input = List(LeftCurlyToken, VarToken, VariableToken("variableName"), ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken, RightCurlyToken)
		val name = VariableName("variableName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val expected = Stream(List(ProtocolPropertyDeclaration(None, None, name, typeAnno, block)))
		assertResult(expected) { Parser("test", Parser.protocol_body, input) }
	}

	//protocol_member
	"protocol_member" should "handle var variableName: Int { get } " in {
		val input = List(VarToken, VariableToken("variableName"), ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken)
		val name = VariableName("variableName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val expected = Stream(ProtocolPropertyDeclaration(None, None, name, typeAnno, block))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	//(protocol_method_declaration passthrough)
	"protocol_member" should "handle func sum (localParamName: Int)" in {
		val input = List(FuncToken, VariableToken("sum"), LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val head = FunctionHead(None, None)
		val name = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val signature = ThrowsFunctionSig(paramClause, None, None, None)
		val expected = Stream(ProtocolMethodDeclaration(head, name, None, signature, None))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	//(initializer_declaration)
	"protocol_member" should "handle init ()" in {
		val input = List(InitToken, LeftParenToken, RightParenToken)
		val initHead = RegInitHead(None, None)
		val expected = Stream(ThrowsProtocolInitializerDeclaration(initHead, None, List(), None, None))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	"protocol_member" should "handle init<Int> ()" in {
		val input = List(InitToken, InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken)
		val initHead = RegInitHead(None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(ThrowsProtocolInitializerDeclaration(initHead, Some(genericParams), List(), None, None))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	"protocol_member" should "handle init<Int> () throws where Int:Int" in {
		val input = List(InitToken, InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken,
						ThrowsToken, WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"))
		val initHead = RegInitHead(None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ThrowsProtocolInitializerDeclaration(initHead, Some(genericParams), List(), Some(ThrowsMod()), Some(genericWhereClause)))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	"protocol_member" should "handle init<Int> () rethrows where Int:Int" in {
		val input = List(InitToken, InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken,
						RethrowsToken, WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"))
		val initHead = RegInitHead(None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(RethrowsProtocolInitializerDeclaration(initHead, Some(genericParams), List(), Some(genericWhereClause)))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	//(protocol_subscript_decalaration passthrough)
	"protocol_member" should "handle subscript () -> Int { get }" in {
		val input = List(SubscriptToken, LeftParenToken, RightParenToken, PrefixOperatorLiteralToken("->"), VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken)
		val head = SubscriptHead(None, None, None, List())
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val result = SubscriptResult(None, theType)
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val expected = Stream(ProtocolSubscriptDeclaration(head, result, None, block))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	//(protocol_associated_type_declaration passthrough)
	"protocol_member" should "handle associatedtype theType" in {
		val input = List(AssociatedTypeToken, VariableToken("theType"))
		val typeAliasName = TypeAliasName("theType")
		val expected = Stream(ProtocolAssociatedTypeDeclaration(None, None, typeAliasName, None, None, None))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}

	//(typealias_declaration passthrough)
	"protocol_member" should "handle typealias theType = Int" in {
		val input = List(TypeAliasToken, VariableToken("theType"), InfixOperatorLiteralToken("="), VariableToken("Int"))
		val expected = Stream(ProtocolTypeAliasDeclaration(TypeAliasDeclaration(None, None, TypeAliasName("theType"), None, NormalTypeIdentifier(TypeName("Int")))))
		assertResult(expected) { Parser("test", Parser.protocol_member, input) }
	}


	//protocol_property_declaration
	"protocol_property_declaration" should "handle var variableName: Int { get } " in {
		val input = List(VarToken, VariableToken("variableName"), ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken)
		val name = VariableName("variableName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val expected = Stream(ProtocolPropertyDeclaration(None, None, name, typeAnno, block))
		assertResult(expected) { Parser("test", Parser.protocol_property_declaration, input) }
	}

	//protocol_method_declaration
	"protocol_method_declaration" should "handle func sum (localParamName: Int)" in {
		val input = List(FuncToken, VariableToken("sum"), LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val head = FunctionHead(None, None)
		val name = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val signature = ThrowsFunctionSig(paramClause, None, None, None)
		val expected = Stream(ProtocolMethodDeclaration(head, name, None, signature, None))
		assertResult(expected) { Parser("test", Parser.protocol_method_declaration, input) }
	}

	"protocol_method_declaration" should "handle func sum<Int> (localParamName: Int) " in {
		val input = List(FuncToken, VariableToken("sum"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"),
						LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken)
		val head = FunctionHead(None, None)
		val name = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val signature = ThrowsFunctionSig(paramClause, None, None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(ProtocolMethodDeclaration(head, name, Some(genericParams), signature, None))
		assertResult(expected) { Parser("test", Parser.protocol_method_declaration, input) }
	}

	"protocol_method_declaration" should "handle func sum<Int> (localParamName: Int) where Int:Int" in {
		val input = List(FuncToken, VariableToken("sum"), InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"),
						LeftParenToken, VariableToken("localParamName"), ColonToken, VariableToken("Int"), RightParenToken, WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"))
		val head = FunctionHead(None, None)
		val name = IdentifierFunctionName("sum")
		val localParamName = LocalParamName("localParamName")
		val typeAnno = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val paramClause = List(OptDefaultArgClauseParameter(None, localParamName, typeAnno, None))
		val signature = ThrowsFunctionSig(paramClause, None, None, None)
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ProtocolMethodDeclaration(head, name, Some(genericParams), signature, Some(genericWhereClause)))
		assertResult(expected) { Parser("test", Parser.protocol_method_declaration, input) }
	}

	//protocol_subscript_decalaration
	"protocol_subscript_decalaration" should "handle subscript () -> Int { get }" in {
		val input = List(SubscriptToken, LeftParenToken, RightParenToken, PrefixOperatorLiteralToken("->"), VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken)
		val head = SubscriptHead(None, None, None, List())
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val result = SubscriptResult(None, theType)
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val expected = Stream(ProtocolSubscriptDeclaration(head, result, None, block))
		assertResult(expected) { Parser("test", Parser.protocol_subscript_declaration, input) }
	}

	"protocol_subscript_decalaration" should "handle subscript () -> Int where Int:Int { get } " in {
		val input = List(SubscriptToken, LeftParenToken, RightParenToken, PrefixOperatorLiteralToken("->"), VariableToken("Int"), WhereToken, VariableToken("Int"),
						ColonToken, VariableToken("Int"), LeftCurlyToken, GetToken, RightCurlyToken)
		val head = SubscriptHead(None, None, None, List())
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val result = SubscriptResult(None, theType)
		val getterKeyword = GetterKeywordClause(None, None)
		val block = GetterSetterKeywordBlock(getterKeyword, None)
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ProtocolSubscriptDeclaration(head, result, Some(genericWhereClause), block))
		assertResult(expected) { Parser("test", Parser.protocol_subscript_declaration, input) }
	}

	//protocol_associated_type_declaration
	"protocol_associated_type_declaration" should "handle associatedtype theType" in {
		val input = List(AssociatedTypeToken, VariableToken("theType"))
		val typeAliasName = TypeAliasName("theType")
		val expected = Stream(ProtocolAssociatedTypeDeclaration(None, None, typeAliasName, None, None, None))
		assertResult(expected) { Parser("test", Parser.protocol_associated_type_declaration, input) }
	}

	"protocol_associated_type_declaration" should "handle @name private associatedtype theType" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, AssociatedTypeToken, VariableToken("theType"))
		val typeAliasName = TypeAliasName("theType")
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(ProtocolAssociatedTypeDeclaration(Some(attributes), Some(PrivateModifier), typeAliasName, None, None, None))
		assertResult(expected) { Parser("test", Parser.protocol_associated_type_declaration, input) }
	}

	"protocol_associated_type_declaration" should "handle @name private associatedtype theType: Int" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, AssociatedTypeToken, VariableToken("theType"), ColonToken, VariableToken("Int"))
		val typeAliasName = TypeAliasName("theType")
		val attributes = List(Attribute(AttributeName("name"), None))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val expected = Stream(ProtocolAssociatedTypeDeclaration(Some(attributes), Some(PrivateModifier), typeAliasName, Some(typeInheritanceClause), None, None))
		assertResult(expected) { Parser("test", Parser.protocol_associated_type_declaration, input) }
	}

	"protocol_associated_type_declaration" should "handle @name private associatedtype theType: Int = Int" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, AssociatedTypeToken, VariableToken("theType"), ColonToken, VariableToken("Int"),
						InfixOperatorLiteralToken("="), VariableToken("Int"))
		val typeAliasName = TypeAliasName("theType")
		val attributes = List(Attribute(AttributeName("name"), None))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeAlias = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(ProtocolAssociatedTypeDeclaration(Some(attributes), Some(PrivateModifier), typeAliasName, Some(typeInheritanceClause), Some(typeAlias), None))
		assertResult(expected) { Parser("test", Parser.protocol_associated_type_declaration, input) }
	}

	"protocol_associated_type_declaration" should "handle @name private associatedtype theType: Int = Int where Int:Int" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, AssociatedTypeToken, VariableToken("theType"), ColonToken, VariableToken("Int"),
						InfixOperatorLiteralToken("="), VariableToken("Int"), WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"))
		val typeAliasName = TypeAliasName("theType")
		val attributes = List(Attribute(AttributeName("name"), None))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeAlias = NormalTypeIdentifier(TypeName("Int"))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ProtocolAssociatedTypeDeclaration(Some(attributes), Some(PrivateModifier), typeAliasName, Some(typeInheritanceClause), Some(typeAlias), Some(genericWhereClause)))
		assertResult(expected) { Parser("test", Parser.protocol_associated_type_declaration, input) }
	}

	"initializer_declaration" should "handle init <Int> () { 5 }" in {
		val input = List(InitToken, PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(ThrowsInitializerDeclaration(initHead, Some(genericParams), List(), None, None, None, block))
		assertResult(expected) { Parser("test", Parser.initializer_declaration, input) }
	}

	"initializer_declaration" should "handle init <Int> () async throws { 5 }" in {
		val input = List(InitToken, PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken,
						AsyncToken, ThrowsToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(ThrowsInitializerDeclaration(initHead, Some(genericParams), List(), Some(AsyncMod()), Some(ThrowsMod()), None, block))
		assertResult(expected) { Parser("test", Parser.initializer_declaration, input) }
	}

	"initializer_declaration" should "handle init <Int> () async throws where Int:Int { 5 }" in {
		val input = List(InitToken, PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken,
						AsyncToken, ThrowsToken, WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ThrowsInitializerDeclaration(initHead, Some(genericParams), List(), Some(AsyncMod()), Some(ThrowsMod()), Some(genericWhereClause), block))
		assertResult(expected) { Parser("test", Parser.initializer_declaration, input) }
	}

	"initializer_declaration" should "handle init () rethrows { 5 }" in {
		val input = List(InitToken, LeftParenToken, RightParenToken, RethrowsToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val expected = Stream(RethrowsInitializerDeclaration(initHead, None, List(), None, None, block))
		assertResult(expected) { Parser("test", Parser.initializer_declaration, input) }
	}

	"initializer_declaration" should "handle init <Int> () rethrows { 5 }" in {
		val input = List(InitToken, PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken, RethrowsToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(RethrowsInitializerDeclaration(initHead, Some(genericParams), List(), None, None, block))
		assertResult(expected) { Parser("test", Parser.initializer_declaration, input) }
	}

	"initializer_declaration" should "handle init <Int> () async rethrows { 5 }" in {
		val input = List(InitToken, PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken,
						AsyncToken, RethrowsToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(RethrowsInitializerDeclaration(initHead, Some(genericParams), List(), Some(AsyncMod()), None, block))
		assertResult(expected) { Parser("test", Parser.initializer_declaration, input) }
	}

	"initializer_declaration" should "handle init <Int> () async rethrows where Int:Int { 5 }" in {
		val input = List(InitToken, PrefixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken,
						AsyncToken, RethrowsToken, WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val initHead = RegInitHead(None, None)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(RethrowsInitializerDeclaration(initHead, Some(genericParams), List(), Some(AsyncMod()), Some(genericWhereClause), block))
		assertResult(expected) { Parser("test", Parser.initializer_declaration, input) }
	}

	//initializer_head
	"initializer_head" should "handle init" in {
		val input = List(InitToken)
		val expected = Stream(RegInitHead(None, None))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle @name init" in {
		val input = List(AtToken, VariableToken("name"), InitToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(RegInitHead(Some(attributes), None))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle @name lazy init" in {
		val input = List(AtToken, VariableToken("name"), LazyToken, InitToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(RegInitHead(Some(attributes), Some(List(LazyModifier))))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle init ?" in {
		val input = List(InitToken, InfixOperatorLiteralToken("?"))
		val expected = Stream(QuestionInitHead(None, None))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle @name init ?" in {
		val input = List(AtToken, VariableToken("name"), InitToken, InfixOperatorLiteralToken("?"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(QuestionInitHead(Some(attributes), None))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle @name lazy init ?" in {
		val input = List(AtToken, VariableToken("name"), LazyToken, InitToken, InfixOperatorLiteralToken("?"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(QuestionInitHead(Some(attributes), Some(List(LazyModifier))))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle init !" in {
		val input = List(InitToken, InfixOperatorLiteralToken("!"))
		val expected = Stream(ExclamationInitHead(None, None))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle @name init !" in {
		val input = List(AtToken, VariableToken("name"), InitToken, InfixOperatorLiteralToken("!"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(ExclamationInitHead(Some(attributes), None))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	"initializer_head" should "handle @name lazy init !" in {
		val input = List(AtToken, VariableToken("name"), LazyToken, InitToken, InfixOperatorLiteralToken("!"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(ExclamationInitHead(Some(attributes), Some(List(LazyModifier))))
		assertResult(expected) { Parser("test", Parser.initializer_head, input) }
	}

	//deinitializer_declaration
	"deinitializer_declaration" should "handle deinit { 5 }" in {
		val input = List(DeinitToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val expected = Stream(DeinitializerDeclaration(None, block))
		assertResult(expected) { Parser("test", Parser.deinitializer_declaration, input) }
	}

	"deinitializer_declaration" should "handle @name deinit { 5 }" in {
		val input = List(AtToken, VariableToken("name"), DeinitToken, LeftCurlyToken, DecimalIntegerLiteralToken("5"), RightCurlyToken)
		val block = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("5")))))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(DeinitializerDeclaration(Some(attributes), block))
		assertResult(expected) { Parser("test", Parser.deinitializer_declaration, input) }
	}

	//extension_declaration
	"extension_declaration" should "handle extension Int { import name }" in {
		val input = List(ExtensionToken, VariableToken("Int"), LeftCurlyToken, ImportToken, VariableToken("name"), RightCurlyToken)
		val typeID = NormalTypeIdentifier(TypeName("Int"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val body = List(DeclarationExtensionMember(decl))
		val expected = Stream(ExtensionDeclaration(None, None, typeID, None, None, body))
		assertResult(expected) { Parser("test", Parser.extension_declaration, input) }
	}

	"extension_declaration" should "handle @name private extension Int { import name }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ExtensionToken, VariableToken("Int"), LeftCurlyToken, ImportToken, VariableToken("name"), RightCurlyToken)
		val typeID = NormalTypeIdentifier(TypeName("Int"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val body = List(DeclarationExtensionMember(decl))
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(ExtensionDeclaration(Some(attributes), Some(PrivateModifier), typeID, None, None, body))
		assertResult(expected) { Parser("test", Parser.extension_declaration, input) }
	}

	"extension_declaration" should "handle @name private extension Int :Int where Int:Int{ import name }" in {
		val input = List(AtToken, VariableToken("name"), PrivateToken, ExtensionToken, VariableToken("Int"), ColonToken, VariableToken("Int"), WhereToken, VariableToken("Int"), ColonToken, VariableToken("Int"), LeftCurlyToken, ImportToken, VariableToken("name"), RightCurlyToken)
		val typeID = NormalTypeIdentifier(TypeName("Int"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val body = List(DeclarationExtensionMember(decl))
		val attributes = List(Attribute(AttributeName("name"), None))
		val typeInheritanceClause = List(TypeInheritance(None, NormalTypeIdentifier(TypeName("Int"))))
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val expected = Stream(ExtensionDeclaration(Some(attributes), Some(PrivateModifier), typeID, Some(typeInheritanceClause), Some(genericWhereClause), body))
		assertResult(expected) { Parser("test", Parser.extension_declaration, input) }
	}

	//extension_body
	"extension_body" should "handle { import name }" in {
		val input = List(LeftCurlyToken, ImportToken, VariableToken("name"), RightCurlyToken)
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(List(DeclarationExtensionMember(decl)))
		assertResult(expected) { Parser("test", Parser.extension_body, input) }
	}

	//extension_member
	"extension_member" should "handle import name" in {
		val input = List(ImportToken, VariableToken("name"))
		val decl = ImportDeclaration(None, None, RegularPath(ImportPathName("name")))
		val expected = Stream(DeclarationExtensionMember(decl))
		assertResult(expected) { Parser("test", Parser.extension_member, input) }
	}

	//subscript_declaration
	"subscript_declaration" should "handle subscript () -> Int { 0 }" in {
		val input = List(SubscriptToken, LeftParenToken, RightParenToken, PrefixOperatorLiteralToken("->"), VariableToken("Int"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val head = SubscriptHead(None, None, None, List())
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val result = SubscriptResult(None, theType)
		val block = AllowableGetterSetterBlock(CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0"))))))
		val expected = Stream(SubscriptDeclaration(head, result, None, block))
		assertResult(expected) { Parser("test", Parser.subscript_declaration, input) }
	}

	"subscript_declaration" should "handle subscript () -> Int where Int: Int{ 0 }" in {
		val input = List(SubscriptToken, LeftParenToken, RightParenToken, PrefixOperatorLiteralToken("->"), VariableToken("Int"), WhereToken, VariableToken("Int"),
						ColonToken, VariableToken("Int"), LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val head = SubscriptHead(None, None, None, List())
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val result = SubscriptResult(None, theType)
		val typeID1 = NormalTypeIdentifier(TypeName("Int"))
		val typeID2 = NormalTypeIdentifier(TypeName("Int"))
		val genericWhereClause = List(ConformanceRequirementDoubleTypeID(typeID1, typeID2))
		val block = AllowableGetterSetterBlock(CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0"))))))
		val expected = Stream(SubscriptDeclaration(head, result, Some(genericWhereClause), block))
		assertResult(expected) { Parser("test", Parser.subscript_declaration, input) }
	}

	//subscript_head
	"subscript_head" should "handle subscript ()" in {
		val input = List(SubscriptToken, LeftParenToken, RightParenToken)
		val expected = Stream(SubscriptHead(None, None, None, List()))
		assertResult(expected) { Parser("test", Parser.subscript_head, input) }
	}

	"subscript_head" should "handle @name lazy subscript ()" in {
		val input = List(AtToken, VariableToken("name"), LazyToken, SubscriptToken, LeftParenToken, RightParenToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val expected = Stream(SubscriptHead(Some(attributes), Some(List(LazyModifier)), None, List()))
		assertResult(expected) { Parser("test", Parser.subscript_head, input) }
	}

	"subscript_head" should "handle @name lazy subscript<Int> ()" in {
		val input = List(AtToken, VariableToken("name"), LazyToken, SubscriptToken, InfixOperatorLiteralToken("<"), VariableToken("Int"), PostfixOperatorLiteralToken(">"), LeftParenToken, RightParenToken)
		val attributes = List(Attribute(AttributeName("name"), None))
		val genericParams: List[GenericParameter] = List(SimpleGenericParameter(TypeName("Int")))
		val expected = Stream(SubscriptHead(Some(attributes), Some(List(LazyModifier)), Some(genericParams), List()))
		assertResult(expected) { Parser("test", Parser.subscript_head, input) }
	}

	//subscript_result
	"subscript_result" should "handle Int" in {
		val input = List(PrefixOperatorLiteralToken("->"), VariableToken("Int"))
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(SubscriptResult(None, theType))
		assertResult(expected) { Parser("test", Parser.subscript_result, input) }
	}

	"subscript_result" should "handle @name Int" in {
		val input = List(PrefixOperatorLiteralToken("->"), AtToken, VariableToken("name"), VariableToken("Int"))
		val attributes = List(Attribute(AttributeName("name"), None))
		val theType = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(SubscriptResult(Some(attributes), theType))
		assertResult(expected) { Parser("test", Parser.subscript_result, input) }
	}

	//allowable_subscript_block
	"allowable_subscript_block" should "handle { 0 }" in {
		val input = List(LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken)
		val expected = Stream(AllowableGetterSetterBlock(CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))))
		assertResult(expected) { Parser("test", Parser.allowable_subscript_block, input) }
	}

	"allowable_subscript_block" should "handle { get {0} }" in {
		val input = List(LeftCurlyToken, GetToken, LeftCurlyToken, DecimalIntegerLiteralToken("0"), RightCurlyToken, RightCurlyToken)
		val codeBlock = CodeBlock(Some(List(ExpressionStmt(DecimalIntegerLiteralExp("0")))))
		val getter = GetterClause(None, None, codeBlock)
		val expected = AllowableGetterSetterBlock(GetterSetterClauseBlock(getter, None))
		val result = Parser("test", Parser.allowable_subscript_block, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.allowable_subscript_block, input) }
	}


	"allowable_subscript_block" should "handle { get }" in {
		val input = List(LeftCurlyToken, GetToken, RightCurlyToken)
		val getterKeyword = GetterKeywordClause(None, None)
		val expected = AllowableKeywordBlock(GetterSetterKeywordBlock(getterKeyword, None))
		val result = Parser("test", Parser.allowable_subscript_block, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.allowable_subscript_block, input) }
	}

	//operator_declaration
	"operator_declaration" should "handle prefix operator +-" in {
		val input = List(PrefixToken, OperatorToken, InfixOperatorLiteralToken("+-"))
		val expected = Stream(PrefixOperatorDeclaration(Operator("+-")))
		assertResult(expected) { Parser("test", Parser.operator_declaration, input) }
	}

	//operator_decl
	"operator_decl" should "handle prefix operator +-" in {
		val input = List(PrefixToken, OperatorToken, InfixOperatorLiteralToken("+-"))
		val expected = Stream(PrefixOperatorDeclaration(Operator("+-")))
		assertResult(expected) { Parser("test", Parser.operator_declaration, input) }
	}

	"operator_decl" should "handle postfix operator +-" in {
		val input = List(PostFixToken, OperatorToken, InfixOperatorLiteralToken("+-"))
		val expected = Stream(PostfixOperatorDeclaration(Operator("+-")))
		assertResult(expected) { Parser("test", Parser.operator_declaration, input) }
	}

	"operator_decl" should "handle infix operator +-" in {
		val input = List(InfixToken, OperatorToken, InfixOperatorLiteralToken("+-"))
		val expected = Stream(InfixOperatorDeclaration(Operator("+-"), None))
		assertResult(expected) { Parser("test", Parser.operator_declaration, input) }
	}

	"operator_decl" should "handle infix operator +- : Addition" in {
		val input = List(InfixToken, OperatorToken, InfixOperatorLiteralToken("+-"), ColonToken, VariableToken("Addition"))
		val group = PrecedenceGroupName("Addition")
		val expected = Stream(InfixOperatorDeclaration(Operator("+-"), Some(group)))
		assertResult(expected) { Parser("test", Parser.operator_declaration, input) }
	}

	"operator_decl" should "handle infix operator +- : AdditionPrecedence" in {
		val input = List(InfixToken, OperatorToken, InfixOperatorLiteralToken("+-"), ColonToken, VariableToken("AdditionPrecedence"))
		val group = PrecedenceGroupName("AdditionPrecedence")
		val expected = Stream(InfixOperatorDeclaration(Operator("+-"), Some(group)))
		assertResult(expected) { Parser("test", Parser.operator_declaration, input) }
	}

	//precedence_group_declaration
	"precedence_group_declaration" should "handle precedencegroup groupName { }" in {
		val input = List(PrecedenceGroupToken, VariableToken("groupName"), LeftCurlyToken, RightCurlyToken)
		val name = PrecedenceGroupName("groupName")
		val expected = Stream(PrecedenceGroupDeclaration(name, None))
		assertResult(expected) { Parser("test", Parser.precedence_group_declaration, input) }
	}

	"precedence_group_declaration" should "handle precedencegroup groupName { higherThan: Addition }" in {
		val input = List(PrecedenceGroupToken, VariableToken("groupName"), LeftCurlyToken, HigherThanToken, ColonToken, VariableToken("Addition"), RightCurlyToken)
		val name = PrecedenceGroupName("groupName")
		val groupAttributes = List(PrecedenceGroupRelationHigher(List(PrecedenceGroupName("Addition"))))
		val expected = Stream(PrecedenceGroupDeclaration(name, Some(groupAttributes)))
		assertResult(expected) { Parser("test", Parser.precedence_group_declaration, input) }
	}

	"precedence_group_declaration" should "handle precedencegroup groupName { higherThan: Addition lowerThan: Idk}" in {
		val input = List(PrecedenceGroupToken, VariableToken("groupName"), LeftCurlyToken, HigherThanToken, ColonToken, VariableToken("Addition"),
						LowerThanToken, ColonToken, VariableToken("Idk"), RightCurlyToken)
		val name = PrecedenceGroupName("groupName")
		val higherRelation = PrecedenceGroupRelationHigher(List(PrecedenceGroupName("Addition")))
		val lowerRelation = PrecedenceGroupRelationLower(List(PrecedenceGroupName("Idk")))
		val groupAttributes = List(higherRelation, lowerRelation)
		val expected = Stream(PrecedenceGroupDeclaration(name, Some(groupAttributes)))
		assertResult(expected) { Parser("test", Parser.precedence_group_declaration, input) }
	}

	//precedence_group_attribute
	"precedence_group_attribute" should "handle higherThan: Addition" in {
		val input = List(HigherThanToken, ColonToken, VariableToken("Addition"))
		val expected = Stream(PrecedenceGroupRelationHigher(List(PrecedenceGroupName("Addition"))))
		assertResult(expected) { Parser("test", Parser.precedence_group_attribute, input) }
	}

	"precedence_group_attribute" should "handle lowerThan: Addition, OtherCategory" in {
		val input = List(LowerThanToken, ColonToken, VariableToken("Addition"), CommaToken, VariableToken("otherCategory"))
		val expected = Stream(PrecedenceGroupRelationLower(List(PrecedenceGroupName("Addition"), PrecedenceGroupName("otherCategory"))))
		assertResult(expected) { Parser("test", Parser.precedence_group_attribute, input) }
	}

	"precedence_group_attribute" should "handle assignment: True" in {
		val input = List(AssignmentToken, ColonToken, TrueToken)
		val expected = Stream(PrecedenceGroupAssignmentTrue)
		assertResult(expected) { Parser("test", Parser.precedence_group_attribute, input) }
	}

	"precedence_group_attribute" should "handle associativity: left" in {
		val input = List(AssociativityToken, ColonToken, LeftToken)
		val expected = Stream(PrecedenceGroupLeftAssociative)
		assertResult(expected) { Parser("test", Parser.precedence_group_attribute, input) }
	}

	"precedence_group_attribute" should "handle associativity: right" in {
		val input = List(AssociativityToken, ColonToken, RightToken)
		val expected = Stream(PrecedenceGroupRightAssociative)
		assertResult(expected) { Parser("test", Parser.precedence_group_attribute, input) }
	}

	"precedence_group_attribute" should "handle associativity: none" in {
		val input = List(AssociativityToken, ColonToken, NoneToken)
		val expected = Stream(PrecedenceGroupNotAssociative)
		assertResult(expected) { Parser("test", Parser.precedence_group_attribute, input) }
	}

	//end of testing for declarations and helpers

	//testing for patterns
	//pattern
	//(wildcard_pattern) thru pattern
	"pattern" should "handle _ " in {
		val input = List(UnderscoreToken)
		val expected = WildcardPattern(None)
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//(identifier_pattern) thru pattern
	"pattern" should "handle name" in {
		val input = List(VariableToken("name"))
		val expected = IdentifierPattern(PatternName("name"), None)
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	"pattern" should "handle name: String" in {
		val input = List(VariableToken("name"), ColonToken, VariableToken("String"))
		val typeAnnotation = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String")))
		val expected = Stream(IdentifierPattern(PatternName("name"), Some(typeAnnotation)))
		assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//(value_binding_pattern) thru pattern
	"pattern" should "handle var _" in {
		val input = List(VarToken, UnderscoreToken)
		val expected = ValueBindingPattern(VarModifier, WildcardPattern(None))
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//(tuple_pattern) thru pattern
	"pattern" should "handle (_)" in {
		val input = List(LeftParenToken, UnderscoreToken, RightParenToken)
		val expected = TuplePattern(List(PatternElement(WildcardPattern(None))), None)
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	"pattern" should "handle (_): Int" in {
		val input = List(LeftParenToken, UnderscoreToken, RightParenToken, ColonToken, VariableToken("Int"))
		val typeAnnotation = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int")))
		val expected = TuplePattern(List(PatternElement(WildcardPattern(None))), Some(typeAnnotation))
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//(enum_case_pattern) thru pattern
	"pattern" should "handle .name" in {
		val input = List(PrefixDotOperatorLiteralToken("."), VariableToken("name"))
		val expected = EnumCasePattern(None, EnumCaseName("name"), None)
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//(optional_pattern) thru pattern
	"pattern" should "handle: name?" in {
		val input = List(VariableToken("name"), PostfixOperatorLiteralToken("?"))
		val expected = OptionalPattern(IdentifierPattern(PatternName("name"), None))
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//(type_casting_pattern) thru pattern
	"pattern" should "handle an is-pattern: is Int" in {
		val input = List(IsToken, VariableToken("Int"))
		val expected = Stream(IsTypeCastingPattern(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//same as above
	"pattern" should "handle an as-pattern: _ as Int" in {
		val input = List(UnderscoreToken, AsToken, VariableToken("Int"))
		val expected = AsTypeCastingPattern(WildcardPattern(None), NormalTypeIdentifier(TypeName("Int")))
		val result = Parser("test", Parser.pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//(expression_pattern) thru pattern
	"pattern" should "handle an expression: 5" in {
		val input = List(DecimalIntegerLiteralToken("5"))
		val expected = Stream(ExpressionPattern(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.pattern, input) }
	}

	//wildcard_pattern
	"wildcard_pattern" should "handle _ " in {
		val input = List(UnderscoreToken)
		val expected = Stream(WildcardPattern(None))
		assertResult(expected) { Parser("test", Parser.wildcard_pattern, input) }
	}

	"wildcard_pattern" should "handle _ : Int" in {
		val input = List(UnderscoreToken, ColonToken, VariableToken("Int"))
		val expected = Stream(WildcardPattern(Some(TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Int"))))))
		assertResult(expected) { Parser("test", Parser.wildcard_pattern, input) }
	}

	"identifier_pattern" should "handle name" in {
		val input = List(VariableToken("name"))
		val expected = Stream(IdentifierPattern(PatternName("name"), None))
		assertResult(expected) { Parser("test", Parser.identifier_pattern, input) }
	}

	"value_binding_pattern" should "handle var _" in {
		val input = List(VarToken, UnderscoreToken)
		val expected = ValueBindingPattern(VarModifier, WildcardPattern(None))
		val result = Parser("test", Parser.value_binding_pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.value_binding_pattern, input) }
	}

	"value_binding_pattern" should "handle let _" in {
		val input = List(LetToken, UnderscoreToken)
		val expected = ValueBindingPattern(LetModifier, WildcardPattern(None))
		val result = Parser("test", Parser.value_binding_pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.value_binding_pattern, input) }
	}

	"tuple_pattern" should "handle (_)" in {
		val input = List(LeftParenToken, UnderscoreToken, RightParenToken)
		val expected = TuplePattern(List(PatternElement(WildcardPattern(None))), None)
		val result = Parser("test", Parser.tuple_pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.tuple_pattern, input) }
	}

	"tuple_pattern" should "handle (_, _)" in {
		val input = List(LeftParenToken, UnderscoreToken, CommaToken, UnderscoreToken, RightParenToken)
		val expected = TuplePattern(List(PatternElement(WildcardPattern(None)), PatternElement(WildcardPattern(None))), None)
		val result = Parser("test", Parser.tuple_pattern, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.tuple_pattern, input) }
	}

	"enum_case_pattern" should "handle .name" in {
		val input = List(PrefixDotOperatorLiteralToken("."), VariableToken("name"))
		val expected = Stream(EnumCasePattern(None, EnumCaseName("name"), None))
		assertResult(expected) { Parser("test", Parser.enum_case_pattern, input) }
	}

	//AMBIGUITY HERE AGAIN ???
 	"enum_case_pattern" should "handle Int.name" in {
		val input = List(VariableToken("Int"), InfixDotOperatorLiteralToken("."), VariableToken("name"))
		val typeIdentifier = NormalTypeIdentifier(TypeName("Int"))
		val expected = Stream(EnumCasePattern(Some(typeIdentifier), EnumCaseName("name"), None))
		assertResult(expected) { Parser("test", Parser.enum_case_pattern, input) }
	}

 	"enum_case_pattern" should "handle .name(_)" in {
		val input = List(PrefixDotOperatorLiteralToken("."), VariableToken("name"), LeftParenToken, UnderscoreToken, RightParenToken)
		val tuplePattern = TuplePattern(List(PatternElement(WildcardPattern(None))), None)
		val expected = Stream(EnumCasePattern(None, EnumCaseName("name"), Some(tuplePattern)))
		assertResult(expected) { Parser("test", Parser.enum_case_pattern, input) }
	}

	"type_casting_pattern" should "handle an is-pattern: is Int" in {
		val input = List(IsToken, VariableToken("Int"))
		val expected = Stream(IsTypeCastingPattern(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.is_type_casting_pattern, input) }
	}

	"expression_pattern" should "handle an expression: 5" in {
		val input = List(DecimalIntegerLiteralToken("5"))
		val expected = Stream(ExpressionPattern(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.expression_pattern, input) }
	}
	//end testing for patterns

	//testing for types
	//typ
	"typ" should "handle Any?.Type" in {
		val input = List(AnyToken, InfixOperatorLiteralToken("?"), InfixDotOperatorLiteralToken("."), TypeToken)
		val expected = Stream(MetatypeTypeType(OptionalType(NormalTypeIdentifier(TypeName("Any")))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	"typ" should "handle [Any?].Type" in {
		val input = List(LeftBracketToken, AnyToken, InfixOperatorLiteralToken("?"), RightBracketToken, InfixDotOperatorLiteralToken("."), TypeToken)
		val expected = Stream(MetatypeTypeType(ArrayType(OptionalType(NormalTypeIdentifier(TypeName("Any"))))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(function_type) thru typ
	"typ" should "handle () -> Int" in {
		val input = List(LeftParenToken, RightParenToken, InfixOperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List()
		val expected = Stream(FunctionType(None, list, None, None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(array_type) thru typ
	"typ" should "handle [Int]" in {
		val input = List(LeftBracketToken, VariableToken("Int"), RightBracketToken)
		val expected = Stream(ArrayType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(dictionary_type) thru typ
	"typ" should "handle [Int: Bool]" in {
		val input = List(LeftBracketToken, VariableToken("Int"), ColonToken, VariableToken("Bool"), RightBracketToken)
		val expected = Stream(DictionaryType(NormalTypeIdentifier(TypeName("Int")), NormalTypeIdentifier(TypeName("Bool"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(type_identifier) thru typ
	"typ" should "handle the normal type: Int" in {
		val input = List(VariableToken("Int"))
		val expected = Stream(NormalTypeIdentifier(TypeName("Int")))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(tuple_type) thru typ
	"typ" should "handle ()" in {
		val input = List(LeftParenToken, RightParenToken)
		val emptyList: List[TupleTypeElement] = List()
		val expected = Stream(TupleType(emptyList))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(optional_type) thru typ
	"typ" should "handle Any?" in {
		val input = List(AnyToken, PostfixOperatorLiteralToken("?"))
		val expected = Stream(OptionalType(NormalTypeIdentifier(TypeName("Any"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	"typ" should "handle Int?" in {
		val input = List(VariableToken("Int"), PostfixOperatorLiteralToken("?"))
		val expected = Stream(OptionalType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(implicitly_unwrapped_optional_type) thru typ
	"typ" should "handle Int!" in {
		val input = List(VariableToken("Int"), PostfixOperatorLiteralToken("!"))
		val expected = Stream(ImplicitlyUnwrappedOptionalType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(protocol_composition_type) thru typ
	"typ" should "handle: Int & Bool" in {
		val input = List(VariableToken("Int"), InfixOperatorLiteralToken("&"), VariableToken("Bool"))
		val list = List(NormalTypeIdentifier(TypeName("Int")),
				   NormalTypeIdentifier(TypeName("Bool")))
		val expected = Stream(ProtocolCompositionType(list))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(opaque_type) thru typ
	"typ" should "handle: some Int" in {
		val input = List(SomeToken, VariableToken("Int"))
		val expected = Stream(OpaqueType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(metatype_type) thru typ
	"typ" should "handle Int.Type" in {
		val input = List(VariableToken("Int"), InfixDotOperatorLiteralToken("."), TypeToken)
		val expected = Stream(MetatypeTypeType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	"typ" should "handle Int.Protocol" in {
		val input = List(VariableToken("Int"), InfixDotOperatorLiteralToken("."), ProtocolToken)
		val expected = Stream(MetatypeProtocolType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}

	//(AnyType())
	"typ" should "handle: Any" in {
		assertResult(Stream(NormalTypeIdentifier(TypeName("Any")))) { Parser("test", Parser.typ, List(AnyToken)) }
	}

	//(SelfType)
	"typ" should "handle: Self" in {
		assertResult(Stream(NormalTypeIdentifier(TypeName("Self")))) { Parser("test", Parser.typ, List(SelfBigToken)) }
	}

	//(in_parens_type) thru typ
	"typ" should "handle: (Int)" in {
		val input = List(LeftParenToken, VariableToken("Int"), RightParenToken)
		val expected = Stream(InParensType(NormalTypeIdentifier(TypeName("Int"))), TupleType(List(TupleTypeElementType(NormalTypeIdentifier(TypeName("Int"))))))
		assertResult(expected) { Parser("test", Parser.typ, input) }
	}


	//function_type
	"function_type" should "handle () -> Int" in {
		val input = List(LeftParenToken, RightParenToken, InfixOperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List()
		val expected = Stream(FunctionType(None, list, None, None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.function_type, input) }
	}

	"function_type" should "handle (Bool) -> Int" in {
		val input = List(LeftParenToken, VariableToken("Bool"), RightParenToken, InfixOperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List(FunctionTypeArg1(None, None, NormalTypeIdentifier(TypeName("Bool"))))
		val expected = Stream(FunctionType(None, list, None, None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.function_type, input) }
	}

	"function_type" should "handle (Bool, x: String) -> Int" in {
		val input = List(LeftParenToken, VariableToken("Bool"), CommaToken, VariableToken("x"), ColonToken, VariableToken("String"), RightParenToken, InfixOperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List(FunctionTypeArg1(None, None, NormalTypeIdentifier(TypeName("Bool"))),
											FunctionTypeArg2(ArgumentLabel("x"), TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String")))))
		val expected = Stream(FunctionType(None, list, None, None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.function_type, input) }
	}

	"function_type" should "handle (Bool, x: String) asynch -> Int" in {
		val input = List(LeftParenToken, VariableToken("Bool"), CommaToken, VariableToken("x"), ColonToken, VariableToken("String"), RightParenToken,
					AsyncToken, InfixOperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List(FunctionTypeArg1(None, None, NormalTypeIdentifier(TypeName("Bool"))),
											FunctionTypeArg2(ArgumentLabel("x"), TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String")))))
		val expected = Stream(FunctionType(None, list, Some(AsyncMod()), None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.function_type, input) }
	}

	//This is the test that caused us to discover the problem >:( and now we're CHEATING
	"function_type" should "handle @name () -> Int" in {
		val input = List(AtToken, VariableToken("name"), LeftParenToken, RightParenToken, InfixOperatorLiteralToken("->"), VariableToken("Int"))
		val list: List[FunctionTypeArg] = List()
		//val emptyBTList: List[BalancedToken] = List()
		val theAttribute = Attribute(AttributeName("name"), None)
		val expected = Stream(FunctionType(Some(List(theAttribute)), list, None, None, NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.function_type, input) }
	}


	//helper: function_type_arg
	"function_type_arg" should "handle x: String" in {
		val input = List(VariableToken("x"), ColonToken, VariableToken("String"))
		val typeAnnotation = TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String")))
		val expected = Stream(FunctionTypeArg2(ArgumentLabel("x"), typeAnnotation))
		assertResult(expected) { Parser("test", Parser.function_type_arg, input) }
	}

	//helper: type_annotation
	"type_annotation" should "handle : String" in {
		val input = List(ColonToken, VariableToken("String"))
		val expected = Stream(TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("String"))))
		assertResult(expected) { Parser("test", Parser.type_annotation, input) }
	}

	"type_annotation" should "handle : @name String" in {
		val input = List(ColonToken, AtToken, VariableToken("name"), VariableToken("String"))
		//val emptyBTList: List[BalancedToken] = List()
		val theAttribute = Attribute(AttributeName("name"), None)
		val expected = Stream(TypeAnnotation(Some(List(theAttribute)), None, NormalTypeIdentifier(TypeName("String"))))
		assertResult(expected) { Parser("test", Parser.type_annotation, input) }
	}

	"type_annotation" should "handle : @name inout String" in {
		val input = List(ColonToken, AtToken, VariableToken("name"), InOutToken, VariableToken("String"))
		//val emptyBTList: List[BalancedToken] = List()
		val theAttribute = Attribute(AttributeName("name"), None)
		val expected = Stream(TypeAnnotation(Some(List(theAttribute)), Some(InOutMod()), NormalTypeIdentifier(TypeName("String"))))
		assertResult(expected) { Parser("test", Parser.type_annotation, input) }
	}

	//array_type
	"array_type" should "handle [Int]" in {
		val input = List(LeftBracketToken, VariableToken("Int"), RightBracketToken)
		val expected = Stream(ArrayType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.array_type, input) }
	}

	//dictionary_type
	"dictionary_type" should "handle [Int: Bool]" in {
		val input = List(LeftBracketToken, VariableToken("Int"), ColonToken, VariableToken("Bool"), RightBracketToken)
		val expected = Stream(DictionaryType(NormalTypeIdentifier(TypeName("Int")), NormalTypeIdentifier(TypeName("Bool"))))
		assertResult(expected) { Parser("test", Parser.dictionary_type, input) }
	}

	//type_identifier
	"type_identifier" should "handle the normal type: Int" in {
		val input = List(VariableToken("Int"))
		val expected = Stream(NormalTypeIdentifier(TypeName("Int")))
		assertResult(expected) { Parser("test", Parser.type_identifier, input) }
	}

	"type_identifier" should "handle the generic type: Int<Bool, Int>" in {
		val input = List(VariableToken("Int"), InfixOperatorLiteralToken("<"), VariableToken("Bool"), CommaToken, VariableToken("Int"), PostfixOperatorLiteralToken(">"))
		val genericList: List[Type] = List(NormalTypeIdentifier(TypeName("Bool")), NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(GenericTypeIdentifier(TypeName("Int"), genericList))
		assertResult(expected) { Parser("test", Parser.type_identifier, input) }
	}

	"type_identifier" should "handle the nested normal type: Number.int" in {
		val input = List(VariableToken("Number"), InfixDotOperatorLiteralToken("."), VariableToken("int"))
		val expected = Stream(NestedNormalTypeIdentifier(TypeName("Number"), NormalTypeIdentifier(TypeName("int"))))
		assertResult(expected) { Parser("test", Parser.type_identifier, input) }
	}

	"type_identifier" should "handle the nested generic type: Number<Int>.int" in {
		val input = List(VariableToken("Number"), InfixOperatorLiteralToken("<"), VariableToken("Int"), InfixOperatorLiteralToken(">"), InfixDotOperatorLiteralToken("."), VariableToken("int"))
		val genericList: List[Type] = List(NormalTypeIdentifier(TypeName("Int")))
		val expected = Stream(NestedGenericTypeIdentifier(TypeName("Number"), genericList ,NormalTypeIdentifier(TypeName("int"))))
		assertResult(expected) { Parser("test", Parser.type_identifier, input) }
	}

	//tuple_type
	"tuple_type" should "handle ()" in {
		val input = List(LeftParenToken, RightParenToken)
		val emptyList: List[TupleTypeElement] = List()
		val expected = Stream(TupleType(emptyList))
		assertResult(expected) { Parser("test", Parser.tuple_type, input) }
	}

	"tuple_type" should "handle (Int)" in {
		val input = List(LeftParenToken, VariableToken("Int"), RightParenToken)
		val list: List[TupleTypeElement] = List(TupleTypeElementType(NormalTypeIdentifier(TypeName("Int"))))
		val expected = Stream(TupleType(list))
		assertResult(expected) { Parser("test", Parser.tuple_type, input) }
	}

	"tuple_type" should "handle (Int, x: Bool)" in {
		val input = List(LeftParenToken, VariableToken("Int"), CommaToken, VariableToken("x"), ColonToken, VariableToken("Bool"), RightParenToken)
		val list: List[TupleTypeElement] = List(TupleTypeElementType(NormalTypeIdentifier(TypeName("Int"))),
												TupleTypeElementNameAnnotation(ElementName("x"),
																				TypeAnnotation(None, None, NormalTypeIdentifier(TypeName("Bool")))))
		val expected = Stream(TupleType(list))
		assertResult(expected) { Parser("test", Parser.tuple_type, input) }
	}

	//protocol_composition_type
	"protocol_composition_type" should "handle: Int & Bool" in {
		val input = List(VariableToken("Int"), InfixOperatorLiteralToken("&"), VariableToken("Bool"))
		val list = List(NormalTypeIdentifier(TypeName("Int")),
				   NormalTypeIdentifier(TypeName("Bool")))
		val expected = Stream(ProtocolCompositionType(list))
		assertResult(expected) { Parser("test", Parser.protocol_composition_type, input) }
	}

	//opaque_type
	"opaque_type" should "handle: some Int" in {
		val input = List(SomeToken, VariableToken("Int"))
		val expected = Stream(OpaqueType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.opaque_type, input) }
	}

	//in_parens_type
	"in_parens_type" should "handle: (Int)" in {
		val input = List(LeftParenToken, VariableToken("Int"), RightParenToken)
		val expected = Stream(InParensType(NormalTypeIdentifier(TypeName("Int"))))
		assertResult(expected) { Parser("test", Parser.in_parens_type, input) }
	}

	//weird types testing:
	//trailer
	"trailer" should "handle !" in {
		val input = List(PostfixOperatorLiteralToken("!"))
		val expected = Stream(ImplicitlyUnwrappedOptionalTypeThing)
		assertResult(expected) { Parser("test", Parser.trailer, input) }
	}

	"trailer" should "handle ?" in {
		val input = List(PostfixOperatorLiteralToken("?"))
		val expected = Stream(OptionalTypeThing)
		assertResult(expected) { Parser("test", Parser.trailer, input) }
	}

	"trailer" should "handle .Type" in {
		val input = List(PrefixDotOperatorLiteralToken("."), TypeToken)
		val expected = Stream(MetatypeTypeThing)
		assertResult(expected) { Parser("test", Parser.trailer, input) }
	}

	"trailer" should "handle .Protocol" in {
		val input = List(PrefixDotOperatorLiteralToken("."), ProtocolToken)
		val expected = Stream(MetatypeProtocolThing)
		assertResult(expected) { Parser("test", Parser.trailer, input) }
	}

	//attribute & attributes
	"attribute" should "handle @varName" in {
		val input = List(AtToken, VariableToken("varName"))
		//val emptyBTList: List[BalancedToken] = List()
		val expected = Stream(Attribute(AttributeName("varName"), None))
		assertResult(expected) { Parser("test", Parser.attribute, input) }
	}

	//commented out because we are CHEATING
 	"attribute" should "handle @varName ()" in {
		val input = List(AtToken, VariableToken("varName"), LeftParenToken, RightParenToken)
		val emptyBTList: List[BalancedToken] = List()
		val expected = Stream(Attribute(AttributeName("varName") , Some(emptyBTList)))
		assertResult(expected) { Parser("test", Parser.attribute, input) }
	}

	"attribute" should "handle @varName (xy)" in {
		val input = List(AtToken, VariableToken("varName"), LeftParenToken, VariableToken("xy"), RightParenToken)
		val list: List[BalancedToken] = List(IdentifierBalancedToken("xy"))
		val expected = Stream(Attribute(AttributeName("varName") , Some(list)))
		assertResult(expected) { Parser("test", Parser.attribute, input) }
	}

	"attribute" should "handle @varName (&)" in {
		val input = List(AtToken, VariableToken("varName"), LeftParenToken, InfixOperatorLiteralToken("&"), RightParenToken)
		val list: List[BalancedToken] = List(PunctuationBalancedToken("&"))
		val expected = Stream(Attribute(AttributeName("varName") , Some(list)))
		assertResult(expected) { Parser("test", Parser.attribute, input) }
	}

	"attribute" should "handle @varName (xy as)" in {
		val input = List(AtToken, VariableToken("varName"), LeftParenToken, VariableToken("xy"), AsToken, RightParenToken)
		val list: List[BalancedToken] = List(IdentifierBalancedToken("xy"), KeywordBalancedToken("as"))
		val expected = Stream(Attribute(AttributeName("varName"), Some(list)))
		assertResult(expected) { Parser("test", Parser.attribute, input) }
	}

	"attributes" should "handle two attributes: @varName @name(xy)" in {
		val input = List(AtToken, VariableToken("varName"), AtToken, VariableToken("name"), LeftParenToken, VariableToken("xy"), RightParenToken)
		val emptyBTList: List[BalancedToken] = List()
		val list: List[BalancedToken] = List(IdentifierBalancedToken("xy"))
		val expected = Stream(List(Attribute(AttributeName("varName") , None),
											Attribute(AttributeName("name") , Some(list))))
		assertResult(expected) { Parser("test", Parser.attributes, input) }
	}

	//attribute_argument_clause
	"attribute_argument_clause" should "handle one balanced token: xy" in {
		val input = List(LeftParenToken, VariableToken("xy"), RightParenToken)
		val expected = Stream(List(IdentifierBalancedToken("xy")))
		assertResult(expected) { Parser("test", Parser.attribute_argument_clause, input) }
	}


	//balanced_token
	"balanced_token" should "handle an identifier: xy" in {
		val input = List(VariableToken("xy"))
		val expected = Stream(IdentifierBalancedToken("xy"))
		assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle a keyword: as" in {
		val input = List(AsToken)
		val expected = Stream(KeywordBalancedToken("as"))
		assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle a literal: 5" in {
		val input = List(DecimalIntegerLiteralToken("5"))
		val expected = Stream(LiteralBalancedToken(DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle an operator: -" in {
		val input = List(InfixOperatorLiteralToken("-"))
		val expected = Stream(OperatorBalancedToken("-"))
		assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle a punctuation: ." in {
		val input = List(InfixDotOperatorLiteralToken("."))
		val expected = PunctuationBalancedToken(".")
		val result = Parser("test", Parser.balanced_token, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle a parenthesized punctuation: (.)" in {
		val input = List(LeftParenToken, InfixDotOperatorLiteralToken("."), RightParenToken)
		val expected = InParensBalancedToken(PunctuationBalancedToken("."))
		val result = Parser("test", Parser.balanced_token, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle a parenthesized identifier: (xy)" in {
		val input = List(LeftParenToken, VariableToken("xy"), RightParenToken)
		val expected = Stream(InParensBalancedToken(IdentifierBalancedToken("xy")))
		assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle a bracketed punctuation: [.]" in {
		val input = List(LeftBracketToken, InfixDotOperatorLiteralToken("."), RightBracketToken)
		val expected = InBracketsBalancedToken(PunctuationBalancedToken("."))
		val result = Parser("test", Parser.balanced_token, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}

	"balanced_token" should "handle a braced punctuation: {.}" in {
		val input = List(LeftCurlyToken, InfixDotOperatorLiteralToken("."), RightCurlyToken)
		val expected = InBracesBalancedToken(PunctuationBalancedToken("."))
		val result = Parser("test", Parser.balanced_token, input)
		assertResult(expected) { result.head }
		//assertResult(expected) { Parser("test", Parser.balanced_token, input) }
	}


	//other

	//function_call_argument
	"function_call_argument" should "handle 5" in {
		assertResult(Stream(ExpFunctionCallArgument(DecimalIntegerLiteralExp("5")))) { Parser("test", Parser.function_call_argument, List(DecimalIntegerLiteralToken("5"))) }
	}

	"function_call_argument" should "handle x:5" in {
		val input = List(VariableToken("x"), ColonToken, DecimalIntegerLiteralToken("5"))
		val expected = Stream(IdentifierColonExpFunctionCallArgument(FunctionCallArgName("x"),
															  DecimalIntegerLiteralExp("5")))
		assertResult(expected) { Parser("test", Parser.function_call_argument, input) }
	}

	"function_call_argument" should "handle +" in {
		val input = List(InfixOperatorLiteralToken("+"))
		val expected = Stream(OperatorFunctionCallArgument(Operator("+")))
		assertResult(expected) { Parser("test", Parser.function_call_argument, input) }
	}

	"function_call_argument" should "handle x:+" in {
		val input = List(VariableToken("x"), ColonToken, PostfixOperatorLiteralToken("+"))
		val expected = Stream(IdentifierColonOperatorFunctionCallArgument(FunctionCallArgName("x"),
																   Operator("+")))
		assertResult(expected) { Parser("test", Parser.function_call_argument, input) }
	}
}