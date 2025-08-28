package FuzzingSwift.parser

import FuzzingSwift.tokenizer._
import FuzzingSwift.parsingCombinators._

class ParserException(message: String) extends Exception(message)

object Parser extends Combinators {
	override type Elem = FuzzingSwift.tokenizer.Token
	
	
	lazy val variable: Parser[VariableToken] = {
		accept({ case id @ VariableToken(name) => id })
	}

	lazy val property_wrapper_projection: Parser[PropertyWrapperProjectionToken] = {
		accept({ case id @ PropertyWrapperProjectionToken(name) => id })
	}
	
	lazy val ip_OR_pwp: Parser[ImplicitParameterOrPropertyWrapperProjectionToken] = {
		accept({ case id @ ImplicitParameterOrPropertyWrapperProjectionToken(name) => id })
	}
	
	lazy val binary_integer: Parser[BinaryIntegerLiteralToken] = {
		accept({ case id @ BinaryIntegerLiteralToken(value) => id })
	}
	
	lazy val octal_integer: Parser[OctalIntegerLiteralToken] = {
		accept({ case id @ OctalIntegerLiteralToken(value) => id })
	}
	
	lazy val decimal_integer: Parser[DecimalIntegerLiteralToken] = {
		accept({ case id @ DecimalIntegerLiteralToken(value) => id })
	}
	
	lazy val hex_integer: Parser[HexIntegerLiteralToken] = {
		accept({ case id @ HexIntegerLiteralToken(value) => id })
	}
	
	lazy val decimal_float: Parser[FloatDecimalLiteralToken] = {
		accept({ case id @ FloatDecimalLiteralToken(value) => id })
	}
	
	lazy val hex_float: Parser[FloatHexLiteralToken] = {
		accept({ case id @ FloatHexLiteralToken(value) => id })
	}
	
	lazy val char_string: Parser[CharLiteralToken] = {
		accept({ case id @ CharLiteralToken(value) => id })
	}
	
	lazy val single_line_string: Parser[SingleLineStringLiteralToken] = {
		accept({ case id @ SingleLineStringLiteralToken(value) => id })
	}
	
	lazy val multi_line_string: Parser[MultiLineStringLiteralToken] = {
		accept({ case id @ MultiLineStringLiteralToken(value) => id })
	}
	
	lazy val interpolated_string: Parser[InterpolatedStringLiteralToken] = {
		accept({case id @ InterpolatedStringLiteralToken(value) => id })
	}
	
	lazy val infix_operator_thing: Parser[InfixOperatorLiteralToken] = {
		accept({ case id @ InfixOperatorLiteralToken(value) => id })
	}
	
	lazy val postfix_operator_thing: Parser[PostfixOperatorLiteralToken] = {
		accept({ case id @ PostfixOperatorLiteralToken(value) => id })
	}
	
	lazy val prefix_operator_thing: Parser[PrefixOperatorLiteralToken] = {
		accept({ case id @ PrefixOperatorLiteralToken(value) => id })
	}
	
	lazy val dot_infix_operator_thing: Parser[InfixDotOperatorLiteralToken] = {
		accept({ case id @ InfixDotOperatorLiteralToken(value) => id })
	}
	
	lazy val dot_postfix_operator_thing: Parser[PostfixDotOperatorLiteralToken] = {
		accept({ case id @ PostfixDotOperatorLiteralToken(value) => id })
	}
	
	lazy val dot_prefix_operator_thing: Parser[PrefixDotOperatorLiteralToken] = {
		accept({ case id @ PrefixDotOperatorLiteralToken(value) => id })
	}
	
	def infix_operator_match(expected: String): Parser[Operator] = {
		infix_operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure(s"expected $expected but recieved $actual.operator"))
	}
	
	def postfix_operator_match(expected: String): Parser[Operator] = {
		postfix_operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure(s"expected $expected but recieved $actual.operator"))
	}
	
	def prefix_operator_match(expected: String): Parser[Operator] = {
		prefix_operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure(s"expected $expected but recieved $actual.operator"))
	}
	
	def operator(expected: String): Parser[Operator] = {
		infix_operator_match(expected) | postfix_operator_match(expected) | prefix_operator_match(expected)
	}
	
	def dot_infix_operator_match(expected: String): Parser[Operator] = {
		dot_infix_operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure(s"expected $expected but recieved $actual.operator"))
	}
	
	def dot_postfix_operator_match(expected: String): Parser[Operator] = {
		dot_postfix_operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure(s"expected $expected but recieved $actual.operator"))
	}
	
	def dot_prefix_operator_match(expected: String): Parser[Operator] = {
		dot_prefix_operator_thing.flatMap(actual => if(expected == actual.operator) success(Operator(expected)) else failure(s"expected $expected but recieved $actual.operator"))
	}
	
	def dot_operator(expected: String): Parser[Operator] = {
		dot_infix_operator_match(expected) | dot_postfix_operator_match(expected) | dot_prefix_operator_match(expected)
	}
	
	var globalFileName: String = ""	
	
	def apply[A](fileName: String, parser: Parser[A], tokens: List[Token]): Seq[A] = {
		globalFileName = fileName
		val withProcs = Set("comma_sep_types", "after_postfix_function_call")
		//parser.withTrace(withProcs).phrase(tokens.filterNot(_.isComment)).foreach(elem => println(s"Solution: $elem"))
		//original:
		parser.phrase(tokens.filterNot(_.isComment)).toSeq
	}
	
	def program: Parser[Program] = {
		//can remove once for demo of syntactically ambiguous programs 
		once(rep(statement)) ^^ { case stmts => Program(stmts)}
	}
	
	//statements
	lazy val statements: Parser[List[Stmt]] = {
		once(rep1(statement))
	}
	
	lazy val label_name: Parser[LabelName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => LabelName(name)
				case ImplicitParameterExpOrPWP(name) => LabelName(name)
				case PropertyWrapperProjectionExp(name) => LabelName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val statement: Parser[Stmt] = proc("statement") {
		label_name ~ ColonToken ~ statement ~ opt(SemicolonToken) ^^ { case label ~ _ ~ stmt ~ _ => LabeledStmt(label, stmt) } |
		declaration_stmt |
		loop_stmt ~ opt(SemicolonToken) ^^ { case loop ~ _ => loop } |
		branch_stmt ~ opt(SemicolonToken) ^^ { case branch ~ _ => branch } |
		control_stmt ~ opt(SemicolonToken) ^^ { case stmt ~ _ => stmt } |
		DeferToken ~> code_block ^^ { case block => DeferStmt(block) } |
		do_stmt ~ opt(SemicolonToken) ^^ { case stmt ~ _ => stmt } |
		expression_stmt |
		compiler_control_stmt
	}
	
	lazy val expression_stmt: Parser[Stmt] = {
		expression ~ opt(SemicolonToken) ^^ { case exp ~ _ => ExpressionStmt(exp) }
	}
	
	lazy val declaration_stmt: Parser[DeclarationStmt] = {
		declaration ~ opt(SemicolonToken) ^^ { case decl ~ _ => DeclarationStmt(decl) }
	}
	
	lazy val loop_stmt: Parser[Stmt] = {
		for_in_stmt |
		while_stmt |
		repeat_while_stmt
	}
	
	lazy val for_in_stmt: Parser[ForInStmt] = proc("for_in_stmt") {
		ForToken ~ opt(case_modifier) ~ once(pattern) ~ InToken ~ expression ~ opt(WhereToken ~ expression) ~ code_block ^^ {
			case _ ~ optCaseMod ~ thePattern ~ _ ~ exp ~ optWhereClause ~ codeBlock => ForInStmt(optCaseMod, thePattern, exp, (optWhereClause).map(_._2), codeBlock)
		}
	}
	
	lazy val case_modifier: Parser[CaseMod] = {
		CaseToken ^^^ CaseMod()
	}
	
	lazy val while_stmt: Parser[WhileStmt] = proc("while_stmt") {
		WhileToken ~ condition_list ~ code_block ^^ { case _ ~ conditionList ~ codeBlock => WhileStmt(conditionList, codeBlock) }
	}
	
	lazy val condition_list: Parser[List[Condition]] = {
		rep1sep(condition, CommaToken) //cannot use once here, need the nondeterminism to tell difference between trailing closure and something like if 5 {0}
	}
	
	lazy val condition: Parser[Condition] = {
		expression ^^ { case exp => ExpressionCondition(exp) } |
		//skipping avaialability condition rn
		CaseToken ~ pattern ~ operator("=") ~ expression ^^ { case _ ~ thePattern ~ _ ~ exp => CaseCondition(thePattern, exp) } |
		LetToken ~ pattern ~ opt(operator("=") ~ expression) ^^ { case _ ~ thePattern ~ optInitializer => OptionalBindingConditionLet(thePattern, (optInitializer).map(_._2)) } |
		VarToken ~ pattern ~ opt(operator("=") ~ expression) ^^ { case _ ~ thePattern ~ optInitializer => OptionalBindingConditionVar(thePattern, (optInitializer).map(_._2)) }
	}
	
	lazy val repeat_while_stmt: Parser[RepeatWhileStmt] = {
		RepeatToken ~ code_block ~ WhileToken ~ expression ^^ { case _ ~ codeBlock ~ _ ~ exp => RepeatWhileStmt(codeBlock, exp) }
	}
	
	lazy val branch_stmt: Parser[Stmt] = {
		if_stmt |
		guard_stmt |
		switch_stmt
	}
	
	lazy val if_stmt: Parser[IfStmt] = {
		IfToken ~> condition_list ~ code_block ~ opt(else_clause) ^^ { case conditionList ~ codeBlock ~ elseClause => IfStmt(conditionList, codeBlock, elseClause) }
	}
	
	lazy val else_clause: Parser[ElseClause] = {
		ElseToken ~> code_block ^^ { case codeBlock => ElseCodeBlock(codeBlock) } |
		ElseToken ~> if_stmt ^^ { case ifStmt => ElseIfStmt(ifStmt) } 
	}
	
	lazy val guard_stmt: Parser[GuardStmt] = {
		GuardToken ~ condition_list ~ ElseToken ~ code_block ^^ { case _ ~ conditionList ~ _ ~ codeBlock => GuardStmt(conditionList, codeBlock) }
	}
	
	lazy val switch_stmt: Parser[SwitchStmt] = {
		SwitchToken ~ expression ~ LeftCurlyToken ~ opt(switch_cases) ~ RightCurlyToken ^^ { case _ ~ exp ~ _ ~ optSwitchCases ~ _ =>
			SwitchStmt(exp, optSwitchCases) }
	}
	
	lazy val switch_cases: Parser[List[SwitchCase]] = {
		once(rep1(switch_case))
	}
	
	lazy val switch_case: Parser[SwitchCase] = {
		case_label ~ once(rep1(statement)) ^^ { case caseLabel ~ stmts => CaseLabelStmts(caseLabel, stmts) } |
		opt(attributes) ~ DefaultToken ~ ColonToken ~ statements ^^ { case optAttributes ~ _ ~ _ ~ stmts => DefaultLabelStmts(optAttributes, stmts) }
		//skipping condition-switch-case for now
	}
	
	lazy val case_label: Parser[CaseLabel] = {
		opt(attributes) ~ CaseToken ~ case_item_list ~ ColonToken ^^ {
			case optAttributes ~ _ ~ caseItemList ~ _ => CaseLabel(optAttributes, caseItemList) 
		}
	}
	
	lazy val case_item_list: Parser[List[CaseItem]] = {
		once(rep1sep(case_item, CommaToken))
	}
	
	lazy val case_item: Parser[CaseItem] = {
		pattern ~ opt(WhereToken ~ expression) ^^ { case thePattern ~ optWhereClause => CaseItem(thePattern, (optWhereClause).map(_._2)) }
	}
	
	lazy val control_stmt: Parser[Stmt] = {
		BreakToken ~> opt(label_name) ^^ { case labelName => BreakStmt(labelName) } |
		ContinueToken ~> opt(label_name) ^^ { case labelName => ContinueStmt(labelName) } |
		FallthroughToken ^^^ FallthroughStmt |
		ReturnToken ~> opt(expression) ^^ { case exp => ReturnStmt(exp) } |
		ThrowToken ~> expression ^^ { case exp => ThrowStmt(exp) }
	}
	
	lazy val do_stmt: Parser[DoStmt] = {
		DoToken ~ code_block ~ opt(once(rep1(catch_clause))) ^^ { case _ ~ block ~ optCatchClauses => DoStmt(block, optCatchClauses) }
	}
	
	lazy val catch_clause: Parser[CatchClause] = {
		CatchToken ~ code_block ^^ { case _ ~ block => CatchClause(None, block) } |
		CatchToken ~> opt(once(rep1sep(catch_pattern, CommaToken))) ~ code_block ^^ { case optCatchPatternList ~ codeBlock => CatchClause(optCatchPatternList, codeBlock) }
	}
	
	lazy val catch_pattern: Parser[CatchPattern] = {
		pattern ~ opt(WhereToken ~ expression) ^^ { case thePattern ~ optWhereClause => CatchPattern(thePattern, (optWhereClause).map(_._2)) }
	}
	//end of statements
	
	//expressions
	//the rep(infix_expressions) cannot use once due to trailing closure ambiguity
	lazy val expression: Parser[Exp] = {
		opt(try_operator) ~ opt(AwaitToken) ~ prefix_expression ~ rep(infix_expression) ^^ {case theTry ~ theAwait ~ prefix ~ infix => {
			val combinedExp = infix_maker(prefix, infix)
			(theTry, theAwait) match {
				case (None, None) => combinedExp
				case (Some(tryModifier), None) => TryExp(tryModifier, combinedExp)
				case (None, Some(await)) => AwaitExp(combinedExp)
				case (Some(tryModifier), Some(await)) => TryExp(tryModifier, AwaitExp(combinedExp))
			}
		}}
	}
	
	def infix_maker(prefix: Exp, list: List[InfixExp]): Exp = {
		list match {
			case Nil => prefix
			case WithOperatorInfixExpression(op, exp) :: tail => TrueInfixExp(prefix, op, infix_maker(exp, tail))
			case TypeCastInfixExpression(op) :: tail => cast_exp_helper(prefix, list.reverse)
			case AssignmentOpInfixExpression(theTry, exp) :: tail => theTry match {
				case None => AssignmentExp(prefix, infix_maker(exp, tail))
				case Some(tryModifier) => AssignmentExp(prefix, infix_maker(TryExp(tryModifier, exp), tail))
			}
			case ConditionalOpInfixExpression(conditionalExp, theTry, exp) :: tail => theTry match {
				case None => ConditionalExp(prefix, conditionalExp, infix_maker(exp, tail))
				case Some(tryModifier) => ConditionalExp(prefix, conditionalExp, infix_maker(TryExp(tryModifier, exp), tail))
			}
		}
	}
	
	def cast_exp_helper(prefix: Exp, list: List[InfixExp]): Exp = {
		list match {
			case Nil => prefix
			case TypeCastInfixExpression(op) :: tail => CastExp(cast_exp_helper(prefix, tail), op)
			case _ => infix_maker(prefix, list)
		}
	}

	lazy val prefix_expression: Parser[Exp] = {
		in_out_expression |
		postfix_expression |
		prefix_operator ~ postfix_expression ^^ { case theOperator ~ exp => PrefixExp(theOperator, exp) }
	}
	
	lazy val in_out_expression: Parser[Exp] = {
		//operator("&") ~ in_out_name ^^ { case _ ~ id => InOutExp(id) }
		operator("&") ~ postfix_expression ^^ { case _ ~ exp => InOutExp(exp)}
		//swift devs accepted my bug and changed it to postfix_expression
	}
	
	lazy val infix_expression: Parser[InfixExp] = {
	operator("=") ~ opt(try_operator) ~ prefix_expression ^^ { case _ ~ theTry ~ exp => AssignmentOpInfixExpression(theTry, exp) } |
	operator("?") ~ expression ~ ColonToken ~ opt(try_operator) ~ prefix_expression ^^ 
		{ case _ ~ conditionalExp ~ _ ~ theTry ~ exp => ConditionalOpInfixExpression(conditionalExp, theTry, exp)} |
	infix_operator ~ prefix_expression ^^ { case op ~ exp => WithOperatorInfixExpression(op, exp) } |
	type_casting_operator ^^ { case op => TypeCastInfixExpression(op) }
	}
	
	//cannot put once on rep(after_postfix_exp) due to trailing closure ambiguity
	lazy val postfix_expression: Parser[Exp] = proc("postfix_expression") {
		primary_expression ~ rep(after_postfix_exp) ^^ { case exp ~ after => postfix_maker(exp, after.reverse) }
	}
	
	def postfix_maker(exp: Exp, list: List[AfterPostfix]): Exp = {
		list match {
			case Nil => exp
			case AfterPostfixOperator(op) :: tail => PostfixWithOpExp(postfix_maker(exp, tail), op)
			case AfterPostfixForcedValue :: tail => PostfixForcedValueExp(postfix_maker(exp, tail))
			case AfterPostfixOptionalChaining :: tail => PostfixOptionalChainingExp(postfix_maker(exp, tail))
			case AfterPostfixFunctionCall(call) :: tail => PostfixFunctionCallExp(postfix_maker(exp, tail), call)
			case AfterPostfixSubscript(list) :: tail => PostfixSubscriptExp(postfix_maker(exp, tail), list)
			case AfterPostfixInitializer(list) :: tail => PostfixInitializerExp(postfix_maker(exp, tail), list)
			case AfterPostfixSelf :: tail => PostfixSelfExp(postfix_maker(exp, tail))
			case AfterPostfixExplicitMember(member) :: tail => PostfixExplicitMemberExp(postfix_maker(exp, tail), member)
		}
	}
	
	lazy val after_postfix_exp: Parser[AfterPostfix] = proc("after_postfix_exp") {
	    operator("!") ^^^ AfterPostfixForcedValue |
		postfix_operator_match("?") ^^^ AfterPostfixOptionalChaining |
		after_postfix_function_call ^^ { case call => AfterPostfixFunctionCall(call) } |
		function_call_argument_list ^^ { case list =>  AfterPostfixSubscript(list) } |
		dot_operator(".") ~ initializer_exp ^^ { case _ ~ afterPostfixInit => afterPostfixInit } |
		dot_operator(".") ~ SelfToken ^^^ AfterPostfixSelf |
		dot_operator(".") ~ explicit_member_exp ^^ { case _ ~ member => AfterPostfixExplicitMember(member) } |
		postfix_operator ^^ { case op => AfterPostfixOperator(op) }
	}
	
	lazy val after_postfix_function_call: Parser[PostfixFunctionCall] = proc("after_postfix_function_call") {
		opt(function_call_argument_clause) ~ trailing_closures ^^ { case optList ~ trailing => ComplexFunctionCall(optList, trailing) } |
		function_call_argument_clause ^^ { case list => SimpleFunctionCall(list) }
	}
	
	lazy val function_call_argument_clause: Parser[List[FunctionCallArgument]] = {
		LeftParenToken ~ repsep(function_call_argument, CommaToken) ~ RightParenToken ^^ { case _ ~ list ~ _ => list }
	}
	
	lazy val trailing_closures: Parser[TrailingClosure] = {
		closure_expression ~ opt(once(rep1(labeled_trailing_closure))) ^^ { case exp ~ trailingList => TrailingClosure(exp, trailingList) }
	}
	
	lazy val trailing_closure_label: Parser[TrailingClosureLabel] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => TrailingClosureLabel(name)
				case ImplicitParameterExpOrPWP(name) => TrailingClosureLabel(name)
				case PropertyWrapperProjectionExp(name) => TrailingClosureLabel(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val labeled_trailing_closure: Parser[LabeledTrailingClosure] = {
		trailing_closure_label ~ ColonToken ~ closure_expression ^^ { case id ~ _ ~ exp => LabeledTrailingClosure(id, exp) }
	}
	
	lazy val initializer_exp: Parser[AfterPostfixInitializer] = {
		InitToken ~ LeftParenToken ~ once(rep1(argument_name)) ~ RightParenToken ^^ { case _ ~ _ ~ list ~ _ => AfterPostfixInitializer(Some(list)) } |
		InitToken ^^^ { AfterPostfixInitializer(None) }
	}
	
	lazy val explicit_member_name: Parser[ExplicitMemberName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ExplicitMemberName(name)
				case ImplicitParameterExpOrPWP(name) => ExplicitMemberName(name)
				case PropertyWrapperProjectionExp(name) => ExplicitMemberName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val explicit_member_exp: Parser[ExplicitMember] = {
		integer_literal ^^ { case num => { 
			num match {
				case DecimalIntegerLiteralExp(number) => ExplicitMemberDecimalDigits(number)
				case BinaryIntegerLiteralExp(number) => ExplicitMemberDecimalDigits(number)
				case OctalIntegerLiteralExp(number) => ExplicitMemberDecimalDigits(number)
				case HexIntegerLiteralExp(number) => ExplicitMemberDecimalDigits(number) 
				case _ => throw new ParserException("oop")//unreachable but the typechecker doesn't think so
			}
		}}|
		explicit_member_name ~ LeftParenToken ~ once(rep1(argument_name)) ~ RightParenToken ^^ { case id ~ _ ~ args ~ _ => ExplicitMemberIdentifierArgs(id, args) } |
		explicit_member_name ~ opt(generic_argument_clause) ^^ { case id ~ clause => ExplicitMemberIdentifierOptGeneric(id, clause) }
	}
	
	lazy val argument_name: Parser[ArgumentName] = {
		identifier ~ ColonToken ^^ { case result ~ _ => {
			result match {
				case VariableExp(name) => ArgumentName(name)
				case ImplicitParameterExpOrPWP(name) => ArgumentName(name)
				case PropertyWrapperProjectionExp(name) => ArgumentName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val generic_name: Parser[GenericName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => GenericName(name)
				case ImplicitParameterExpOrPWP(name) => GenericName(name)
				case PropertyWrapperProjectionExp(name) => GenericName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val primary_expression: Parser[Exp] = {
		generic_name ~ generic_argument_clause ^^ { case name ~ listOfTypes => GenericExp(name, listOfTypes) } |
		identifier |
		literal_expression |
		self_expression |
		superclass_expression |
		closure_expression |
		parenthesized_expression |
		tuple_expression |
		implicit_member_expression |
		wildcard_expression |
		key_path_expression |
		selector_expression |
		key_path_string_expression
	}
	
	lazy val identifier: Parser[Exp] = {
		keyword_as_identifier ^^ { case name => VariableExp(name) } |
		variable ^^ { case VariableToken(name) => VariableExp(name) } |
		ip_OR_pwp ^^ { case ImplicitParameterOrPropertyWrapperProjectionToken(name) => ImplicitParameterExpOrPWP(name) } |
		property_wrapper_projection ^^ { case PropertyWrapperProjectionToken(name) => PropertyWrapperProjectionExp(name) }
	}
	
	lazy val generic_argument_clause: Parser[List[Type]] = {
		operator("<") ~ comma_sep_types ~ operator(">") ^^ { case _ ~ typs ~ _ => typs }
	}
	
	lazy val comma_sep_types: Parser[List[Type]] = proc("comma_sep_types") {
		once(rep1sep(typ, CommaToken))
	}
	

	lazy val literal_expression: Parser[Exp] = {
		literal | array_literal |
		dictionary_literal | playground_literal //|
/* 		hashFileToken ^^^ HashFileExp |
		hashFileIDToken ^^^ HashFileIDExp |
		hashFilePathToken ^^^ HashFilePathExp |
		hashLineToken ^^^ HashLineExp |
		hashColumnToken ^^^ HashColumnExp |
		HashFunctionToken ^^^ HashFunctionExp |
		HashDSOHandleToken ^^^ HashDSOHandleExp */
		//above removed bcuz swift changed language, double check about ver for this
	}
	
	lazy val literal: Parser[Exp] = {
		numeric_literal | string_literal |
		boolean_literal | nil_literal
	}
	
	lazy val numeric_literal: Parser[Exp] = {
		opt(operator("-")) ~ integer_literal ^^ { case optMinus ~ intLiteral => optMinus.map(_ => PrefixExp(Operator("-"), intLiteral)).getOrElse(intLiteral) } |
		opt(operator("-")) ~ float_literal ^^ { case optMinus ~ floatLiteral => optMinus.map(_ => PrefixExp(Operator("-"), floatLiteral)).getOrElse(floatLiteral) }
	}
	
	lazy val integer_literal: Parser[Exp] = {
		decimal_integer ^^ { case DecimalIntegerLiteralToken(value) => DecimalIntegerLiteralExp(value) } |
		binary_integer ^^ { case BinaryIntegerLiteralToken(value) => BinaryIntegerLiteralExp(value) } |
		octal_integer ^^ { case OctalIntegerLiteralToken(value) => OctalIntegerLiteralExp(value) } |
		hex_integer ^^ { case HexIntegerLiteralToken(value) => HexIntegerLiteralExp(value) }
	}
	
	lazy val float_literal: Parser[Exp] = {
		decimal_float ^^ { case FloatDecimalLiteralToken(value) => DecimalFloatLiteralExp(value) } |
		hex_float ^^ { case FloatHexLiteralToken(value) => HexFloatLiteralExp(value) }
	}
	
	lazy val string_literal: Parser[Exp] = {
		char_string ^^ { case CharLiteralToken(value) => CharStringExp(value) } |
		single_line_string ^^ { case SingleLineStringLiteralToken(value) => SingleLineStaticStringLiteralExp(value) } |
		multi_line_string ^^ { case MultiLineStringLiteralToken(value) => MultiLineStaticStringLiteralExp(value) } |
		interpolated_string ^^ { case InterpolatedStringLiteralToken(value) => InterpolatedStringLiteralExp(value) }
	}
	
	def boolean_literal: Parser[Exp] = {
		TrueToken ^^^ TrueLiteralExp() | FalseToken ^^^ FalseLiteralExp()
	}
	
	def nil_literal: Parser[Exp] = {
		NilToken ^^^ NilExp()
	}
	
	lazy val array_literal: Parser[Exp] = {
		(LeftBracketToken ~ comma_sep_exps ~ opt(CommaToken) ~ RightBracketToken).flatMap(
			{ case _ ~ expList ~ maybe ~ _ =>
				if(expList.isEmpty && maybe.nonEmpty)
					{ failure("random comma not preceeded by an exp") } else { success(ArrayLiteralExp(expList)) }})
	}
	
	lazy val comma_sep_exps: Parser[List[Exp]] = {
		once(repsep(expression, CommaToken))
	}
	
	lazy val dictionary_literal: Parser[Exp] = {
		LeftBracketToken ~ ColonToken ~ RightBracketToken ^^^ DictionaryLiteralExp(List()) |
		LeftBracketToken ~ comma_sep_dictionary ~ opt(CommaToken) ~ RightBracketToken ^^ { case _ ~ list ~ _ ~ _ => DictionaryLiteralExp(list) } //|
	}
	
	lazy val comma_sep_dictionary: Parser[List[(Exp, Exp)]] = {
		once(rep1sep(expression ~ ColonToken ~ expression, CommaToken).map(_.map(input => (input._1._1, input._2))))
	}
	
	lazy val playground_literal: Parser[Exp] = {
		HashColorLiteralToken ~ LeftParenToken ~ RedToken ~ ColonToken ~ expression ~ CommaToken ~ GreenToken ~ ColonToken ~ expression ~ CommaToken ~ BlueToken ~ ColonToken ~ expression ~ CommaToken ~ AlphaToken ~ ColonToken ~ expression ~ RightParenToken ^^ 
			{ case _ ~ _ ~ _ ~ _ ~ exp1 ~ _ ~ _ ~ _ ~ exp2 ~ _ ~ _ ~ _ ~ exp3 ~ _ ~ _ ~ _ ~ exp4 ~ _ => ColorPlaygroundLiteralExp(exp1, exp2, exp3, exp4) } |
		HashFileLiteralToken ~ LeftParenToken ~ ResourceNameToken ~ ColonToken ~ expression ~ RightParenToken ^^
			{ case _ ~ _ ~ _ ~ _ ~ exp ~ _ =>  FilePlaygroundLiteralExp(exp) } |
		HashImageLiteralToken ~ LeftParenToken ~ ResourceNameToken ~ ColonToken ~ expression ~ RightParenToken ^^
			{ case _ ~ _ ~ _ ~ _ ~ exp ~ _ => ImagePlaygroundLiteralExp(exp) }
	}
	
	lazy val self_method_name: Parser[SelfMethodName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => SelfMethodName(name)
				case ImplicitParameterExpOrPWP(name) => SelfMethodName(name)
				case PropertyWrapperProjectionExp(name) => SelfMethodName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	//gonna need to possibly change to def later to accomodate SoloSelfExp and InitSelfExp needing separate objects for the type env
	//leaving as lazy val for now for performance since we aren't mutating these exps currently
	lazy val self_expression: Parser[Exp] = {
		SelfToken ~ dot_operator(".") ~ self_method_name ^^ { case _ ~ _ ~ selfMethodName => MethodSelfExp(selfMethodName) } |
		SelfToken ~ dot_operator(".") ~ InitToken ^^^ InitSelfExp() |
		SelfToken ~ function_call_argument_list ^^ { case _ ~ list => SubscriptSelfExp(list) } |
		SelfToken ^^^ SoloSelfExp()
	}
	
	lazy val super_method_name: Parser[SuperMethodName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => SuperMethodName(name)
				case ImplicitParameterExpOrPWP(name) => SuperMethodName(name)
				case PropertyWrapperProjectionExp(name) => SuperMethodName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	//gonna need to possibly change to def later to accomodate InitSuperExp needing separate objects for the type env
	//leaving as lazy val for now for performance since we aren't mutating these exps currently
	lazy val superclass_expression: Parser[Exp] = {
		SuperToken ~ dot_operator(".") ~ super_method_name ^^ { case _ ~ _ ~ superMethodName => MethodSuperExp(superMethodName) } |
		SuperToken ~ dot_operator(".") ~ InitToken ^^^ InitSuperExp() |
		SuperToken ~ function_call_argument_list ^^ { case _ ~ list => SubscriptSuperExp(list) }
	}
	
	lazy val function_call_argument_list: Parser[List[FunctionCallArgument]] = {
		LeftBracketToken ~ once(rep1sep(function_call_argument, CommaToken)) ~ RightBracketToken ^^ { case _ ~ list ~ _ => list }
	}
	
	lazy val function_call_arg_name: Parser[FunctionCallArgName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => FunctionCallArgName(name)
				case ImplicitParameterExpOrPWP(name) => FunctionCallArgName(name)
				case PropertyWrapperProjectionExp(name) => FunctionCallArgName(name)
				case _ => throw new ParserException("should never get here")
			}
		}} |
		keyword_as_function_param ^^ { case name => FunctionCallArgName(name) }
	}
	
	lazy val function_call_argument: Parser[FunctionCallArgument] = {
		function_call_arg_name ~ ColonToken ~ expression ^^ { case id ~ _ ~ exp => IdentifierColonExpFunctionCallArgument(id, exp) } |
		function_call_arg_name ~ ColonToken ~ operator ^^ { case id ~ _ ~ op => IdentifierColonOperatorFunctionCallArgument(id, op) } |
		expression ^^ { case exp => ExpFunctionCallArgument(exp) } |
		operator ^^ { case op => OperatorFunctionCallArgument(op) }
	}
	
	lazy val closure_expression: Parser[ClosureExp] = {
		LeftCurlyToken ~ opt(attributes) ~ opt(closure_signature) ~ opt(statements) ~ RightCurlyToken ^^ { case _ ~ optAttributes ~ optSig ~ optStmts ~ _ => ClosureExp(optAttributes, optSig, optStmts) }
	}
	
	//attributes
	//cannot have a once on the rep because of the ambiguity with empty parens
	lazy val attributes: Parser[List[Attribute]] = {
		rep1(attribute)
	}
	
	lazy val attribute_name: Parser[AttributeName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => AttributeName(name)
				case ImplicitParameterExpOrPWP(name) => AttributeName(name)
				case PropertyWrapperProjectionExp(name) => AttributeName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val attribute: Parser[Attribute] = {
		AtToken ~ attribute_name ~ attribute_argument_clause ^^ { case _ ~ name ~ clause => Attribute(name, Some(clause)) } |
		AtToken ~ attribute_name ^^ { case _ ~ name => Attribute(name, None) }
	}
	

	lazy val attribute_argument_clause: Parser[List[BalancedToken]] = {
		LeftParenToken ~ once(rep(balanced_token)) ~ RightParenToken ^^ { case _ ~ tokens ~ _ => tokens}
	}

	lazy val balanced_token: Parser[BalancedToken] = {
		keyword ^^ { case str => KeywordBalancedToken(str) } |	//bottom of file
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => IdentifierBalancedToken(name)
				case ImplicitParameterExpOrPWP(name) => IdentifierBalancedToken(name)
				case PropertyWrapperProjectionExp(name) => IdentifierBalancedToken(name)
				case _ => throw new ParserException("should never get here")
			}
		}} |
		literal ^^ { case lit => LiteralBalancedToken(lit) } |
		punctuation ^^ { case str => PunctuationBalancedToken(str) } |	//also bottom of file with keywords
		operator ^^ { case op => OperatorBalancedToken(op.value) } |
		LeftParenToken ~ balanced_token ~ RightParenToken ^^ { case _ ~ token ~ _ => InParensBalancedToken(token) } |
		LeftBracketToken ~ balanced_token ~ RightBracketToken ^^ { case _ ~ token ~ _ => InBracketsBalancedToken(token) } |
		LeftCurlyToken ~ balanced_token ~ RightCurlyToken ^^ { case _ ~ token ~ _ => InBracesBalancedToken(token) }
	}

	
	lazy val closure_signature: Parser[ClosureSignature] = {
		opt(capture_list) ~ closure_parameter_clause ~ opt(asynch_modifier) ~ opt(throws_modifier) ~ opt(function_result) ~ InToken ^^
			{ case captureList ~ clause ~ asynch ~ throws ~ funcResult ~ _ => ClosureSignatureComplex(captureList, clause, asynch, throws, funcResult) } |
		capture_list ~ InToken ^^ { case list ~ _ => ClosureSignatureSimple(list) } 
	}
	
	//cannot use once for rep1sep(capture_list_item, CommaToken) due to ambiguity with keywords used as idenitfiers
	lazy val capture_list: Parser[List[CaptureListItem]] = {
		LeftBracketToken ~ rep1sep(capture_list_item, CommaToken) ~ RightBracketToken ^^ { case _ ~ list ~ _ => list }
	}
	
	lazy val capture_list_item_name: Parser[CaptureListItemName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => CaptureListItemName(name)
				case ImplicitParameterExpOrPWP(name) => CaptureListItemName(name)
				case PropertyWrapperProjectionExp(name) => CaptureListItemName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val capture_list_item: Parser[CaptureListItem] = {
		opt(capture_specifier) ~ capture_list_item_name ~ operator("=") ~ expression ^^ { case cp ~ id ~ _ ~ exp => CaptureListItemAssignment(cp, id, exp) } |
		opt(capture_specifier) ~ capture_list_item_name ^^ { case cp ~ id => CaptureListItemIdentifier(cp, id) } |
		opt(capture_specifier) ~ self_expression ^^ { case cp ~ self => CaptureListItemSelf(cp, self) }
	}
	
	lazy val capture_specifier: Parser[CaptureSpecifier] = {
		WeakToken ^^^ WeakCaptureSpecifier |
		UnownedToken ~ LeftParenToken ~ SafeToken ~ RightParenToken ^^^ UnownedSafeCaptureSpecifier |
		UnownedToken ~ LeftParenToken ~ UnsafeToken ~ RightParenToken ^^^ UnownedUnsafeCaptureSpecifier |
		UnownedToken ^^^ UnownedCaptureSpecifier
	}
	
	lazy val unknown_identifier: Parser[String] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => name
				case ImplicitParameterExpOrPWP(name) => name
				case PropertyWrapperProjectionExp(name) => name
				case _ => throw new ParserException("should never get here")
			}
		}} |
		UnderscoreToken ^^^ "_"
	}
	
	lazy val closure_parameter_clause: Parser[ClosureParameterClause] = {
		once(rep1sep(unknown_identifier, CommaToken)) ^^ { case idList => CPCIdentifierList(idList) } |
		LeftParenToken ~ once(rep1sep(closure_parameter, CommaToken)) ~ RightParenToken ^^ { case _ ~ paramList ~ _ => CPCClosureParameterList(paramList) } |
		LeftParenToken ~ RightParenToken ^^^ CPCClosureParameterList(List())
	}
	
	lazy val closure_param_name: Parser[ClosureParamName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ClosureParamName(name)
				case ImplicitParameterExpOrPWP(name) => ClosureParamName(name)
				case PropertyWrapperProjectionExp(name) => ClosureParamName(name)
				case _ => throw new ParserException("should never get here")
			}
		}} |
		UnderscoreToken ^^^ ClosureParamName("_")
	}
	
	lazy val closure_parameter: Parser[ClosureParameter] = {
		closure_param_name ~ type_annotation ~ dot_operator("...") ^^ { case id ~ annotation ~ _ => ClosureParameterElipses(id, annotation) } |
		closure_param_name ~ opt(type_annotation) ^^ { case id ~ optAnnotation => ClosureParameterReg(id, optAnnotation) }
	}
	
	lazy val asynch_modifier: Parser[AsyncMod] = { AsyncToken ^^^ AsyncMod() }
	
	lazy val throws_modifier: Parser[ThrowsMod] = { ThrowsToken ^^^ ThrowsMod() }
	
	lazy val parenthesized_expression: Parser[ParenthesizedExp] = {
		(LeftParenToken | LeftParenNewLineToken) ~ expression ~ RightParenToken ^^ { case _ ~ exp ~ _ => ParenthesizedExp(exp) } 
	}
	
	lazy val tuple_expression: Parser[TupleExp] = {
		((LeftParenToken | LeftParenNewLineToken) ~ once(repsep(tuple_element, CommaToken)) ~ RightParenToken).flatMap({ case _ ~ list ~ _ => if(list.length == 1) { failure("cannot have only 1 item") } else { success(TupleExp(list)) }})
	}
	
	lazy val tuple_element_name: Parser[TupleElementName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => TupleElementName(name)
				case ImplicitParameterExpOrPWP(name) => TupleElementName(name)
				case PropertyWrapperProjectionExp(name) => TupleElementName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val tuple_element: Parser[TupleElement] = {
		tuple_element_name ~ ColonToken ~ expression ^^ { case name ~ _ ~  exp => IdentifierColonExpTuple(name, exp) } |
		expression ^^ { case exp => ExpTuple(exp) }
	}
	
	lazy val implicit_member_name: Parser[ImplicitMemberName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ImplicitMemberName(name)
				case ImplicitParameterExpOrPWP(name) => ImplicitMemberName(name)
				case PropertyWrapperProjectionExp(name) => ImplicitMemberName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val implicit_member_expression: Parser[Exp] = {
		dot_operator(".") ~ implicit_member_name ~ dot_operator(".") ~ postfix_expression ^^ { case _ ~ id ~ _ ~ exp => IdentifierDotPostfixImplicitMemberExp(id, exp) } |
		dot_operator(".") ~ implicit_member_name ^^ { case _ ~ id => IdentifierImplicitMemberExp(id) }
	}
	
	//gonna need to possibly change to def later to accomodate WildcardExp needing different objects in type env
	//leaving as lazy val for now for performance since we aren't mutating wildcards
	lazy val wildcard_expression: Parser[Exp] = {
		UnderscoreToken ^^^ WildcardExp()
	}
	
	lazy val key_path_expression: Parser[KeyPathExp] = {
		BackSlashToken ~ opt(typ) ~ dot_operator(".") ~ once(rep1sep(key_path_component, operator("."))) ^^ 
			{ case _ ~ optType ~ _ ~ keyPathComponents => KeyPathExp(optType, keyPathComponents) }
	}
	
	lazy val key_path_component_name: Parser[KeyPathComponentName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => KeyPathComponentName(name)
				case ImplicitParameterExpOrPWP(name) => KeyPathComponentName(name)
				case PropertyWrapperProjectionExp(name) => KeyPathComponentName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val key_path_component: Parser[KeyPathComponent] = {
		key_path_component_name ~ opt(key_path_postfixes) ^^ { case id ~ optPostfixes => IdentifierThenOptPostfixesKPC(id, optPostfixes) } |
		key_path_postfixes ^^ { case postfixes => PostfixesKPC(postfixes) }
	}
	
	lazy val key_path_postfixes: Parser[List[KeyPathPostfix]] = {
		once(rep1(key_path_postfix))
	}
	
	lazy val key_path_postfix: Parser[KeyPathPostfix] = {
		operator("?") ^^^ QuestionKPP |
		operator("!") ^^^ ExclamationKPP |
		SelfToken ^^^ SelfKPP |
		function_call_argument_list ^^ { case list => FuncCallArgListKPP(list) }
	}
	
	lazy val selector_expression: Parser[Exp] = {
		HashSelectorToken ~ LeftParenToken ~ SetterToken ~ ColonToken ~ expression ~ RightParenToken ^^ { case _ ~ _ ~ _ ~ _ ~ exp ~ _ => SelectorSetterExp(exp) } |
		HashSelectorToken ~ LeftParenToken ~ GetterToken ~ ColonToken ~ expression ~ RightParenToken ^^ { case _ ~ _ ~ _ ~ _ ~ exp ~ _ => SelectorGetterExp(exp) } |
		HashSelectorToken ~ LeftParenToken ~ expression ~ RightParenToken ^^ { case _ ~ _ ~ exp ~ _ => SelectorExp(exp) }
	}
	
	lazy val key_path_string_expression: Parser[KeyPathStringExp] = {
		HashKeyPathToken ~ LeftParenToken ~ expression ~ RightParenToken ^^ { case _ ~ _ ~ exp ~ _ => KeyPathStringExp(exp) }
	}
	//end of expressions and their direct helpers
	
	//declarations
	lazy val declaration: Parser[Declaration] = {
		import_declaration |
		constant_declaration  ~ opt(SemicolonToken) ^^ { case decl ~ _ => decl }|
		variable_declaration  <~ opt(SemicolonToken) ^^ { case decl => decl} |
		typealias_declaration |
		function_declaration |
		enum_declaration |
		struct_declaration |
		class_declaration |
		actor_declaration |
		protocol_declaration |
		initializer_declaration |
		deinitializer_declaration |
		extension_declaration |
		subscript_declaration |
		operator_declaration |
		precedence_group_declaration
	}
	
	lazy val import_declaration: Parser[ImportDeclaration] = {
		opt(attributes) ~ ImportToken ~ opt(import_kind) ~ import_path ^^ { case optAttributes ~ _ ~ kind ~ path => ImportDeclaration(optAttributes, kind, path) }
	}
	
	lazy val import_kind: Parser[ImportKind] = {
		TypeAliasToken ^^^ TypeAliasKind |
		StructToken ^^^ StructKind |
		ClassToken ^^^ ClassKind |
		EnumToken ^^^ EnumKind |
		ProtocolToken ^^^ ProtocolKind |
		LetToken ^^^ LetKind |
		VarToken ^^^ VarKind |
		FuncToken ^^^ FuncKind
	}
	
	lazy val import_path_name: Parser[ImportPathName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ImportPathName(name)
				case ImplicitParameterExpOrPWP(name) => ImportPathName(name)
				case PropertyWrapperProjectionExp(name) => ImportPathName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val import_path: Parser[ImportPath] = {
		import_path_name ~ dot_operator(".") ~ import_path ^^ { case id ~ _ ~ path => NestedPath(id, path) } |
		import_path_name ^^ { case id => RegularPath(id) }
	}
	
	lazy val constant_declaration: Parser[ConstantDeclaration] = {
		opt(attributes) ~ opt(declaration_modifiers) ~ LetToken ~ pattern_initializer_list ^^ { case optAttributes ~ optMods ~ _ ~ patternList => ConstantDeclaration(optAttributes, optMods, patternList) }
	}
	
	lazy val declaration_modifiers: Parser[List[DeclarationModifier]] = {
		once(rep1(declaration_modifier))
	}
	
	lazy val declaration_modifier: Parser[DeclarationModifier] = {
		ClassToken ^^^ ClassModifier |
		ConvenienceToken ^^^ ConvenienceModifier |
		DynamicToken ^^^ DynamicModifier |
		FinalToken ^^^ FinalModifier |
		InfixToken ^^^ InfixModifier |
		LazyToken ^^^ LazyModifier |
		OptionalToken ^^^ OptionalModifier |
		OverrideToken ^^^ OverrideModifier |
		PostFixToken ^^^ PostfixModifier |
		PrefixToken ^^^ PrefixModifier |
		RequiredToken ^^^ RequiredModifier |
		StaticToken ^^^ StaticModifier |
		UnownedToken ~ LeftParenToken ~ SafeToken ~ RightParenToken ^^^ UnownedSafeModifier |
		UnownedToken ~ LeftParenToken ~ UnsafeToken ~ RightParenToken ^^^ UnownedUnsafeModifier |
		UnownedToken ^^^ UnownedModifier |
		WeakToken ^^^ WeakModifier |
		access_level_modifier ^^ { case mod => AccessLevelModifier(mod) } |
		mutation_modifier ^^ { case mod => MutationModifier(mod) }|
		NonIsolatedToken ^^^ ActorIsolationModifier
	}
	
	lazy val access_level_modifier: Parser[AccessLevelMod] = {
		PrivateToken ~ LeftParenToken ~ SetToken ~ RightParenToken ^^^ PrivateSetModifier |
		PrivateToken ^^^ PrivateModifier |
		FilePrivateToken ~ LeftParenToken ~ SetToken ~ RightParenToken ^^^ FilePrivateSetModifier |
		FilePrivateToken ^^^ FilePrivateModifier |
		InternalToken ~ LeftParenToken ~ SetToken ~ RightParenToken ^^^ InternalSetModifier |
		InternalToken ^^^ InternalModifier |
		PublicToken ~ LeftParenToken ~ SetToken ~ RightParenToken ^^^ PublicSetModifier |
		PublicToken ^^^ PublicModifier |
		OpenToken ~ LeftParenToken ~ SetToken ~ RightParenToken ^^^ OpenSetModifier |
		OpenToken ^^^ OpenModifier
	}
	
	lazy val mutation_modifier: Parser[MutationMod] = {
		MutatingToken ^^^ MutatingModifier |
		NonmutatingToken ^^^ NonMutatingModifier
	}
	
	lazy val pattern_initializer_list: Parser[List[PatternInitializer]] = {
		once(rep1sep(pattern_initializer, CommaToken))
	}
	
	lazy val pattern_initializer: Parser[PatternInitializer] = {
		pattern ~ opt(operator("=") ~ expression) ^^ { case thePattern ~ theInitializer => PatternInitializer(thePattern, (theInitializer).map(_._2)) }
	}
	
	lazy val variable_name: Parser[VariableName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => VariableName(name)
				case ImplicitParameterExpOrPWP(name) => VariableName(name)
				case PropertyWrapperProjectionExp(name) => VariableName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val variable_declaration: Parser[Declaration] = {
		opt(attributes) ~ opt(declaration_modifiers) ~ VarToken ~ variable_name ~ type_annotation ~ opt(operator("=") ~> expression) ~ set_block ^^ { case optAttributes ~ optMods ~ _ ~ name ~ typeAnno ~ initializer ~ theBlock => VariableDeclaration6(optAttributes, optMods, name, typeAnno, initializer, theBlock) } |
		opt(attributes) ~ opt(declaration_modifiers) ~ VarToken ~ variable_name ~ type_annotation ~ once(getter_setter_block) ^^ { case optAttributes ~ optMods ~ _ ~ name ~ typeAnno ~ theBlock => VariableDeclaration23(optAttributes, optMods, name, typeAnno, theBlock) } |
		opt(attributes) ~ opt(declaration_modifiers) ~ VarToken ~ variable_name ~ type_annotation ~ getter_setter_keyword_block ^^ { case optAttributes ~ optMods ~ _ ~ name ~ typeAnno ~ theBlock => VariableDeclaration4(optAttributes, optMods, name, typeAnno, theBlock) } |
		opt(attributes) ~ opt(declaration_modifiers) ~ VarToken ~ variable_name ~ (operator("=") ~> expression) ~ set_block ^^ { case optAttributes ~ optMods ~ _ ~ name ~ initializer ~ theBlock => VariableDeclaration5(optAttributes, optMods, name, initializer, theBlock) } |
		opt(attributes) ~ opt(declaration_modifiers) ~ VarToken ~ pattern_initializer_list ^^ { case optAttributes ~ optMods ~ _ ~ list => VariableDeclaration1(optAttributes, optMods, list) }
	}
	
	lazy val getter_setter_block: Parser[GetterSetterBlock] = {
		LeftCurlyToken ~> getter_clause ~ opt(setter_clause) <~ RightCurlyToken ^^ { case getter ~ optSetter => GetterSetterClauseBlock(getter, optSetter) } |
		LeftCurlyToken ~> setter_clause ~ getter_clause <~ RightCurlyToken ^^ { case setter ~ getter => SetterGetterClauseBlock(setter, getter) } |
		code_block
	}
	
	lazy val getter_setter_keyword_block: Parser[KeywordBlock] = {
		LeftCurlyToken ~> getter_keyword_clause ~ opt(setter_keyword_clause) <~ RightCurlyToken ^^ { case getter ~ optSetter => GetterSetterKeywordBlock(getter, optSetter) } |
		LeftCurlyToken ~> setter_keyword_clause ~ getter_keyword_clause <~ RightCurlyToken ^^ { case setter ~ getter => SetterGetterKeywordBlock(setter, getter) }
	}
	
	lazy val code_block: Parser[CodeBlock] = {
		LeftCurlyToken ~ opt(statements) ~ RightCurlyToken ^^ { case _ ~ stmts ~ _ => CodeBlock(stmts) }
	}
	
	lazy val set_block: Parser[SetBlock] = {
		LeftCurlyToken ~> will_set_clause ~ opt(did_set_clause) <~ RightCurlyToken ^^ { case will ~ did => WillDidSetBlock(will, did) } |
		LeftCurlyToken ~> did_set_clause ~ opt(will_set_clause) <~ RightCurlyToken ^^ { case did ~ will => DidWillSetBlock(did, will) }
	}
	
	lazy val getter_clause: Parser[GetterClause] = {
		opt(attributes) ~ opt(mutation_modifier) ~ GetToken ~ code_block ^^ { case optAttributes ~ optMod ~ _ ~ codeBlock => GetterClause(optAttributes, optMod, codeBlock) }
	}
	
	lazy val setter_name: Parser[SetterName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => SetterName(name)
				case ImplicitParameterExpOrPWP(name) => SetterName(name)
				case PropertyWrapperProjectionExp(name) => SetterName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val setter_clause: Parser[SetterClause] = {
		opt(attributes) ~ opt(mutation_modifier) ~ SetToken ~ opt(LeftParenToken ~ setter_name ~ RightParenToken) ~ code_block ^^ {
			case optAttributes ~ mutationMod ~ _ ~ optSetterName ~ codeBlock => SetterClause(optAttributes, mutationMod, ((optSetterName).map(_._1)).map(_._2), codeBlock)
		}
	}
	
	lazy val getter_keyword_clause: Parser[GetterKeywordClause] = {
		opt(attributes) ~ opt(mutation_modifier) <~ GetToken ^^ { case optAttributes ~ optMod => GetterKeywordClause(optAttributes, optMod) }
	}
	
	lazy val setter_keyword_clause: Parser[SetterKeywordClause] = {
		opt(attributes) ~ opt(mutation_modifier) <~ SetToken ^^ { case optAttributes ~ optMod => SetterKeywordClause(optAttributes, optMod) }
	}
	
	lazy val will_set_clause: Parser[WillSetClause] = {
		opt(attributes) ~ opt(mutation_modifier) ~ WillSetToken ~ opt(LeftParenToken ~> setter_name <~ RightParenToken) ~ code_block ^^ {
			case optAttributes ~ optMod ~ _ ~ optSetterName ~ codeBlock => WillSetClause(optAttributes, optMod, optSetterName, codeBlock)
		}
	}
	
	lazy val did_set_clause: Parser[DidSetClause] = {
		opt(attributes) ~ opt(mutation_modifier) ~ DidSetToken ~ opt(LeftParenToken ~> setter_name <~ RightParenToken) ~ code_block ^^ {
			case optAttributes ~ optMod ~ _ ~ optSetterName ~ codeBlock => DidSetClause(optAttributes, optMod, optSetterName, codeBlock)
		}
	}
	
	lazy val typealias_name: Parser[TypeAliasName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => TypeAliasName(name)
				case ImplicitParameterExpOrPWP(name) => TypeAliasName(name)
				case PropertyWrapperProjectionExp(name) => TypeAliasName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val typealias_declaration: Parser[TypeAliasDeclaration] = {
		opt(attributes) ~ opt(access_level_modifier) ~ TypeAliasToken ~ typealias_name ~ opt(generic_parameter_clause) ~ operator("=") ~ typ ^^ {
			case optAttribute ~ optMod ~ _ ~ name ~ optParams ~ _ ~ theType => TypeAliasDeclaration(optAttribute, optMod, name, optParams, theType)
		}
	}
	
	//try removing once
	lazy val generic_parameter_clause: Parser[List[GenericParameter]] = {
		operator("<") ~> once(rep1sep(generic_parameter, CommaToken)) <~ operator(">") ^^ { case list => list } |
		operator("<") ~> once(rep1sep(generic_parameter, CommaToken)) ^^ { case list => list }
		}
	
	lazy val generic_parameter: Parser[GenericParameter] = {
		type_name ~ ColonToken ~ protocol_composition_type ^^ { case name ~ _ ~ theType => ProtocolCompGenericParameter(name, theType) } |
		type_name ~ ColonToken ~ type_identifier ^^ { case name ~ _ ~ typeID => AnnotatedGenericParameter(name, typeID) } |
		type_name ^^ { case name => SimpleGenericParameter(name) }
	}
	
	lazy val function_declaration: Parser[FunctionDeclaration] = {
		function_head ~ function_name ~ opt(generic_parameter_clause) ~ function_signature ~ opt(generic_where_clause) ~ opt(code_block) ^^ {
			case funcHead ~ name ~ optGenericParams ~ funcSig ~ optWhere ~ optBody => FunctionDeclaration(funcHead, name, optGenericParams, funcSig, optWhere, optBody)
		}
	}
	
	lazy val function_head: Parser[FunctionHead] = {
		opt(attributes) ~ opt(declaration_modifiers) <~ FuncToken ^^ { case theAttributes ~ declMods => FunctionHead(theAttributes, declMods) }
	}
	
	lazy val function_name: Parser[FunctionName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => IdentifierFunctionName(name)
				case ImplicitParameterExpOrPWP(name) => IdentifierFunctionName(name)
				case PropertyWrapperProjectionExp(name) => IdentifierFunctionName(name)
				case _ => throw new ParserException("should never get here")
			}
		}} |
		operator ^^ { case name => OperatorFunctionName(name) }
	}
	
	lazy val function_signature: Parser[FunctionSignature] = {
		parameter_clause ~ opt(asynch_modifier) ~ RethrowsToken ~ opt(function_result) ^^ { case paramClause ~ optAsync ~ _ ~ optFuncRes => RethrowsFunctionSig(paramClause, optAsync, optFuncRes) } |
		parameter_clause ~ opt(asynch_modifier) ~ opt(throws_modifier) ~ opt(function_result) ^^ { case paramClause ~ optAsync ~ optThrows ~ optFuncRes => ThrowsFunctionSig(paramClause, optAsync, optThrows, optFuncRes) }
	}
	
	lazy val parameter_clause: Parser[List[Parameter]] = {
		LeftParenToken ~> once(repsep(parameter, CommaToken)) <~ RightParenToken ^^ { case paramClause => paramClause }
	}
	
	lazy val local_or_external_param: Parser[String] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => name
				case ImplicitParameterExpOrPWP(name) => name
				case PropertyWrapperProjectionExp(name) => name
				case _ => throw new ParserException("should never get here")
			}
		}} |
		UnderscoreToken ^^^ "_" |
		keyword_as_function_param ^^ { case str => str }
	}
	
	lazy val parameter: Parser[Parameter] = {
		local_or_external_param ~ opt(local_or_external_param) ~ type_annotation ~ dot_operator("...") ^^ { case optExternalParamOrPossibleLocalParam ~ localParam ~ typeAnno ~ _ => {
			localParam match {
				case Some(param) => ElipsesParameter(Some(ExternalParamName(optExternalParamOrPossibleLocalParam)), LocalParamName(param), typeAnno)
				case None => ElipsesParameter(None, LocalParamName(optExternalParamOrPossibleLocalParam), typeAnno)
			}
		}
		} |
		local_or_external_param ~ opt(local_or_external_param) ~ type_annotation ~ opt(infix_operator_match("=") ~ expression) ^^ { case optExternalParamOrPossibleLocalParam ~ localParam ~ typeAnno ~ optDefaultArgClause => {
			localParam match {
				case Some(param) => OptDefaultArgClauseParameter(Some(ExternalParamName(optExternalParamOrPossibleLocalParam)), LocalParamName(param), typeAnno, (optDefaultArgClause).map(_._2))
				case None => OptDefaultArgClauseParameter(None, LocalParamName(optExternalParamOrPossibleLocalParam), typeAnno, (optDefaultArgClause).map(_._2))
			}
		}
		}
	}
	
	lazy val function_result: Parser[FunctionResult] = {
		operator("->") ~> opt(attributes) ~ typ ^^ { case optAttributes ~ theType => FunctionResult(optAttributes, theType) }
	}
	
	lazy val generic_where_clause: Parser[List[Requirement]] = {
		WhereToken ~> once(rep1sep(requirement, CommaToken)) ^^ { case requirementList => requirementList }
	}
	
	lazy val requirement: Parser[Requirement] = {
		type_identifier ~ ColonToken ~ protocol_composition_type ^^ { case typeID ~ _ ~ protocolCompType => ConformanceRequirementTypeIDProtocolCompType(typeID, protocolCompType) } |
		type_identifier ~ ColonToken ~ type_identifier ^^ { case typeID1 ~ _ ~ typeID2 => ConformanceRequirementDoubleTypeID(typeID1, typeID2) } |
		type_identifier ~ operator("==") ~ typ ^^ { case typeID ~ _ ~ theType => SameTypeRequirement(typeID, theType) }
	}
	
	lazy val enum_name: Parser[EnumName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => EnumName(name)
				case ImplicitParameterExpOrPWP(name) => EnumName(name)
				case PropertyWrapperProjectionExp(name) => EnumName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val enum_declaration: Parser[Declaration] = {
		opt(attributes) ~ opt(access_level_modifier) ~ opt(indirect_mod) ~ EnumToken ~ enum_name ~ opt(generic_parameter_clause) ~ opt(type_inheritance_clause) ~ opt(generic_where_clause) ~ LeftCurlyToken ~ opt(once(rep1(union_style_enum_member))) ~ RightCurlyToken ^^ {
			case optAttributes ~ accessMod ~ optIndirect ~ _ ~ enumName ~ genericParamClause ~ typeInheritanceClause ~ genericWhereClause ~ _ ~ enumMembers ~ _ => UnionStyleEnumDeclaration(optAttributes, accessMod, optIndirect, enumName, genericParamClause, typeInheritanceClause, genericWhereClause, enumMembers)
		} |
		opt(attributes) ~ opt(access_level_modifier) ~ EnumToken ~ enum_name ~ opt(generic_parameter_clause) ~ type_inheritance_clause ~ opt(generic_where_clause) ~ LeftCurlyToken ~ once(rep1(raw_value_style_enum_member)) ~ RightCurlyToken ^^ {
			case optAttributes ~ accessMod ~ _ ~ enumName ~ genericParamClause ~ typeInheritanceClause ~ genericWhereClause ~ _ ~ enumMembers ~ _ => RawValueStyleEnumDeclaration(optAttributes, accessMod, enumName, genericParamClause, typeInheritanceClause, genericWhereClause, enumMembers)
		}
	}
	
	lazy val type_inheritance_clause: Parser[List[TypeInheritance]] = {
		ColonToken ~> once(rep1sep(type_inheritance, CommaToken)) ^^ { case list => list }
	}
	
	lazy val type_inheritance: Parser[TypeInheritance] = {
		opt(attributes) ~ type_identifier ^^ { case optAttributes ~ typeID => TypeInheritance(optAttributes, typeID) } |
		opt(attributes) ~ LeftParenToken ~ type_identifier ~ RightParenToken ^^ { case optAttributes ~ _ ~ typeID ~ _ => TypeInheritance(optAttributes, typeID) }
	}
	
	lazy val raw_value_style_enum_member: Parser[RawValueStyleEnumMember] = {
		declaration ^^ { case decl => DeclarationRVSEnumMember(decl) } |
		opt(attributes) ~ CaseToken ~ once(rep1sep(raw_value_style_enum_case, CommaToken)) ^^ { case optAttributes ~ _ ~ caseList => EnumCaseClauseRVSEnumMember(optAttributes, caseList) } |
		compiler_control_stmt ^^ { case stmt => CompilerControlRVSEnumMember(stmt) }
	}
	
	lazy val enum_case_name: Parser[EnumCaseName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => EnumCaseName(name)
				case ImplicitParameterExpOrPWP(name) => EnumCaseName(name)
				case PropertyWrapperProjectionExp(name) => EnumCaseName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val raw_value_style_enum_case: Parser[RawValueStyleEnumCase] = {
		enum_case_name ~ opt(operator("=") ~ raw_value_literal) ^^ { case name ~ optRawValue => RawValueStyleEnumCase(name, (optRawValue).map(_._2)) }
	}
	
	lazy val raw_value_literal: Parser[RawValue] = {
		numeric_literal ^^ { case num => {
			num match {
				case DecimalIntegerLiteralExp(theNum) => NumericLiteralRawValue(theNum)
				case BinaryIntegerLiteralExp(theNum) => NumericLiteralRawValue(theNum)
				case OctalIntegerLiteralExp(theNum) => NumericLiteralRawValue(theNum)
				case HexIntegerLiteralExp(theNum) => NumericLiteralRawValue(theNum)
				case DecimalFloatLiteralExp(theNum) => NumericLiteralRawValue(theNum)
				case HexFloatLiteralExp(theNum) => NumericLiteralRawValue(theNum)
				case _ => throw new ParserException("impossible")
			}
		}} |
		string_literal ^^ { case string => {
			string match {
					case CharStringExp(value) => StaticStringLiteralRawValue(value)
					case SingleLineStaticStringLiteralExp(value) => StaticStringLiteralRawValue(value)
					case MultiLineStaticStringLiteralExp(value) => StaticStringLiteralRawValue(value)
					case _ => throw new ParserException("impossible")

			}
		}} |
		boolean_literal ^^ { case boolLit => {
			boolLit match {
				case TrueLiteralExp() => BooleanTrueLiteralRawValue
				case FalseLiteralExp() => BooleanFalseLiteralRawValue
				case _ => throw new ParserException("impossible")
			}
		}}
	}
	
	lazy val union_style_enum_member: Parser[UnionStyleEnumMember] = {
		declaration ^^ { decl => DeclarationUSEnumMember(decl) } |
		opt(attributes) ~ opt(indirect_mod) ~ CaseToken ~ once(rep1sep(union_style_enum_case, CommaToken)) ^^ {
			case optAttributes ~ optIndirect ~ _ ~ caseList => EnumCaseClauseUSEnumMember(optAttributes, optIndirect, caseList)
		} |
		compiler_control_stmt ^^ { case stmt => CompilerControlEnumMember(stmt) }
	}
	
	lazy val indirect_mod: Parser[IndirectMod] = {
		IndirectToken ^^^ IndirectMod()
	}
	
	lazy val union_style_enum_case: Parser[UnionStyleEnumCase] = {
		enum_case_name ~ opt(tuple_type) ^^ { case name ~ optTupleType => UnionStyleEnumCase(name, optTupleType) }
	}
	
	lazy val structname: Parser[Structname] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => Structname(name)
				case ImplicitParameterExpOrPWP(name) => Structname(name)
				case PropertyWrapperProjectionExp(name) => Structname(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val struct_declaration: Parser[StructDeclaration] = {
		opt(attributes) ~ opt(access_level_modifier) ~ StructToken ~ structname ~ opt(generic_parameter_clause) ~ opt(type_inheritance_clause) ~ opt(generic_where_clause) ~ struct_body ^^ {
			case optAttributes ~ accessMod ~ _ ~ name ~ genericParamClause ~ typeInheritanceClause ~ genericWhereClause ~ body => StructDeclaration(optAttributes, accessMod, name, genericParamClause, typeInheritanceClause, genericWhereClause, body)
		}
	}
	
	lazy val struct_body: Parser[List[StructMember]] = {
		LeftCurlyToken ~> once(rep(struct_member)) <~ RightCurlyToken ^^ { case body => body }
	}
	
	lazy val struct_member: Parser[StructMember] = {
		declaration ^^ { decl => DeclarationStructMember(decl) } |
		compiler_control_stmt ^^ { case stmt => CompilerControlStructMember(stmt) }
	}
	
	lazy val classname: Parser[Classname] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => Classname(name)
				case ImplicitParameterExpOrPWP(name) => Classname(name)
				case PropertyWrapperProjectionExp(name) => Classname(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val class_declaration: Parser[Declaration] = {
		opt(attributes) ~ FinalToken ~ opt(access_level_modifier) ~ ClassToken ~ classname ~ opt(generic_parameter_clause) ~ opt(type_inheritance_clause) ~ opt(generic_where_clause) ~ class_body ^^ {
			case optAttributes ~ _ ~ accessMod ~ _ ~ name ~ genericParamClause ~ typeInheritanceClause ~ genericWhereClause ~ body => ForcedFinalClassDeclaration(optAttributes, accessMod, name, genericParamClause, typeInheritanceClause, genericWhereClause, body)
		} |
		opt(attributes) ~ opt(access_level_modifier) ~ opt(final_modifier) ~ ClassToken ~ classname ~ opt(generic_parameter_clause) ~ opt(type_inheritance_clause) ~ opt(generic_where_clause) ~ class_body ^^ {
			case optAttributes ~ accessMod ~ optFinalMod ~ _ ~ name ~ genericParamClause ~ typeInheritanceClause ~ genericWhereClause ~ body => RegClassDeclaration(optAttributes, accessMod, optFinalMod, name, genericParamClause, typeInheritanceClause, genericWhereClause, body)
		}
	}
	
	lazy val class_body: Parser[List[ClassMember]] = {
		LeftCurlyToken ~> once(rep(class_member)) <~ RightCurlyToken ^^ { case list => list }
	}
	
	lazy val class_member: Parser[ClassMember] = {
		declaration ^^ { decl => DeclarationClassMember(decl) } |
		compiler_control_stmt ^^ { case stmt => CompilerControlClassMember(stmt) }
	}
	
	lazy val final_modifier: Parser[FinalMod] = {
		FinalToken ^^^ FinalMod()
	}
	
	lazy val actor_name: Parser[ActorName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ActorName(name)
				case ImplicitParameterExpOrPWP(name) => ActorName(name)
				case PropertyWrapperProjectionExp(name) => ActorName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val actor_declaration: Parser[ActorDeclaration] = {
		opt(attributes) ~ opt(declaration_modifier) ~ ActorToken ~ actor_name ~ opt(generic_parameter_clause) ~ opt(type_inheritance_clause) ~ opt(generic_where_clause) ~ actor_body ^^ {
			case optAttributes ~ accessMod ~ _ ~ name ~ genericParamClause ~ typeInheritanceClause ~ genericWhereClause ~ body => ActorDeclaration(optAttributes, accessMod, name, genericParamClause, typeInheritanceClause, genericWhereClause, body)
		}
	}
	
	lazy val actor_body: Parser[List[ActorMember]] = {
		LeftCurlyToken ~> once(rep(actor_member)) <~ RightCurlyToken ^^ { case list => list }
	}
	
	lazy val actor_member: Parser[ActorMember] = {
		declaration ^^ { decl => DeclarationActorMember(decl) } |
		compiler_control_stmt ^^ { case stmt => CompilerControlActorMember(stmt) }
	}
	
	lazy val protocol_name: Parser[ProtocolName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ProtocolName(name)
				case ImplicitParameterExpOrPWP(name) => ProtocolName(name)
				case PropertyWrapperProjectionExp(name) => ProtocolName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val protocol_declaration: Parser[ProtocolDeclaration] = {
		opt(attributes) ~ opt(access_level_modifier) ~ ProtocolToken ~ protocol_name ~ opt(protocol_specific_type_inheritance_clause) ~ opt(generic_where_clause) ~ protocol_body ^^ {
			case optAttributes ~ accessMod ~ _ ~ name ~ typeInheritanceClause ~ genericWhereClause ~ body => ProtocolDeclaration(optAttributes, accessMod, name, typeInheritanceClause, genericWhereClause, body)
		}
	}
	
	lazy val protocol_specific_type_inheritance_clause: Parser[List[TypeInheritance]] = {
		ColonToken ~> once(rep1sep(protocol_specific_type_inheritance, CommaToken)) ^^ { case list => list }
	}
	
	lazy val protocol_specific_type_inheritance: Parser[TypeInheritance] = {
		opt(attributes) ~ type_identifier ^^ { case optAttributes ~ typeID => TypeInheritance(optAttributes, typeID) } |
		ClassToken ^^^ TypeInheritance(None, NormalTypeIdentifier(TypeName("class")))
	}
	
	lazy val protocol_body: Parser[List[ProtocolMember]] = {
		LeftCurlyToken ~> once(rep(protocol_member)) <~ RightCurlyToken ^^ { case list => list }
	}
	
	lazy val protocol_member: Parser[ProtocolMember] = {
		protocol_property_declaration |
		protocol_method_declaration |
		initializer_head ~ opt(generic_parameter_clause) ~ parameter_clause ~ RethrowsToken ~ opt(generic_where_clause) <~ opt(SemicolonToken) ^^ { case head ~ optGenericParamClause ~ paramClause ~ _ ~ optGenericWhereClause =>
			RethrowsProtocolInitializerDeclaration(head, optGenericParamClause, paramClause, optGenericWhereClause) } |
		initializer_head ~ opt(generic_parameter_clause) ~ parameter_clause ~ opt(throws_modifier) ~ opt(generic_where_clause) <~ opt(SemicolonToken) ^^ {
			case head ~ optGenericParamClause ~ paramClause ~ optThrowsMod ~ optGenericWhereClause =>
				ThrowsProtocolInitializerDeclaration(head, optGenericParamClause, paramClause, optThrowsMod, optGenericWhereClause)
		} |
		protocol_subscript_declaration |
		protocol_associated_type_declaration |
		typealias_declaration ^^ { case decl => ProtocolTypeAliasDeclaration(decl) } |
		compiler_control_stmt ^^ { case stmt => CompilerControlProtocolMember(stmt) }
	}
	
	lazy val protocol_property_declaration: Parser[ProtocolPropertyDeclaration] = {
		opt(attributes) ~ opt(declaration_modifiers) ~ VarToken ~ variable_name ~ type_annotation ~ getter_setter_keyword_block <~ opt(SemicolonToken) ^^ {
			case optAttributes ~ optMods ~ _ ~ name ~ typeAnno ~ block => ProtocolPropertyDeclaration(optAttributes, optMods, name, typeAnno, block)
		}
	}
	
	lazy val protocol_method_declaration: Parser[ProtocolMethodDeclaration] = {
		function_head ~ function_name ~ opt(generic_parameter_clause) ~ function_signature ~ opt(generic_where_clause) <~ opt(SemicolonToken) ^^ {
			case head ~ name ~ genericParamClause ~ functionSig ~ genericWhereClause => ProtocolMethodDeclaration(head, name, genericParamClause, functionSig, genericWhereClause)
		}
	}
	
	lazy val protocol_subscript_declaration: Parser[ProtocolSubscriptDeclaration] = {
		subscript_head ~ subscript_result ~ opt(generic_where_clause) ~ getter_setter_keyword_block <~ opt(SemicolonToken) ^^ { case head ~ result ~ optGenericWhereClause ~ block =>
			ProtocolSubscriptDeclaration(head, result, optGenericWhereClause, block) }
	}
	
	lazy val protocol_associated_type_declaration: Parser[ProtocolAssociatedTypeDeclaration] = {
		opt(attributes) ~ opt(access_level_modifier) ~ AssociatedTypeToken ~ typealias_name ~ opt(type_inheritance_clause) ~ opt(operator("=") ~ typ) ~ opt(generic_where_clause) <~ opt(SemicolonToken) ^^ {
			case optAttributes ~ optAccessMod ~ _ ~ name ~ optTypeInheritanceClause ~ optTypealiasAssignment ~ optGenericWhereClause => 
				ProtocolAssociatedTypeDeclaration(optAttributes, optAccessMod, name, optTypeInheritanceClause, (optTypealiasAssignment).map(_._2), optGenericWhereClause)
		}
	}
	
	lazy val initializer_declaration: Parser[Declaration] = {
		initializer_head ~ opt(generic_parameter_clause) ~ parameter_clause ~ opt(asynch_modifier) ~ opt(throws_modifier) ~ opt(generic_where_clause) ~ code_block ^^ {
			case head ~ optGenericParamClause ~ paramClause ~ optAsyncMod ~ optThrowsMod ~ optGenericWhereClause ~ block =>
				ThrowsInitializerDeclaration(head, optGenericParamClause, paramClause, optAsyncMod, optThrowsMod, optGenericWhereClause, block)
		}  |
		initializer_head ~ opt(generic_parameter_clause) ~ parameter_clause ~ opt(asynch_modifier) ~ RethrowsToken ~ opt(generic_where_clause) ~ code_block ^^ {
			case head ~ optGenericParamClause ~ paramClause ~ optAsyncMod ~ _ ~ optGenericWhereClause ~ block =>
				RethrowsInitializerDeclaration(head, optGenericParamClause, paramClause, optAsyncMod, optGenericWhereClause, block)
		}
	}
	
	lazy val initializer_head: Parser[InitializerHead] = {
		opt(attributes) ~ opt(declaration_modifiers) ~ InitToken ~ operator("?") ^^ { case optAttributes ~ optDeclModifiers ~ _ ~ _ =>
			QuestionInitHead(optAttributes, optDeclModifiers) } |
		opt(attributes) ~ opt(declaration_modifiers) ~ InitToken ~ operator("!") ^^ { case optAttributes ~ optDeclModifiers ~ _ ~ _ =>
			ExclamationInitHead(optAttributes, optDeclModifiers) } |
		opt(attributes) ~ opt(declaration_modifiers) ~ InitToken ^^ { case optAttributes ~ optDeclModifiers ~ _ => RegInitHead(optAttributes, optDeclModifiers) }
	}
	
	lazy val deinitializer_declaration: Parser[DeinitializerDeclaration] = {
		opt(attributes) ~ DeinitToken ~ code_block ^^ { case optAttributes ~ _ ~ block => DeinitializerDeclaration(optAttributes, block) }
	}
	
	lazy val extension_declaration: Parser[ExtensionDeclaration] = {
		opt(attributes) ~ opt(access_level_modifier) ~ ExtensionToken ~ type_identifier ~ opt(type_inheritance_clause) ~ opt(generic_where_clause) ~ extension_body ^^ {
			case optAttributes ~ optAccessMod ~ _ ~ typeID ~ optTypeInheritanceClause ~ optGenericWhereClause ~ body => ExtensionDeclaration(optAttributes, optAccessMod, typeID, optTypeInheritanceClause, optGenericWhereClause, body)
		}
	}
	
	lazy val extension_body: Parser[List[ExtensionMember]] = {
		LeftCurlyToken ~> once(rep(extension_member)) <~ RightCurlyToken ^^ { case list => list }
	}
	
	lazy val extension_member: Parser[ExtensionMember] = {
		declaration ^^ { decl => DeclarationExtensionMember(decl) } |
		compiler_control_stmt ^^ { case stmt => CompilerControlExtensionMember(stmt) }
	}
	
	lazy val subscript_declaration: Parser[SubscriptDeclaration] = {
		subscript_head ~ subscript_result ~ opt(generic_where_clause) ~ allowable_subscript_block ^^ { case head ~ result ~ optGenericWhereClause ~ block =>
			SubscriptDeclaration(head, result, optGenericWhereClause, block) }
	}
	
	lazy val subscript_head: Parser[SubscriptHead] = {
		opt(attributes) ~ opt(declaration_modifiers) ~ SubscriptToken ~ opt(generic_parameter_clause) ~ parameter_clause ^^ {
			case optAttributes ~ optDeclModifiers ~ _ ~ optGenericParamClause ~ paramClause => SubscriptHead(optAttributes, optDeclModifiers, optGenericParamClause, paramClause)
		}
	}
	
	lazy val subscript_result: Parser[SubscriptResult] = {
		operator("->") ~ opt(attributes) ~ typ ^^ { case _ ~ optAttributes ~ theType => SubscriptResult(optAttributes, theType) }
	}
	
	lazy val allowable_subscript_block: Parser[AllowableSubscriptBlock] = {
		getter_setter_keyword_block ^^ { case block => AllowableKeywordBlock(block) } |
		getter_setter_block ^^ { case block => AllowableGetterSetterBlock(block) }
	}
	
	lazy val operator_declaration: Parser[Declaration] = {
		PrefixToken ~ OperatorToken ~ operator ^^ { case _ ~ _ ~ op => PrefixOperatorDeclaration(op) } |
		PostFixToken ~ OperatorToken ~ operator ^^ { case _ ~ _ ~ op => PostfixOperatorDeclaration(op) } |
		InfixToken ~ OperatorToken ~ operator ~ opt(ColonToken ~ precedence_group_name) ^^ { case _ ~ _ ~ op ~ optGroup => InfixOperatorDeclaration(op, (optGroup).map(_._2)) }
	}
	
	lazy val precedence_group_name: Parser[PrecedenceGroupName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => PrecedenceGroupName(name)
				case ImplicitParameterExpOrPWP(name) => PrecedenceGroupName(name)
				case PropertyWrapperProjectionExp(name) => PrecedenceGroupName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val precedence_group_declaration: Parser[PrecedenceGroupDeclaration] = {
		PrecedenceGroupToken ~ precedence_group_name ~ LeftCurlyToken ~ opt(precedence_group_attributes) ~ RightCurlyToken ^^ { case _ ~ name ~ _ ~ attributes ~ _ =>
			PrecedenceGroupDeclaration(name, attributes) }
	}
	
	lazy val precedence_group_attributes: Parser[List[PrecedenceGroupAttribute]] = {
		once(rep1(precedence_group_attribute))
	}
	
	lazy val precedence_group_attribute: Parser[PrecedenceGroupAttribute] = {
		HigherThanToken ~ ColonToken ~ once(rep1sep(precedence_group_name, CommaToken)) ^^ { case _ ~ _ ~ groupNames => PrecedenceGroupRelationHigher(groupNames) } |
		LowerThanToken ~ ColonToken ~ once(rep1sep(precedence_group_name, CommaToken)) ^^ { case _ ~ _ ~ groupNames => PrecedenceGroupRelationLower(groupNames) } |
		AssignmentToken ~ ColonToken ~ TrueToken ^^^ PrecedenceGroupAssignmentTrue |
		AssignmentToken ~ ColonToken ~ FalseToken ^^^ PrecedenceGroupAssignmentFalse |
		AssociativityToken ~ ColonToken ~ LeftToken ^^^ PrecedenceGroupLeftAssociative |
		AssociativityToken ~ ColonToken ~ RightToken ^^^ PrecedenceGroupRightAssociative |
		AssociativityToken ~ ColonToken ~ NoneToken ^^^ PrecedenceGroupNotAssociative
	}
	//end of declarations
	
	//patterns
	lazy val pattern: Parser[Pattern] = {
		primary_pattern ~ opt((astype_casting)) ^^ { case primary ~ optAsCast => {
			optAsCast match {
				case Some(asCast) => AsTypeCastingPattern(primary, asCast)
				case None => primary
			}
		}}
	}
	
	lazy val primary_pattern: Parser[Pattern] = {
		wildcard_pattern |
		optional_pattern |
		identifier_pattern |
		value_binding_pattern |
		tuple_helper |
		enum_case_pattern |
		is_type_casting_pattern |
		expression_pattern
	}
	
	lazy val astype_casting: Parser[Type] = {
		AsToken ~ typ ^^ { case _ ~ theType => theType }
	}
	
	lazy val optional_pattern: Parser[OptionalPattern] = {
		identifier_pattern ~ operator("?") ^^ { case thePattern ~ _ => OptionalPattern(thePattern) }
	}
	
	lazy val wildcard_pattern: Parser[WildcardPattern] = {
		UnderscoreToken ~ opt(type_annotation) ^^ { case _ ~ optTypeAnno => WildcardPattern(optTypeAnno) }
	}
	
	lazy val identifier_pattern: Parser[IdentifierPattern] = {
		pattern_name ~ opt(type_annotation) ^^ { case patternName ~ optTypeAnno => {
			optTypeAnno match {
				case Some(typeAnno) => IdentifierPattern(patternName, Some(typeAnno))
				case None => IdentifierPattern(patternName, None)
			}
		}}
	}
	
	lazy val pattern_name: Parser[PatternName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => PatternName(name)
				case ImplicitParameterExpOrPWP(name) => PatternName(name)
				case PropertyWrapperProjectionExp(name) => PatternName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val value_binding_pattern: Parser[ValueBindingPattern] = {
		VarToken ~ pattern ^^ { case _ ~ thePattern => ValueBindingPattern(VarModifier, thePattern) } |
		LetToken ~ pattern ^^ { case _ ~ thePattern => ValueBindingPattern(LetModifier, thePattern) }
	}
	
	lazy val tuple_helper: Parser[TuplePattern] = {
		tuple_pattern ~ opt(type_annotation) ^^ { case ogPattern ~ optTypeAnno => {
			optTypeAnno match {
				case Some(typeAnno) => TuplePattern(ogPattern.list, Some(typeAnno))
				case None => ogPattern
			}
		}}
	}
	
	lazy val tuple_pattern: Parser[TuplePattern] = {
		LeftParenToken ~ once(repsep(tuple_pattern_element, CommaToken)) ~ RightParenToken ^^ { case _ ~ list ~ _ => TuplePattern(list, None) }
	}
	
	lazy val tuple_pattern_element: Parser[TuplePatternElement] = {
		pattern_name ~ ColonToken ~ pattern ^^ { case id ~ _ ~ thePattern => IdentifierPatternElement(id, thePattern) } |
		pattern ^^ { case thePattern => PatternElement(thePattern) }
	}
	
	lazy val enum_case_pattern: Parser[EnumCasePattern] = {
		opt(type_identifier) ~ dot_operator(".") ~ enum_case_name ~ opt(tuple_pattern) ^^ { case optTypeID ~ _ ~ enumCaseName ~ optTuplePattern => EnumCasePattern(optTypeID, enumCaseName, optTuplePattern) }
	}
	
	lazy val is_type_casting_pattern: Parser[IsTypeCastingPattern] = {
		IsToken ~ typ ^^ { case _ ~ theType => IsTypeCastingPattern(theType) }
	}
	
	lazy val expression_pattern: Parser[ExpressionPattern] = {
		expression ^^ { case exp => ExpressionPattern(exp) }
	}
	//end of patterns and any direct helpers
	
	//operators
	lazy val try_operator: Parser[TryModifier] = {
		TryToken ~ operator("?") ^^^ QuestionTryModifier |
		TryToken ~ operator("!") ^^^ ExclamationTryModifier |
		TryToken ^^^ RegTryModifier
	}
	
	lazy val type_casting_operator: Parser[TypeCastingOp] = {
		IsToken ~ typ ^^ { case _ ~ theType => IsType(theType) } |
		AsToken ~ operator("?") ~ typ ^^ { case _ ~ _ ~ theType => AsQuestionType(theType) } |
		AsToken ~ operator("!") ~ typ ^^ { case _ ~ _ ~ theType => AsExclamationType(theType) } |
		AsToken ~ typ ^^ { case _ ~ theType => AsType(theType) }
	}
	
	lazy val prefix_operator: Parser[Operator] = {
		prefix_operator_thing ^^ { case PrefixOperatorLiteralToken(value) => Operator(value) } |
		dot_prefix_operator_thing ^^ { case PrefixDotOperatorLiteralToken(value) => Operator(value) }
	}
	
	lazy val infix_operator: Parser[Operator] = {
		infix_operator_thing ^^ { case InfixOperatorLiteralToken(value) => Operator(value) } |
		dot_infix_operator_thing ^^ { case InfixDotOperatorLiteralToken(value) => Operator(value) }
	}
	
	lazy val postfix_operator: Parser[Operator] = {
		postfix_operator_thing ^^ { case PostfixOperatorLiteralToken(value) => Operator(value) } |
		dot_postfix_operator_thing ^^ { case PostfixDotOperatorLiteralToken(value) => Operator(value) }
	}
	
	lazy val operator: Parser[Operator] = {
		prefix_operator | infix_operator | postfix_operator
	}
	//end of operators
	
	//types
	lazy val typ: Parser[Type] = {
		primary_type ~ once(rep(trailer)) ^^ { case first ~ second => { type_maker(first, second.reverse) } }
	}
	
	def type_maker(primaryType: Type, list: List[TrailorTypeThing]): Type = {
		list match {
			case Nil => primaryType
			case ImplicitlyUnwrappedOptionalTypeThing :: someOtherStuff => ImplicitlyUnwrappedOptionalType(type_maker(primaryType, someOtherStuff))
			case OptionalTypeThing :: someOtherStuff => OptionalType(type_maker(primaryType, someOtherStuff))
			case MetatypeTypeThing :: someOtherStuff => MetatypeTypeType(type_maker(primaryType, someOtherStuff))
			case MetatypeProtocolThing :: someOtherStuff => MetatypeProtocolType(type_maker(primaryType, someOtherStuff))
		}
	}
	
	//non-problematic types
	lazy val primary_type: Parser[Type] = {
		function_type |
		array_type |
		dictionary_type |
		protocol_composition_type |
		opaque_type |
		in_parens_type |
		tuple_type |
		small_any_type |
		type_identifier
		// AnyToken ^^^ AnyType() |
		// SelfBigToken ^^^ SelfType()
	}
	
	lazy val trailer: Parser[TrailorTypeThing] = {
		operator("!") ^^^ ImplicitlyUnwrappedOptionalTypeThing |
		operator("?") ^^^ OptionalTypeThing |
		dot_operator(".") ~ TypeToken ^^^ MetatypeTypeThing |
		dot_operator(".") ~ ProtocolToken ^^^ MetatypeProtocolThing
	}
	
	lazy val small_any_type: Parser[BoxedProtocolType] = {
		SmallAnyToken ~> typ ^^ { case theType => BoxedProtocolType(theType) }
	}
	
	lazy val function_type: Parser[FunctionType] = {
		opt(attributes) ~ function_type_arg_clause ~ opt(asynch_modifier) ~ opt(throws_modifier) ~ operator("->") ~ typ ^^ { case optAttributes ~ argClause ~ optAsync ~ optThrows ~ _ ~ theType => FunctionType(optAttributes, argClause, optAsync, optThrows, theType) }
	}
	
	lazy val function_type_arg_clause: Parser[List[FunctionTypeArg]] = {
		LeftParenToken ~ RightParenToken ^^^ List() |
		LeftParenToken ~ function_type_arg_list ~ opt(operator(".")) ~ opt(operator(".")) ~ opt(operator(".")) ~ RightParenToken ^^ { case _ ~ list ~ _ ~ _ ~ _ ~ _ => list }
	}
	
	lazy val function_type_arg_list: Parser[List[FunctionTypeArg]] = {
		once(rep1sep(function_type_arg, CommaToken))
	}
	
	lazy val argument_label: Parser[ArgumentLabel] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ArgumentLabel(name)
				case ImplicitParameterExpOrPWP(name) => ArgumentLabel(name)
				case PropertyWrapperProjectionExp(name) => ArgumentLabel(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val function_type_arg: Parser[FunctionTypeArg] = {
		argument_label ~ type_annotation ^^ { case id ~ typeAnnotation => FunctionTypeArg2(id, typeAnnotation) } |
		opt(attributes) ~ opt(in_out_modifier) ~ typ ^^ { case optAttributes ~ optInOut ~ theType => FunctionTypeArg1(optAttributes, optInOut, theType) } //|
	}
	
	lazy val array_type: Parser[ArrayType] = {
		LeftBracketToken ~ typ ~ RightBracketToken ^^ { case _ ~ theType ~ _ => ArrayType(theType) }
	}
	
	lazy val dictionary_type: Parser[DictionaryType] = {
		LeftBracketToken ~ typ ~ ColonToken ~ typ ~ RightBracketToken ^^ { case _ ~ typ1 ~ _ ~ typ2 ~ _ => DictionaryType(typ1, typ2) }
	}
	
	lazy val type_name: Parser[TypeName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => { TypeName(name) }
				case ImplicitParameterExpOrPWP(name) => TypeName(name)
				case PropertyWrapperProjectionExp(name) => TypeName(name)
				case _ => throw new ParserException("should never get here")
			}
		}} |
		SelfBigToken ^^^ TypeName("Self") |
		AnyToken ^^^ TypeName("Any")
	}
	
	lazy val type_name_after_dot: Parser[TypeName] = {
		keyword_as_identifier_member_restrictions ^^ { case name => TypeName(name)}
		variable ^^ { case VariableToken(name) => TypeName(name) } |
		ip_OR_pwp ^^ { case ImplicitParameterOrPropertyWrapperProjectionToken(name) => TypeName(name) } |
		property_wrapper_projection ^^ { case PropertyWrapperProjectionToken(name) => TypeName(name) } |
		SelfBigToken ^^^ TypeName("Self") |
		AnyToken ^^^ TypeName("Any")
	}
	
	lazy val type_identifier_after_dot: Parser[Type] = {
		type_name_after_dot ~ dot_operator(".") ~ type_identifier_after_dot ^^ { case name ~ _ ~ nestedType => NestedNormalTypeIdentifier(name, nestedType) } |
		type_name_after_dot ~ generic_argument_clause ~ dot_operator(".") ~ type_identifier_after_dot ^^ { case name ~ types ~ _ ~ recursive => NestedGenericTypeIdentifier(name, types, recursive) } |
		type_name_after_dot ~ generic_argument_clause ^^ { case name ~ types => GenericTypeIdentifier(name, types) } |
		type_name_after_dot ^^ { case name => NormalTypeIdentifier(name) }
	}
	
	lazy val type_identifier: Parser[Type] = {
		type_name ~ dot_operator(".") ~ type_identifier_after_dot ^^ { case name ~ _ ~ nestedType => NestedNormalTypeIdentifier(name, nestedType) } |
		type_name ~ generic_argument_clause ~ dot_operator(".") ~ type_identifier_after_dot ^^ { case name ~ types ~ _ ~ recursive => NestedGenericTypeIdentifier(name, types, recursive) } |
		type_name ~ generic_argument_clause ^^ { case name ~ types => GenericTypeIdentifier(name, types) } |
		type_name ^^ { case name => NormalTypeIdentifier(name) }
	}
	
	lazy val tuple_type: Parser[TupleType] = {
		LeftParenToken ~ RightParenToken ^^^ TupleType(List()) |
		LeftParenToken ~ tuple_type_element_list ~ RightParenToken ^^ { case _ ~ list ~ _ => TupleType(list) }
	}
	
	lazy val tuple_type_element_list: Parser[List[TupleTypeElement]] = {
		once(rep1sep(tuple_type_element, CommaToken))
	}
	
	lazy val element_name: Parser[ElementName] = {
		identifier ^^ { case result => {
			result match {
				case VariableExp(name) => ElementName(name)
				case ImplicitParameterExpOrPWP(name) => ElementName(name)
				case PropertyWrapperProjectionExp(name) => ElementName(name)
				case _ => throw new ParserException("should never get here")
			}
		}}
	}
	
	lazy val tuple_type_element: Parser[TupleTypeElement] = {
		element_name ~ type_annotation ^^ { case id ~ typeAnnotation => TupleTypeElementNameAnnotation(id, typeAnnotation) } |
		typ ^^ { case theType => TupleTypeElementType(theType) }
	}
	
	lazy val type_annotation: Parser[TypeAnnotation] = {
		ColonToken ~ opt(attributes) ~ opt(in_out_modifier) ~ typ ^^ { case _ ~ optAttributes ~ optInOut ~ theType => TypeAnnotation(optAttributes, optInOut, theType) }
	}
	
	lazy val in_out_modifier: Parser[InOutMod] = {
		InOutToken ^^^ InOutMod()
	}
	
	lazy val protocol_composition_type: Parser[ProtocolCompositionType] = {
		type_IDs ^^ { case typeIDs => ProtocolCompositionType(typeIDs) }
	}
	
	lazy val type_IDs: Parser[List[Type]] = {
		type_identifier ~ operator("&") ~ once(rep1sep(type_identifier, operator("&"))) ^^ { case first ~ _ ~ rest => first+:rest }
	}
	
	lazy val opaque_type: Parser[OpaqueType] = {
		SomeToken ~ typ ^^ { case _ ~ theType => OpaqueType(theType) }
	}
	
	lazy val in_parens_type: Parser[InParensType] = {
		LeftParenToken ~ typ ~ RightParenToken ^^ { case _ ~ theType ~ _ => InParensType(theType) }
	}
	//end of types and any direct helpers
	
	lazy val compiler_control_stmt: Parser[CompilerControlStmt] = {
		conditional_compilation_block ^^ { case ctrl => CompilerControlStmt(ctrl) } |
		line_control_stmt ^^ { case ctrl => CompilerControlStmt(ctrl) } |
		availability_condition ^^ { case ctrl => CompilerControlStmt(ctrl) }
	}
	
	lazy val line_control_stmt: Parser[LineControlStmt] = {
		SourceLocationToken ~ LeftParenToken ~ FileToken ~ ColonToken ~ string_literal ~ CommaToken ~ LineToken ~ ColonToken ~ integer_literal ~ RightParenToken ^^ 
			{ case _ ~ _ ~ _ ~ _ ~ file ~ _ ~ _ ~ _ ~ line ~ _ => LineControlStmt(Some(file), Some(line)) } |
		SourceLocationToken ~ LeftParenToken ~ RightParenToken ^^^ LineControlStmt(None, None)
	}
	
	lazy val availability_condition: Parser[CompilerCtrl] = {
		AvailableToken ~ LeftParenToken ~ once(rep1sep(availability_argument, CommaToken)) ~ RightParenToken ^^ {
			case _ ~ _ ~ argList ~ _ => AvailabilityConditionAvailable(argList) 
		} |
		UnavailableToken ~ LeftParenToken ~ once(rep1sep(availability_argument, CommaToken)) ~ RightParenToken ^^ {
			case _ ~ _ ~ argList ~ _ => AvailabilityConditionUnavailable(argList)
		}
	}
	
	lazy val availability_argument: Parser[AvailabilityArgument] = {
		operator("*") ^^^ AvailabilityArgument("*")  |
		platform_name ~ numeric_literal ^^ { case str ~ exp => AvailabilityArgument(str+exp.toString) }
	}
	
	lazy val platform_name: Parser[String] = {
		IOSToken ^^^ "iOS" |
		IOSApplicationExtensionToken ^^^ "iOSApplicationExtension" |
		MacOSToken ^^^ "macOS" |
		MacOSApplicationExtensionToken ^^^ "macOSApplicationExtension" |
		WatchOSToken ^^^ "watchOS" |
		WatchOSApplicationExtensionToken ^^^ "watchOSApplicationExtension" |
		TVOSToken ^^^ "tvOS" |
		TVOSApplicationExtensionToken ^^^ "tvOSApplicationExtension"
	}
	
	lazy val conditional_compilation_block: Parser[ConditionalCompilationBlock] = {
		if_directive_clause ~ opt(once(rep1(else_if_directive_clause))) ~ opt(else_directive_clause) ~ HashEndIfToken ^^ {
			case ifDirectiveClause ~ optListElseIfDirectiveClause ~ optElseDirectiveClause ~ _ => ConditionalCompilationBlock(ifDirectiveClause, optListElseIfDirectiveClause, optElseDirectiveClause)
		}
	}
	
	lazy val if_directive_clause: Parser[IfDirectiveClause] = {
		HashIfToken ~ compilation_condition ~ opt(once(rep1(statement))) ^^ { case _ ~ compCond ~ optListStmt => IfDirectiveClause(compCond, optListStmt) }
	}
	
	lazy val else_if_directive_clause: Parser[ElseIfDirectiveClause] = {
		HashElseIfToken ~ compilation_condition ~ opt(once(rep1(statement))) ^^ { case _ ~ compCond ~ optListStmt => ElseIfDirectiveClause(compCond, optListStmt) }
	}
	
	lazy val else_directive_clause: Parser[ElseDirectiveClause] = {
		HashElseToken ~ opt(once(rep1(statement))) ^^ { case _ ~ optListStmt => ElseDirectiveClause(optListStmt) }
	}
	
	def compilation_condition_maker(first: CompilationCondition, rest: List[ConditionTrailer]): CompilationCondition = {
		rest match {
			case Nil => first
			case AndTrailer(compCond) :: tail => AndCondition(first, compilation_condition_maker(first, tail))
			case OrTrailer(compCond) :: tail => OrCondition(first, compilation_condition_maker(first, tail))
		}
	}
	
	lazy val compilation_condition_trailer: Parser[ConditionTrailer] = {
		operator("&&") ~ compilation_condition ^^ { case _ ~ compCond => AndTrailer(compCond) } |
		operator("||") ~ compilation_condition ^^ { case _ ~ compCond => OrTrailer(compCond) }
	}
	
	lazy val compilation_condition: Parser[CompilationCondition] = {
		primary_compilation_condition ~ once(rep(compilation_condition_trailer)) ^^ {
			case firstCompCond ~ restCompCond => compilation_condition_maker(firstCompCond, restCompCond)
		}
	}
	
	lazy val primary_compilation_condition: Parser[CompilationCondition] = {
		platform_condition |
		identifier ^^ { case idExp => IdentifierCondition(idExp) } |
		boolean_literal ^^ { case boolLitExp => BooleanLiteralCondition(boolLitExp) } |
		LeftParenToken ~> compilation_condition <~ RightParenToken ^^ { case compCond => InParensCondition(compCond) } |
		operator("!") ~ compilation_condition ^^ { case _ ~ compCond => NotCondition(compCond) }
	}
	
	lazy val platform_condition: Parser[CompilationCondition] = {
		OSToken ~ LeftParenToken ~ operating_system ~ RightParenToken ^^ {
			case _ ~ _ ~ str ~ _ => PlatformConditionOS(str)
		} |
		ArchToken ~ LeftParenToken ~ architecture ~ RightParenToken ^^ {
			case _ ~ _ ~ str ~ _ => PlatformConditionArch(str)
		} |
		SwiftToken ~ LeftParenToken ~ (operator(">=") | operator("<")) ~ numeric_literal ~ RightParenToken ^^ {
			case _ ~ _ ~ op ~ num ~ _ => {
				num match {
					case DecimalIntegerLiteralExp(value) => PlatformConditionSwiftVer(op, value)
					case DecimalFloatLiteralExp(value) => PlatformConditionSwiftVer(op, value)
					case _ => throw new ParserException("u shouldn't be here")
				}
			}
		} |
		CompilerToken ~ LeftParenToken ~ (operator(">=") | operator("<")) ~ numeric_literal ~ RightParenToken ^^ {
			case _ ~ _ ~ op ~ num ~ _ => {
				num match {
					case DecimalIntegerLiteralExp(value) => PlatformConditionCompilerVer(op, value)
					case DecimalFloatLiteralExp(value) => PlatformConditionCompilerVer(op, value)
					case _ => throw new ParserException("u shouldn't be here")
				}
			}
		} |
		CanImportToken ~ LeftParenToken ~ import_path ~ RightParenToken ^^ { case _ ~ _ ~ path ~ _ => PlatformConditionImport(path) } |
		TargetEnvironmentToken ~ LeftParenToken ~ environment ~ RightParenToken ^^ {
			case _ ~ _ ~ str ~ _ => PlatformConditionEnv(str)
		}
	}
	
	lazy val environment: Parser[String] = {
		SimulatorToken ^^^ "simulator" |
		MacCatalystToken ^^^ "macCatalyst"
	}
	
	lazy val architecture: Parser[String] = {
		I386Token ^^^ "i386" |
		X86_64Token ^^^ "x86_64" |
		ArmToken ^^^ "arm" |
		Arm64Token ^^^ "arm64"
	}
	
	lazy val operating_system: Parser[String] = {
		MacOSToken ^^^ "macOS" |
		IOSToken ^^^ "iOS" |
		WatchOSToken ^^^ "watchOS" |
		TVOSToken ^^^ "tvOS" |
		LinuxToken ^^^ "Linux" |
		WindowsToken ^^^ "Windows"
	}
	
	lazy val keyword_as_identifier: Parser[String] = {
		AssociativityToken ^^^ "associativity" |
		ConvenienceToken ^^^ "convenience" |
		DidSetToken ^^^ "didSet" |
		DynamicToken ^^^ "dynamic" |
		FinalToken ^^^ "final" |
		GetToken ^^^ "get" |
		IndirectToken ^^^ "indirect" |
		InfixToken ^^^ "infix" |
		LazyToken ^^^ "lazy" |
		LeftToken ^^^ "left" |
		MutatingToken ^^^ "mutating" |
		NoneToken ^^^ "none" |
		NonmutatingToken ^^^ "NonmutatingToken" |
		OptionalToken ^^^ "optional" |
		OverrideToken ^^^ "override" |
		PostFixToken ^^^ "postfix" |
		PrecedenceToken ^^^ "precedence" |
		PrefixToken ^^^ "prefix" |
		ProtocolToken ^^^ "Protocol" |
		RequiredToken ^^^ "required" |
		RightToken ^^^ "right" |
		SetToken ^^^ "set" |
		SomeToken ^^^ "some" |
		TypeToken ^^^ "Type" |
		UnownedToken ^^^ "unowned" |
		WeakToken ^^^ "weak" |
		WillSetToken ^^^ "willSet" |
		SmallAnyToken ^^^ "any" |
		LineToken ^^^ "line" 
	}
	
	lazy val keyword_as_identifier_member_restrictions: Parser[String] = {
		//purposely leaving the comments to show those AREN'T allowed
		AssociativityToken ^^^ "associativity" |
		ConvenienceToken ^^^ "convenience" |
		DidSetToken ^^^ "didSet" |
		DynamicToken ^^^ "dynamic" |
		FinalToken ^^^ "final" |
		GetToken ^^^ "get" |
		IndirectToken ^^^ "indirect" |
		InfixToken ^^^ "infix" |
		LazyToken ^^^ "lazy" |
		LeftToken ^^^ "left" |
		MutatingToken ^^^ "mutating" |
		NoneToken ^^^ "none" |
		NonmutatingToken ^^^ "NonmutatingToken" |
		OptionalToken ^^^ "optional" |
		OverrideToken ^^^ "override" |
		PostFixToken ^^^ "postfix" |
		PrecedenceToken ^^^ "precedence" |
		PrefixToken ^^^ "prefix" |
		//ProtocolToken ^^^ "Protocol" |
		RequiredToken ^^^ "required" |
		RightToken ^^^ "right" |
		SetToken ^^^ "set" |
		SomeToken ^^^ "some" |
		//TypeToken ^^^ "Type" |
		UnownedToken ^^^ "unowned" |
		WeakToken ^^^ "weak" |
		WillSetToken ^^^ "willSet"
	}
	
	lazy val keyword_as_function_param: Parser[String] = {
		//purposely leaving the comments to show those AREN'T allowed
		AssociatedTypeToken ^^^ "associatedtype" |
		ClassToken ^^^ "class" |
		DeinitToken ^^^ "deinit" |
		EnumToken ^^^ "enum" |
		ExtensionToken ^^^ "extension" |
		FilePrivateToken ^^^ "fileprivate" |
		FuncToken ^^^ "func" |
		ImportToken ^^^ "import" |
		InitToken ^^^ "init" |
		//InOutToken ^^^ "inout" |
		InternalToken ^^^ "internal" |
		//LetToken ^^^ "let" |
		OpenToken ^^^ "open" |
		OperatorToken ^^^ "operator" |
		PrivateToken ^^^ "private" |
		PrecedenceGroupToken ^^^ "precedencegroup" |
		ProtocolToken ^^^ "protocol" |
		PublicToken ^^^ "public" |
		RethrowsToken ^^^ "rethrows" |
		StaticToken ^^^ "static" |
		StructToken ^^^ "struct" |
		SubscriptToken ^^^ "subscript" |
		TypeAliasToken ^^^ "typealias" |
		//VarToken ^^^ "var" |
		BreakToken ^^^ "break" |
		CaseToken ^^^ "case" |
		CatchToken ^^^ "catch" |
		ContinueToken ^^^ "continue" |
		DefaultToken ^^^ "default" |
		DeferToken ^^^ "defer" |
		DoToken ^^^ "do" |
		ElseToken ^^^ "else" |
		FallthroughToken ^^^ "fallthrough" |
		ForToken ^^^ "for" |
		GuardToken ^^^ "guard" |
		IfToken ^^^ "if" |
		InToken ^^^ "in" |
		RepeatToken ^^^ "repeat" |
		ReturnToken ^^^ "return" |
		ThrowToken ^^^ "throw" |
		SwitchToken ^^^ "switch" |
		WhereToken ^^^ "where" |
		WhileToken ^^^ "while" |
		AnyToken ^^^ "Any" |
		AsToken ^^^ "as" |
		AwaitToken ^^^ "await" |
		CatchToken ^^^ "catch" |
		FalseToken ^^^ "false" |
		IsToken ^^^ "is" |
		NilToken ^^^ "nil" |
		SelfToken ^^^ "self" |
		SelfBigToken ^^^ "Self" |
		SuperToken ^^^ "super" |
		ThrowsToken ^^^ "throws" |
		TrueToken ^^^ "true" |
		TryToken ^^^ "try" |
		UnderscoreToken ^^^ "_" |
		AvailableToken ^^^ "#available" |
		HashColorLiteralToken ^^^ "#colorLiteral" |
		HashElseIfToken ^^^ "#elseif" |
		HashElseToken ^^^ "#else" |
		HashEndIfToken ^^^ "#endif" |
		HashFileLiteralToken ^^^ "#fileLiteral" |
		HashIfToken ^^^ "#if" |
		HashImageLiteralToken ^^^ "#imageLiteral" |
		HashKeyPathToken ^^^ "#keyPath" |
		HashSelectorToken ^^^ "#selector" |
		SourceLocationToken ^^^ "#sourceLocation" |
		AssociativityToken ^^^ "associaitivty" |
		ConvenienceToken ^^^ "convenience" |
		DidSetToken ^^^ "didSet" |
		DynamicToken ^^^ "dynamic" |
		FinalToken ^^^ "final" |
		GetToken ^^^ "get" |
		IndirectToken ^^^ "indirect" |
		InfixToken ^^^ "infix" |
		LazyToken ^^^ "lazy" |
		LeftToken ^^^ "left" |
		MutatingToken ^^^ "mutating" |
		NoneToken ^^^ "none" |
		NonmutatingToken ^^^ "nonmutating" |
		OptionalToken ^^^ "optional" |
		OverrideToken ^^^ "override" |
		PostFixToken ^^^ "postfix" |
		PrecedenceToken ^^^ "precedence" |
		PrefixToken ^^^ "prefix" |
		ProtocolToken ^^^ "Protocol" |
		RequiredToken ^^^ "required" |
		RightToken ^^^ "right" |
		SetToken ^^^ "set" |
		SomeToken ^^^ "some"
	}

	lazy val punctuation: Parser[String] = {
		dot_operator(".") ^^^ "." |
		CommaToken ^^^ "," |
		ColonToken ^^^ ":" |
		SemicolonToken ^^^ ";" |
		operator("=") ^^^ "=" |
		AtToken ^^^ "@" |
		HashToken ^^^ "#" |
		operator("&") ^^^ "&" |
		operator("->") ^^^ "->" |
		BackTickToken ^^^ "`" |
		operator("?") ^^^ "?" |
		operator("!") ^^^ "!"
	}

	lazy val keyword: Parser[String] = {
		AssociatedTypeToken ^^^ "associatedtype" |
		ClassToken ^^^ "class" |
		DeinitToken ^^^ "deinit" |
		EnumToken ^^^ "enum" |
		ExtensionToken ^^^ "extension" |
		FilePrivateToken ^^^ "fileprivate" |
		FuncToken ^^^ "func" |
		ImportToken ^^^ "import" |
		InitToken ^^^ "init" |
		InOutToken ^^^ "inout" |
		InternalToken ^^^ "internal" |
		LetToken ^^^ "let" |
		OpenToken ^^^ "open" |
		OperatorToken ^^^ "operator" |
		PrivateToken ^^^ "private" |
		PrecedenceGroupToken ^^^ "precedencegroup" |
		ProtocolToken ^^^ "protocol" |
		PublicToken ^^^ "public" |
		RethrowsToken ^^^ "rethrows" |
		StaticToken ^^^ "static" |
		StructToken ^^^ "struct" |
		SubscriptToken ^^^ "subscript" |
		TypeAliasToken ^^^ "typealias" |
		VarToken ^^^ "var" |
		BreakToken ^^^ "break" |
		CaseToken ^^^ "case" |
		CatchToken ^^^ "catch" |
		ContinueToken ^^^ "continue" |
		DefaultToken ^^^ "default" |
		DeferToken ^^^ "defer" |
		DoToken ^^^ "do" |
		ElseToken ^^^ "else" |
		FallthroughToken ^^^ "fallthrough" |
		ForToken ^^^ "for" |
		GuardToken ^^^ "guard" |
		IfToken ^^^ "if" |
		InToken ^^^ "in" |
		RepeatToken ^^^ "repeat" |
		ReturnToken ^^^ "return" |
		ThrowToken ^^^ "throw" |
		SwitchToken ^^^ "switch" |
		WhereToken ^^^ "where" |
		WhileToken ^^^ "while" |
		AnyToken ^^^ "Any" |
		AsToken ^^^ "as" |
		AwaitToken ^^^ "await" |
		CatchToken ^^^ "catch" |
		FalseToken ^^^ "false" |
		IsToken ^^^ "is" |
		NilToken ^^^ "nil" |
		SelfToken ^^^ "self" |
		SelfBigToken ^^^ "Self" |
		SuperToken ^^^ "super" |
		ThrowsToken ^^^ "throws" |
		TrueToken ^^^ "true" |
		TryToken ^^^ "try" |
		UnderscoreToken ^^^ "_" |
		AvailableToken ^^^ "#available" |
		HashColorLiteralToken ^^^ "#colorLiteral" |
		HashElseIfToken ^^^ "#elseif" |
		HashElseToken ^^^ "#else" |
		HashEndIfToken ^^^ "#endif" |
		HashFileLiteralToken ^^^ "#fileLiteral" |
		HashIfToken ^^^ "#if" |
		HashImageLiteralToken ^^^ "#imageLiteral" |
		HashKeyPathToken ^^^ "#keyPath" |
		HashSelectorToken ^^^ "#selector" |
		SourceLocationToken ^^^ "#sourceLocation" |
		AssociativityToken ^^^ "associaitivty" |
		ConvenienceToken ^^^ "convenience" |
		DidSetToken ^^^ "didSet" |
		DynamicToken ^^^ "dynamic" |
		FinalToken ^^^ "final" |
		GetToken ^^^ "get" |
		IndirectToken ^^^ "indirect" |
		InfixToken ^^^ "infix" |
		LazyToken ^^^ "lazy" |
		LeftToken ^^^ "left" |
		MutatingToken ^^^ "mutating" |
		NoneToken ^^^ "none" |
		NonmutatingToken ^^^ "nonmutating" |
		OptionalToken ^^^ "optional" |
		OverrideToken ^^^ "override" |
		PostFixToken ^^^ "postfix" |
		PrecedenceToken ^^^ "precedence" |
		PrefixToken ^^^ "prefix" |
		ProtocolToken ^^^ "Protocol" |
		RequiredToken ^^^ "required" |
		RightToken ^^^ "right" |
		SetToken ^^^ "set" |
		SomeToken ^^^ "some"
	}
}