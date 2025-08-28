package FuzzingSwift.toast

import FuzzingSwift.tokenizer._
import FuzzingSwift.parser._

import scala.collection.mutable.Map
import scala.io.Source

class ToastException(message: String) extends Exception(message)

object Toast {

	var precedenceOrder: scala.collection.mutable.Map[Operator, Int] = Map(
		//DotPrecedence:
		Operator(".") -> 0,
		//BitwiseShiftPrecedence:
		Operator("<<") -> 1,
		Operator(">>") -> 1,
		Operator("&<<") -> 1,
		Operator("&>>") -> 1,
		//MultiplicationPrecedence:
		Operator("*") -> 2,
		Operator("/") -> 2,
		Operator("%") -> 2,
		Operator("&") -> 2,
		Operator("&*") -> 2,
		//AdditionPrecedence:
		Operator("+") -> 3,
		Operator("-") -> 3,
		Operator("|") -> 3,
		Operator("^") -> 3,
		Operator("&+") -> 3,
		Operator("&-") -> 3,
		//RangeFormationPrecedence:
		Operator("..<") -> 4,
		Operator("...") -> 4,
		//CastingPrecedence:
		//supposed to be 5
		//NilCoalescingPrecedence:
		Operator("??") -> 6,
		//ComparisonPrecedence:
		Operator("<") -> 7,
		Operator("<=") -> 7,
		Operator(">") -> 7,
		Operator(">=") -> 7,
		Operator("==") -> 7,
		Operator("!=") -> 7,
		Operator("===") -> 7,
		Operator("!==") -> 7,
		Operator("~=") -> 7,
		Operator(".<") -> 7,
		Operator(".<=") -> 7,
		Operator(".>") -> 7,
		Operator(".>=") -> 7,
		Operator(".==") -> 7,
		Operator(".!=") -> 7,
		//LogicalConjunctionPrecedence:
		Operator("&&") -> 8,
		Operator(".&") -> 8,
		//LogicalDisjunctionPrecednece:
		Operator("||") -> 9,
		Operator(".|") -> 9,
		Operator(".^") -> 9,
		//DefaultPrecedence:
		//this will be populated with custom operators but it's 10
		//TernaryPrecedence:
		Operator("?:") -> 11,
		//AssignmentPrecedence:
		Operator("=") -> 12,
		Operator("*=") -> 12,
		Operator("/=") -> 12,
		Operator("%=") -> 12,
		Operator("+=") -> 12,
		Operator("-=") -> 12,
		Operator("<<=") -> 12,
		Operator(">>=") -> 12,
		Operator("&=") -> 12,
		Operator("|=") -> 12,
		Operator("^=") -> 12,
		Operator("&*=") -> 12,
		Operator("&+=") -> 12,
		Operator("&-=") -> 12,
		Operator("&<<=") -> 12,
		Operator("&>>=") -> 12,
		Operator(".&=") -> 12,
		Operator(".|=") -> 12,
		Operator(".^=") -> 12
	)
	
	val	precedenceGroupNameToNum = Map(
		"BitwiseShiftPrecedence" -> 1,
		"MultiplicationPrecedence" -> 2,
		"AdditionPrecedence" -> 3,
		"RangeFormationPrecedence" -> 4,
		"CastingPrecedence" -> 5,
		"NilCoalescingPrecedence" -> 6,
		"ComparisonPrecedence" -> 7,
		"LogicalConjuctionPrecedence" -> 8,
		"LogicalDisjunctionPrecedence" -> 9,
		"DefaultPrecedence" -> 10,
		"TernaryPrecedence" -> 11,
		"AssignmentPrecedence" -> 12
	)

	def apply(fileName: String, input: String): Program = {
		val tokens = Tokenizer(fileName, input)
		val astList: Seq[Program] = Parser(fileName, Parser.program, tokens.toList)
		// println(astList.size)
		// astList.foreach(x => println(x + "\n----------------"))
		// Program(List())
		
		if (astList.isEmpty) {
			throw new ToastException(s"$fileName did not parse")
		} else {
			traverse(astList.head) //use the first program that it produced
		}
		
		//what happens if it make multiple programs? global ambiguity, really shouldn't be happening
	}
	
	def traverseOptionalThing[A](thing: Option[A])(func: A => A): Option[A] = {
		thing match {
			case Some(thing) => Some(func(thing))
			case None => None
		}
	}
	
	def traverseList[A](list: List[A])(func: A => A): List[A] = {
		list.map(x => func(x))
	}
	
	def traverse(prog: Program): Program = {
		Program(traverseList(prog.stmts)(traverseStmt _))
	}
	
	def traverseStmt(stmt: Stmt): Stmt = {
		stmt match {
			case ExpressionStmt(exp) => ExpressionStmt(traverseExp(exp))
			case DeclarationStmt(decl) => DeclarationStmt(traverseDecl(decl))
			case ForInStmt(mod, pattern, exp, optExp, codeBlock) => ForInStmt(mod, traversePattern(pattern), traverseExp(exp), traverseOptionalThing(optExp)(traverseExp _), traverseCodeBlock(codeBlock))
			case WhileStmt(conditionList, codeBlock) => WhileStmt(traverseList(conditionList)(traverseCondition _), traverseCodeBlock(codeBlock))
			case RepeatWhileStmt(codeBlock, exp) => RepeatWhileStmt(traverseCodeBlock(codeBlock), traverseExp(exp))
			case IfStmt(conditionList, codeBlock, optElseClause) => IfStmt(traverseList(conditionList)(traverseCondition _), traverseCodeBlock(codeBlock), traverseOptionalThing(optElseClause)(traverseElseClause _))
			case GuardStmt(conditionList, codeBlock) => GuardStmt(traverseList(conditionList)(traverseCondition _), traverseCodeBlock(codeBlock))
			case SwitchStmt(exp, optCaseList) => SwitchStmt(traverseExp(exp), traverseOptionalThing(optCaseList)(traverseList(_)(traverseSwitchCase _)))
			case LabeledStmt(labelName, allowableStmt) => LabeledStmt(labelName, traverseStmt(allowableStmt))
			case BreakStmt(labelName) => BreakStmt(labelName)
			case ContinueStmt(labelName) => ContinueStmt(labelName)
			case FallthroughStmt => FallthroughStmt
			case ReturnStmt(optExp) => ReturnStmt(traverseOptionalThing(optExp)(traverseExp _))
			case ThrowStmt(exp) => ThrowStmt(traverseExp(exp))
			case DeferStmt(codeBlock) => DeferStmt(traverseCodeBlock(codeBlock))
			case DoStmt(codeBlock, optCatchClauseList) => DoStmt(traverseCodeBlock(codeBlock), traverseOptionalThing(optCatchClauseList)(traverseList(_)(traverseCatchClause _)))
			case CompilerControlStmt(compCtrl) => CompilerControlStmt(traverseCompilerCtrl(compCtrl))
		}
	}
	
	def traverseCompilerCtrl(compCtrl: CompilerCtrl): CompilerCtrl = {
		compCtrl match {
			case ConditionalCompilationBlock(ifClause, optElseIfClauses, optElseClause) => {
				ConditionalCompilationBlock(traverseIfDirectiveClause(ifClause), traverseOptionalThing(optElseIfClauses)(traverseList(_)(traverseElseIfDirectiveClause _)), traverseOptionalThing(optElseClause)(traverseElseDirectiveClause _))
			}
			case thing: LineControlStmt => thing
			case thing: AvailabilityConditionAvailable => thing
			case thing: AvailabilityConditionUnavailable => thing
		}
	}
	
	def traverseIfDirectiveClause(ifClause: IfDirectiveClause): IfDirectiveClause = {
		ifClause match {
			case IfDirectiveClause(compCond, optStmtList) => IfDirectiveClause(compCond, traverseOptionalThing(optStmtList)(traverseList(_)(traverseStmt _)))
		}
	}
	
	def traverseElseIfDirectiveClause(elseIfClause: ElseIfDirectiveClause): ElseIfDirectiveClause = {
		elseIfClause match {
			case ElseIfDirectiveClause(compCond, optStmtList) => ElseIfDirectiveClause(compCond, traverseOptionalThing(optStmtList)(traverseList(_)(traverseStmt _)))
		}
	}
	
	def traverseElseDirectiveClause(elseClause: ElseDirectiveClause): ElseDirectiveClause = {
		elseClause match {
			case ElseDirectiveClause(optStmtList) => ElseDirectiveClause(traverseOptionalThing(optStmtList)(traverseList(_)(traverseStmt _)))
		}
	}
 	
	def traverseCondition(condition: Condition): Condition = {
		condition match {
			case ExpressionCondition(exp) => ExpressionCondition(traverseExp(exp))
			case CaseCondition(pattern, exp) => CaseCondition(traversePattern(pattern), traverseExp(exp))
			case OptionalBindingConditionLet(pattern, optExp) => OptionalBindingConditionLet(traversePattern(pattern), traverseOptionalThing(optExp)(traverseExp _))
			case OptionalBindingConditionVar(pattern, optExp) => OptionalBindingConditionVar(traversePattern(pattern), traverseOptionalThing(optExp)(traverseExp _))
		}
	}
	
	def traverseElseClause(clause: ElseClause): ElseClause = {
		clause match {
			case ElseCodeBlock(codeBlock) => ElseCodeBlock(traverseCodeBlock(codeBlock))
			case ElseIfStmt(ifStmt) => ElseIfStmt(traverseStmt(ifStmt))
		}
	}
	
	def traverseCaseItem(item: CaseItem): CaseItem = {
		item match {
			case CaseItem(pattern, optExp) => CaseItem(traversePattern(pattern), traverseOptionalThing(optExp)(traverseExp _))
		}
	}
	
	def traverseCaseLabel(label: CaseLabel): CaseLabel = {
		label match {
			case CaseLabel(aList, itemList) => CaseLabel(aList, traverseList(itemList)(traverseCaseItem _))
		}
	}
	
	def traverseSwitchCase(sCase: SwitchCase): SwitchCase = {
		sCase match {
			case CaseLabelStmts(caseLabel, stmtList) => CaseLabelStmts(traverseCaseLabel(caseLabel), traverseList(stmtList)(traverseStmt _))
			case DefaultLabelStmts(aList, stmtList) => DefaultLabelStmts(aList, traverseList(stmtList)(traverseStmt _))
		}
	}
	
	def traverseCatchPattern(pattern: CatchPattern): CatchPattern = {
		pattern match {
			case CatchPattern(pattern, optExp) => CatchPattern(traversePattern(pattern), traverseOptionalThing(optExp)(traverseExp _))
		}
	}
	
	def traverseCatchClause(clause: CatchClause): CatchClause = {
		clause match {
			case CatchClause(optCatchPatternList, codeBlock) => CatchClause(traverseOptionalThing(optCatchPatternList)(traverseList(_)(traverseCatchPattern _)), traverseCodeBlock(codeBlock))
		}
	}
	
	def traverseExpPairs(pair: (Exp, Exp)): (Exp, Exp) = {
		pair match {
			case (x, y) => (traverseExp(x), traverseExp(y))
		}
	}
	
	def traverseExp(exp: Exp): Exp = {
		exp match {
			case TryExp(modifier, exp) => TryExp(modifier, traverseExp(exp))
			case AwaitExp(exp) => AwaitExp(traverseExp(exp))
			case PrefixExp(op, exp) => PrefixExp(op, traverseExp(exp))
			case PostfixWithOpExp(exp, op) => PostfixWithOpExp(traverseExp(exp), op)
			case PostfixForcedValueExp(exp) => PostfixForcedValueExp(traverseExp(exp))
			case PostfixOptionalChainingExp(exp) => PostfixOptionalChainingExp(traverseExp(exp))
			case PostfixFunctionCallExp(exp, after) => PostfixFunctionCallExp(traverseExp(exp), traversePostfixFunctionCall(after))
			case PostfixSubscriptExp(exp, functionCallArgList) => PostfixSubscriptExp(traverseExp(exp), traverseList(functionCallArgList)(traverseFunctionCallArgument _))
			case PostfixInitializerExp(exp, optList) => PostfixInitializerExp(traverseExp(exp), optList)
			case PostfixSelfExp(exp) => PostfixSelfExp(traverseExp(exp))
			case PostfixExplicitMemberExp(exp, after) => PostfixExplicitMemberExp(traverseExp(exp), traverseExplicitMember(after))
			case InOutExp(exp) => InOutExp(traverseExp(exp))
			case CastExp(exp, op) => CastExp(traverseExp(exp), op)
			case AssignmentExp(prefix, exp) => AssignmentExp(traverseExp(prefix), traverseExp(exp))
			case ConditionalExp(prefix, conditionalExp, exp) => ConditionalExp(traverseExp(prefix), traverseExp(conditionalExp), traverseExp(exp))
			case TrueInfixExp(exp1, op, exp2) => {
				val (left, list) = toListLike(traverseExp(exp1), op, exp2)
				val intermediateExp = handlePrecedence(left, list)
				handleConditionalPrecedence(intermediateExp)
			}
			case GenericExp(genericName, list) => GenericExp(genericName, list)
			case literal: VariableExp => literal
			case literal: ImplicitParameterExpOrPWP => literal
			case literal: PropertyWrapperProjectionExp => literal
			case literal: DecimalIntegerLiteralExp => literal
			case literal: BinaryIntegerLiteralExp => literal
			case literal: OctalIntegerLiteralExp => literal
			case literal: HexIntegerLiteralExp => literal
			case literal: DecimalFloatLiteralExp => literal
			case literal: HexFloatLiteralExp => literal
			case literal: CharStringExp => literal
			case literal: SingleLineStaticStringLiteralExp => literal
			case literal: MultiLineStaticStringLiteralExp => literal
			case literal: InterpolatedStringLiteralExp => literal
			case literal: TrueLiteralExp => literal
			case literal: FalseLiteralExp => literal
			case literal: NilExp => literal
			case ArrayLiteralExp(expList) => ArrayLiteralExp(traverseList(expList)(traverseExp _))
			case DictionaryLiteralExp(expPairsList) => DictionaryLiteralExp(traverseList(expPairsList)(traverseExpPairs _))
			case ColorPlaygroundLiteralExp(exp1, exp2, exp3, exp4) => ColorPlaygroundLiteralExp(traverseExp(exp1), traverseExp(exp2), traverseExp(exp3), traverseExp(exp4))
			case FilePlaygroundLiteralExp(exp) => FilePlaygroundLiteralExp(traverseExp(exp))
			case ImagePlaygroundLiteralExp(exp) => ImagePlaygroundLiteralExp(traverseExp(exp))
/* 			case HashFileExp => HashFileExp
			case HashFileIDExp => HashFileIDExp
			case HashFilePathExp => HashFilePathExp
			case HashLineExp => HashLineExp
			case HashColumnExp => HashColumnExp
			case HashFunctionExp => HashFunctionExp
			case HashDSOHandleExp => HashDSOHandleExp */
			case theExp: SoloSelfExp => theExp
			case MethodSelfExp(selfMethodName) => MethodSelfExp(selfMethodName)
			case SubscriptSelfExp(functionCallArgList) => SubscriptSelfExp(traverseList(functionCallArgList)(traverseFunctionCallArgument _))
			case theExp: InitSelfExp => theExp
			case MethodSuperExp(superMethodName) => MethodSuperExp(superMethodName)
			case SubscriptSuperExp(functionCallArgList) => SubscriptSuperExp(traverseList(functionCallArgList)(traverseFunctionCallArgument _))
			case theExp: InitSuperExp => theExp
			case ClosureExp(list, optClosureSig, optStmtList) => ClosureExp(list, traverseOptionalThing(optClosureSig)(traverseClosureSignature _), traverseOptionalThing(optStmtList)(traverseList(_)(traverseStmt _)))
			case ParenthesizedExp(exp) => ParenthesizedExp(traverseExp(exp))
			case TupleExp(tupleElementList) => TupleExp(traverseList(tupleElementList)(traverseTupleElement _))
			case IdentifierImplicitMemberExp(implicitMemberName) => IdentifierImplicitMemberExp(implicitMemberName)
			case IdentifierDotPostfixImplicitMemberExp(implicitMemberName, postfixExp) => IdentifierDotPostfixImplicitMemberExp(implicitMemberName, traverseExp(postfixExp))
			case theExp: WildcardExp => theExp
			case KeyPathExp(typ, keyPathComponentList) => KeyPathExp(typ, traverseList(keyPathComponentList)(traverseKeyPathComponent _))
			case SelectorExp(exp) => SelectorExp(traverseExp(exp))
			case SelectorGetterExp(exp) => SelectorGetterExp(traverseExp(exp))
			case SelectorSetterExp(exp) => SelectorSetterExp(traverseExp(exp))
			case KeyPathStringExp(exp) => KeyPathStringExp(traverseExp(exp))
		}
	}
	
	def traverseFunctionCallArgument(arg: FunctionCallArgument): FunctionCallArgument = {
		arg match {
			case ExpFunctionCallArgument(exp) => ExpFunctionCallArgument(traverseExp(exp))
			case IdentifierColonExpFunctionCallArgument(functionCallArgName, exp) => IdentifierColonExpFunctionCallArgument(functionCallArgName, traverseExp(exp))
			case OperatorFunctionCallArgument(op) => OperatorFunctionCallArgument(op)
			case IdentifierColonOperatorFunctionCallArgument(functionCallArgName, op) => IdentifierColonOperatorFunctionCallArgument(functionCallArgName, op)
		}
	}
	
	def traverseLabeledTrailingClosure(closure: LabeledTrailingClosure): LabeledTrailingClosure = {
		closure match {
			case LabeledTrailingClosure(trailingClosureLabel, closureExp) => LabeledTrailingClosure(trailingClosureLabel, traverseExp(closureExp))
		}
	}
	
	def traverseTrailingClosure(closure: TrailingClosure): TrailingClosure = {
		closure match {
			case TrailingClosure(exp, optLabeledTrailingList) => TrailingClosure(traverseExp(exp), traverseOptionalThing(optLabeledTrailingList)(traverseList(_)(traverseLabeledTrailingClosure _)))
		}
	}
	
	def traversePostfixFunctionCall(call: PostfixFunctionCall): PostfixFunctionCall = {
		call match {
			case SimpleFunctionCall(functionCallArgList) => SimpleFunctionCall(traverseList(functionCallArgList)(traverseFunctionCallArgument _))
			case ComplexFunctionCall(optFunctionCallArgList, trailing) => ComplexFunctionCall(traverseOptionalThing(optFunctionCallArgList)(traverseList(_)(traverseFunctionCallArgument _)), traverseTrailingClosure(trailing))
		}
	}
	
	def traverseExplicitMember(member: ExplicitMember): ExplicitMember = {
		member match {
			case ExplicitMemberDecimalDigits(nums) => ExplicitMemberDecimalDigits(nums)
			case ExplicitMemberIdentifierOptGeneric(explicitMemberName, list) => ExplicitMemberIdentifierOptGeneric(explicitMemberName, list)
			case ExplicitMemberIdentifierArgs(explicitMemberName, list) => ExplicitMemberIdentifierArgs(explicitMemberName, list)
		}
	}
	
	def traverseCaptureListItem(item: CaptureListItem): CaptureListItem = {
		item match {
			case CaptureListItemIdentifier(specifier, name) => CaptureListItemIdentifier(specifier, name)
			case CaptureListItemAssignment(specifier, name, exp) => CaptureListItemAssignment(specifier, name, traverseExp(exp))
			case CaptureListItemSelf(specifier, selfExp) => CaptureListItemSelf(specifier, traverseExp(selfExp))
		}
	}
	
	def traverseClosureSignature(sig: ClosureSignature): ClosureSignature = {
		sig match {
			case ClosureSignatureComplex(optCaptureListItemList, clause, asynch, throws, funcResult) => ClosureSignatureComplex(traverseOptionalThing(optCaptureListItemList)(traverseList(_)(traverseCaptureListItem _)), clause, asynch, throws, funcResult)
			case ClosureSignatureSimple(captureItemList) => ClosureSignatureSimple(traverseList(captureItemList)(traverseCaptureListItem _))
		}
	}
	
	def traverseTupleElement(element: TupleElement): TupleElement = {
		element match {
			case ExpTuple(exp) => ExpTuple(traverseExp(exp))
			case IdentifierColonExpTuple(tupleElementName, exp) => IdentifierColonExpTuple(tupleElementName, traverseExp(exp))
		}
	}
	
	def traverseKeyPathPostfix(postfix: KeyPathPostfix): KeyPathPostfix = {
		postfix match {
			case QuestionKPP => QuestionKPP
			case ExclamationKPP => ExclamationKPP
			case SelfKPP => SelfKPP
			case FuncCallArgListKPP(functionCallArgList) => FuncCallArgListKPP(traverseList(functionCallArgList)(traverseFunctionCallArgument _))
		}
	}
	
	def traverseKeyPathComponent(component: KeyPathComponent): KeyPathComponent = {
		component match {
			case IdentifierThenOptPostfixesKPC(keyPathComponentName, optKeyPathPostfixList) => IdentifierThenOptPostfixesKPC(keyPathComponentName, traverseOptionalThing(optKeyPathPostfixList)(traverseList(_)(traverseKeyPathPostfix _)))
			case PostfixesKPC(keyPathPostfixList) => PostfixesKPC(traverseList(keyPathPostfixList)(traverseKeyPathPostfix _))
		}
	}

	def traverseDecl(decl: Declaration): Declaration = {
		decl match {
			case ImportDeclaration(aList, kind, path) => ImportDeclaration(aList, kind, path)
			case ConstantDeclaration(aList, modifier, patternInitList) => ConstantDeclaration(aList, modifier, traverseList(patternInitList)(traversePatternInitializer _))
			case VariableDeclaration1(optAttributeList, optModList, patternInitList) => VariableDeclaration1(optAttributeList, optModList, traverseList(patternInitList)(traversePatternInitializer _))
			case VariableDeclaration23(optAttributeList, optModList, varName, ta, block) => VariableDeclaration23(optAttributeList, optModList, varName, ta, traverseGetterSetterBlock(block))
			case VariableDeclaration4(optAttributeList, optModList, varName, ta, kBlock) => VariableDeclaration4(optAttributeList, optModList, varName, ta, kBlock)
			case VariableDeclaration5(optAttributeList, optModList, varName, exp, setBlock) => VariableDeclaration5(optAttributeList, optModList, varName, traverseExp(exp), traverseSetBlock(setBlock))
			case VariableDeclaration6(optAttributeList, optModList, varName, ta, optExp, setBlock) => VariableDeclaration6(optAttributeList, optModList, varName, ta, traverseOptionalThing(optExp)(traverseExp _), traverseSetBlock(setBlock))
			case TypeAliasDeclaration(aList, mod, typeAliasName, gParamClause, typ) => TypeAliasDeclaration(aList, mod, typeAliasName, gParamClause, typ)
			case FunctionDeclaration(funcHead, funcName, gParamClause, funcSig, gWhereClause, optBlock) => FunctionDeclaration(funcHead, funcName, gParamClause, traverseFunctionSignature(funcSig), gWhereClause, traverseOptionalThing(optBlock)(traverseCodeBlock _))
			case UnionStyleEnumDeclaration(aList, modList, mod, enumName, gParamClause, tIClause, gWhereClause, optEnumMembers) => UnionStyleEnumDeclaration(aList, modList, mod, enumName, gParamClause, tIClause, gWhereClause, traverseOptionalThing(optEnumMembers)(traverseList(_)(traverseUnionStyleEnumMember _)))
			case RawValueStyleEnumDeclaration(aList, modList, enumName, gParamClause, tIClause, gWhereClause, enumMembers) => RawValueStyleEnumDeclaration(aList, modList, enumName, gParamClause, tIClause, gWhereClause, traverseList(enumMembers)(traverseRawValueStyleEnumMember _))
			case StructDeclaration(aList, mod, structname, gParamClause, tIClause, gWhereClause, structMemberList) => StructDeclaration(aList, mod, structname, gParamClause, tIClause, gWhereClause, traverseList(structMemberList)(traverseStructMember _))
			case RegClassDeclaration(aList, aMod, fMod, classname, gParamClause, tIClause, gWhereClause, classMemberList) => RegClassDeclaration(aList, aMod, fMod, classname, gParamClause, tIClause, gWhereClause, traverseList(classMemberList)(traverseClassMember _))
			case ForcedFinalClassDeclaration(aList, mod, classname, gParamClause, tIClause, gWhereClause, classMemberList) => ForcedFinalClassDeclaration(aList, mod, classname, gParamClause, tIClause, gWhereClause, traverseList(classMemberList)(traverseClassMember _))
			case ActorDeclaration(aList, mod, actorName, gParamClause, tIClause, gWhereClause, actorMemberList) => ActorDeclaration(aList, mod, actorName, gParamClause, tIClause, gWhereClause, traverseList(actorMemberList)(traverseActorMember _))
			case ProtocolDeclaration(aList, mod, protocolName, tIClause, gWhereClause, protocolMemberList) => ProtocolDeclaration(aList, mod, protocolName, tIClause, gWhereClause, traverseList(protocolMemberList)(traverseProtocolMember _))
			case ThrowsInitializerDeclaration(initHead, gParamClause, paramClause, aMod, tMod, gWhereClause, block) => ThrowsInitializerDeclaration(initHead, gParamClause, traverseList(paramClause)(traverseParameter _), aMod, tMod, gWhereClause, traverseCodeBlock(block))
			case RethrowsInitializerDeclaration(initHead, gParamClause, paramClause, mod, gWhereClause, block) => RethrowsInitializerDeclaration(initHead, gParamClause, traverseList(paramClause)(traverseParameter _), mod, gWhereClause, traverseCodeBlock(block))
			case DeinitializerDeclaration(aList, block) => DeinitializerDeclaration(aList, traverseCodeBlock(block))
			case ExtensionDeclaration(aList, mod, typeID, tIClause, gWhereClause, extensionMemberList) => ExtensionDeclaration(aList, mod, typeID, tIClause, gWhereClause, traverseList(extensionMemberList)(traverseExtensionMember _))
			case SubscriptDeclaration(subscriptHead, res, gWhereClause, allowableBlock) => SubscriptDeclaration(traverseSubscriptHead(subscriptHead), res, gWhereClause, traverseAllowableSubscriptBlock(allowableBlock))
			case opDecl: PrefixOperatorDeclaration => checkPrecedence(opDecl)
			case opDecl: PostfixOperatorDeclaration => checkPrecedence(opDecl)
			case opDecl: InfixOperatorDeclaration => checkPrecedence(opDecl)
			case decl: PrecedenceGroupDeclaration => {
				addPrecedenceGroup(decl)
				decl
			}
		}
	}
	
	def addPrecedenceGroup(decl: PrecedenceGroupDeclaration): Unit = {
		decl match {
			case PrecedenceGroupDeclaration(name, optAttributeList) => optAttributeList match {
				case None => {
					precedenceGroupNameToNum += (name.name -> 10) //default precedence
				}
				case Some(attributeList) => //nothing
			}
		}
	}
	
	def traversePatternInitializer(patternInit: PatternInitializer): PatternInitializer = {
		patternInit match {
			case PatternInitializer(pattern, optExp) => PatternInitializer(traversePattern(pattern), traverseOptionalThing(optExp)(traverseExp _))
		}
	}
	
	def traverseGetterClause(clause: GetterClause): GetterClause = {
		clause match {
			case GetterClause(aList, mod, block) => GetterClause(aList, mod, traverseCodeBlock(block))
		}
	}

	def traverseSetterClause(clause: SetterClause): SetterClause = {
		clause match {
			case SetterClause(aList, mod, setterName, block) => SetterClause(aList, mod, setterName, traverseCodeBlock(block))
		}
	}
	
	def traverseCodeBlock(block: CodeBlock): CodeBlock = {
		block match {
			case CodeBlock(optStmtList) => CodeBlock(traverseOptionalThing(optStmtList)(traverseList(_)(traverseStmt(_))))
		}
	}
	
	def traverseGetterSetterBlock(block: GetterSetterBlock): GetterSetterBlock = {
		block match {
			case CodeBlock(optStmtList) => CodeBlock(traverseOptionalThing(optStmtList)(traverseList(_)(traverseStmt(_))))
			case GetterSetterClauseBlock(getter, optSetter) => GetterSetterClauseBlock(traverseGetterClause(getter), traverseOptionalThing(optSetter)(traverseSetterClause _))
			case SetterGetterClauseBlock(setter, getter) => SetterGetterClauseBlock(traverseSetterClause(setter), traverseGetterClause(getter))
		}
	}
	
	def traverseWillSetClause(clause: WillSetClause): WillSetClause = {
		clause match {
			case WillSetClause(aList, optMod, setterName, block) => WillSetClause(aList, optMod, setterName, traverseCodeBlock(block))
		}
	}
	
	def traverseDidSetClause(clause: DidSetClause): DidSetClause = {
		clause match {
			case DidSetClause(aList, optMod, setterName, block) => DidSetClause(aList, optMod, setterName, traverseCodeBlock(block))
		}
	}
	
	def traverseSetBlock(block: SetBlock): SetBlock = {
		block match {
			case WillDidSetBlock(will, optDid) => WillDidSetBlock(traverseWillSetClause(will), traverseOptionalThing(optDid)(traverseDidSetClause _))
			case DidWillSetBlock(did, optWill) => DidWillSetBlock(traverseDidSetClause(did), traverseOptionalThing(optWill)(traverseWillSetClause _))
		}
	}
	
	def traverseParameter(param: Parameter): Parameter = {
		param match {
			case OptDefaultArgClauseParameter(externalParamName, localParamName, ta, optExp) => OptDefaultArgClauseParameter(externalParamName, localParamName, ta, traverseOptionalThing(optExp)(traverseExp _))
			case ElipsesParameter(externalParamName, localParamName, ta) => ElipsesParameter(externalParamName, localParamName, ta)
		}
	}
	
	def traverseFunctionSignature(sig: FunctionSignature): FunctionSignature = {
		sig match {
			case ThrowsFunctionSig(paramClause, aMod, tMod, funcResult) => ThrowsFunctionSig(traverseList(paramClause)(traverseParameter _), aMod, tMod, funcResult)
			case RethrowsFunctionSig(paramClause, mod, funcResult) => RethrowsFunctionSig(traverseList(paramClause)(traverseParameter _), mod, funcResult)
		}
	}
	
	def traverseUnionStyleEnumMember(member: UnionStyleEnumMember): UnionStyleEnumMember = {
		member match {
			case DeclarationUSEnumMember(decl) => DeclarationUSEnumMember(traverseDecl(decl))
			case EnumCaseClauseUSEnumMember(optAttributeList, mod, caseList) => EnumCaseClauseUSEnumMember(optAttributeList, mod, caseList)
			case CompilerControlEnumMember(compCtrlStmt) => CompilerControlEnumMember(traverseStmt(compCtrlStmt))
		}
	}
	
	def traverseRawValueStyleEnumMember(member: RawValueStyleEnumMember): RawValueStyleEnumMember = {
		member match {
			case DeclarationRVSEnumMember(decl) => DeclarationRVSEnumMember(traverseDecl(decl))
			case EnumCaseClauseRVSEnumMember(optAttributeList, caseList) => EnumCaseClauseRVSEnumMember(optAttributeList, caseList)
			case CompilerControlRVSEnumMember(compCtrlStmt) => CompilerControlRVSEnumMember(traverseStmt(compCtrlStmt))
		}
	}
	
	def traverseStructMember(member: StructMember): StructMember = {
		member match {
			case DeclarationStructMember(decl) => DeclarationStructMember(traverseDecl(decl))
			case CompilerControlStructMember(compCtrlStmt) => CompilerControlStructMember(traverseStmt(compCtrlStmt))
		}
	}
	
	def traverseClassMember(member: ClassMember): ClassMember = {
		member match {
			case DeclarationClassMember(decl) => DeclarationClassMember(traverseDecl(decl))
			case CompilerControlClassMember(compCtrlStmt) => CompilerControlClassMember(traverseStmt(compCtrlStmt))
		}
	}
	
	def traverseActorMember(member: ActorMember): ActorMember = {
		member match {
			case DeclarationActorMember(decl) => DeclarationActorMember(traverseDecl(decl))
			case CompilerControlActorMember(compCtrlStmt) => CompilerControlActorMember(traverseStmt(compCtrlStmt))
		}
	}
	
	def traverseSubscriptHead(head: SubscriptHead): SubscriptHead = {
		head match {
			case SubscriptHead(aList, mod, gParamClause, paramClause) => SubscriptHead(aList, mod, gParamClause, traverseList(paramClause)(traverseParameter _))
		}
	}
	
	def traverseProtocolMember(member: ProtocolMember): ProtocolMember = {
		member match {
			case ProtocolPropertyDeclaration(optAttributeList, optModList, varName, ta, kBlock) => ProtocolPropertyDeclaration(optAttributeList, optModList, varName, ta, kBlock)
			case ProtocolMethodDeclaration(funcHead, funcName, gParamClause, funcSig, gWhereClause) => ProtocolMethodDeclaration(funcHead, funcName, gParamClause, traverseFunctionSignature(funcSig), gWhereClause)
			case ThrowsProtocolInitializerDeclaration(initHead, gParamClause, paramClause, mod, gWhereClause) => ThrowsProtocolInitializerDeclaration(initHead, gParamClause, traverseList(paramClause)(traverseParameter _), mod, gWhereClause)
			case RethrowsProtocolInitializerDeclaration(initHead, gParamClause, paramClause, gWhereClause) => RethrowsProtocolInitializerDeclaration(initHead, gParamClause, traverseList(paramClause)(traverseParameter _), gWhereClause)
			case ProtocolSubscriptDeclaration(subscriptHead, res, gWhereClause, kBlock) => ProtocolSubscriptDeclaration(traverseSubscriptHead(subscriptHead), res, gWhereClause, kBlock)
			case ProtocolAssociatedTypeDeclaration(aList, mod, typeAliasName, tIClause, tAlias, gWhereClause) => ProtocolAssociatedTypeDeclaration(aList, mod, typeAliasName, tIClause, tAlias, gWhereClause)
			case ProtocolTypeAliasDeclaration(tAliasDecl) => ProtocolTypeAliasDeclaration(tAliasDecl)
			case CompilerControlProtocolMember(compCtrlStmt) => CompilerControlProtocolMember(traverseStmt(compCtrlStmt))
		}
	}
	
	def traverseExtensionMember(member: ExtensionMember): ExtensionMember = {
		member match {
			case DeclarationExtensionMember(decl) => DeclarationExtensionMember(traverseDecl(decl))
			case CompilerControlExtensionMember(compCtrlStmt) => CompilerControlExtensionMember(traverseStmt(compCtrlStmt))
		}
	}
	
	def traverseAllowableSubscriptBlock(block: AllowableSubscriptBlock): AllowableSubscriptBlock = {
		block match {
			case AllowableGetterSetterBlock(block) => AllowableGetterSetterBlock(traverseGetterSetterBlock(block))
			case AllowableKeywordBlock(kBlock) => AllowableKeywordBlock(kBlock)	
		}
	}
	
	def traversePattern(pattern: Pattern): Pattern = {
		pattern match {
			case WildcardPattern(ta) => WildcardPattern(ta)
			case IdentifierPattern(patternName, ta) => IdentifierPattern(patternName, ta)
			case ValueBindingPattern(mod, pattern) => ValueBindingPattern(mod, traversePattern(pattern))
			case TuplePattern(list, ta) => TuplePattern(traverseList(list)(traverseTuplePatternElement(_)), ta)
			case EnumCasePattern(optTypeIdentifier, enumCaseName, Some(tuplePattern)) => EnumCasePattern(optTypeIdentifier, enumCaseName, Some(traversePattern(tuplePattern)))
			case EnumCasePattern(optTypeIdentifier, enumCaseName, None) => EnumCasePattern(optTypeIdentifier, enumCaseName, None)
			case OptionalPattern(pattern) => OptionalPattern(traversePattern(pattern))
			case ExpressionPattern(exp) => ExpressionPattern(traverseExp(exp))
			case IsTypeCastingPattern(typ) => IsTypeCastingPattern(typ)
			case AsTypeCastingPattern(pattern, typ) => AsTypeCastingPattern(traversePattern(pattern), typ)
		} 
	}
	
	def traverseTuplePatternElement(x: TuplePatternElement): TuplePatternElement = {
		x match {
			case PatternElement(pattern) => PatternElement(traversePattern(pattern))
			case IdentifierPatternElement(patternName, pattern) => IdentifierPatternElement(patternName, traversePattern(pattern))
		}
	}
	
	def checkPrecedence(opDecl: Declaration): Declaration = {
		opDecl match {
			case PrefixOperatorDeclaration(op) => PrefixOperatorDeclaration(op)
			case PostfixOperatorDeclaration(op) => PostfixOperatorDeclaration(op)
			case InfixOperatorDeclaration(op, groupName) => {
				addOpToPrecedenceGroup(op, groupName)
				InfixOperatorDeclaration(op, groupName)
			}
			case _ => throw new ToastException("unreachable")
		}
	}
	
	def addOpToPrecedenceGroup(op: Operator, precedenceGroupName: Option[PrecedenceGroupName]): Unit = {
		precedenceGroupName match {
			case None => return
			case Some(PrecedenceGroupName(name)) => precedenceOrder += (op -> precedenceGroupNameToNum(name))
			case Some(_) => return
		}
	}

	def precedenceLevel(op: Operator): Int = {
		val result = precedenceOrder.get(op)
		result match {
			case Some(num) => num
			case None => -5
		}
	}

	def isConditionalOp(op: Operator): Boolean = {
	  	op match {
	  		case Operator("==") => true
		case Operator("!=") => true
		case Operator(">") => true
		case Operator("<") => true
		case Operator("<=") => true
		case Operator(">=") => true
		case _ => false
	  	}
	}
	
	def handleConditionalPrecedence(exp: Exp): Exp = {
		exp match {
			case TrueInfixExp(exp1, op, exp2) => {
				if (isConditionalOp(op)) {
					exp2 match {
						case ConditionalExp(guard, truePath, falsePath) => ConditionalExp(TrueInfixExp(exp1, op, guard), truePath, falsePath)
						case TrueInfixExp(newExp, newOp, newExp2) => TrueInfixExp(exp1, op, handleConditionalPrecedence(exp2))
						case _ => exp
					}
				}
				else {
					TrueInfixExp(exp1, op, handleConditionalPrecedence(exp2))
				}
			}
			case _ => exp
		}
	}
	
    def toListLike(left: Exp, op: Operator, right: Exp): (Exp, List[(Operator, Exp)]) = {
      @scala.annotation.tailrec
      def loop(op: Operator, right: Exp, accum: List[(Operator, Exp)]): List[(Operator, Exp)] = {
        right match {
          case TrueInfixExp(innerLeft, innerOp, innerRight) => {
            loop(innerOp, innerRight, (op, traverseExp(innerLeft)) :: accum)
          }
          case _ => {
            ((op, traverseExp(right)) :: accum).reverse
          }
        }
      }
      (left, loop(op, right, List()))
    }

  // (Exp, List[(Op, Exp)]) => Exp
  def handlePrecedence(left: Exp, list: List[(Operator, Exp)]): Exp = {
    // basic idea: find the lowest precedence operator in the list
    // take the first one, and combine it
	//try this:
	//performSteps(left, list)
	//end try this
    def step(left: Exp, list: List[(Operator, Exp)]): (Exp, List[(Operator, Exp)]) = {
      assert(list.nonEmpty)
      val lowestPrecedence = list.map(pair => precedenceLevel(pair._1)).min
      if (precedenceLevel(list.head._1) == lowestPrecedence) {
        (TrueInfixExp(left, list.head._1, list.head._2), list.tail)
      } else {
        assert(list.tail.nonEmpty)
        def loop(list: List[(Operator, Exp)]): List[(Operator, Exp)] = {
          list match {
            case (op1, exp1) :: (op2, exp2) :: rest => {
              // 3, List((+, 4), (*, 7), (/, 8))
              // 3, List((+, (4 * 7)), (/, 8))
              //
              // List((*, 4), (+, 2))
              if (precedenceLevel(op2) == lowestPrecedence) {
                (op1, TrueInfixExp(exp1, op2, exp2)) :: rest
              } else {
                //assert(precedenceLevel(op1) >= precedenceLevel(op2)) //this is actually incorrect
                assert(precedenceLevel(op2) > lowestPrecedence)
                assert(rest.size >= 1)
                (op1, exp1) :: loop((op2, exp2) :: rest)
              }
            }
            case _ => ???
          }
        }
		(left, loop(list))
      }
    } // step

    @scala.annotation.tailrec
    def performSteps(left: Exp, list: List[(Operator, Exp)]): Exp = {
      if (list.isEmpty) {
        left
      } else {
        val (newLeft, newList) = step(left, list)
        performSteps(newLeft, newList)
      }
    }

	performSteps(left, list)
  }
}