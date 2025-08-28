package FuzzingSwift.generator

class GeneratorException(message: String) extends Exception(message)

object Generator {

	import FuzzingSwift.unification._
	import FuzzingSwift.unification.UIterator._
	import FuzzingSwift.parser._
	import FuzzingSwift.toast._
	import FuzzingSwift.typechecker._

	import scala.util.Random
	
	val random = new Random()
	var lvlFlag = ""
	
	type TypeEnv = Map[String, (Typ, Mutability)]
	type Funcs = Map[String, (Typ, Option[Throwing], List[Parameter])]
	
	trait PrologList[A]
	def nil[A]: StructureTerm[PrologList[A]] = VariantOf[PrologList[A]]("nil")
	def cons[A](head: Term[A], tail: Term[PrologList[A]]): StructureTerm[PrologList[A]] = {
		VariantOf[PrologList[A]]("cons", head, tail)
	}

	trait UnificationType {}
	val intUnificationType = VariantOf[UnificationType]("intType")
	val doubleUnificationType = VariantOf[UnificationType]("doubleType")
	val boolUnificationType = VariantOf[UnificationType]("boolType")
	val stringUnificationType = VariantOf[UnificationType]("stringType")
	val charUnificationType = VariantOf[UnificationType]("charType")
	val voidUnificationType = VariantOf[UnificationType]("voidType")
	def optionalUnificationType(theTyp: Term[UnificationType]) = VariantOf[UnificationType]("optionalType", theTyp)
	def arrayUnificationType(theTyp: Term[UnificationType]) = VariantOf[UnificationType]("arrayType", theTyp)
	def dictionaryUnificationType(theTyp1: Term[UnificationType], theTyp2: Term[UnificationType]) = VariantOf[UnificationType]("dictionaryType", theTyp1, theTyp2)
	def opaqueUnificationType(theTyp: Term[UnificationType]) = VariantOf[UnificationType]("opaqueType", theTyp)
	def namedUnificationType(name: Term[String]) = VariantOf[UnificationType]("namedType", name)
	val anyUnificationType = VariantOf[UnificationType]("anyType")
	//val selfUnificationType = VariantOf[UnificationType]("selfType")
	def implicitlyUnwrappedOptionalUnificationType(theType: Term[UnificationType]) = VariantOf[UnificationType]("unwrappedOptionalType", theType)
	def metatypeTypeUnificationType(theType: Term[UnificationType]) = VariantOf[UnificationType]("metatypeTypeType", theType)
	def metatypeProtocolUnificationType(theType: Term[UnificationType]) = VariantOf[UnificationType]("metatypeProtocolType", theType)
	def functionUnificationType(input: Term[PrologList[UnificationType]], output: Term[UnificationType]) = VariantOf[UnificationType]("functionType", input, output)
	
	def apply(program: Program, flag: String): UIterator[Unit, Program] = {
		lvlFlag = flag
		maybeRewriteProgram(program)
	}
	
	def maybeRewriteProgram(prog: Program): UIterator[Unit, Program] = {
		prog match {
			case Program(stmtList) => {
				for {
					newStmtList <- uimap(stmtList)(maybeRewriteStmt _)
				} yield Program(newStmtList)
			}
		}
	}
	
	def maybeRewriteCondition(cond: Condition): UIterator[Unit, Condition] = {
		cond match {
			case ExpressionCondition(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ExpressionCondition(newExp)
			}
			case CaseCondition(pattern, exp) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newExp <- maybeRewriteExp(exp)
				} yield CaseCondition(newPattern, newExp)
			}
			case OptionalBindingConditionLet(pattern, optExp) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
				} yield OptionalBindingConditionLet(newPattern, newOptExp)
			}
			case OptionalBindingConditionVar(pattern, optExp) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
				} yield OptionalBindingConditionVar(newPattern, newOptExp)
			}
		}
	}
	
	def maybeRewriteElseClause(clause: ElseClause): UIterator[Unit, ElseClause] = {
		clause match {
			case ElseCodeBlock(codeBlock) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield ElseCodeBlock(newCodeBlock)
			}
			case ElseIfStmt(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield ElseIfStmt(newStmt)
			}
		}
	}
	
	def maybeRewriteCaseItem(item: CaseItem): UIterator[Unit, CaseItem] = {
		item match {
			case CaseItem(pattern, optExp) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
				} yield CaseItem(newPattern, newOptExp)
			}
		}
	}
	
	def maybeRewriteCaseLabel(caseLabel: CaseLabel): UIterator[Unit, CaseLabel] = {
		caseLabel match {
			case CaseLabel(optAttributeList, caseItemList) => {
				for {
					newCaseItemList <- uimap(caseItemList)(maybeRewriteCaseItem _)
				} yield CaseLabel(optAttributeList, newCaseItemList)
			}
		}
	}
	
	def maybeRewriteSwitchCase(sCase: SwitchCase): UIterator[Unit, SwitchCase] = {
		sCase match {
			case CaseLabelStmts(caseLabel, stmtList) => {
				for {
					newCaseLabel <- maybeRewriteCaseLabel(caseLabel)
					newStmtList <- uimap(stmtList)(maybeRewriteStmt _)
				} yield CaseLabelStmts(newCaseLabel, newStmtList)
			}
			case DefaultLabelStmts(optAttributeList, stmtList) => {
				for {
					newStmtList <- uimap(stmtList)(maybeRewriteStmt _)
				} yield DefaultLabelStmts(optAttributeList, newStmtList)
			}
		}
	}
	
	def maybeRewriteCatchPattern(pattern: CatchPattern): UIterator[Unit, CatchPattern] = {
		pattern match {
			case CatchPattern(pattern, optExp) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
				} yield CatchPattern(newPattern, newOptExp)
			}
		}
	}
	
	def maybeRewriteCatchClause(clause: CatchClause): UIterator[Unit, CatchClause] = {
		clause match {
			case CatchClause(optCatchPatternList, codeBlock) => {
				for {
					newOptCatchPatternList <- combined(optCatchPatternList)(maybeRewriteCatchPattern _)
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield CatchClause(newOptCatchPatternList, newCodeBlock)
			}
		}
	}
	
	def maybeRewriteStmt(stmt: Stmt): UIterator[Unit, Stmt] = {
		stmt match {
			case ExpressionStmt(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ExpressionStmt(newExp)
			}
			case DeclarationStmt(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield DeclarationStmt(newDecl)
			}
			case ForInStmt(optMod, pattern, exp, optExp, codeBlock) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newExp <- maybeRewriteExp(exp)
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield ForInStmt(optMod, newPattern, newExp, newOptExp, newCodeBlock)
			}
			case WhileStmt(conditionList, codeBlock) => {
				for {
					newConditionList <- uimap(conditionList)(maybeRewriteCondition _)
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield WhileStmt(newConditionList, newCodeBlock)
			}
			case RepeatWhileStmt(codeBlock, exp) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
					newExp <- maybeRewriteExp(exp)
				} yield RepeatWhileStmt(newCodeBlock, newExp)
			}
			case IfStmt(conditionList, codeBlock, optElseClause) => {
				for {
					newConditionList <- uimap(conditionList)(maybeRewriteCondition _)
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
					newOptElseClasuse <- maybeRewriteOptionalThing(optElseClause)(maybeRewriteElseClause _)
				} yield IfStmt(newConditionList, newCodeBlock, newOptElseClasuse)
			}
			case GuardStmt(conditionList, codeBlock) => {
				for {
					newConditionList <- uimap(conditionList)(maybeRewriteCondition _)
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield GuardStmt(newConditionList, newCodeBlock)
			}
			case SwitchStmt(exp, optSwitchCaseList) => {
				for {
					newExp <- maybeRewriteExp(exp)
					newOptSwitchCaseList <- combined(optSwitchCaseList)(maybeRewriteSwitchCase _)
				} yield SwitchStmt(newExp, newOptSwitchCaseList)
			}
			case LabeledStmt(labelName, allowableStmt) => {
				for {
					newAllowableStmt <- maybeRewriteStmt(allowableStmt)
				} yield LabeledStmt(labelName, newAllowableStmt)
			}
			case BreakStmt(optLabelName) => singleton(BreakStmt(optLabelName))
			case ContinueStmt(optLabelName) => singleton(ContinueStmt(optLabelName))
			case FallthroughStmt => singleton(FallthroughStmt)
			case ReturnStmt(optExp) => {
				for {
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
				} yield ReturnStmt(newOptExp)
			}
			case ThrowStmt(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ThrowStmt(newExp)
			}
			case DeferStmt(codeBlock) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield DeferStmt(newCodeBlock)
			}
			case DoStmt(codeBlock, optCatchClauseList) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
					newOptCatchClauseList <- combined(optCatchClauseList)(maybeRewriteCatchClause _)
				} yield DoStmt(newCodeBlock, newOptCatchClauseList)
			}
			case CompilerControlStmt(compilerCtrl) => {
				for {
					newCompilerCtrl <- maybeRewriteCompilerCtrl(compilerCtrl)
				} yield CompilerControlStmt(newCompilerCtrl)
			}
		}
	}
	
	def maybeRewriteCompilerCtrl(compCtrl: CompilerCtrl): UIterator[Unit, CompilerCtrl] = {
		compCtrl match {
			case ConditionalCompilationBlock(ifClause, optElseIfClauseList, optElseClause) => {
				for {
					newIfClause <- maybeRewriteIfDirectiveClause(ifClause)
					newOptElseIfClauseList <- combined(optElseIfClauseList)(maybeRewriteElseIfClause _)
					newOptElseClause <- maybeRewriteOptionalThing(optElseClause)(maybeRewriteElseDirectiveClause _)
				} yield ConditionalCompilationBlock(newIfClause, newOptElseIfClauseList, newOptElseClause)
			}
			case thing: LineControlStmt => singleton(thing)
			case thing: AvailabilityConditionAvailable => singleton(thing)
			case thing: AvailabilityConditionUnavailable => singleton(thing)
		}
	}
	
	def maybeRewriteElseDirectiveClause(elseClause: ElseDirectiveClause): UIterator[Unit, ElseDirectiveClause] = {
		elseClause match {
			case ElseDirectiveClause(optStmtList) => {
				for {
					newOptStmtList <- combined(optStmtList)(maybeRewriteStmt _)
				} yield ElseDirectiveClause(newOptStmtList)
			}
		}
	}
	
	def maybeRewriteElseIfClause(elseIfClause: ElseIfDirectiveClause): UIterator[Unit, ElseIfDirectiveClause] = {
		elseIfClause match {
			case ElseIfDirectiveClause(compCond, optStmtList) => {
				for {
					newOptStmtList <- combined(optStmtList)(maybeRewriteStmt _)
				} yield ElseIfDirectiveClause(compCond, newOptStmtList)
			}
		}
	}
	
	def maybeRewriteIfDirectiveClause(ifClause: IfDirectiveClause): UIterator[Unit, IfDirectiveClause] = {
		ifClause match {
			case IfDirectiveClause(compCond, optStmtList) => {
				for {
					newOptStmtList <- combined(optStmtList)(maybeRewriteStmt _)
				} yield IfDirectiveClause(compCond, newOptStmtList)
			}
		}
	}

	def maybeRewriteImportPath(path: ImportPath): UIterator[Unit, ImportPath] = {
		path match {
			case RegularPath(pathName) => singleton(RegularPath(pathName))
			case NestedPath(pathName, importPath) => {
				for {
					newPath <- maybeRewriteImportPath(importPath)
				} yield NestedPath(pathName, newPath)
			}
		}
	}

	def maybeRewritePatternInitializer(patternInit: PatternInitializer): UIterator[Unit, PatternInitializer] = {
		patternInit match {
			case PatternInitializer(pattern, optExp) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
				} yield PatternInitializer(newPattern, newOptExp)
			}
		}
	}

	def maybeRewriteCodeBlock(block: CodeBlock): UIterator[Unit, CodeBlock] = {
		block match {
			case CodeBlock(stmtList) => {
				for {
					newStmtList <- combined(stmtList)(maybeRewriteStmt _)
				} yield CodeBlock(newStmtList)
			}
		}
	}

	def maybeRewriteGetterClause(clause: GetterClause): UIterator[Unit, GetterClause] = {
		clause match {
			case GetterClause(optAttributeList, optMod, codeBlock) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield GetterClause(optAttributeList, optMod, newCodeBlock)
			}
		}
	}

	def maybeRewriteSetterClause(clause: SetterClause): UIterator[Unit, SetterClause] = {
		clause match {
			case SetterClause(optAttributeList, optMod, optSetterName, codeBlock) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield SetterClause(optAttributeList, optMod, optSetterName, newCodeBlock)
			}
		}
	}

	def maybeRewriteGetterSetterBlock(block: GetterSetterBlock): UIterator[Unit, GetterSetterBlock] = {
		block match {
			case codeBlock: CodeBlock => { maybeRewriteCodeBlock(codeBlock) }
			case GetterSetterClauseBlock(getterClause, optSetterClause) => {
				for {
					newGetterClause <- maybeRewriteGetterClause(getterClause)
					newOptSetterClause <- maybeRewriteOptionalThing(optSetterClause)(maybeRewriteSetterClause _)
				} yield GetterSetterClauseBlock(newGetterClause, newOptSetterClause)
			}
			case SetterGetterClauseBlock(setterClause, getterClause) => {
				for {
					newSetterClause <- maybeRewriteSetterClause(setterClause)
					newGetterClause <- maybeRewriteGetterClause(getterClause)
				} yield SetterGetterClauseBlock(newSetterClause, newGetterClause)
			}
		}
	}

	def maybeRewriteKeywordBlock(block: KeywordBlock): UIterator[Unit, KeywordBlock] = {
		block match {
			case GetterSetterKeywordBlock(getterKeywordClause, optSetterKeywordClause) => {
				singleton(GetterSetterKeywordBlock(getterKeywordClause, optSetterKeywordClause))
			}
			case SetterGetterKeywordBlock(setterKeywordClause, getterKeywordClause) => {
				singleton(SetterGetterKeywordBlock(setterKeywordClause, getterKeywordClause))
			}
		}
	}
	
	def maybeRewriteWillSetClause(clause: WillSetClause): UIterator[Unit, WillSetClause] = {
		clause match {
			case WillSetClause(optAttributeList, optMod, optSetterName, codeBlock) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield WillSetClause(optAttributeList, optMod, optSetterName, newCodeBlock)
			}
		}
	}
	
	def maybeRewriteDidSetClause(clause: DidSetClause): UIterator[Unit, DidSetClause] = {
		clause match {
			case DidSetClause(optAttributeList, optMod, optSetterName, codeBlock) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield DidSetClause(optAttributeList, optMod, optSetterName, newCodeBlock)
			}
		}
	}
	
	def maybeRewriteSetBlock(block: SetBlock): UIterator[Unit, SetBlock] = {
		block match {
			case WillDidSetBlock(will, optDid) => {
				for {
					newWill <- maybeRewriteWillSetClause(will)
					newOptDid <- maybeRewriteOptionalThing(optDid)(maybeRewriteDidSetClause _)
				} yield WillDidSetBlock(newWill, newOptDid)
			}
			case DidWillSetBlock(did, optWill) => {
				for {
					newDid <- maybeRewriteDidSetClause(did)
					newOptWill <- maybeRewriteOptionalThing(optWill)(maybeRewriteWillSetClause _)
				} yield DidWillSetBlock(newDid, newOptWill)
			}
		}
	}
	
	def maybeRewriteGenericParam(param: GenericParameter): UIterator[Unit, GenericParameter] = {
		param match {
			case SimpleGenericParameter(typeName) => singleton(SimpleGenericParameter(typeName))
			case AnnotatedGenericParameter(typeName, typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield AnnotatedGenericParameter(typeName, newType)
			}
			case ProtocolCompGenericParameter(typeName, typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield ProtocolCompGenericParameter(typeName, newType)
			}
		}
	}
	
	def maybeRewriteParameter(param: Parameter): UIterator[Unit, Parameter] = {
		param match {
			case OptDefaultArgClauseParameter(optExternalParamName, localParamName, typeAnno, optExp) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
					newOptExp <- maybeRewriteOptionalThing(optExp)(maybeRewriteExp _)
				} yield OptDefaultArgClauseParameter(optExternalParamName, localParamName, newTypeAnno, newOptExp)
			}
			case ElipsesParameter(optExternalParamName, localParamName, typeAnno) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
				} yield ElipsesParameter(optExternalParamName, localParamName, newTypeAnno)
			}
		}
	}
	
	def maybeRewriteFunctionSignature(sig: FunctionSignature): UIterator[Unit, FunctionSignature] = {
		sig match {
			case ThrowsFunctionSig(paramList, optAsync, optThrows, optFuncResult) => {
				for {
					newParamList <- uimap(paramList)(maybeRewriteParameter _)
					newOptFuncResult <- maybeRewriteOptionalThing(optFuncResult)(maybeRewriteFunctionResult _)
				} yield ThrowsFunctionSig(newParamList, optAsync, optThrows, newOptFuncResult)
			}
			case RethrowsFunctionSig(paramList, optAsync, optFuncResult) => {
				for {
					newParamList <- uimap(paramList)(maybeRewriteParameter _)
					newOptFuncResult <- maybeRewriteOptionalThing(optFuncResult)(maybeRewriteFunctionResult _)
				} yield RethrowsFunctionSig(newParamList, optAsync, newOptFuncResult)
			}
		}
	}
	
	def maybeRewriteRequirement(req: Requirement): UIterator[Unit, Requirement] = {
		req match {
			case ConformanceRequirementDoubleTypeID(type1, type2) => {
				for {
					newType1 <- maybeRewriteType(type1)
					newType2 <- maybeRewriteType(type2)
				} yield ConformanceRequirementDoubleTypeID(newType1, newType2)
			}
			case ConformanceRequirementTypeIDProtocolCompType(typeID, protocolCompType) => {
				for {
					newTypeID <- maybeRewriteType(typeID)
					newProtocolCompType <- maybeRewriteType(protocolCompType)
				} yield ConformanceRequirementTypeIDProtocolCompType(newTypeID, newProtocolCompType)
			}
			case SameTypeRequirement(type1, type2) => {
				for {
					newType1 <- maybeRewriteType(type1)
					newType2 <-  maybeRewriteType(type2)
				} yield SameTypeRequirement(newType1, newType2)
			}
		}
	}
	
	def maybeRewriteTypeInheritance(inheritance: TypeInheritance): UIterator[Unit, TypeInheritance] = {
		inheritance match {
			case TypeInheritance(optAttributeList, typeID) => {
				for {
					newTypeID <- maybeRewriteType(typeID)
				} yield TypeInheritance(optAttributeList, newTypeID)
			}
		}
	}
	
	def maybeRewriteUnionStyleEnumCase(usCase: UnionStyleEnumCase): UIterator[Unit, UnionStyleEnumCase] = {
		usCase match {
			case UnionStyleEnumCase(enumCaseName, optType) => {
				for {
					newOptType <- maybeRewriteOptionalThing(optType)(maybeRewriteType _)
				} yield UnionStyleEnumCase(enumCaseName, newOptType)
			}
		}
	}
	
	def maybeRewriteUnionStyleEnumMember(member: UnionStyleEnumMember): UIterator[Unit, UnionStyleEnumMember] = {
		member match {
			case DeclarationUSEnumMember(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield DeclarationUSEnumMember(newDecl)
			}
			case EnumCaseClauseUSEnumMember(optAttributeList, optMod, usEnumCaseList) => {
				for {
					newUSEnumCaseList <- uimap(usEnumCaseList)(maybeRewriteUnionStyleEnumCase _)
				} yield EnumCaseClauseUSEnumMember(optAttributeList, optMod, newUSEnumCaseList)
			}
			case CompilerControlEnumMember(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield CompilerControlEnumMember(newStmt)
			}
		}
	}
	
	def maybeRewriteRawValueStyleEnumCase(rvsCase: RawValueStyleEnumCase): UIterator[Unit, RawValueStyleEnumCase] = {
		rvsCase match {
			case RawValueStyleEnumCase(enumCaseName, optRawValue) => singleton(RawValueStyleEnumCase(enumCaseName, optRawValue))
		}
	}
	
	def maybeRewriteRawValueStyleEnumMember(member: RawValueStyleEnumMember): UIterator[Unit, RawValueStyleEnumMember] = {
		member match {
			case DeclarationRVSEnumMember(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield DeclarationRVSEnumMember(newDecl)
			}
			case EnumCaseClauseRVSEnumMember(optAttributeList, rvsEnumCaseList) => {
				for {
					newRVSEnumCaseList <- uimap(rvsEnumCaseList)(maybeRewriteRawValueStyleEnumCase _)
				} yield EnumCaseClauseRVSEnumMember(optAttributeList, newRVSEnumCaseList)
			}
			case CompilerControlRVSEnumMember(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield CompilerControlRVSEnumMember(newStmt)
			}
		}
	}
	
	def maybeRewriteStructMember(member: StructMember): UIterator[Unit, StructMember] = {
		member match {
			case DeclarationStructMember(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield DeclarationStructMember(newDecl)
			}
			case CompilerControlStructMember(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield CompilerControlStructMember(newStmt)
			}
		}
	}
	
	def maybeRewriteClassMember(member: ClassMember): UIterator[Unit, ClassMember] = {
		member match {
			case DeclarationClassMember(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield DeclarationClassMember(newDecl)
			}
			case CompilerControlClassMember(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield CompilerControlClassMember(newStmt)
			}
		}
	}
	
	def maybeRewriteActorMember(member: ActorMember): UIterator[Unit, ActorMember] = {
		member match {
			case DeclarationActorMember(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield DeclarationActorMember(newDecl)
			}
			case CompilerControlActorMember(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield CompilerControlActorMember(newStmt)
			}
		}
	}
	
	def maybeRewriteSubscriptHead(head: SubscriptHead): UIterator[Unit, SubscriptHead] = {
		head match {
			case SubscriptHead(optAttributeList, optMods, optGenericParamList, paramList) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newParamList <- uimap(paramList)(maybeRewriteParameter _)
				} yield SubscriptHead(optAttributeList, optMods, newOptGenericParamList, newParamList)
			}
		}
	}
	
	def maybeRewriteSubscriptResult(result: SubscriptResult): UIterator[Unit, SubscriptResult] = {
		result match {
			case SubscriptResult(optAttributeList, typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield SubscriptResult(optAttributeList, newType)
			}
		}
	}
	
	def maybeRewriteAllowableSubscriptBlock(block: AllowableSubscriptBlock): UIterator[Unit, AllowableSubscriptBlock] = {
		block match {
			case AllowableGetterSetterBlock(getterSetterBlock) => {
				for {
					newGetterSetterBlock <- maybeRewriteGetterSetterBlock(getterSetterBlock)
				} yield AllowableGetterSetterBlock(newGetterSetterBlock)
			}
			case AllowableKeywordBlock(keywordBlock) => {
				for {
					newKeywordBlock <- maybeRewriteKeywordBlock(keywordBlock)
				} yield AllowableKeywordBlock(newKeywordBlock)
			}
		}
	}
	
	def maybeRewriteProtocolMember(member: ProtocolMember): UIterator[Unit, ProtocolMember] = {
		member match {
			case ProtocolPropertyDeclaration(optAttributeList, optMods, varName, typeAnno, keywordBlock) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
					newKeywordBlock <- maybeRewriteKeywordBlock(keywordBlock)
				} yield ProtocolPropertyDeclaration(optAttributeList, optMods, varName, newTypeAnno, newKeywordBlock)
			}
			case ProtocolMethodDeclaration(funcHead, funcName, optGenericParamList, funcSig, optGenericWhereClause) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newFuncSig <- maybeRewriteFunctionSignature(funcSig)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
				} yield ProtocolMethodDeclaration(funcHead, funcName, newOptGenericParamList, newFuncSig, newOptGenericWhereClause)
			}
			case ThrowsProtocolInitializerDeclaration(initHead, optGenericParamList, paramList, optMod, optGenericWhereClause) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newParamList <- uimap(paramList)(maybeRewriteParameter _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
				} yield ThrowsProtocolInitializerDeclaration(initHead, newOptGenericParamList, newParamList, optMod, newOptGenericWhereClause)
			}
			case RethrowsProtocolInitializerDeclaration(initHead, optGenericParamList, paramList, optGenericWhereClause) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newParamList <- uimap(paramList)(maybeRewriteParameter _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
				} yield RethrowsProtocolInitializerDeclaration(initHead, newOptGenericParamList, newParamList, newOptGenericWhereClause)
			}
			case ProtocolSubscriptDeclaration(subscriptHead, subscriptResult, optGenericWhereClause, keywordBlock) => {
				for {
					newSubscriptHead <- maybeRewriteSubscriptHead(subscriptHead)
					newSubscriptResult <- maybeRewriteSubscriptResult(subscriptResult)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newKeywordBlock <- maybeRewriteKeywordBlock(keywordBlock)
				} yield ProtocolSubscriptDeclaration(newSubscriptHead, newSubscriptResult, newOptGenericWhereClause, newKeywordBlock)
			}
			case ProtocolAssociatedTypeDeclaration(optAttributeList, optAccessMod, typeAliasName, optTypeInheritanceList, optType, optGenericWhereClause) => {
				for {
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance _)
					newOptType <- maybeRewriteOptionalThing(optType)(maybeRewriteType _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
				} yield ProtocolAssociatedTypeDeclaration(optAttributeList, optAccessMod, typeAliasName, newOptTypeInheritanceList, newOptType, newOptGenericWhereClause)
			}
			case ProtocolTypeAliasDeclaration(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield ProtocolTypeAliasDeclaration(newDecl)
			}
			case CompilerControlProtocolMember(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield CompilerControlProtocolMember(newStmt)
			}
		}
	}
	
	def maybeRewriteExtensionMember(member: ExtensionMember): UIterator[Unit, ExtensionMember] = {
		member match {
			case DeclarationExtensionMember(decl) => {
				for {
					newDecl <- maybeRewriteDeclaration(decl)
				} yield DeclarationExtensionMember(newDecl)
			}
			case CompilerControlExtensionMember(stmt) => {
				for {
					newStmt <- maybeRewriteStmt(stmt)
				} yield CompilerControlExtensionMember(newStmt)
			}
		}
	}
	
	def maybeRewritePrecedenceGroupAttribute(attribute: PrecedenceGroupAttribute): UIterator[Unit, PrecedenceGroupAttribute] = {
		attribute match {
			case PrecedenceGroupRelationHigher(precedenceGroupNameList) => singleton(PrecedenceGroupRelationHigher(precedenceGroupNameList))
			case PrecedenceGroupRelationLower(precedenceGroupNameList) => singleton(PrecedenceGroupRelationLower(precedenceGroupNameList))
			case PrecedenceGroupAssignmentTrue => singleton(PrecedenceGroupAssignmentTrue)
			case PrecedenceGroupAssignmentFalse => singleton(PrecedenceGroupAssignmentFalse)
			case PrecedenceGroupLeftAssociative => singleton(PrecedenceGroupLeftAssociative)
			case PrecedenceGroupRightAssociative => singleton(PrecedenceGroupRightAssociative)
			case PrecedenceGroupNotAssociative => singleton(PrecedenceGroupNotAssociative)
		}
	}
	
	def maybeRewriteDeclaration(decl: Declaration): UIterator[Unit, Declaration] = {
		decl match {
			case ImportDeclaration(optAttributeList, optImportKind, path) => {
				for {
					newPath <- maybeRewriteImportPath(path)
				} yield ImportDeclaration(optAttributeList, optImportKind, newPath)
			}
			case ConstantDeclaration(optAttributeList, optModList, patternInitializerList) => {
				for {
					newPatternInitializerList <- uimap(patternInitializerList)(maybeRewritePatternInitializer _)
				} yield ConstantDeclaration(optAttributeList, optModList, newPatternInitializerList)
			}
			case VariableDeclaration1(optAttributeList, optModList, patternInitializerList) => {
				for {
					newPatternInitializerList <- uimap(patternInitializerList)(maybeRewritePatternInitializer _)
				} yield VariableDeclaration1(optAttributeList, optModList, newPatternInitializerList)
			}
			case VariableDeclaration23(optAttributeList, optModList, varName, typeAnno, getterSetterBlock) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
					newGetterSetterBlock <- maybeRewriteGetterSetterBlock(getterSetterBlock)
				} yield VariableDeclaration23(optAttributeList, optModList, varName, newTypeAnno, newGetterSetterBlock)
			}
			case VariableDeclaration4(optAttributeList, optModList, varName, typeAnno, keywordBlock) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
					newKeywordBlock <- maybeRewriteKeywordBlock(keywordBlock)
				} yield VariableDeclaration4(optAttributeList, optModList, varName, newTypeAnno, newKeywordBlock)
			}
			case VariableDeclaration5(optAttributeList, optModList, varName, initExp, setBlock) => {
				for {
					newInitExp <- maybeRewriteExp(initExp)
					newSetBlock <- maybeRewriteSetBlock(setBlock)
				} yield VariableDeclaration5(optAttributeList, optModList, varName, newInitExp, newSetBlock)
			}
			case VariableDeclaration6(optAttributeList, optModList, varName, typeAnno, optInitExp, setBlock) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
					newOptInitExp <- maybeRewriteOptionalThing(optInitExp)(maybeRewriteExp _)
					newSetBlock <- maybeRewriteSetBlock(setBlock)
				} yield VariableDeclaration6(optAttributeList, optModList, varName, newTypeAnno, newOptInitExp, newSetBlock)
			}
			case TypeAliasDeclaration(optAttributeList, optMod, typeAliasName, optGenericParamList, typ) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newType <- maybeRewriteType(typ)
				} yield TypeAliasDeclaration(optAttributeList, optMod, typeAliasName, newOptGenericParamList, newType)
			}
			case FunctionDeclaration(funcHead, funcName, optGenericParamList, funcSig, optGenericWhereClause, optCodeBlock) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newFuncSig <- maybeRewriteFunctionSignature(funcSig)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newOptCodeBlock <- maybeRewriteOptionalThing(optCodeBlock)(maybeRewriteCodeBlock _)
				} yield FunctionDeclaration(funcHead, funcName, newOptGenericParamList, newFuncSig, newOptGenericWhereClause, newOptCodeBlock)
			}
			case UnionStyleEnumDeclaration(optAttributeList, optAccessMod, optIndirectMod, enumName, optGenericParamList, optTypeInheritanceList, optGenericWhereClause, optUSEnumMemberList) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newOptUSEnumMemberList <- combined(optUSEnumMemberList)(maybeRewriteUnionStyleEnumMember _)
				} yield UnionStyleEnumDeclaration(optAttributeList, optAccessMod, optIndirectMod, enumName, newOptGenericParamList, newOptTypeInheritanceList, newOptGenericWhereClause, newOptUSEnumMemberList)
			}
			case RawValueStyleEnumDeclaration(optAttributeList, optAccessMod, enumName, optGenericParamList, typeInheritanceList, optGenericWhereClause, rvsEnumMemberList) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newTypeInheritanceList <- uimap(typeInheritanceList)(maybeRewriteTypeInheritance _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newRVSEnumMemberList <- uimap(rvsEnumMemberList)(maybeRewriteRawValueStyleEnumMember _)
				} yield RawValueStyleEnumDeclaration(optAttributeList, optAccessMod, enumName, newOptGenericParamList, newTypeInheritanceList, newOptGenericWhereClause, newRVSEnumMemberList) 
			}
			case StructDeclaration(optAttributeList, optAccessMod, structname, optGenericParamList, optTypeInheritanceList, optGenericWhereClause, structMemberList) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newStructMemberList <- uimap(structMemberList)(maybeRewriteStructMember _)
				} yield StructDeclaration(optAttributeList, optAccessMod, structname, newOptGenericParamList, newOptTypeInheritanceList, newOptGenericWhereClause, newStructMemberList)
			}
			case RegClassDeclaration(optAttributeList, optAccessMod, optFinalMod, classname, optGenericParamList, optTypeInheritanceList, optGenericWhereClause, classMemberList) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newClassMemberList <- uimap(classMemberList)(maybeRewriteClassMember _)
				} yield RegClassDeclaration(optAttributeList, optAccessMod, optFinalMod, classname, newOptGenericParamList, newOptTypeInheritanceList, newOptGenericWhereClause, newClassMemberList)
			}
			case ForcedFinalClassDeclaration(optAttributeList, optAccessMod, classname, optGenericParamList, optTypeInheritanceList, optGenericWhereClause, classMemberList) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newClassMemberList <- uimap(classMemberList)(maybeRewriteClassMember _)
				} yield ForcedFinalClassDeclaration(optAttributeList, optAccessMod, classname, newOptGenericParamList, newOptTypeInheritanceList, newOptGenericWhereClause, newClassMemberList)
			}
			case ActorDeclaration(optAttributeList, optAccessMod, actorName, optGenericParamList, optTypeInheritanceList, optGenericWhereClause, actorMemberList) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newActorMemberList <- uimap(actorMemberList)(maybeRewriteActorMember _)
				} yield ActorDeclaration(optAttributeList, optAccessMod, actorName, newOptGenericParamList, newOptTypeInheritanceList, newOptGenericWhereClause, newActorMemberList)
			}
			case ProtocolDeclaration(optAttributeList, optAccessMod, protocolName, optTypeInheritanceList, optGenericWhereClause, protocolMemberList) => {
				for {
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newProtocolMemberList <- uimap(protocolMemberList)(maybeRewriteProtocolMember _)
				} yield ProtocolDeclaration(optAttributeList, optAccessMod, protocolName, newOptTypeInheritanceList, newOptGenericWhereClause, newProtocolMemberList)
			}
			case ThrowsInitializerDeclaration(initHead, optGenericParamList, paramList, optAsync, optThrows, optGenericWhereClause, codeBlock) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newParamList <- uimap(paramList)(maybeRewriteParameter _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield ThrowsInitializerDeclaration(initHead, newOptGenericParamList, newParamList, optAsync, optThrows, newOptGenericWhereClause, newCodeBlock)
			}
			case RethrowsInitializerDeclaration(initHead, optGenericParamList, paramList, optAsync, optGenericWhereClause, codeBlock) => {
				for {
					newOptGenericParamList <- combined(optGenericParamList)(maybeRewriteGenericParam _)
					newParamList <- uimap(paramList)(maybeRewriteParameter _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield RethrowsInitializerDeclaration(initHead, newOptGenericParamList, newParamList, optAsync, newOptGenericWhereClause, newCodeBlock)
			}
			case DeinitializerDeclaration(optAttributeList, codeBlock) => {
				for {
					newCodeBlock <- maybeRewriteCodeBlock(codeBlock)
				} yield DeinitializerDeclaration(optAttributeList, newCodeBlock)
			}
			case ExtensionDeclaration(optAttributeList, optAccessMod, typ, optTypeInheritanceList, optGenericWhereClause, extensionMemberList) => {
				for {
					newType <- maybeRewriteType(typ)
					newOptTypeInheritanceList <- combined(optTypeInheritanceList)(maybeRewriteTypeInheritance _)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newExtensionMemberList <- uimap(extensionMemberList)(maybeRewriteExtensionMember _)
				} yield ExtensionDeclaration(optAttributeList, optAccessMod, newType, newOptTypeInheritanceList, newOptGenericWhereClause, newExtensionMemberList)
			}
			case SubscriptDeclaration(subscriptHead, subscriptResult, optGenericWhereClause, allowableSubscriptBlock) => {
				for {
					newSubscriptHead <- maybeRewriteSubscriptHead(subscriptHead)
					newSubscriptResult <- maybeRewriteSubscriptResult(subscriptResult)
					newOptGenericWhereClause <- combined(optGenericWhereClause)(maybeRewriteRequirement _)
					newAllowableSubscriptBlock <- maybeRewriteAllowableSubscriptBlock(allowableSubscriptBlock)
				} yield SubscriptDeclaration(newSubscriptHead, newSubscriptResult, newOptGenericWhereClause, newAllowableSubscriptBlock)
			}
			case prefixOp: PrefixOperatorDeclaration => singleton(prefixOp)
			case postfixOp: PostfixOperatorDeclaration => singleton(postfixOp)
			case InfixOperatorDeclaration(op, optPrecedenceGroupName) => singleton(InfixOperatorDeclaration(op, optPrecedenceGroupName))
			case PrecedenceGroupDeclaration(precedenceGroupName, optPrecedenceGroupAttributeList) => {
				for {
					newOptPrecedenceGroupAttributeList <- combined(optPrecedenceGroupAttributeList)(maybeRewritePrecedenceGroupAttribute _)
				} yield PrecedenceGroupDeclaration(precedenceGroupName, newOptPrecedenceGroupAttributeList)
			}
		}
	}

	def maybeRewriteTuplePatternElement(element: TuplePatternElement): UIterator[Unit, TuplePatternElement] = {
		element match {
			case PatternElement(pattern) => {
				for {
					newPattern <-  maybeRewritePattern(pattern)
				} yield PatternElement(newPattern)
			}
			case IdentifierPatternElement(patternName, pattern) => {
				for {
					newPattern <-  maybeRewritePattern(pattern)
				} yield IdentifierPatternElement(patternName, newPattern)
			}
		}
	}

	def maybeRewritePattern(pattern: Pattern): UIterator[Unit, Pattern] = {
		pattern match {
			case WildcardPattern(optTypeAnno) => {
				for {
					newOptTypeAnno <- maybeRewriteOptionalThing(optTypeAnno)(maybeRewriteTypeAnnotation _)
				} yield WildcardPattern(newOptTypeAnno)
			}
			case IdentifierPattern(patternName, optTypeAnno) => {
				for {
					newOptTypeAnno <- maybeRewriteOptionalThing(optTypeAnno)(maybeRewriteTypeAnnotation _)
				} yield IdentifierPattern(patternName, newOptTypeAnno)
			}
			case ValueBindingPattern(mod, pattern) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
				} yield ValueBindingPattern(mod, newPattern)
			}
			case TuplePattern(tuplePatternElementList, optTypeAnno) => {
				for {
					newTuplePatternElementList <- uimap(tuplePatternElementList)(maybeRewriteTuplePatternElement _)
					newOptTypeAnno <- maybeRewriteOptionalThing(optTypeAnno)(maybeRewriteTypeAnnotation _)
				} yield TuplePattern(newTuplePatternElementList, newOptTypeAnno)
			}
			case EnumCasePattern(optTypeIdentifier, enumCaseName, optPattern) => {
				for {
					newOptTypeIdentifier <- maybeRewriteOptionalThing(optTypeIdentifier)(maybeRewriteType _)
					newOptPattern <- maybeRewriteOptionalThing(optPattern)(maybeRewritePattern _)
				} yield EnumCasePattern(newOptTypeIdentifier, enumCaseName, newOptPattern)
			}
			case OptionalPattern(pattern) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
				} yield OptionalPattern(newPattern)
			}
			case ExpressionPattern(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ExpressionPattern(newExp)
			}
			case IsTypeCastingPattern(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield IsTypeCastingPattern(newType)
			}
			case AsTypeCastingPattern(pattern, typ) => {
				for {
					newPattern <- maybeRewritePattern(pattern)
					newType <- maybeRewriteType(typ)
				} yield AsTypeCastingPattern(newPattern, newType)
			}
		}
	}

	def maybeRewriteFunctionTypeArg(arg: FunctionTypeArg): UIterator[Unit, FunctionTypeArg] = {
		arg match {
			case FunctionTypeArg1(optAttributeList, optInOutMod, typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield FunctionTypeArg1(optAttributeList, optInOutMod, newType)
			}
			case FunctionTypeArg2(argumentLabel, typeAnno) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
				} yield FunctionTypeArg2(argumentLabel, newTypeAnno)
			}
		}
	}

	def maybeRewriteTupleTypeElement(element: TupleTypeElement): UIterator[Unit, TupleTypeElement] = {
		element match {
			case TupleTypeElementNameAnnotation(elementName, typeAnno) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
				} yield TupleTypeElementNameAnnotation(elementName, newTypeAnno)
			}
			case TupleTypeElementType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield TupleTypeElementType(newType)
			}
		}
	}

	def maybeRewriteType(theTyp: Type): UIterator[Unit, Type] = {
		theTyp match {
			case FunctionType(optAttributeList, functionTypeArgList, optAsync, optThrows, typ) => {
				for {
					newFunctionTypeArgList <- uimap(functionTypeArgList)(maybeRewriteFunctionTypeArg _)
					newType <- maybeRewriteType(typ)
				} yield FunctionType(optAttributeList, newFunctionTypeArgList, optAsync, optThrows, newType)
			}
			case ArrayType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield ArrayType(newType)
			}
			case DictionaryType(typ1, typ2) => {
				for {
					newType1 <- maybeRewriteType(typ1)
					newType2 <- maybeRewriteType(typ2)
				} yield DictionaryType(newType1, newType2)
			}
			case NormalTypeIdentifier(typeName) => singleton(NormalTypeIdentifier(typeName))
			case GenericTypeIdentifier(typeName, typeList) => {
				for {
					newTypeList <- uimap(typeList)(maybeRewriteType _)
				} yield GenericTypeIdentifier(typeName, newTypeList)
			}
			case NestedNormalTypeIdentifier(typeName, nestedType) => {
				for {
					newType <- maybeRewriteType(nestedType)
				} yield NestedNormalTypeIdentifier(typeName, newType)
			}
			case NestedGenericTypeIdentifier(typeName, typeList, typ) => {
				for {
					newTypeList <- uimap(typeList)(maybeRewriteType _)
					newType <- maybeRewriteType(typ)
				} yield NestedGenericTypeIdentifier(typeName, newTypeList, newType)
			}
			case TupleType(tupleTypeElementList) => {
				for {
					newTTEList <- uimap(tupleTypeElementList)(maybeRewriteTupleTypeElement _)
				} yield TupleType(newTTEList)
			}
			case ProtocolCompositionType(typeList) => {
				for {
					newTypeList <- uimap(typeList)(maybeRewriteType _)
				} yield ProtocolCompositionType(newTypeList)
			}
			case OpaqueType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield OpaqueType(newType)
			}
			case AnyType => singleton(AnyType)
			case SelfType => singleton(SelfType)
			case InParensType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield InParensType(newType)
			}
			case OptionalType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield OptionalType(newType)
			}
			case ImplicitlyUnwrappedOptionalType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield ImplicitlyUnwrappedOptionalType(newType)
			}
			case MetatypeTypeType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield MetatypeTypeType(newType)
			}
			case MetatypeProtocolType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield MetatypeProtocolType(newType)
			}
			case BoxedProtocolType(typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield BoxedProtocolType(newType)
			}
		}
	}

	def maybeRewriteTupleElement(tupleElement: TupleElement): UIterator[Unit, TupleElement] = {
		tupleElement match {
			case ExpTuple(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ExpTuple(newExp)
			}
			case IdentifierColonExpTuple(tupleElementName, exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield IdentifierColonExpTuple(tupleElementName, newExp)
			}
		}
	}

	def maybeRewriteLabeledTrailingClosure(closure: LabeledTrailingClosure): UIterator[Unit, LabeledTrailingClosure] = {
		closure match {
			case LabeledTrailingClosure(label, exp) => {
				for {
					newClosureExp <- maybeRewriteExp(exp)
				} yield LabeledTrailingClosure(label, newClosureExp)
			}
		}
	}

	def maybeRewriteTrailingClosure(closure: TrailingClosure): UIterator[Unit, TrailingClosure] = {
		closure match {
			case TrailingClosure(exp, list) => {
				for {
					newClosureExp <- maybeRewriteExp(closure.closureExp)
					newLabeledTrailingClosureList <- combined(closure.list)(maybeRewriteLabeledTrailingClosure _)
				} yield TrailingClosure(newClosureExp, newLabeledTrailingClosureList)
			}
		}
	}

	def maybeRewritePostfixFunctionCall(call: PostfixFunctionCall): UIterator[Unit, PostfixFunctionCall] = {
		call match {
			case SimpleFunctionCall(functionCallArgList) => {
				for {
					newList <- uimap(functionCallArgList)(maybeRewriteFunctionCallArg _)
				} yield SimpleFunctionCall(newList.toList)
			}
			case ComplexFunctionCall(optList, trailing) => {
				for {
					newOpList <- combined(optList)(maybeRewriteFunctionCallArg _)
					newTrailing <- maybeRewriteTrailingClosure(trailing)
				} yield ComplexFunctionCall(newOpList, newTrailing)
			}
		}
	}

	def maybeRewriteFunctionCallArg(arg: FunctionCallArgument): UIterator[Unit, FunctionCallArgument] = {
		arg match {
			case ExpFunctionCallArgument(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ExpFunctionCallArgument(newExp)
			}
			case IdentifierColonExpFunctionCallArgument(functionCallArgName, exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield IdentifierColonExpFunctionCallArgument(functionCallArgName, newExp)
			}
			case op: OperatorFunctionCallArgument => singleton(op)
			case IdentifierColonOperatorFunctionCallArgument(functionCallArgName, op) => singleton(IdentifierColonOperatorFunctionCallArgument(functionCallArgName, op))
		}
	}
	
	def maybeRewriteExplicitMember(member: ExplicitMember): UIterator[Unit, ExplicitMember] = {
		member match {
			case ExplicitMemberDecimalDigits(num) => singleton(ExplicitMemberDecimalDigits(num))
			case ExplicitMemberIdentifierOptGeneric(explicitMemberName, optTypeList) => {
				for {
					newOptTypeList <- combined(optTypeList)(maybeRewriteType _)
				} yield ExplicitMemberIdentifierOptGeneric(explicitMemberName, newOptTypeList)
			}
			case ExplicitMemberIdentifierArgs(explicitMemberName, argumentNameList) => singleton(ExplicitMemberIdentifierArgs(explicitMemberName, argumentNameList))
		} 
	}

	def maybeRewriteCaptureListItem(item: CaptureListItem): UIterator[Unit, CaptureListItem] = {
		item match {
			case CaptureListItemIdentifier(optSpecifier, captureListItemName) => singleton(CaptureListItemIdentifier(optSpecifier, captureListItemName))
			case CaptureListItemAssignment(optSpecifier, captureListItemName, exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield CaptureListItemAssignment(optSpecifier, captureListItemName, newExp)
			}
			case CaptureListItemSelf(optSpecifier, selfExp) => {
				for {
					newSelfExp <- maybeRewriteExp(selfExp)
				} yield CaptureListItemSelf(optSpecifier, newSelfExp)
			}
		}
	}

	def maybeRewriteTypeAnnotation(typeAnno: TypeAnnotation): UIterator[Unit, TypeAnnotation] = {
		typeAnno match {
			case TypeAnnotation(optAttributeList, inoutMod, typ) => {
				for {
					newType <- maybeRewriteType(typ)
				} yield TypeAnnotation(optAttributeList, inoutMod, newType)
			}
		}
	}

	def maybeRewriteClosureParameter(param: ClosureParameter): UIterator[Unit, ClosureParameter] = {
		param match {
			case ClosureParameterReg(closureParamName, optTypeAnno) => {
				for {
					newOptTypeAnno <- maybeRewriteOptionalThing(optTypeAnno)(maybeRewriteTypeAnnotation _)
				} yield ClosureParameterReg(closureParamName, newOptTypeAnno)
			}
			case ClosureParameterElipses(closureParamName, typeAnno) => {
				for {
					newTypeAnno <- maybeRewriteTypeAnnotation(typeAnno)
				} yield ClosureParameterElipses(closureParamName, newTypeAnno)
			}
		}
	}

	def maybeRewriteClosureParameterClause(clause: ClosureParameterClause): UIterator[Unit, ClosureParameterClause] = {
		clause match {
			case CPCIdentifierList(unknownIdentifierList) => { singleton(CPCIdentifierList(unknownIdentifierList)) }
			case CPCClosureParameterList(closureParamList) => {
				for {
					newClosureParamList <- uimap(closureParamList)(maybeRewriteClosureParameter _)
				} yield CPCClosureParameterList(newClosureParamList)
			}
		}
	}

	def maybeRewriteFunctionResult(funcResult: FunctionResult): UIterator[Unit, FunctionResult] = {
		funcResult match {
			case FunctionResult(optAttributeList, typ) => {
				for {
					newType <-  maybeRewriteType(typ)
				} yield FunctionResult(optAttributeList, newType)
			}
		}
	}

	def maybeRewriteClosureSignature(sig: ClosureSignature): UIterator[Unit, ClosureSignature] = {
		sig match {
			case ClosureSignatureComplex(optListCaptureItem, clause, optAsync, optThrows, optFunctionResult) => {
				for {
					newOptListCaptureItem <- combined(optListCaptureItem)(maybeRewriteCaptureListItem _)
					newClause <- maybeRewriteClosureParameterClause(clause)
					newOptFunctionResult <- maybeRewriteOptionalThing(optFunctionResult)(maybeRewriteFunctionResult _)
				} yield ClosureSignatureComplex(newOptListCaptureItem, newClause, optAsync, optThrows, newOptFunctionResult)
			}
			case ClosureSignatureSimple(captureListItemList) => {
				for {
					newCaptureListItemList <- uimap(captureListItemList)(maybeRewriteCaptureListItem _)
				} yield ClosureSignatureSimple(newCaptureListItemList)
			}
		}
	}

	def maybeRewriteKeyPathPostfix(postfix: KeyPathPostfix): UIterator[Unit, KeyPathPostfix] = {
		postfix match {
			case FuncCallArgListKPP(functionCallArgList) => {
				for {
					newFunctionCallArgList <- uimap(functionCallArgList)(maybeRewriteFunctionCallArg _)
				} yield FuncCallArgListKPP(newFunctionCallArgList)
			}
			case _ => singleton(postfix)
		}
	}

	def maybeRewriteKeyPathComponent(component: KeyPathComponent): UIterator[Unit, KeyPathComponent] = {
		component match {
			case IdentifierThenOptPostfixesKPC(keyPathComponentName, optPostfixesList) => {
				for {
					newOptPostfixesList <- combined(optPostfixesList)(maybeRewriteKeyPathPostfix _)
				} yield IdentifierThenOptPostfixesKPC(keyPathComponentName, newOptPostfixesList)
			}
			case PostfixesKPC(postfixList) => {
				for {
					newPostfixList <- uimap(postfixList)(maybeRewriteKeyPathPostfix _)
				} yield PostfixesKPC(newPostfixList)
			}
		}
	}

	def maybeRewriteOptionalThing[A](thing: Option[A])(func: A => UIterator[Unit, A]): UIterator[Unit, Option[A]] = {
		thing match {
			case Some(theThing) => {
				for {
					newTheThing <- func(theThing)
				} yield Some(newTheThing)
			}
			case None => singleton(None)
		}
	}
	
	def isAssignmentOp(op: Operator): Boolean = {
		op match {
			case Operator("=") => true
			case Operator("*=") => true
			case Operator("/=") => true
			case Operator("%=") => true
			case Operator("+=") => true
			case Operator("-=") => true
			case Operator("<<=") => true
			case Operator(">>=") => true
			case Operator("&=") => true
			case Operator("|=") => true
			case Operator("^=") => true
			case Operator("&*=") => true
			case Operator("&+=") => true
			case Operator("&-=") => true
			case Operator("&<<=") => true
			case Operator("&>>=") => true
			case Operator(".&=") => true
			case Operator(".^=") => true
			case _ => false
		}
	}

	def dontRewriteDirect(expression: Exp): UIterator[Unit, Exp] = {
		val retval: UIterator[Unit, Exp] = expression match {
			case TryExp(modifier, exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield TryExp(modifier, newExp)
			}
			case AwaitExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield AwaitExp(newExp)
			}
			case PrefixExp(op, exp) => {
				//for {
					//currently not doing this becuase of swift's weird unary rules.(have to directly prepend to literal)
					//newExp <- maybeRewriteExp(exp)
				//} yield PrefixExp(op, newExp)
				singleton(PrefixExp(op, exp))
			}
			case PostfixWithOpExp(exp, op) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield PostfixWithOpExp(newExp, op)
			}
			case PostfixForcedValueExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield PostfixForcedValueExp(newExp)
			}
			case PostfixOptionalChainingExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield PostfixOptionalChainingExp(newExp)
			}
			case PostfixFunctionCallExp(exp, postfixFunctionCall) => {
				for {
					newExp <- maybeRewriteExp(exp)
					newPostfixFunctionCall <- maybeRewritePostfixFunctionCall(postfixFunctionCall)
				} yield PostfixFunctionCallExp(newExp, newPostfixFunctionCall)
			}
			case PostfixSubscriptExp(exp, functionCallArgList) => {
				for {
					newExp <- maybeRewriteExp(exp)
					newList <- uimap(functionCallArgList)(maybeRewriteFunctionCallArg _)
				} yield PostfixSubscriptExp(newExp, newList)
			}
			case PostfixInitializerExp(exp, argumentNameList) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield PostfixInitializerExp(newExp, argumentNameList)
			}
			case PostfixSelfExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield PostfixSelfExp(newExp)
			}
			case PostfixExplicitMemberExp(exp, explicitMember) => {
				for {
					newExp <- maybeRewriteExp(exp)
					newExplicitMember <- maybeRewriteExplicitMember(explicitMember)
				} yield PostfixExplicitMemberExp(newExp, newExplicitMember)
			}
			case InOutExp(exp) => { singleton(InOutExp(exp)) //not safe to do this bcuz for example if it's and exp w/ type int we cannot replace it with a literal
				// for {
				// 	newExp <- maybeRewriteExp(exp)
				// } yield InOutExp(newExp)
			}
			case CastExp(exp, op) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield CastExp(newExp, op)
			}
			case AssignmentExp(prefixExp, exp) => {
				for {
					//newPrefixExp <- maybeRewriteExp(prefixExp) //actually cannot simply rewrite this because it could be replaced with a nonmtuable var
					newExp <- maybeRewriteExp(exp)
				} yield AssignmentExp(prefixExp, newExp)
			}
			case ConditionalExp(prefixExp, conditionalExp, exp) => {
				for {
					newPrefixExp <- maybeRewriteExp(prefixExp)
					newConditionalExp <- maybeRewriteExp(conditionalExp)
					newExp <- maybeRewriteExp(exp)
				} yield ConditionalExp(newPrefixExp, newConditionalExp, newExp)
			}
			case TrueInfixExp(exp1, op, exp2) => { isAssignmentOp(op) match {
				case true => {
					for {
						newExp2 <- maybeRewriteExp(exp2)
					} yield TrueInfixExp(exp1, op, newExp2)
				}
				case false => {
					for {
						newExp1 <- maybeRewriteExp(exp1)
						newExp2 <- maybeRewriteExp(exp2)
					} yield TrueInfixExp(newExp1, op, newExp2)
				}
			}}
			case GenericExp(genericName, typeList) => {
				for {
					newTypeList <- uimap(typeList)(maybeRewriteType _)
				} yield GenericExp(genericName, newTypeList)
			}
			case idLiteral: VariableExp => singleton(idLiteral)
			case idLiteral: ImplicitParameterExpOrPWP => singleton(idLiteral)
			case idLiteral: PropertyWrapperProjectionExp => singleton(idLiteral)
			case literalExp: DecimalIntegerLiteralExp => singleton(literalExp)
			case literalExp: BinaryIntegerLiteralExp => singleton(literalExp)
			case literalExp: OctalIntegerLiteralExp => singleton(literalExp)
			case literalExp: HexIntegerLiteralExp => singleton(literalExp)
			case literalExp: DecimalFloatLiteralExp => singleton(literalExp)
			case literalExp: HexFloatLiteralExp => singleton(literalExp)
			case literalExp: CharStringExp => singleton(literalExp)
			case literalExp: SingleLineStaticStringLiteralExp => singleton(literalExp)
			case literalExp: MultiLineStaticStringLiteralExp => singleton(literalExp)
			case literalExp: InterpolatedStringLiteralExp => singleton(literalExp)
			case literalExp: TrueLiteralExp => singleton(literalExp)
			case literalExp: FalseLiteralExp => singleton(literalExp)
			case literalExp: NilExp => singleton(literalExp)
			case ArrayLiteralExp(expList) => {
				for {
					newExpList <- uimap(expList)(maybeRewriteExp _)
				} yield ArrayLiteralExp(newExpList)
			}
			case DictionaryLiteralExp(exps) => {
				for {
					result <- uimap(exps)(pair => {
						for {
							exp1 <- maybeRewriteExp(pair._1)
							exp2 <- maybeRewriteExp(pair._2)
							} yield (exp1, exp2)
							})
					} yield DictionaryLiteralExp(result)
			}
			case ColorPlaygroundLiteralExp(exp1, exp2, exp3, exp4) => {
				for {
					newExp1 <- maybeRewriteExp(exp1)
					newExp2 <- maybeRewriteExp(exp2)
					newExp3 <- maybeRewriteExp(exp3)
					newExp4 <- maybeRewriteExp(exp4)
				} yield ColorPlaygroundLiteralExp(newExp1, newExp2, newExp3, newExp4)
			}
			case FilePlaygroundLiteralExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield FilePlaygroundLiteralExp(newExp)
			}
			case ImagePlaygroundLiteralExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ImagePlaygroundLiteralExp(newExp)
			}
/* 			case HashFileExp => singleton(HashFileExp)
			case HashFileIDExp => singleton(HashFileIDExp)
			case HashFilePathExp => singleton(HashFilePathExp)
			case HashLineExp => singleton(HashLineExp)
			case HashColumnExp => singleton(HashColumnExp)
			case HashFunctionExp => singleton(HashFunctionExp)
			case HashDSOHandleExp => singleton(HashDSOHandleExp) */
			case theExp: SoloSelfExp => singleton(theExp)
			case MethodSelfExp(selfMethodName) => singleton(MethodSelfExp(selfMethodName))
			case SubscriptSelfExp(functionCallArgList) => {
				for {
					newList <- uimap(functionCallArgList)(maybeRewriteFunctionCallArg _)
				} yield SubscriptSelfExp(newList)
			}
			case theExp: InitSelfExp => singleton(InitSelfExp())
			case MethodSuperExp(superMethodName) => singleton(MethodSuperExp(superMethodName))
			case SubscriptSuperExp(functionCallArgList) => {
				for {
					newList <- uimap(functionCallArgList)(maybeRewriteFunctionCallArg _)
				} yield SubscriptSuperExp(newList)
			}
			case theExp: InitSuperExp => singleton(theExp)
			case ClosureExp(optAttributeList, optClosureSig, optStmtList) => {
				for {
					newOptClosureSig <- maybeRewriteOptionalThing(optClosureSig)(maybeRewriteClosureSignature _)
					newOptStmtList <- combined(optStmtList)(maybeRewriteStmt _)
				} yield ClosureExp(optAttributeList, newOptClosureSig, newOptStmtList)
			}
			case ParenthesizedExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield ParenthesizedExp(newExp)
			}
			case TupleExp(tupleElementList) => {
				for {
					newList <-uimap(tupleElementList)(maybeRewriteTupleElement _)
				} yield TupleExp(newList)
			}
			case IdentifierImplicitMemberExp(implicitMemberName) => singleton(IdentifierImplicitMemberExp(implicitMemberName))
			case IdentifierDotPostfixImplicitMemberExp(implicitMemberName, postfixExp) => {
				for {
					newPostfixExp <- maybeRewriteExp(postfixExp)
				} yield IdentifierDotPostfixImplicitMemberExp(implicitMemberName, newPostfixExp)
			}
			case theExp: WildcardExp => singleton(theExp)
			case KeyPathExp(optType, keyPathComponentList) => {
				for {
					newOptType <- maybeRewriteOptionalThing(optType)(maybeRewriteType _)
					newKeyPathComponentList <- uimap(keyPathComponentList)(maybeRewriteKeyPathComponent _)
				} yield KeyPathExp(newOptType, newKeyPathComponentList)
			}
			case SelectorExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield SelectorExp(newExp)
			}
			case SelectorGetterExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield SelectorGetterExp(newExp)
			}
			case SelectorSetterExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield SelectorSetterExp(newExp)
			}
			case KeyPathStringExp(exp) => {
				for {
					newExp <- maybeRewriteExp(exp)
				} yield KeyPathStringExp(newExp)
			}
		}
		retval.map(exp => {
			exp.expType = expression.expType
			exp
			})
	}
	
	def maybeRewriteExp(exp: Exp): UIterator[Unit, Exp] = {
		lvlFlag match {
			case "-1" => maybeRewriteExpHelper(exp, genExpLvl1 _)
			case "-2" => maybeRewriteExpHelper(exp, genExpLvl2 _)
			case "-3" => maybeRewriteExpHelper(exp, genExpLvl3 _)
			case _ => throw new GeneratorException("invalid level flag")
		}
	}
	
	def maybeRewriteExpHelper(exp: Exp, genFunc: (Term[UnificationType], Int, TypeEnv, Funcs) => UIterator[Unit, Exp]): UIterator[Unit, Exp] = {
		//println("Maybe rewriting: " + exp)
		//println("With type: " + exp.expType)
		//println("Current env: " + exp.typeEnv)
		//println("----------")
		exp.expType match {
			case None => dontRewriteDirect(exp)
			case Some(theType) => {
				//println("With converted type: " + convertType(theType))
				(exp.typeEnv, exp.functions) match {
					case (Some(env), Some(funcEnv)) => {
						randomizedDisjuncts(
							random,
						//disjuncts(
							dontRewriteDirect(exp),
							genFunc(convertType(theType), 2, env, funcEnv)
						)						
					}
					case (Some(env), None) => {
						randomizedDisjuncts(
							random,
						//disjuncts(
							dontRewriteDirect(exp),
							genFunc(convertType(theType), 2, env, Map())
						)	
					}
					case (None, _) => {
						randomizedDisjuncts(
							random,
						//disjuncts(
							dontRewriteDirect(exp),
							genFunc(convertType(theType), 2, Map(), Map())
						)
					}
				}
			}
		}
	}

	def convertType(typ: Typ): Term[UnificationType] = {
		typ match {
			case IntType => intUnificationType
			case DoubleType => doubleUnificationType
			case BoolType => boolUnificationType
			case StringType => stringUnificationType
			case CharType => charUnificationType
			case VoidType => voidUnificationType
			case OptionalTyp(theTyp) => optionalUnificationType(convertType(theTyp))
			case ArrayTyp(theTyp) => arrayUnificationType(convertType(theTyp))
			case DictionaryTyp(theTyp1, theTyp2) => dictionaryUnificationType(convertType(theTyp1), convertType(theTyp2))
			case OpaqueTyp(theTyp) => opaqueUnificationType(convertType(theTyp))
			case AnyTyp => anyUnificationType
			//case SelfTyp => selfUnificationType
			case ImplicitlyUnwrappedOptionalTyp(theTyp) => implicitlyUnwrappedOptionalUnificationType(convertType(theTyp))
			case MetatypeTypeTyp(theTyp) => metatypeTypeUnificationType(convertType(theTyp))
			case MetatypeProtocolTyp(theTyp) => metatypeProtocolUnificationType(convertType(theTyp))
			case FunctionTyp(typList, theTyp) => functionUnificationType(buildPrologList(typList), convertType(theTyp))
			case NamedType(name) => namedUnificationType(StringTerm(name))
			case _ => throw new GeneratorException("Did not match a type in convertType function")
		}
	}
	
	// trait PrologList[A]
	// def nil[A]: StructureTerm[PrologList[A]] = VariantOf[PrologList[A]]("nil")
	// def cons[A](head: Term[A], tail: Term[PrologList[A]]): StructureTerm[PrologList[A]] = {
	// 	VariantOf[PrologList[A]]("cons", head, tail)
	// }
	
	def buildPrologList(typeList: List[Typ]): Term[PrologList[UnificationType]] = {
		typeList match {
			case Nil => nil[UnificationType]
			case head :: tail => cons[UnificationType](convertType(head), buildPrologList(tail))
		}
	}
	
	//form: operator, leftOperandType, rightOperandType, returnType
	val binops: Seq[(Operator, Term[UnificationType], Term[UnificationType], Term[UnificationType])] =
		Seq (
				(Operator("+"), intUnificationType, intUnificationType, intUnificationType), 	//arith op +
				(Operator("+"), doubleUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("+"), intUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("+"), doubleUnificationType, intUnificationType, doubleUnificationType),
				(Operator("+"), stringUnificationType, stringUnificationType, stringUnificationType), 	//string concat
				(Operator("-"), intUnificationType, intUnificationType, intUnificationType), 	//arith op -
				(Operator("-"), doubleUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("-"), intUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("-"), doubleUnificationType, intUnificationType, doubleUnificationType),
				(Operator("*"), intUnificationType, intUnificationType, intUnificationType), 	//arith op *
				(Operator("*"), doubleUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("*"), intUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("*"), doubleUnificationType, intUnificationType, doubleUnificationType),
				(Operator("/"), intUnificationType, intUnificationType, intUnificationType), 	//arith op /
				(Operator("/"), doubleUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("/"), intUnificationType, doubleUnificationType, doubleUnificationType),
				(Operator("/"), doubleUnificationType, intUnificationType, doubleUnificationType),
				(Operator("%"), intUnificationType, intUnificationType, intUnificationType), 	//remainder op
				(Operator("=="), intUnificationType, intUnificationType, boolUnificationType), 	//exactly equals op
				(Operator("=="), doubleUnificationType, doubleUnificationType, boolUnificationType),
				(Operator("=="), boolUnificationType, boolUnificationType, boolUnificationType),
				(Operator("=="), stringUnificationType, stringUnificationType, boolUnificationType),
				(Operator("!="), intUnificationType, intUnificationType, boolUnificationType), 	//not equals op
				(Operator("!="), doubleUnificationType, doubleUnificationType, boolUnificationType),
				(Operator("!="), boolUnificationType, boolUnificationType, boolUnificationType),
				(Operator("!="), stringUnificationType, stringUnificationType, boolUnificationType),
				(Operator(">"), intUnificationType, intUnificationType, boolUnificationType), 	//comparison op >
				(Operator(">"), doubleUnificationType, doubleUnificationType, boolUnificationType),
				(Operator(">"), intUnificationType, doubleUnificationType, boolUnificationType),
				(Operator(">"), doubleUnificationType, intUnificationType, boolUnificationType),
				(Operator("<"), intUnificationType, intUnificationType, boolUnificationType), 	//comparison op <
				(Operator("<"), doubleUnificationType, doubleUnificationType, boolUnificationType),
				(Operator("<"), intUnificationType, doubleUnificationType, boolUnificationType),
				(Operator("<"), doubleUnificationType, intUnificationType, boolUnificationType),
				(Operator(">="), intUnificationType, intUnificationType, boolUnificationType), 	//comparison op >=
				(Operator(">="), doubleUnificationType, doubleUnificationType, boolUnificationType),
				(Operator(">="), intUnificationType, doubleUnificationType, boolUnificationType),
				(Operator(">="), doubleUnificationType, intUnificationType, boolUnificationType),
				(Operator("<="), intUnificationType, intUnificationType, boolUnificationType), 	//comparison op <=
				(Operator("<="), doubleUnificationType, doubleUnificationType, boolUnificationType),
				(Operator("<="), intUnificationType, doubleUnificationType, boolUnificationType),
				(Operator("<="), doubleUnificationType, intUnificationType, boolUnificationType),
				(Operator("&&"), boolUnificationType, boolUnificationType, boolUnificationType), 	//logical AND
				(Operator("||"), boolUnificationType, boolUnificationType, boolUnificationType) 	//logical OR
		)
		
	def binopHelper(leftType: Term[UnificationType], rightType: Term[UnificationType], resultType: Term[UnificationType]): UIterator[Unit, Operator] = {
		for {
			(op, expectedLeft, expectedRight, expectedResult) <- toUIterator(binops.iterator)
			_ <- unify(leftType, expectedLeft)
			_ <- unify(rightType, expectedRight)
			_ <- unify(resultType, expectedResult)
		} yield op
	}
	
	def genExpLvl1(ofType: Term[UnificationType], bound: Int, env: TypeEnv, funcs: Funcs): UIterator[Unit, Exp] = {
		//println("Converted type of current exp being generated: " + ofType)
		if (bound == 0) {
			empty
		} else {
			randomizedDisjuncts(
				random,
			//disjuncts(
				for {	//DecimalInteger //BEGIN LVL 1
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield DecimalIntegerLiteralExp(num.toString),
				for {	//BinaryInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0b" + num.toBinaryString
					BinaryIntegerLiteralExp(swiftifyNum)
				},
				for {	//OctalInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0o" + num.toOctalString
					OctalIntegerLiteralExp(swiftifyNum)
				},
				for {	//HexInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(10).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0x" + num.toHexString
					HexIntegerLiteralExp(swiftifyNum)
				},
				for {	//DecimalDouble
					_ <- unify(ofType, doubleUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					num <- makeRandomUIDouble(size)
				} yield DecimalFloatLiteralExp(num.toString),
				for {	//HexDouble
					_ <- unify(ofType, doubleUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					num <- makeRandomUIDouble(size)
				} yield HexFloatLiteralExp(java.lang.Double.toHexString(num)),
				for {	//Boolean
					_ <- unify(ofType, boolUnificationType)
					values = Seq(true, false)
					value <- toUIterator(values.toIterator)
				} yield {
					value match {
						case true => TrueLiteralExp()
						case false => FalseLiteralExp()
					}
				},
				for {	//StaticStringLiteralExp
					_ <- unify(ofType, stringUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					resultString <- makeUIString(size)
				} yield SingleLineStaticStringLiteralExp(resultString),
				for {
					_ <- unify(ofType, charUnificationType)
					resultChar <- makeUIString(1)
				} yield CharStringExp(resultChar)
			)
		}
	}
	
	def genExpLvl2(ofType: Term[UnificationType], bound: Int, env: TypeEnv, funcs: Funcs): UIterator[Unit, Exp] = {
		//println("Converted type of current exp being generated: " + ofType)
		if (bound == 0) {
			empty
		} else {
			randomizedDisjuncts(
				random,
			//disjuncts(
				for {	//DecimalInteger //BEGIN LVL 1
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield DecimalIntegerLiteralExp(num.toString),
				for {	//BinaryInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0b" + num.toBinaryString
					BinaryIntegerLiteralExp(swiftifyNum)
				},
				for {	//OctalInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0o" + num.toOctalString
					OctalIntegerLiteralExp(swiftifyNum)
				},
				for {	//HexInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(10).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0x" + num.toHexString
					HexIntegerLiteralExp(swiftifyNum)
				},
				for {	//DecimalDouble
					_ <- unify(ofType, doubleUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					num <- makeRandomUIDouble(size)
				} yield DecimalFloatLiteralExp(num.toString),
				for {	//HexDouble
					_ <- unify(ofType, doubleUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					num <- makeRandomUIDouble(size)
				} yield HexFloatLiteralExp(java.lang.Double.toHexString(num)),
				for {	//Boolean
					_ <- unify(ofType, boolUnificationType)
					values = Seq(true, false)
					value <- toUIterator(values.toIterator)
				} yield {
					value match {
						case true => TrueLiteralExp()
						case false => FalseLiteralExp()
					}
				},
				for {	//StaticStringLiteralExp
					_ <- unify(ofType, stringUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					resultString <- makeUIString(size)
				} yield SingleLineStaticStringLiteralExp(resultString),
				for {
					_ <- unify(ofType, charUnificationType)
					resultChar <- makeUIString(1)
				} yield CharStringExp(resultChar),
				for { //PrefixExp (built in ops + and -) //BEGIN LVL 2
					_ <- unify(ofType, intUnificationType)
					num <- singleton(DecimalIntegerLiteralExp("1"))
					values = Seq(Operator("+"), Operator("-"))
					op <- toUIterator(values.toIterator)
				} yield PrefixExp(op, num),
				for { //PrefixExp (logical not !)
					_ <- unify(ofType, boolUnificationType)
					exp <- genExpLvl2(boolUnificationType, bound-1, env, funcs)
				} yield PrefixExp(Operator("!"), exp),
				{ 	//TrueInfixExp (built in ops)
					val leftType = NewVariable[UnificationType]
					val rightType = NewVariable[UnificationType]
					for {
						op <- binopHelper(leftType, rightType, ofType)
						left <- genExpLvl2(leftType, bound-1, env, funcs)
						right <- genExpLvl2(rightType, bound-1, env, funcs)
					} yield TrueInfixExp(left, op, right)
				},
				for { //ConditionalExp
					trueExp <- genExpLvl2(ofType, bound-1, env, funcs)
					falseExp <- genExpLvl2(ofType, bound-1, env, funcs)
					guard <- genExpLvl2(boolUnificationType, bound-1, env, funcs)
				} yield ConditionalExp(guard, trueExp, falseExp)
			)
		}
	}

	def genExpLvl3(ofType: Term[UnificationType], bound: Int, env: TypeEnv, funcs: Funcs): UIterator[Unit, Exp] = {
		//println("Converted type of current exp being generated: " + ofType)
		if (bound == 0) {
			empty
		} else {
			randomizedDisjuncts(
				random,
			//disjuncts(
				for {	//DecimalInteger //BEGIN LVL 1
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield DecimalIntegerLiteralExp(num.toString),
				for {	//BinaryInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0b" + num.toBinaryString
					BinaryIntegerLiteralExp(swiftifyNum)
				},
				for {	//OctalInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(2).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0o" + num.toOctalString
					OctalIntegerLiteralExp(swiftifyNum)
				},
				for {	//HexInteger
					_ <- unify(ofType, intUnificationType)
					//num <- toUIterator(0.to(10).toIterator)
					num <- singleton(1)
				} yield {
					val swiftifyNum = "0x" + num.toHexString
					HexIntegerLiteralExp(swiftifyNum)
				},
				for {	//DecimalDouble
					_ <- unify(ofType, doubleUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					num <- makeRandomUIDouble(size)
				} yield DecimalFloatLiteralExp(num.toString),
				for {	//HexDouble
					_ <- unify(ofType, doubleUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					num <- makeRandomUIDouble(size)
				} yield HexFloatLiteralExp(java.lang.Double.toHexString(num)),
				for {	//Boolean
					_ <- unify(ofType, boolUnificationType)
					values = Seq(true, false)
					value <- toUIterator(values.toIterator)
				} yield {
					value match {
						case true => TrueLiteralExp()
						case false => FalseLiteralExp()
					}
				},
				for {	//StaticStringLiteralExp
					_ <- unify(ofType, stringUnificationType)
					size <- toUIterator(0.to(1).toIterator)
					resultString <- makeUIString(size)
				} yield SingleLineStaticStringLiteralExp(resultString),
				for {
					_ <- unify(ofType, charUnificationType)
					resultChar <- makeUIString(1)
				} yield CharStringExp(resultChar),
				for { //PrefixExp (built in ops + and -) //BEGIN LVL 2
					_ <- unify(ofType, intUnificationType)
					num <- singleton(DecimalIntegerLiteralExp("1"))
					values = Seq(Operator("+"), Operator("-"))
					op <- toUIterator(values.toIterator)
				} yield PrefixExp(op, num),
				for { //PrefixExp (logical not !)
					_ <- unify(ofType, boolUnificationType)
					exp <- genExpLvl3(boolUnificationType, bound-1, env, funcs)
				} yield PrefixExp(Operator("!"), exp),
				{ 	//TrueInfixExp (built in ops)
					val leftType = NewVariable[UnificationType]
					val rightType = NewVariable[UnificationType]
					for {
						op <- binopHelper(leftType, rightType, ofType)
						left <- genExpLvl3(leftType, bound-1, env, funcs)
						right <- genExpLvl3(rightType, bound-1, env, funcs)
					} yield TrueInfixExp(left, op, right)
				},
				for { //ConditionalExp
					trueExp <- genExpLvl3(ofType, bound-1, env, funcs)
					falseExp <- genExpLvl3(ofType, bound-1, env, funcs)
					guard <- genExpLvl3(boolUnificationType, bound-1, env, funcs)
				} yield ConditionalExp(guard, trueExp, falseExp),
				for { //variable that is previously declared //BEGIN LVL 3
					(varName, varType) <- toUIterator(env.iterator).map(x => (x._1, x._2._1))
					_ <- unify(ofType, convertType(varType))
				} yield VariableExp(varName),
				for { //assignment exp
					_ <- unify(ofType, voidUnificationType)
					(mutableVarName, varType) <- toUIterator(env.iterator).withFilter(_._2._2.isInstanceOf[Mutable]).map(x => (x._1, x._2._1))
					exp <- genExpLvl3(convertType(varType), bound-1, env, funcs)
				} yield AssignmentExp(VariableExp(mutableVarName), exp),
				{ //array literal exp with only 1 exp rn
					val theType = NewVariable[UnificationType]
					for {
						_ <- unify(ofType, arrayUnificationType(theType))
						exp <- genExpLvl3(theType, bound-1, env, funcs)
					} yield ArrayLiteralExp(List(exp))
				},
				 for {
				 	(funcName, funcType, params) <- toUIterator(funcs.iterator).withFilter(_._2._2 == None).map(x => (x._1, x._2._1, x._2._3))
					(funcName, funcType) <- toUIterator(funcs.iterator).map(x => (x._1, x._2._1))
				 	_ <- unify(ofType, convertType(funcType))
				 	funcArgList <- functionParamHelper(params, env, bound, funcs)
				} yield //{ println(funcName + "and" + convertType(funcType))
					PostfixFunctionCallExp(VariableExp(funcName), SimpleFunctionCall(funcArgList))//}
			)
		}
	}
	
	def functionParamHelper(params: List[Parameter], env: TypeEnv, bound: Int, funcs: Funcs): UIterator[Unit, List[FunctionCallArgument]] = {
		params match {
			case Nil => singleton(List())
			case head :: tail => {
				head match {
					case OptDefaultArgClauseParameter(optExternalParamName, localParamName, typeAnno, _) => {
						functionParamHelperHelper(env, bound, funcs, optExternalParamName, localParamName, typeAnno, tail)
					}
					case ElipsesParameter(optExternalParamName, localParamName, typeAnno) => {
						functionParamHelperHelper(env, bound, funcs, optExternalParamName, localParamName, typeAnno, tail)
					}
				}
			}
		}
	}

	def functionParamHelperHelper(env: TypeEnv, bound: Int, funcs: Funcs, optExternalParamName: Option[ExternalParamName],
									localParamName: LocalParamName, typeAnno: TypeAnnotation, tail: List[Parameter]): UIterator[Unit, List[FunctionCallArgument]] = {
		val theType: Typ = typeAnno match {
			case TypeAnnotation(_, _, typ) => typ.resolvedTyp match {
				case Some(theTyp) => theTyp
				case None => UnknownType
			}
		}
		optExternalParamName match {
			case Some(name) => {
				if (name.name == "_") {
					for {
						exp <- genExpLvl3(convertType(theType), bound-1, env, funcs)
						rest <- functionParamHelper(tail, env, bound, funcs)
					} yield ExpFunctionCallArgument(exp) :: rest
				} else {
					for {
						exp <- genExpLvl3(convertType(theType), bound-1, env, funcs)
						rest <- functionParamHelper(tail, env, bound, funcs)
					} yield IdentifierColonExpFunctionCallArgument(FunctionCallArgName(name.name), exp) :: rest
				}
			}
			case None => {
				for {
					exp <- genExpLvl3(convertType(theType), bound-1, env, funcs)
					rest <- functionParamHelper(tail, env, bound, funcs)
				} yield IdentifierColonExpFunctionCallArgument(FunctionCallArgName(localParamName.name), exp) :: rest
			}
		}
	}

	def makeRandomUIDouble(size: Int): UIterator[Unit, Double] = {
		val r: Random = new Random()
		singleton(0 + (size - 0) * r.nextDouble())
	}

	def makeString(size: Int): String = {
		size match {
			case 0 => ""
			case _ => Iterator.continually(Random.nextPrintableChar()).filter(_.isLetterOrDigit).take(size).mkString("")
		}
	}

	def makeUIString(size: Int) : UIterator[Unit, String] = {
		singleton(makeString(size))
	}

 	//def main(args: Array[String]) {
		//genExp(optionalUnificationType(intUnificationType), 2, Map()).reify(new UnificationEnvironment, 1).map(_._3).foreach(x => println(x))
		//genExp(intUnificationType, 2, Map()).reify(new UnificationEnvironment, 1).map(_._3).foreach(x => println(x))
		
		// val expType = NewVariable[UnificationType]
		// genExp(expType, 2).reify(new UnificationEnvironment, ()).map(_._3).foreach(x => println(x))
		//
		// val exp = DecimalIntegerLiteralExp("10")
		// exp.expType = Some(DecimalIntType)
		// maybeRewriteExp(exp).reify(new UnificationEnvironment, ()).map(_._3).foreach(x => println(x))
		//
		// val exp = DecimalIntegerLiteralExp("10")
		// exp.expType = Some(DecimalIntType)
		// val stmt = ExpressionStmt(exp)
		// val prog = Program(List(stmt))
		// maybeRewriteProgram(prog).reify(new UnificationEnvironment, ()).map(_._3).foreach(x => println(x))
		//
		// val numExp = NumericLiteralExp(DecimalInteger("10"))
		// numExp.expType = Some(DecimalIntType)
		// val tupleExp = TupleExp(List(ExpTuple(numExp)))
		// maybeRewriteExp(tupleExp).reify(new UnificationEnvironment, ()).map(_._3).foreach(x => println(x))


	//}

}