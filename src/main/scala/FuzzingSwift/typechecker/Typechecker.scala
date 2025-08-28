package FuzzingSwift.typechecker

import FuzzingSwift.toast._
import FuzzingSwift.parser._

class TypecheckerException(message: String) extends Exception(message)

object Typechecker {
	
	type TypeEnv = Map[String, (Typ, Mutability)]
	type Funcs = Map[String, (Typ, Option[Throwing], List[Parameter])]
	
	var expCount = 0
	var expReplaceCount = 0

	def typecheckOptionalThing[A](a: Option[A])(func: A => Unit): Unit = {
		a match {
			case Some(theThing) => func(theThing)
			case None => ()
		}
	}
	
	def typecheckOptionalThing[A](a: Option[A], env: TypeEnv)(func: (A, TypeEnv) => Unit): Unit = {
		a match {
			case Some(theThing) => func(theThing, env)
			case None => ()
		}
	}
	
	def typecheckOptionalThing[A](a: Option[A], env: TypeEnv, funcs: Funcs)(func: (A, TypeEnv, Funcs) => Unit): Unit = {
		a match {
			case Some(theThing) => func(theThing, env, funcs)
			case None => ()
		}
	}

	def apply(ast: Program): (Double, Double) = {
		typecheck(ast)
		(expCount, expReplaceCount)
	}
	
	def typecheck(ast: Program): Unit = {
		val env: TypeEnv = Map()
		val funcs: Funcs = Map()
		//ast.stmts.foldLeft(env)((curEnv, curStmt) => typecheckStmt(curStmt, curEnv, funcs))
		ast.stmts.foldLeft((env, funcs))((curTuple: (TypeEnv, Funcs), curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
		// ast.stmts.foreach(x => x match {
		// 	//case ExpressionStmt(theExp) => println(x + " " + theExp.typeEnv)
		// 	case DeclarationStmt(decl) => { decl match {
		// 		case ConstantDeclaration(_, _, patternInitList) => { patternInitList.map(x => x match {
		// 			case PatternInitializer(_, optExp) => optExp match {
		// 				case Some(exp) => println(x + " LOOKING AT " + exp + " " + exp.typeEnv)
		// 			}})
		// 		}
		// 		case _ => //nothing
		// 	}}
		// 	case _ => //nothing
		// })
	}
	
	//start of stmts
	def typecheckStmt(stmt: Stmt, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		stmt match {
			case ExpressionStmt(exp) => {
				typecheckExp(exp, env, funcs)
				(env, funcs)
			}
			case DeclarationStmt(decl) => {
				//println("before: " + env)
				val newEnv = typecheckDecl(decl, env, funcs)
				//println("after: " + newEnv)
				newEnv
			}
			case ForInStmt(mod, pattern, exp, optExp, codeBlock) => {
				typecheckPattern(pattern, env, funcs)
				typecheckExp(exp, env, funcs)
				optExp match {
					case Some(theExp) => typecheckExp(theExp, env, funcs)
					case None => //nothing
				}
				typecheckCodeBlock(codeBlock, env, funcs)
				(env, funcs)
			}
			case WhileStmt(conditionList, codeBlock) => {
				conditionList.map(x => typecheckCondition(x, env, funcs))
				typecheckCodeBlock(codeBlock, env, funcs)
				(env, funcs)
			}
			case RepeatWhileStmt(codeBlock, exp) => {
				typecheckCodeBlock(codeBlock, env, funcs)
				typecheckExp(exp, env, funcs)
				(env, funcs)
			}
			case IfStmt(conditionList, codeBlock, optElseClause) => {
				conditionList.map(x => typecheckCondition(x, env, funcs))
				typecheckCodeBlock(codeBlock, env, funcs)
				optElseClause match {
					case Some(elseClause) => typecheckElseClause(elseClause, env, funcs)
					case None => //nothing
				}
				(env, funcs)
			}
			case GuardStmt(conditionList, codeBlock) => {
				conditionList.map(x => typecheckCondition(x, env, funcs))
				typecheckCodeBlock(codeBlock, env, funcs)
				(env, funcs)
			}
			case SwitchStmt(exp, optCaseList) => {
				typecheckExp(exp, env, funcs)
				optCaseList match {
					case Some(caseList) => caseList.map(x => typecheckSwitchCase(x, env, funcs))
					case None => //nothing
				}
				(env, funcs)
			}
			case LabeledStmt(labelName, allowableStmt) => typecheckStmt(allowableStmt, env, funcs)
			case BreakStmt(optLabelName) => (env, funcs)
			case ContinueStmt(optLabelName) => (env, funcs)
			case FallthroughStmt => (env, funcs)
			case ReturnStmt(optExp) => {
				optExp match {
					case Some(exp) => typecheckExp(exp, env, funcs)
					case None => //nothing
				}
				(env, funcs)
			}
			case ThrowStmt(exp) => {
				typecheckExp(exp, env, funcs)
				(env, funcs)
			}
			case DeferStmt(codeBlock) => {
				typecheckCodeBlock(codeBlock, env, funcs)
				(env, funcs)
			}
			case DoStmt(codeBlock, optCatchClauseList) => {
				typecheckCodeBlock(codeBlock, env, funcs)
				optCatchClauseList match {
					case Some(catchClauseList) => catchClauseList.map(x => typecheckCatchClause(x, env, funcs))
					case None => //nothing
				}
			(env, funcs)
			}
			case CompilerControlStmt(compCtrl) => {
				typecheckCompilerCtrl(compCtrl, env, funcs)
			}
		}
	} 
	
	def typecheckCompilerCtrl(compCtrl: CompilerCtrl, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		compCtrl match {
			case ConditionalCompilationBlock(ifClause, optElseIfClauseList, optElseClause) => {
				typecheckIfDirectiveClause(ifClause, env, funcs)
				optElseIfClauseList match {
					case Some(elseIfClauseList) => elseIfClauseList.foldLeft(env, funcs)((curTuple, curClause) => typecheckElseIfDirectiveClause(curClause, curTuple._1, curTuple._2))
					case None => (env, funcs)
				}
				optElseClause match {
					case Some(elseClause) => typecheckElseDirectiveClause(elseClause, env, funcs)
					case None => (env, funcs)
				}
			}
			case thing: LineControlStmt => (env, funcs)
			case thing: AvailabilityConditionAvailable => (env, funcs)
			case thing: AvailabilityConditionUnavailable => (env, funcs)
		}
	}
	
	def typecheckElseDirectiveClause(elseClause: ElseDirectiveClause, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		elseClause match {
			case ElseDirectiveClause(optStmtList) => optStmtList match {
				case Some(stmtList) => stmtList.foldLeft(env, funcs)((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
				case None => (env, funcs)
			}
		}
	}
	
	def typecheckElseIfDirectiveClause(elseIfClause: ElseIfDirectiveClause, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		elseIfClause match {
			case ElseIfDirectiveClause(compCond, optStmtList) => {
				optStmtList match {
					case Some(stmtList) => stmtList.foldLeft(env, funcs)((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
					case None => (env, funcs)
				}
			}
		}
	}
	
	def typecheckIfDirectiveClause(ifClause: IfDirectiveClause, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		ifClause match {
			case IfDirectiveClause(compCond, optStmtList) => {
				optStmtList match {
					case Some(stmtList) => stmtList.foldLeft(env, funcs)((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
					case None => (env, funcs)
				}
			}
		}
	}
	
	def typecheckCondition(condition: Condition, env: TypeEnv, funcs: Funcs): Unit = {
		condition match {
			case ExpressionCondition(exp) => typecheckExp(exp, env, funcs)
			case CaseCondition(pattern, exp) => {
				typecheckPattern(pattern, env, funcs)
				typecheckExp(exp, env, funcs)
			}
			case OptionalBindingConditionLet(pattern, optExp) => {
				typecheckPattern(pattern, env, funcs)
				optExp match {
					case Some(exp) => typecheckExp(exp, env, funcs)
					case None => //nothing
				}
			}
			case OptionalBindingConditionVar(pattern, optExp) => {
				typecheckPattern(pattern, env, funcs)
				optExp match {
					case Some(exp) => typecheckExp(exp, env, funcs)
					case None => //nothing
				}
			}
		}
	}
	
	def typecheckElseClause(clause: ElseClause, env: TypeEnv, funcs: Funcs): Unit = {
		clause match {
			case ElseCodeBlock(codeBlock) => typecheckCodeBlock(codeBlock, env, funcs)
			case ElseIfStmt(ifStmt) => typecheckStmt(ifStmt, env, funcs) //this is returning a typeEnv but i don't think we care about it?
		}
	}
	
	def typecheckCaseItem(item: CaseItem, env: TypeEnv, funcs: Funcs): Unit = {
		item match {
			case CaseItem(pattern, optExp) => {
				typecheckPattern(pattern, env, funcs)
				optExp match {
					case Some(exp) => typecheckExp(exp, env, funcs)
					case None => //nothing
				}
			}
		}
	}
	
	def typecheckCaseLabel(label: CaseLabel, env: TypeEnv, funcs: Funcs): Unit = {
		label match {
			case CaseLabel(aList, itemList) => itemList.map(x => typecheckCaseItem(x, env, funcs))
		}
	}
	
	def typecheckSwitchCase(sCase: SwitchCase, env: TypeEnv, funcs: Funcs): Unit = {
		sCase match {
			case CaseLabelStmts(caseLabel, stmtList) => {
				typecheckCaseLabel(caseLabel, env, funcs)
				stmtList.foldLeft(env, funcs)((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
			}
			case DefaultLabelStmts(aList, stmtList) => stmtList.foldLeft(env, funcs)((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
		}
	}
	
	def typecheckCatchPattern(pattern: CatchPattern, env: TypeEnv, funcs: Funcs): Unit = {
		pattern match {
			case CatchPattern(pattern, optExp) => {
				typecheckPattern(pattern, env, funcs)
				optExp match {
					case Some(exp) => typecheckExp(exp, env, funcs)
					case None => //nothing
				}
			}
		}
	}
	
	def typecheckCatchClause(clause: CatchClause, env: TypeEnv, funcs: Funcs): Unit = {
		clause match {
			case CatchClause(optCatchPatternList, codeBlock) => {
				optCatchPatternList match {
					case Some(catchPatternList) => catchPatternList.map(x => typecheckCatchPattern(x, env, funcs))
					case None => //nothing
				}
				typecheckCodeBlock(codeBlock, env, funcs)
			}
		}
	}
	//end of stmts
	
	//starts of decls
	def typecheckDecl(decl: Declaration, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		decl match {
			case ImportDeclaration(optAList, kind, path) => (env, funcs)
			case ConstantDeclaration(optAList, modifier, patternInitList) => {
				patternInitList.map(x => typecheckPatternInitializer(x, env, funcs))
				addToTypeEnv(ConstantDeclaration(optAList, modifier, patternInitList), env, funcs)
			}
			case VariableDeclaration1(optAList, mods, patternInitList) => {
				patternInitList.map(x => typecheckPatternInitializer(x, env, funcs))
				addToTypeEnv(VariableDeclaration1(optAList, mods, patternInitList), env, funcs)
			}
			case VariableDeclaration23(optAList, mods, varName, ta, block) => {
				typecheckTypeAnnotation(ta)
				typecheckGetterSetterBlock(block, env, funcs)
				(env, funcs)
			}
			case VariableDeclaration4(optAList, mods, varName, ta, kBlock) => {
				typecheckTypeAnnotation(ta)
				(env, funcs)
			}
			case VariableDeclaration5(optAList, mods, varName, exp, setBlock) => {
				typecheckExp(exp, env, funcs)
				typecheckSetBlock(setBlock, env, funcs)
				(env, funcs)
			}
			case VariableDeclaration6(optAList, mods, varName, ta, optExp, setBlock) => {
				typecheckTypeAnnotation(ta)
				optExp match {
					case Some(exp) => typecheckExp(exp, env, funcs)
					case None => //nothing
				}
				typecheckSetBlock(setBlock, env, funcs)
				(env, funcs)
			}
			case TypeAliasDeclaration(optAList, mod, typeAliasName, optGParamClause, typ) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				typecheckType(typ)
				(env, funcs)
			}
			case FunctionDeclaration(funcHead, funcName, optGParamClause, funcSig, optGWhereClause, optBlock) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				val returnType = typecheckFunctionSignature(funcSig, env, funcs)
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				optBlock match {
					case Some(block) => typecheckCodeBlock(block, env, funcs)
					case None => //nothing
				}
				returnType match {
					case (Some(typ), throwing, params) => addToFuncs(env, funcs, funcName, typ, throwing, params)
					case (None, _, _) => (env, funcs)
				}
			}
			case UnionStyleEnumDeclaration(optAList, modList, mod, enumName, optGParamClause, optTIClause, optGWhereClause, optEnumMembers) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				optEnumMembers match {
					case Some(enumMembers) => enumMembers.foldLeft(env, funcs)((curTuple, curMember) => typecheckUnionStyleEnumMember(curMember, curTuple._1, curTuple._2))
					case None => //nothing
				}
				(env, funcs)
			}
			case RawValueStyleEnumDeclaration(optAList, mod, enumName, optGParamClause, tIClause, optGWhereClause, enumMembers) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				tIClause.map(x => typecheckTypeInheritance(x))
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				enumMembers.foldLeft(env, funcs)((curTuple, curMember) => typecheckRawValueStyleEnumMember(curMember, curTuple._1, curTuple._2))
				(env, funcs)
			}
			case StructDeclaration(optAList, mod, structname, optGParamClause, optTIClause, optGWhereClause, structMemberList) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				structMemberList.foldLeft(env, funcs)((curTuple, curMember) => typecheckStructMember(curMember, curTuple._1, curTuple._2))
				(env, funcs)
			}
			case RegClassDeclaration(optAList, aMod, fMod, classname, optGParamClause, optTIClause, optGWhereClause, classMemberList) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				classMemberList.foldLeft(env, funcs)((curTuple, curMember) => typecheckClassMember(curMember, curTuple._1, curTuple._2))
				(env, funcs)
			}
			case ForcedFinalClassDeclaration(optAList, mod, classname, optGParamClause, optTIClause, optGWhereClause, classMemberList) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				classMemberList.foldLeft(env, funcs)((curTuple, curMember) => typecheckClassMember(curMember, curTuple._1, curTuple._2))
				(env, funcs)
			}
			case ActorDeclaration(optAList, mod, actorName, optGParamClause, optTIClause, optGWhereClause, actorMemberList) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				actorMemberList.foldLeft(env, funcs)((curTuple, curMember) => typecheckActorMember(curMember, curTuple._1, curTuple._2))
				(env, funcs)
			}
			case ProtocolDeclaration(optAList, mod, protocolName, optTIClause, optGWhereClause, protocolMemberList) => {
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				protocolMemberList.map(x => typecheckProtocolMember(x, env, funcs))
				(env, funcs)
			}
			case ThrowsInitializerDeclaration(initHead, optGParamClause, paramClause, aMod, tMod, optGWhereClause, block) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				paramClause.map(x => typecheckParameter(x, env, funcs))
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				typecheckCodeBlock(block, env, funcs)
				(env, funcs)
			}
			case RethrowsInitializerDeclaration(initHead, optGParamClause, paramClause, mod, optGWhereClause, block) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				paramClause.map(x => typecheckParameter(x, env, funcs))
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				typecheckCodeBlock(block, env, funcs)
				(env, funcs)
			}
			case DeinitializerDeclaration(optAList, block) => {
				typecheckCodeBlock(block, env, funcs)
				(env, funcs)
			}
			case ExtensionDeclaration(optAList, mod, typeID, optTIClause, optGWhereClause, extensionMemberList) => {
				typecheckType(typeID)
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				extensionMemberList.foldLeft(env, funcs)((curTuple, curMember) => typecheckExtensionMember(curMember, curTuple._1, curTuple._2))
				(env, funcs)
			}
			case SubscriptDeclaration(subscriptHead, res, optGWhereClause, allowableBlock) => {
				typecheckSubscriptHead(subscriptHead, env, funcs)
				typecheckSubscriptResult(res)
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
				typecheckAllowableSubscriptBlock(allowableBlock, env, funcs)
				(env, funcs)
			}
			case PrefixOperatorDeclaration(op) => (env, funcs)
			case PostfixOperatorDeclaration(op) => (env, funcs)
			case InfixOperatorDeclaration(op, optPrecedenceGroupName) => (env, funcs)
			case PrecedenceGroupDeclaration(precedenceGroupName, optPrecedenceList) => (env, funcs)
		}
	}
	
	def addToFuncs(env: TypeEnv, funcs: Funcs, funcName: FunctionName, funcType: Type, throwing: Option[Throwing], params: List[Parameter]): (TypeEnv, Funcs) = {
		funcName match {
			case IdentifierFunctionName(name) => {
				funcType.resolvedTyp match {
					case Some(typ) => (env, funcs + (name -> (typ, throwing, params)))
					case None => (env, funcs)
				}
			}
			case OperatorFunctionName(op) => {
				funcType.resolvedTyp match {
					case Some(typ) => (env, funcs + (op.value -> (typ, throwing, params)))
					case None => (env, funcs)
				}
			}
		}
	}
	
	def addToTypeEnv(decl: Declaration, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		decl match {
			case ConstantDeclaration(_, _, patternInitList) => {
				patternInitList.foldLeft(env, funcs)((curTuple, curInit) => addToTypeEnvHelper(curInit, curTuple._1, Immutable, curTuple._2))
			}
			case VariableDeclaration1(_, _, patternInitList) => {
				patternInitList.foldLeft(env, funcs)((curTuple, curInit) => addToTypeEnvHelper(curInit, curTuple._1, Mutable, curTuple._2))
			}
			//currently only handling "normal" vardecs
			case _ => (env, funcs)
		}
	}
	
	def addToTypeEnvHelper(patternInit: PatternInitializer, env: TypeEnv, mutability: Mutability, funcs: Funcs): (TypeEnv, Funcs) = {
		patternInit match {
			case PatternInitializer(pattern, optExp) => {
				optExp match {
					case Some(exp) => pattern match {
						case IdentifierPattern(patternName, optTypeAnno) => optTypeAnno match {
							case Some(typeAnno) => typeAnno.typ.resolvedTyp match {
								case Some(theResolvedTyp) => (env + (patternName.name -> (theResolvedTyp, mutability)), funcs)
								case None => (env, funcs) //there's a type anno but we don't know what it is
							}
							case None => typeOfExp(exp, env, funcs) match {
								case Some(expType) => (env + (patternName.name -> (expType, mutability)), funcs)
								case None => (env, funcs) //there's an exp but we can't figure out its type
							}
						}
						case _ => (env, funcs) //other patterns
					}
					case None => (env, funcs) // variable does not have value assigned to it
				}
			}
		}
	}
	
	def typecheckPatternInitializer(patternInit: PatternInitializer, env: TypeEnv, funcs: Funcs): Unit = {
		patternInit match {
			case PatternInitializer(pattern, optExp) => {
				optExp match {
					case Some(exp) => pattern match {
						case IdentifierPattern(_, optTypeAnno) => optTypeAnno match {
							case None => typecheckExp(exp, env, funcs)
							case Some(_) => //we don't want to typecheck the exp in this case
						}
						case WildcardPattern(optTypeAnno) => optTypeAnno match {
							case None => typecheckExp(exp, env, funcs)
							case Some(_) => //nothing
						}
						case TuplePattern(_, optTypeAnno) => optTypeAnno match {
							case None => typecheckExp(exp, env, funcs)
							case Some(_) => //nothing
						}
						case _ => //need to supress compiler warning
					}
					case None => //no initializer expression to worry about
				}
				typecheckPattern(pattern, env, funcs)
			}
		}
	}
	
	def typecheckCodeBlock(block: CodeBlock, env: TypeEnv, funcs: Funcs): Unit = {
		block match {
			case CodeBlock(optStmtList) => { 
				optStmtList match {
					case Some(stmtList) => stmtList.foldLeft(env, funcs)((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
					case _ => //nothing 
				}
			}
		}
	}
	
	def typecheckGetterClause(clause: GetterClause, env: TypeEnv, funcs: Funcs): Unit = {
		clause match {
			case GetterClause(optAList, mod, block) => typecheckCodeBlock(block, env, funcs)
		}
	}
	
	def typecheckSetterClause(clause: SetterClause, env: TypeEnv, funcs: Funcs): Unit = {
		clause match {
			case SetterClause(optAList, mod, optSetterName, block) => typecheckCodeBlock(block, env, funcs)
		}
	}
	
	def typecheckGetterSetterBlock(block: GetterSetterBlock, env: TypeEnv, funcs: Funcs): Unit = {
		block match {
			//ast.stmts.foldLeft(env)((curEnv, curStmt) => typecheckStmt(curStmt, curEnv))
			case CodeBlock(optStmtList) => { 
				optStmtList match {
					case Some(stmtList) => stmtList.foldLeft(env, funcs)((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
					case _ => //nothing 
				}
			}
			case GetterSetterClauseBlock(getter, optSetter) => {
				typecheckGetterClause(getter, env, funcs)
				optSetter match {
					case Some(setter) => typecheckSetterClause(setter, env, funcs)
					case None => //nothing
				}
			}
			case SetterGetterClauseBlock(setter, getter) => {
				typecheckSetterClause(setter, env, funcs)
				typecheckGetterClause(getter, env, funcs)
			}
		}
	}
	
	def typecheckWillSetClause(clause: WillSetClause, env: TypeEnv, funcs: Funcs): Unit = {
		clause match {
			case WillSetClause(optAList, optMod, optSetterName, block) => typecheckCodeBlock(block, env, funcs)
		}
	}
	
	def typecheckDidSetClause(clause: DidSetClause, env: TypeEnv, funcs: Funcs): Unit = {
		clause match {
			case DidSetClause(optAList, optMod, optSetterName, block) => typecheckCodeBlock(block, env, funcs)
		}
	}
	
	def typecheckSetBlock(block: SetBlock, env: TypeEnv, funcs: Funcs): Unit = {
		block match {
			case WillDidSetBlock(will, optDid) => {
				typecheckWillSetClause(will, env, funcs)
				optDid match {
					case Some(did) => typecheckDidSetClause(did, env, funcs)
					case None => //nothing
				}
			}
			case DidWillSetBlock(did, optWill) => {
				typecheckDidSetClause(did, env, funcs)
				optWill match {
					case Some(will) => typecheckWillSetClause(will, env, funcs)
					case None => //nothing
				}
			}
		}
	}
	
	def typecheckGenericParameter(param: GenericParameter): Unit = {
		param match {
			case SimpleGenericParameter(typeName) => //nothing
			case AnnotatedGenericParameter(typeName, typeId) => typecheckType(typeId)
			case ProtocolCompGenericParameter(typeName, protocolCompType) => typecheckType(protocolCompType)
		}
	}
	
	def typecheckParameter(param: Parameter, env: TypeEnv, funcs: Funcs): Unit = {
		param match {
			case OptDefaultArgClauseParameter(optExternalParamName, localParamName, ta, optExp) => {
				typecheckTypeAnnotation(ta)
				optExp match {
					case Some(exp) => typecheckExp(exp, env, funcs)
					case None => //nothing
				}
			}
			case ElipsesParameter(optExternalParamName, localParamName, ta) => typecheckTypeAnnotation(ta)
		}
	}
	
	def typecheckFunctionSignature(sig: FunctionSignature, env: TypeEnv, funcs: Funcs): (Option[Type], Option[Throwing], List[Parameter]) = {
		sig match {
			case ThrowsFunctionSig(paramClause, aMod, tMod, optFuncResult) => {
				paramClause.map(x => typecheckParameter(x, env, funcs))
				(optFuncResult, tMod) match {
					case (Some(funcResult), Some(_)) => {
						typecheckFunctionResult(funcResult)
						(Some(funcResult.typ), Some(Throws), paramClause)
					}
					case (Some(funcResult), None) => {
						typecheckFunctionResult(funcResult)
						(Some(funcResult.typ), None, paramClause)
					}
					case (None, _) => (None, None, List())
				}
			}
			case RethrowsFunctionSig(paramClause, mod, optFuncResult) => {
				paramClause.map(x => typecheckParameter(x, env, funcs))
				optFuncResult match {
					case Some(funcResult) => {
						typecheckFunctionResult(funcResult)
						(Some(funcResult.typ), Some(Rethrows), paramClause)
					}
					case (None) => (None, None, paramClause)
				}
			}
		}
	}
	
	def typecheckTypeInheritance(typeInheritance: TypeInheritance): Unit = {
		typeInheritance match {
			case TypeInheritance(optAList, typeId) => typecheckType(typeId)
		}
	}
	
	def typecheckRequirement(req: Requirement): Unit = {
		req match {
			case ConformanceRequirementDoubleTypeID(typeId1, typeId2) => {
				typecheckType(typeId1)
				typecheckType(typeId2)
			}
			case ConformanceRequirementTypeIDProtocolCompType(typeId, protocolCompType) => {
				typecheckType(typeId)
				typecheckType(protocolCompType)
			}
			case SameTypeRequirement(typeId, typ) => {
				typecheckType(typeId)
				typecheckType(typ)
			}
		}
	}
	
	def typecheckUnionStyleEnumCase(enumCase: UnionStyleEnumCase): Unit = {
		enumCase match {
			case UnionStyleEnumCase(enumCaseName, optTupleType) => optTupleType match {
				case Some(tupleType) => typecheckType(tupleType)
				case None => //nothing
			}
		}
	}
	
	def typecheckUnionStyleEnumMember(member: UnionStyleEnumMember, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		member match {
			case DeclarationUSEnumMember(decl) => typecheckDecl(decl, env, funcs) 
			case EnumCaseClauseUSEnumMember(optAList, mod, caseList) => {
				caseList.map(x => typecheckUnionStyleEnumCase(x))
				(env, funcs)
			}
			case CompilerControlEnumMember(stmt) => typecheckStmt(stmt, env, funcs)
		}
	}
	
	def typecheckRawValueStyleEnumMember(member: RawValueStyleEnumMember, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		member match {
			case DeclarationRVSEnumMember(decl) => typecheckDecl(decl, env, funcs)
			case EnumCaseClauseRVSEnumMember(optAList, caseList) => (env, funcs)
			case CompilerControlRVSEnumMember(stmt) => typecheckStmt(stmt, env, funcs)
		}
	}
	
	def typecheckStructMember(member: StructMember, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		member match {
			case DeclarationStructMember(decl) => decl match { //we currently don't want to add any instance vars to the typeEnv
			case idc: ConstantDeclaration => (env, funcs)
			case idc: VariableDeclaration1 => (env, funcs)
			case idc: FunctionDeclaration => (env, funcs)
			case _ => typecheckDecl(decl, env, funcs)	
			}
			case CompilerControlStructMember(stmt) => typecheckStmt(stmt, env, funcs)
		}
	}
	
	def typecheckClassMember(member: ClassMember, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		member match {
			case DeclarationClassMember(decl) => decl match { //we currently don't want to add any instance vars to the typeEnv
			case idc: ConstantDeclaration => (env, funcs)
			case idc: VariableDeclaration1 => (env, funcs)
			case idc: FunctionDeclaration => (env, funcs)
			case _ => typecheckDecl(decl, env, funcs)	
			}
			case CompilerControlClassMember(stmt) => typecheckStmt(stmt, env, funcs)
		}
	}
	
	def typecheckActorMember(member: ActorMember, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		member match {
			case DeclarationActorMember(decl) => decl match {
				case idc: ConstantDeclaration => (env, funcs)
				case idc: VariableDeclaration1 => (env, funcs)
				case idc: FunctionDeclaration => (env, funcs)
				case _ => typecheckDecl(decl, env, funcs)
			}
			case CompilerControlActorMember(stmt) => typecheckStmt(stmt, env, funcs)
		}
	}
	
	def typecheckSubscriptHead(head: SubscriptHead, env: TypeEnv, funcs: Funcs): Unit = {
		head match {
			case SubscriptHead(optAList, mod, optGParamClause, paramClause) =>{
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				paramClause.map(x => typecheckParameter(x, env, funcs))
			}
		}
	}
	
	def typecheckSubscriptResult(result: SubscriptResult): Unit = {
		result match {
			case SubscriptResult(optAList, typ) => typecheckType(typ)
		}
	}
	
	def typecheckProtocolMember(member: ProtocolMember, env: TypeEnv, funcs: Funcs): Unit = {
		member match {
			case ProtocolPropertyDeclaration(optAList, mods, varName, ta, kBlock) => {
				typecheckTypeAnnotation(ta)
			}
			case ProtocolMethodDeclaration(funcHead, funcName, optGParamClause, funcSig, optGWhereClause) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				typecheckFunctionSignature(funcSig, env, funcs)
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
			}
			case ThrowsProtocolInitializerDeclaration(initHead, optGParamClause, paramClause, mod, optGWhereClause) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				paramClause.map(x => typecheckParameter(x, env, funcs))
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
			}
			case RethrowsProtocolInitializerDeclaration(initHead, optGParamClause, paramClause, optGWhereClause) => {
				optGParamClause match {
					case Some(genericParamClause) => genericParamClause.map(x => typecheckGenericParameter(x))
					case None => //nothing
				}
				paramClause.map(x => typecheckParameter(x, env, funcs))
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
			}
			case ProtocolSubscriptDeclaration(subscriptHead, res, optGWhereClause, kBlock) => {
				typecheckSubscriptHead(subscriptHead, env, funcs)
				typecheckSubscriptResult(res)
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
			}
			case ProtocolAssociatedTypeDeclaration(optAList, mod, typeAliasName, optTIClause, optType, optGWhereClause) => {
				optTIClause match {
					case Some(tiClause) => tiClause.map(x => typecheckTypeInheritance(x))
					case None => //nothing
				}
				optType match {
					case Some(theType) => typecheckType(theType)
					case None => //nothing
				}
				optGWhereClause match {
					case Some(genericWhereClause) => genericWhereClause.map(x => typecheckRequirement(x))
					case None => //nothing
				}
			}
			case ProtocolTypeAliasDeclaration(tAliasDecl) => typecheckDecl(tAliasDecl, env, funcs)
			case CompilerControlProtocolMember(stmt) => typecheckStmt(stmt, env, funcs)
		}
	}
	
	def typecheckExtensionMember(member: ExtensionMember, env: TypeEnv, funcs: Funcs): (TypeEnv, Funcs) = {
		member match {
			case DeclarationExtensionMember(decl) => typecheckDecl(decl, env, funcs)
			case CompilerControlExtensionMember(stmt) => typecheckStmt(stmt, env, funcs)
		}
	}
	
	def typecheckAllowableSubscriptBlock(block: AllowableSubscriptBlock, env: TypeEnv, funcs: Funcs): Unit = {
		block match {
			case AllowableGetterSetterBlock(block) => typecheckGetterSetterBlock(block, env, funcs)
			//case AllowableKeywordBlock(kBlock) => //nothing
			case _ => //nothing
		}
	}
	
	def typecheckPattern(pattern: Pattern, env: TypeEnv, funcs: Funcs): Unit = {
		pattern match {
			case WildcardPattern(optTA) => optTA match {
				case Some(ta) => typecheckTypeAnnotation(ta)
				case None => //nothing
			}
			case IdentifierPattern(patternName, optTA) => optTA match {
				case Some(ta) => typecheckTypeAnnotation(ta)
				case None => //nothing
			}
			case ValueBindingPattern(mod, pattern) => typecheckPattern(pattern, env, funcs)
			case TuplePattern(tuplePatternElementList, optTA) => {
				tuplePatternElementList.map(x => typecheckTuplePatternElement(x, env, funcs))
				optTA match {
					case Some(ta) => typecheckTypeAnnotation(ta)
					case None => //nothing
				}
			}
			case EnumCasePattern(optTypeIdentifier, enumCaseName, optTuplePattern) => {
				typecheckOptionalThing(optTypeIdentifier)(typecheckType _)
				typecheckOptionalThing(optTuplePattern, env, funcs)(typecheckPattern _)
			}
			case OptionalPattern(pattern) => typecheckPattern(pattern, env, funcs)
			case ExpressionPattern(exp) => typecheckExp(exp, env, funcs)
			case IsTypeCastingPattern(typ) => typecheckType(typ)
			case AsTypeCastingPattern(pattern, typ) => {
				typecheckPattern(pattern, env, funcs)
				typecheckType(typ)
			}
		}
	}
	
	def typecheckTuplePatternElement(element: TuplePatternElement, env: TypeEnv, funcs: Funcs): Unit = {
		element match {
			case PatternElement(pattern) => typecheckPattern(pattern, env, funcs)
			case IdentifierPatternElement(patternName, pattern) => typecheckPattern(pattern, env, funcs)
		}
	}
	
	def typecheckType(typ: Type): Unit = {
		typ match {
			case FunctionType(optAList, functionTypeArgList, aMod, tMod, theType) => {
				functionTypeArgList.map(x => typecheckFunctionTypeArg(x))
				typecheckType(theType)
				annotateType(typ)
			}
			case ArrayType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
			case DictionaryType(type1, type2) => {
				typecheckType(type1)
				typecheckType(type2)
				annotateType(typ)
			}
			case theType: NormalTypeIdentifier => annotateType(theType)
			case GenericTypeIdentifier(typeName, typeList) => {
				typeList.map(x => typecheckType(x))
				annotateType(typ)
			}
			case NestedNormalTypeIdentifier(typeName, nestedType) => {
				typecheckType(nestedType)
				annotateType(typ)
			}
			case NestedGenericTypeIdentifier(typeName, typeList, nestedType) => {
				typeList.map(x => typecheckType(x))
				typecheckType(nestedType)
				annotateType(typ)
			}
			case TupleType(listTTElements) => {
				listTTElements.map(x => typecheckTupleTypeElement(x))
				annotateType(typ)
			}
			case ProtocolCompositionType(listTypeIds) => {
				listTypeIds.map(x => typecheckType(x))
				annotateType(typ)
			}
			case OpaqueType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
			case AnyType => annotateType(typ)
			case SelfType => annotateType(typ)
			case InParensType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
			case OptionalType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
			case ImplicitlyUnwrappedOptionalType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
			case MetatypeTypeType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
			case MetatypeProtocolType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
			case BoxedProtocolType(theType) => {
				typecheckType(theType)
				annotateType(typ)
			}
		}
	}
	
	def annotateType(theType: Type): Unit = {
		theType.resolvedTyp = typeToTyp(theType)
	}
	
	def typeToTyp(theType: Type): Option[Typ] = {
		theType match {
			case AnyType => Some(AnyTyp)
			//case SelfType => Some(SelfTyp)
			case NormalTypeIdentifier(name) => name.name match {
				case ("Int") => Some(IntType)
				case ("Double") => Some(DoubleType)
				case ("Bool") => Some(BoolType)
				case ("String") => Some(StringType)
				case ("Void") => Some(VoidType)
				case _ => Some(NamedType(name.name)) //a named type that isn't one of the "built in" types
			}
			case InParensType(theType) => typeToTyp(theType)
			case OptionalType(theType) => typeToTyp(theType) match {
				case Some(theInnerTyp) => Some(OptionalTyp(theInnerTyp))
				case None => None //could not resolve the optional type
			}
			case ArrayType(theType) => typeToTyp(theType) match {
				case Some(theInnerTyp) => Some(ArrayTyp(theInnerTyp))
				case None => None // could not resolve the array type
			}
			case DictionaryType(type1, type2) => typeToTyp(type1) match {
				case Some(theInnerTyp1) => typeToTyp(type2) match {
					case Some(theInnerTyp2) => Some(DictionaryTyp(theInnerTyp1, theInnerTyp2))
					case None => None //could not resolve the second type in the dictionary type
				}
				case None => None //could not resolve the first type in the dictionary type
			}
			case OpaqueType(theType) => typeToTyp(theType) match {
				case Some(theInnerTyp) => Some(OpaqueTyp(theInnerTyp))
				case None => None //could not resolve the opaque type
			}
			case ImplicitlyUnwrappedOptionalType(theType) => typeToTyp(theType) match {
				case Some(theInnerTyp) => Some(ImplicitlyUnwrappedOptionalTyp(theInnerTyp))
				case None => None //could not resolve the implcitly unwrapped type
			}
			case MetatypeTypeType(theType) => typeToTyp(theType) match {
				case Some(theInnerTyp) => Some(MetatypeTypeTyp(theInnerTyp))
				case None => None // could not resolve the metatype-type type
			}
			case MetatypeProtocolType(theType) => typeToTyp(theType) match {
				case Some(theInnerTyp) => Some(MetatypeProtocolTyp(theInnerTyp))
				case None => None // could not resolve the metatype-protocol type
			}
			case FunctionType(_, typeList, _, _, returnType) => {
				val newTypeList = typeList.map(x => x match {
					case FunctionTypeArg1(_, _, theType) => theType.resolvedTyp match {
						case Some(theResolvedTyp) => theResolvedTyp
						case None => UnknownType //can't figure out the type of the function arg
					}
					case FunctionTypeArg2(_, typeAnno) => typeAnno.typ.resolvedTyp match {
						case Some(theResolvedTyp) => theResolvedTyp
						case None => UnknownType //can't figure out the type of the function arg
					}
				})
				if (newTypeList.contains(UnknownType)) None else {
					returnType.resolvedTyp match {
						case Some(theReturnTyp) => Some(FunctionTyp(newTypeList, theReturnTyp))
						case None => None //can't figure out the return type
					}
				}
			}
			case BoxedProtocolType(theType) => typeToTyp(theType) match {
				case Some(theInnerTyp) => Some(BoxedProtocolTyp(theInnerTyp))
				case None => None
			}
			case _ => None //only implementing some rn
		}
	}
	
	def typecheckTypeAnnotation(ta: TypeAnnotation): Unit = {
		ta match {
			case TypeAnnotation(optAList, mod, typ) => typecheckType(typ)
		}
	}
	
	def typecheckFunctionTypeArg(arg: FunctionTypeArg): Unit = {
		arg match {
			case FunctionTypeArg1(optAList, mod, theType) => typecheckType(theType)
			case FunctionTypeArg2(argumentLabel, ta) => typecheckTypeAnnotation(ta)
		}
	}
	
	def typecheckTupleTypeElement(element: TupleTypeElement): Unit = {
		element match {
			case TupleTypeElementNameAnnotation(elementName, ta) => typecheckTypeAnnotation(ta)
			case TupleTypeElementType(theType) => typecheckType(theType)
		}
	}
	
	def typecheckFunctionCallArgument(arg: FunctionCallArgument, env: TypeEnv, funcs: Funcs): Unit = {
		arg match {
			case ExpFunctionCallArgument(exp) => typecheckExp(exp, env, funcs)
			case IdentifierColonExpFunctionCallArgument(functionCallArgName, exp) => typecheckExp(exp, env, funcs)
			case _ => //nothing
			//case OperatorFunctionCallArgument(op) => //nothing
			//case IdentifierColonOperatorFunctionCallArgument(functionCallArgName, op) => //nothing
		}
	}
	
	def typecheckLabeledTrailingClosure(closure: LabeledTrailingClosure, env: TypeEnv, funcs: Funcs): Unit = {
		closure match {
			case LabeledTrailingClosure(trailingClosureLabel, closureExp) => typecheckExp(closureExp, env, funcs)
		}
	}
	
	def typecheckTrailingClosure(closure: TrailingClosure, env: TypeEnv, funcs: Funcs): Unit = {
		closure match {
			case TrailingClosure(exp, labeledTrailingClosureList) => typecheckExp(exp, env, funcs)
		}
	}
	
	def typecheckPostfixFunctionCall(call: PostfixFunctionCall, env: TypeEnv, funcs: Funcs): Unit = {
		call match {
			case SimpleFunctionCall(functionCallArgList) => functionCallArgList.map(x => typecheckFunctionCallArgument(x, env, funcs))
			case ComplexFunctionCall(optFunctionCallArgList, trailing) => {
				optFunctionCallArgList match {
					case Some(functionCallArgList) => functionCallArgList.map(x => typecheckFunctionCallArgument(x, env, funcs))
					case None => //nothing
				}
				typecheckTrailingClosure(trailing, env, funcs)
			}
		}
	}
	
	def typecheckExplicitMember(member: ExplicitMember): Unit = {
		member match {
			//case ExplicitMemberDecimalDigits(nums) => //nothing
			case ExplicitMemberIdentifierOptGeneric(explicitMemberName, optTypeList) => {
				optTypeList match {
					case Some(typeList) => typeList.map(x => typecheckType(x))
					case None => //nothing
				}
			}
			//case ExplicitMemberIdentifierArgs(explicitMemberName, list) => //nothing
			case _ => //nothing
		}
	}
	
	def typecheckCaptureListItem(item: CaptureListItem, env: TypeEnv, funcs: Funcs): Unit = {
		item match {
			//case CaptureListItemIdentifier(specifier, captureListItemName) => //nothing
			case CaptureListItemAssignment(specifier, captureListItemName, exp) => typecheckExp(exp, env, funcs)
			case CaptureListItemSelf(specifier, selfExp) => typecheckExp(selfExp, env, funcs)
			case _ => //nothing
		}
	}
	
	def typecheckClosureParameter(param: ClosureParameter): Unit = {
		param match {
			case ClosureParameterReg(closureParamName, optTypeAnno) => optTypeAnno match {
				case Some(typeAnno) => typecheckTypeAnnotation(typeAnno)
				case None => //nothing
			}
			case ClosureParameterElipses(closureParamName, typeAnno) => typecheckTypeAnnotation(typeAnno)
		}
	}
	
	def typecheckClosureParameterClause(clause: ClosureParameterClause): Unit = {
		clause match {
			case CPCIdentifierList(unkownIdentifierList) => //nothing
			case CPCClosureParameterList(cpList) => cpList.map(x => typecheckClosureParameter(x))
		}
	}
	
	def typecheckFunctionResult(res: FunctionResult): Unit = {
		res match {
			case FunctionResult(aList, theType) => typecheckType(theType)
		}
	}
	
	def typecheckClosureSignature(sig: ClosureSignature, env: TypeEnv, funcs: Funcs): Unit = {
		sig match {
			case ClosureSignatureComplex(optCaptureListItemList, clause, asynch, throws, optFuncResult) => {
				optCaptureListItemList match {
					case Some(captureListItemList) => captureListItemList.map(x => typecheckCaptureListItem(x, env, funcs))
					case None => //nothing
				}
				typecheckClosureParameterClause(clause)
				optFuncResult match {
					case Some(funcResult) => typecheckFunctionResult(funcResult)
					case None => //nothing
				}
			}
			case ClosureSignatureSimple(captureListItemList) => captureListItemList.map(x => typecheckCaptureListItem(x, env, funcs))
		}
	}
	
	def typecheckTupleElement(element: TupleElement, env: TypeEnv, funcs: Funcs): Unit = {
		element match {
			case ExpTuple(exp) => typecheckExp(exp, env, funcs)
			case IdentifierColonExpTuple(tupleElementName, exp) => typecheckExp(exp, env, funcs)
		}
	}
	
	def typecheckKeyPathComponent(component: KeyPathComponent, env: TypeEnv, funcs: Funcs): Unit = {
		component match {
			case IdentifierThenOptPostfixesKPC(keyPathComponentName, optKeyPathPostfixList) => optKeyPathPostfixList match {
				case Some(keyPathPostfixList) => keyPathPostfixList.map(x => typecheckKeyPathPostfix(x, env, funcs))
				case None => //nothing
			}
			case PostfixesKPC(keyPathPostfixList) => keyPathPostfixList.map(x => typecheckKeyPathPostfix(x, env, funcs))
		}
	}
	
	def typecheckKeyPathPostfix(postfix: KeyPathPostfix, env: TypeEnv, funcs: Funcs): Unit = {
		postfix match {
			//case QuestionKPP => //nothing
			//case ExclamationKPP => //nothing
			//case SelfKPP => //nothing
			case FuncCallArgListKPP(functionCallArgList) => functionCallArgList.map(x => typecheckFunctionCallArgument(x, env, funcs))
			case _ => //nothing
		}
	}
	
	def typecheckExpPairs(pair: (Exp, Exp), env: TypeEnv, funcs: Funcs): Unit = {
		pair match {
			case (x, y) => (typecheckExp(x, env, funcs), typecheckExp(y, env, funcs))
		}
	}
	
	def typecheckExp(exp: Exp, env: TypeEnv, funcs: Funcs): Unit = {
		exp match {
			case TryExp(modifier, nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case AwaitExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PrefixExp(op, nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PostfixWithOpExp(nestedExp, op) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PostfixForcedValueExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PostfixOptionalChainingExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PostfixFunctionCallExp(nestedExp, after) => {
				typecheckExp(nestedExp, env, funcs)
				typecheckPostfixFunctionCall(after, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PostfixSubscriptExp(nestedExp, functionCallArgList) => {
				typecheckExp(nestedExp, env, funcs)
				functionCallArgList.map(x => typecheckFunctionCallArgument(x, env, funcs))
				annotateExp(exp, env, funcs)
			}
			case PostfixInitializerExp(nestedExp, optArgNameList) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PostfixSelfExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case PostfixExplicitMemberExp(nestedExp, after) => {
				typecheckExp(nestedExp, env, funcs)
				//if the nestedExp is a variableexp
				//check if the exp is in the env, if so: if it has type Array(_) AND is mutable
				//remove the resolved type because it is not always safe to replace it
				nestedExp match {
					case VariableExp(varName) => {
						val (varType, mut) = env.getOrElse(varName, (None, None))
						(varType, mut) match {
							case (ArrayTyp(x), Mutable) => nestedExp.expType = None
							case _ => //nothing
						}
					}
					case _ => //nothing
				}
				typecheckExplicitMember(after)
				annotateExp(exp, env, funcs)
			}
			case InOutExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case CastExp(nestedExp, op) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case AssignmentExp(prefix, nestedExp) => {
				typecheckExp(prefix, env, funcs)
				typecheckExp(nestedExp, env, funcs)
				//we need to check if the two sides have the same annotated type.
				(prefix.expType, nestedExp.expType) match {
					case (Some(x), Some(y)) => if (x != y) { nestedExp.expType = prefix.expType } //if they're different then change rhs to the type of the lhs
					case _ => //all other cases we don't care about
				}
				annotateExp(exp, env, funcs)
			}
			case ConditionalExp(prefix, conditionalExp, nestedExp) => {
				typecheckExp(prefix, env, funcs)
				typecheckExp(conditionalExp, env, funcs)
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case TrueInfixExp(nestedExp1, op, nestedExp2) => {
				typecheckExp(nestedExp1, env, funcs)
				typecheckExp(nestedExp2, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case GenericExp(genericName, listTypes) => {
				listTypes.map(x => typecheckType(x))
				annotateExp(exp, env, funcs)
			}
			case literal: VariableExp => annotateExp(literal, env, funcs)
			case literal: ImplicitParameterExpOrPWP => annotateExp(literal, env, funcs)
			case literal: PropertyWrapperProjectionExp => annotateExp(literal, env, funcs)
			case literal: DecimalIntegerLiteralExp => annotateExp(literal, env, funcs)
			case literal: BinaryIntegerLiteralExp => annotateExp(literal, env, funcs)
			case literal: OctalIntegerLiteralExp => annotateExp(literal, env, funcs)
			case literal: HexIntegerLiteralExp => annotateExp(literal, env, funcs)
			case literal: DecimalFloatLiteralExp => annotateExp(literal, env, funcs)
			case literal: HexFloatLiteralExp => annotateExp(literal, env, funcs)
			case literal: CharStringExp => annotateExp(literal, env, funcs)
			case literal: SingleLineStaticStringLiteralExp => annotateExp(literal, env, funcs)
			case literal: InterpolatedStringLiteralExp => annotateExp(literal, env, funcs)
			case literal: MultiLineStaticStringLiteralExp => annotateExp(literal, env, funcs)
			case literal: TrueLiteralExp => annotateExp(literal, env, funcs)
			case literal: FalseLiteralExp => annotateExp(literal, env, funcs)
			case literal: NilExp => annotateExp(literal, env, funcs)
			case ArrayLiteralExp(expList) => {
				expList.map(x => typecheckExp(x, env, funcs))
				annotateExp(exp, env, funcs)
			}
			case DictionaryLiteralExp(expPairsList) => {
				expPairsList.map(x => typecheckExpPairs(x, env, funcs))
				annotateExp(exp, env, funcs)
			}
			case ColorPlaygroundLiteralExp(nestedExp1, nestedExp2, nestedExp3, nestedExp4) => {
				typecheckExp(nestedExp1, env, funcs)
				typecheckExp(nestedExp2, env, funcs)
				typecheckExp(nestedExp3, env, funcs)
				typecheckExp(nestedExp4, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case FilePlaygroundLiteralExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case ImagePlaygroundLiteralExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case theExp: SoloSelfExp => annotateExp(theExp, env, funcs)
			case theExp: MethodSelfExp => annotateExp(theExp, env, funcs)
			case SubscriptSelfExp(functionCallArgList) => {
				functionCallArgList.map(x => typecheckFunctionCallArgument(x, env, funcs))
				annotateExp(exp, env, funcs)
			}
			case theExp: InitSelfExp => annotateExp(theExp, env, funcs)
			case theExp: MethodSuperExp => annotateExp(theExp, env, funcs)
			case SubscriptSuperExp(functionCallArgList) => {
				functionCallArgList.map(x => typecheckFunctionCallArgument(x, env, funcs))
				annotateExp(exp, env, funcs)
			}
			case theExp: InitSuperExp => annotateExp(theExp, env, funcs)
			case ClosureExp(optAttributeList, optClosureSig, optStmtList) => {
				optStmtList match {
				case Some(stmtList) => stmtList.foldLeft((env, funcs))((curTuple, curStmt) => typecheckStmt(curStmt, curTuple._1, curTuple._2))
				case _ => //nothing
				}
				optClosureSig match {
					case Some(closureSig) => typecheckClosureSignature(closureSig, env, funcs)
					case None => //nothing
				}
				annotateExp(exp, env, funcs)
			}
			case ParenthesizedExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case TupleExp(tupleElementList) => {
				tupleElementList.map(x => typecheckTupleElement(x, env, funcs))
				annotateExp(exp, env, funcs)
			}
			case theExp: IdentifierImplicitMemberExp => annotateExp(theExp, env, funcs)
			case IdentifierDotPostfixImplicitMemberExp(implicitMemberName, postfixExp) => {
				typecheckExp(postfixExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case theExp: WildcardExp => annotateExp(theExp, env, funcs)
			case KeyPathExp(optType, keyPathComponentList) => {
				optType match {
					case Some(theType) => typecheckType(theType)
					case None => //nothing
				}
				keyPathComponentList.map(x => typecheckKeyPathComponent(x, env, funcs))
				annotateExp(exp, env, funcs)
			}
			case SelectorExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case SelectorGetterExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case SelectorSetterExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
			case KeyPathStringExp(nestedExp) => {
				typecheckExp(nestedExp, env, funcs)
				annotateExp(exp, env, funcs)
			}
		}
	}
	
	def annotateExp(exp: Exp, env: TypeEnv, funcs: Funcs): Unit = {
		//println("Current exp in TC: " + exp)
		expCount += 1
		exp.expType = typeOfExp(exp, env, funcs)
		exp.expType match {
			case Some(_) => expReplaceCount += 1
			case None => //nothing
		}
		exp.typeEnv = Some(env)
		exp.functions = Some(funcs)
		//println("With type of: " + exp.expType)
		//println("Curent env for that exp in TC: " + exp.typeEnv)
		//println("----------")
	}
	
	def isArithOp(op: Operator): Boolean = {
		op match {
			case Operator("+") => true
			case Operator("-") => true
			case Operator("*") => true
			case Operator("/") => true
			case _ => false
		}
	}
	
	def isEqualityOp(op: Operator): Boolean = {
		op match {
			case Operator("==") => true
			case Operator("!=") => true
			case _ => false
		}
	}
	
	def isComparisonOp(op: Operator): Boolean = {
		op match {
			case Operator(">") => true
			case Operator("<") => true
			case Operator(">=") => true
			case Operator("<=") => true
			case _ => false
		}
	}
	
	//currently only supporting "basic operators" from swift doc that only use num, bool, and string type
	//arithmetic operators
	//remainder operator
	//comparison operators
	//conditional ternary operator
	//logical operators (only AND and OR)
	def typeOfBinopExp(op: Operator, type1: Option[Typ], type2: Option[Typ]): Option[Typ] = {
		(op, type1, type2) match {
			case (op, Some(typ1), Some(typ2)) if (isArithOp(op)) => { //arithmetic operators
				(typ1, typ2) match {
					case (IntType, IntType) => Some(IntType)
					case (DoubleType, DoubleType) => Some(DoubleType)
					case (IntType, DoubleType) => Some(DoubleType)
					case (DoubleType, IntType) => Some(DoubleType)
					case _ => None
				}
			}
			case (Operator("+"), Some(StringType), Some(StringType)) => Some(StringType) //string concat
			case (Operator("%"), Some(IntType), Some(IntType)) => Some(IntType) //remainder operator
			case (op, Some(typ1), Some(typ2)) if (isEqualityOp(op) && (typ1 == typ2)) => Some(BoolType) //== and !=
			case (op, Some(typ1), Some(typ2)) if (isComparisonOp(op)) => { //rest of comparison ops
				(typ1, typ2) match {
					case (IntType, DoubleType) => Some(BoolType)
					case (DoubleType, IntType) => Some(BoolType)
					case (IntType, IntType) => Some(BoolType)
					case (DoubleType, DoubleType) => Some(BoolType)
					case _ => None
				}
			}
			case (Operator("&&"), Some(BoolType), Some(BoolType)) => Some(BoolType) //logical AND
			case (Operator("||"), Some(BoolType), Some(BoolType)) => Some(BoolType) //logical OR
			//not done
			case _ => None
		}
	}
	
	def typeOfTernaryExp(exp1Type: Option[Typ], exp2Type: Option[Typ], exp3Type: Option[Typ]): Option[Typ] = {
		(exp1Type, exp2Type, exp3Type) match {
			case (Some(BoolType), Some(typ1), Some(typ2)) if (typ1 == typ2) => Some(typ1)
			case _ => None
		}
	}
	
	def typeOfExp(exp: Exp, env: TypeEnv, funcs: Funcs): Option[Typ] = {
		exp match {
			case DecimalIntegerLiteralExp(_) => Some(IntType)
			case BinaryIntegerLiteralExp(_) => Some(IntType)
			case OctalIntegerLiteralExp(_) => Some(IntType)
			case HexIntegerLiteralExp(_) => Some(IntType)
			case DecimalFloatLiteralExp(_) => Some(DoubleType)
			case HexFloatLiteralExp(_) => Some(DoubleType)
			case TrueLiteralExp() => Some(BoolType)
			case FalseLiteralExp() => Some(BoolType)
			case CharStringExp(_) => Some(CharType)
			case SingleLineStaticStringLiteralExp(_) => Some(StringType)
			case MultiLineStaticStringLiteralExp(_) => Some(StringType)
			case TrueInfixExp(exp1, op, exp2) => typeOfBinopExp(op, exp1.expType, exp2.expType)
			case ConditionalExp(exp1, exp2, exp3) => typeOfTernaryExp(exp1.expType, exp2.expType, exp3.expType)
			case PrefixExp(op, exp) => typeOfExp(exp, env, funcs) 	//only prefix +, -, and !
			case AssignmentExp(exp1, exp2) => Some(VoidType)
			case VariableExp(name) => env.get(name).map(_._1)
			case ArrayLiteralExp(expList) => {
				if (expList.map(x => typeOfExp(x, env, funcs)).toSet.size == 1) {
					typeOfExp(expList.head, env, funcs) match {
						case Some(theType) => Some(ArrayTyp(theType))
						case None => None
					}
				} else { Some(AnyTyp) } 
			}
			case PostfixFunctionCallExp(exp, after) => {
				exp match {
					case VariableExp(name) => funcs.get(name).map(_._1)
					case _ => None
				}
			}
			case _ => None
		}
	}
}