package FuzzingSwift.printer

import FuzzingSwift.parser._

object PrettyPrinter {

	def apply(ast: Program): String = {
		printSemiColonSepList(ast.stmts)(printStmt _)
	}

	def printOptionalThing[A](thing: Option[A])(func: A => String): String = {
		thing match {
			case Some(theThing) => {
				func(theThing)
			}
			case None => ""
		}
	}

	def printCommaSepList[A](list: List[A])(func: A => String): String = {
		list.map(x=> func(x)).mkString(",")
	}

	def printDotSepList[A](list: List[A])(func: A => String): String = {
		list.map(x=> func(x)).mkString(".")
	}

	def printAmpSepList[A](list: List[A])(func: A => String): String = {
		list.map(x=> func(x)).mkString("&")
	}

	def printNonSepList[A](list: List[A])(func: A => String): String = {
		list.map(x=> func(x)).mkString(" ") + " "
	}
	
	def oldPrintSemiColonSepList[A](list: List[A])(func: A => String): String = {
		list.map(x=> func(x)).mkString(";")
	}
	
	def printSemiColonSepList[A](list: List[A])(func: A => String): String = {
		val theString = ""
		list.foldLeft(theString)((curString, curStmt) => if (curStmt match {case CompilerControlStmt(ctrl) => true
																			case CompilerControlExtensionMember(stmt) => true
																			case CompilerControlRVSEnumMember(stmt) => true
																			case CompilerControlEnumMember(stmt) => true
																			case CompilerControlStructMember(stmt) => true
																			case CompilerControlClassMember(stmt) => true
																			case CompilerControlActorMember(stmt) => true
																			case CompilerControlProtocolMember(stmt) => true
																			case _ => false}) curString + func(curStmt) + "\n" else curString + func(curStmt) + ";")
	}

	def printTryModifier(mod: TryModifier): String = {
		mod match {
			case RegTryModifier => "try"
			case QuestionTryModifier => "try?"
			case ExclamationTryModifier => "try!"
		}
	}

	def printPostfixFunctionCall(call: PostfixFunctionCall): String = {
		call match {
			case SimpleFunctionCall(functionCallArgList) => functionCallArgList match {
				case Nil => "()"
				case _ => "(" + printCommaSepList(functionCallArgList)(printFunctionCallArgument _) + ")"
			}
			case ComplexFunctionCall(optFunctionCallArgList, trailingClosure) => {
				val optFunctionCallArgListAsString = optFunctionCallArgList match {
					case None => ""
					case Some(list) => "(" + printCommaSepList(list)(printFunctionCallArgument _) + ")"
				}
				val trailingClosureAsString = printTrailingClosures(trailingClosure)
				optFunctionCallArgListAsString + " " + trailingClosureAsString
			}
		}
	}

	def printFunctionCallArgument(arg: FunctionCallArgument): String = {
		arg match {
			case ExpFunctionCallArgument(exp) => printExp(exp)
			case IdentifierColonExpFunctionCallArgument(functionCallArgName, exp) => functionCallArgName.name + ":" + printExp(exp)
			case OperatorFunctionCallArgument(op) => op.value
			case IdentifierColonOperatorFunctionCallArgument(functionCallArgName, op) => functionCallArgName.name + ":" + op.value
		}
	}

	def printTrailingClosures(closure: TrailingClosure): String = {
		val closureExpAsString = printExp(closure.closureExp)
		val listTrailingClosureAsString = closure.list match {
			case Some(theList) => theList.map(listItem => printLabeledTrailingClosure(listItem)).mkString(" ")
			case None => ""
		}
		closureExpAsString + listTrailingClosureAsString
	}

	def printLabeledTrailingClosure(closure: LabeledTrailingClosure): String = {
		val closureExpAsString = printExp(closure.closureExp)
		closure.trailingClosureLabel.name + ":" + closureExpAsString
	}

	def printExplicitMember(member: ExplicitMember): String = {
		member match {
			case ExplicitMemberDecimalDigits(num) => num
			case ExplicitMemberIdentifierOptGeneric(explicitMemberName, genericArgList) => {
				val optGenericArgList = genericArgList match {
					case None => ""
					case Some(list) => "<" + list.map(listItem => printType(listItem)).mkString(",") + ">"
				}
				explicitMemberName.name + optGenericArgList
			}
			case ExplicitMemberIdentifierArgs(explicitMemberName, argNames) =>
				explicitMemberName.name + "(" + argNames.map(listItem => (listItem.name)).mkString(":") + ":" + ")"
		}
	}

	def printTypeCastingOp(op: TypeCastingOp): String = {
		op match {
			case IsType(typ) => " is " + printType(typ)
			case AsType(typ) => " as " + printType(typ)
			case AsQuestionType(typ) => " as? " + printType(typ)
			case AsExclamationType(typ) => " as! " + printType(typ)
		}
	}

	def printGenericArgumentClause(clause: List[Type]): String = {
		val listAsString = clause.map(x => printType(x)).mkString(",")
		"<" + listAsString + ">"
	}

	def printAttribute(attribute: Attribute): String = {
		attribute match {
			case Attribute(attributeName, optBalancedTokenList) => optBalancedTokenList match {
				case Some(balancedTokenList) => "@" + attributeName.name + "(" + printNonSepList(balancedTokenList)(printBalancedToken _) + ") "
				case None => "@" + attributeName.name + " "
			}
		}
	}
	
	def printBalancedToken(bt: BalancedToken): String = {
		bt match {
			case InParensBalancedToken(token) => printBalancedToken(token)
			case InBracketsBalancedToken(token) => printBalancedToken(token)
			case InBracesBalancedToken(token) => printBalancedToken(token)
			case IdentifierBalancedToken(str) => str
			case KeywordBalancedToken(str) => str
			case LiteralBalancedToken(exp) => {
				val str = printExp(exp)
				str.substring(1, str.size-1)
			}
			case OperatorBalancedToken(str) => str
			case PunctuationBalancedToken(str) => str
		}
	}

	def printCaptureSpecifier(spec: CaptureSpecifier): String = {
		spec match {
			case WeakCaptureSpecifier => "weak"
			case UnownedCaptureSpecifier => "unowned"
			case UnownedSafeCaptureSpecifier => "unowned(safe)"
			case UnownedUnsafeCaptureSpecifier => "unowned(unsafe)"
		}
	}

	def printCaptureListItem(item: CaptureListItem): String = {
		item match {
			case CaptureListItemIdentifier(optCaptureSpecifier, captureListItemName) => {
				printOptionalThing(optCaptureSpecifier)(printCaptureSpecifier _) + " " + captureListItemName.name
			}
			case CaptureListItemAssignment(optCaptureSpecifier, captureListItemName, exp) => {
				printOptionalThing(optCaptureSpecifier)(printCaptureSpecifier _) + " " + captureListItemName.name + " = " + printExp(exp)
			}
			case CaptureListItemSelf(optCaptureSpecifier, selfExp) => {
				printOptionalThing(optCaptureSpecifier)(printCaptureSpecifier _) +  " " + printExp(selfExp)
			}
		}
	}

	def printInOutModifier(mod: InOutMod): String = {
		"inout "
	}

	def printTypeAnnotation(typeAnno: TypeAnnotation): String = {
		typeAnno match {
			case TypeAnnotation(optAttributesList, optInOutMod, typ) => {
				":" + printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optInOutMod)(printInOutModifier _) + printType(typ)
			}
		}
	}

	def printClosureParameter(param: ClosureParameter): String = {
		param match {
			case ClosureParameterReg(closureParamName, optTypeAnno) => {
				closureParamName.name + printOptionalThing(optTypeAnno)(printTypeAnnotation _)
			}
			case ClosureParameterElipses(closureParamName, typeAnno) => {
				closureParamName.name + printTypeAnnotation(typeAnno)
			}
		}
	}

	def printClosureParameterClause(clause: ClosureParameterClause): String = {
		clause match {
			case CPCIdentifierList(unkownIdentifierList) => {
				unkownIdentifierList.mkString(",")
			}
			case CPCClosureParameterList(closureParamList) => {
				closureParamList match {
					case Nil => "()"
					case _ => "(" + printCommaSepList(closureParamList)(printClosureParameter _) + ")"
				}
			}
		}
	}

	def printAsync(mod: AsyncMod): String = {
		"async "
	}

	def printThrows(mod: ThrowsMod): String = {
		"throws "
	}

	def printFunctionResult(res: FunctionResult): String = {
		res match {
			case FunctionResult(optAttributesList, typ) => {
				"->" + printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printType(typ)
			}
		}
	}

	def printClosureSignature(sig: ClosureSignature): String = {
		sig match {
			case ClosureSignatureComplex(optCaptureListItemList, closureParamClause, optAsync, optThrows, optFunctionResult) => optCaptureListItemList match {
				case Some(captureListItemList) => "[" + printCommaSepList(captureListItemList)(printCaptureListItem _) + "]" + printClosureParameterClause(closureParamClause) +
					printOptionalThing(optAsync)(printAsync _) + printOptionalThing(optThrows)(printThrows _) + printOptionalThing(optFunctionResult)(printFunctionResult _) + " in "
				case None => printClosureParameterClause(closureParamClause) + printOptionalThing(optAsync)(printAsync _) + printOptionalThing(optThrows)(printThrows _) + 
					printOptionalThing(optFunctionResult)(printFunctionResult _) + " in "
			}
			case ClosureSignatureSimple(captureListItemList) => {
				"[" + printCommaSepList(captureListItemList)(printCaptureListItem _) + "] in "
			}
		}
	}

	def printTupleElement(element: TupleElement): String = {
		element match {
			case ExpTuple(exp) => printExp(exp)
			case IdentifierColonExpTuple(tupleElementName, exp) => tupleElementName.name + ":" + printExp(exp)
		}
	}

	def printKeyPathPostfix(postfix: KeyPathPostfix): String = {
		postfix match {
			case QuestionKPP => "?"
			case ExclamationKPP => "!"
			case SelfKPP => "self"
			case FuncCallArgListKPP(functionCallArgList) => "[" + printCommaSepList(functionCallArgList)(printFunctionCallArgument _) + "]"
		}
	}

	def printKeyPathComponent(component: KeyPathComponent): String = {
		component match {
			case IdentifierThenOptPostfixesKPC(keyPathComponentName, optKeyPathPostfixList) => {
				keyPathComponentName.name + printOptionalThing(optKeyPathPostfixList)(printNonSepList(_)(printKeyPathPostfix _))
			}
			case PostfixesKPC(keyPathPostfixList) => printNonSepList(keyPathPostfixList)(printKeyPathPostfix _)
		}
	}
	
	def removeParens(exp: Exp): String = {
		val expAsString = printExp(exp)
		if ((expAsString.head == '(') && (expAsString.last == ')')) {
			expAsString.substring(1, expAsString.size-1)
		} else {
			expAsString
		}
	}

	//should make a helper function that removes parens bcuz there are multiple instances below where that needs to be done
	def printExp(exp: Exp): String = {
		exp match {
			case TryExp(tryModifier, exp) => "(" + printTryModifier(tryModifier) + printExp(exp) + ")"
			case AwaitExp(exp) => "(" + "await" + printExp(exp) + ")"
			case PrefixExp(op, exp) => {
				if (op.value == "-") {
					val theExp = printExp(exp)
					"(" + op.value + theExp.substring(1, theExp.length-1) + ")"
				}
				else {
					"(" + op.value + printExp(exp) + ")"
				}
			}
			case PostfixWithOpExp(postfixExp, op) => { //included in issue with parens
				"(" + removeParens(postfixExp) + op.value + ")"
			}
			case PostfixForcedValueExp(postfixExp) => "(" + printExp(postfixExp) + "!" + ")" //included in issue with parens
			case PostfixOptionalChainingExp(postfixExp) => printExp(postfixExp) + "?" //included in issue with parens
			case PostfixFunctionCallExp(postfixExp, postfixFunctionCall) => { //included in issue with parens
				"(" + removeParens(postfixExp) + printPostfixFunctionCall(postfixFunctionCall) + ")"
			}
			case PostfixSubscriptExp(postfixExp, list) => "(" + printExp(postfixExp) + "[" + printCommaSepList(list)(printFunctionCallArgument _) + "]" + ")" //included in issue with parens
			case PostfixInitializerExp(postfixExp, optList) => { //included in issue with parens
				val optListAsString = optList match {
				case None => ""
				case Some(list) => "(" + list.map(listItem => (listItem.name)).mkString(":") + ":" + ")"
				}
				"(" + printExp(postfixExp) + ".init" + optListAsString + ")"
			}
			case PostfixSelfExp(postfixExp) => "(" + removeParens(postfixExp) + ".self" + ")" //included in issue with parens
			case PostfixExplicitMemberExp(postfixExp, explicitMember) => {
				postfixExp match {
					case infix: TrueInfixExp => "(" + printExp(postfixExp) + "." + printExplicitMember(explicitMember) + ")"
					case _ => "(" + removeParens(postfixExp) + "." + printExplicitMember(explicitMember) + ")" //included in issue with parens
				}
			}
			case InOutExp(exp) => {
				val expAsString = printExp(exp)
				if (expAsString.head == ' ') {
					"&" + expAsString.substring(1, expAsString.size)
				} else {
					"&" + removeParens(exp)
				}
			}
			case CastExp(exp, typeCastOp) => "(" + printExp(exp) + printTypeCastingOp(typeCastOp) + ")"
			case AssignmentExp(prefix, exp) => "(" + printExp(prefix) + " = " + printExp(exp) + ")"
			case ConditionalExp(prefix, conditionalExp, exp) => "((" + printExp(prefix) + " ? " + printExp(conditionalExp) + ":" + printExp(exp) + "))"
			case TrueInfixExp(exp1, op, exp2) => {
				"(" + printExp(exp1) + " " + op.value + " " + printExp(exp2) + ")"
				// op match {
				// 	case Operator("!=") => "(" + printExp(exp1) + " " + op.value + " " + printExp(exp2) + ")"
				// 	case Operator("??") => "(" + printExp(exp1) + " " + op.value + " " + printExp(exp2) + ")"
				// 	case _ => "(" + printExp(exp1) + op.value + printExp(exp2) + ")"
				// }
			}
			case GenericExp(genericName, listTypes) => "(" + genericName.name + printGenericArgumentClause(listTypes) + ")"
			case VariableExp(value) => "(" + value + ")"
			case ImplicitParameterExpOrPWP(value) => "(" + value + ")"
			case PropertyWrapperProjectionExp(value) => "(" + value + " )"
			case DecimalIntegerLiteralExp(value) => "(" + value + ")"
			case BinaryIntegerLiteralExp(value) => "(" + value + ")"
			case OctalIntegerLiteralExp(value) => "(" + value + ")"
			case HexIntegerLiteralExp(value) => "(" + value + ")"
			case DecimalFloatLiteralExp(value) => "(" + value + ")"
			case HexFloatLiteralExp(value) => "(" + value + ")"
			case CharStringExp(value) => "(" + "\"" + value + "\"" + ")"
			case SingleLineStaticStringLiteralExp(value) => "(" + "\"" + value + "\"" + ")"
			case MultiLineStaticStringLiteralExp(value) => "(" + "\"\"\"" + value + "\"\"\"" + ")"
			case InterpolatedStringLiteralExp(value) => "(" + "\"" + value + "\"" + ")"
			case TrueLiteralExp() => "(" + "true" + ")"
			case FalseLiteralExp() => "(" + "false" + ")"
			case NilExp() => "(" + "nil" + ")"
			case ArrayLiteralExp(expList) => "(" + "[" + expList.map(listItem => printExp(listItem)).mkString(",") + "]" + ")"
			case DictionaryLiteralExp(expPairsList) => {
				val listAsString = expPairsList match {
					case Nil => ":"
					case _ => expPairsList.map(listItem => printExp(listItem._1) + ":" + printExp(listItem._2)).mkString(",")
				}
				"(" + "[" + listAsString + "]" + ")"
			}
			case ColorPlaygroundLiteralExp(exp1, exp2, exp3, exp4) => "(" + "#colorLiteral(red:" + printExp(exp1) + ",green:" + printExp(exp2) + ",blue:" + printExp(exp3) +
																		",alpha:" + printExp(exp4) + ")" + ")"
			case FilePlaygroundLiteralExp(exp) => "(" + "#fileLiteral(resourceName:" + printExp(exp) + ")" + ")"
			case ImagePlaygroundLiteralExp(exp) => "(" + "#imageLiteral(resourceName:" + printExp(exp) + ")" + ")"
			case SoloSelfExp() => " " + "self"//having issue: some progs need space after and others cannot have
			case MethodSelfExp(selfMethodName) => "(" + "self." + selfMethodName.name + ")"
			case SubscriptSelfExp(functionCallArgList) => "(" + "self[" + printCommaSepList(functionCallArgList)(printFunctionCallArgument _) + "]" + ")"
			case InitSelfExp() => "(" + "self.init" + ")"
			case MethodSuperExp(superMethodName) => "(" + "super." + superMethodName.name + ")"
			case SubscriptSuperExp(functionCallArgList) => "(" + "super[" + printCommaSepList(functionCallArgList)(printFunctionCallArgument _) + "]" + ")"
			case InitSuperExp() => "(" + "super.init" + ")"
			case ClosureExp(optAttributesList, optClosureSig, optStmtList) => "{" + printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optClosureSig)(printClosureSignature _) +
																				printOptionalThing(optStmtList)(printSemiColonSepList(_)(printStmt _)) + "}"
			case ParenthesizedExp(exp) => "(" + printExp(exp) + ")"
			case TupleExp(tupleElementList) => {
				tupleElementList match {
					case Nil => "()"
					case _ => "(" + printCommaSepList(tupleElementList)(printTupleElement _) + ")"
				}
			}
			case IdentifierImplicitMemberExp(implicitMemberName) => "(" + "." + implicitMemberName.name + ")"
			case IdentifierDotPostfixImplicitMemberExp(implicitMemberName, postfixExp) => "(" + "." + implicitMemberName.name + "." + printExp(postfixExp) + ")"
			case WildcardExp() => "(" + "_" + ")"
			case KeyPathExp(optType, keyPathComponentList) => "(" + "\\" + printOptionalThing(optType)(printType _) + "." + printDotSepList(keyPathComponentList)(printKeyPathComponent _) + ")"
			case SelectorExp(exp) => "(" + "#selector" + printExp(exp) + ")"
			case SelectorGetterExp(exp) => "(" + "#selector(getter:" + printExp(exp) + "))"
			case SelectorSetterExp(exp) => "(" + "#selector(setter:" + printExp(exp) + "))"
			case KeyPathStringExp(exp) => "#keyPath(" + removeParens(exp) + ")"
		}
	}

	def printFunctionTypeArg(arg: FunctionTypeArg): String = {
		arg match {
			case FunctionTypeArg1(optAttributesList, optInOutMod, typ) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optInOutMod)(printInOutModifier _) + printType(typ)
			}
			case FunctionTypeArg2(argumentLabel, typeAnno) => {
				argumentLabel.name + printTypeAnnotation(typeAnno)
			}
		}
	}

	//elipses missing?
	def printFunctionTypeClause(list: List[FunctionTypeArg]): String = {
		list match {
			case Nil => "()"
			case _ => "(" + printCommaSepList(list)(printFunctionTypeArg _) + ")"
		}
	}

	def printTupleTypeElement(element: TupleTypeElement): String = {
		element match {
			case TupleTypeElementNameAnnotation(elementName, typeAnno) => elementName.name + printTypeAnnotation(typeAnno)
			case TupleTypeElementType(typ) => printType(typ)
		}
	}

	def printType(typ: Type): String = {
		typ match {
			case FunctionType(optAttributesList, functionTypeArgList, optAsync, optThrows, typ) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printFunctionTypeClause(functionTypeArgList) + printOptionalThing(optAsync)(printAsync _) +
					printOptionalThing(optThrows)(printThrows _) + "->" + printType(typ)
			}
			case ArrayType(typ) => "[" + printType(typ) + "]"
			case DictionaryType(type1, type2) => "[" + printType(type1) + ":" + printType(type2) + "]"
			case NormalTypeIdentifier(typeName) => typeName.name
			case GenericTypeIdentifier(typeName, typeList) => typeName.name + printGenericArgumentClause(typeList)
			case NestedNormalTypeIdentifier(typeName, nestedType) => typeName.name + "." + printType(nestedType)
			case NestedGenericTypeIdentifier(typeName, genericTypeList, nestedType) => typeName.name + printGenericArgumentClause(genericTypeList) + "." + printType(nestedType)
			case TupleType(tupleTypeElementList) => {
				tupleTypeElementList match {
					case Nil => "()"
					case _ => "(" + printCommaSepList(tupleTypeElementList)(printTupleTypeElement _) + ")"
				}
			}
			case ProtocolCompositionType(typeList) => printAmpSepList(typeList)(printType _)
			case OpaqueType(typ) => "some " + printType(typ)
			case AnyType => "Any"
			case SelfType => "Self"
			case InParensType(typ) => "(" + printType(typ) + ")"
			case OptionalType(typ) => printType(typ) + "?"
			case ImplicitlyUnwrappedOptionalType(typ) => printType(typ) + "!"
			case MetatypeTypeType(typ) => printType(typ) + ".Type"
			case MetatypeProtocolType(typ) => printType(typ) + ".Protocol"
			case BoxedProtocolType(typ) => "any " + printType(typ)
		}
	}

	def printMutableMod(mod: MutableModifier): String = {
		mod match {
			case VarModifier => "var "
			case LetModifier => "let "
		}
	}

	def printTuplePatternElement(element: TuplePatternElement): String = {
		element match {
			case PatternElement(pattern) => printPattern(pattern)
			case IdentifierPatternElement(patternName, pattern) => patternName.name + ":" + printPattern(pattern)
		}
	}

	def printPattern(pattern: Pattern): String = {
		pattern match {
			case WildcardPattern(optTypeAnno) => "_" + printOptionalThing(optTypeAnno)(printTypeAnnotation _)
			case IdentifierPattern(patternName, optTypeAnno) => patternName.name + printOptionalThing(optTypeAnno)(printTypeAnnotation _)
			case ValueBindingPattern(mod, pattern) => printMutableMod(mod) + printPattern(pattern)
			case TuplePattern(tuplePatternElementList, optTypeAnno) => {
				val list = tuplePatternElementList match {
					case Nil => "()"
					case _ => "(" + printCommaSepList(tuplePatternElementList)(printTuplePatternElement _) + ")"
				}
				list + printOptionalThing(optTypeAnno)(printTypeAnnotation _)
			}
			case EnumCasePattern(optTypeIdentifier, enumCaseName, optPattern) => printOptionalThing(optTypeIdentifier)(printType _) + "." + enumCaseName.name + printOptionalThing(optPattern)(printPattern _)
			case OptionalPattern(pattern) => printPattern(pattern) + "?"
			case ExpressionPattern(exp) => printExp(exp)
			case IsTypeCastingPattern(typ) => " is " + printType(typ)
			case AsTypeCastingPattern(pattern, typ) => printPattern(pattern) + " as " + printType(typ)
		}
	}

	def printImportKind(kind: ImportKind): String = {
		kind match {
			case TypeAliasKind => "typealias "
			case StructKind => "struct "
			case ClassKind => "class "
			case EnumKind => "enum "
			case ProtocolKind => "protocol "
			case LetKind => "let "
			case VarKind => "var "
			case FuncKind => "func "
		}
	}

	def printImportPath(path: ImportPath): String = {
		path match {
			case RegularPath(pathName) => pathName.name
			case NestedPath(pathName, importPath) => pathName.name + "." + printImportPath(importPath)
		}
	}

	def printAccessLevelMod(mod: AccessLevelMod): String = {
		mod match {
			case PrivateModifier => "private "
			case PrivateSetModifier => "private(set) "
			case FilePrivateModifier => "fileprivate "
			case FilePrivateSetModifier => "fileprivate(set) "
			case InternalModifier => "internal "
			case InternalSetModifier => "internal(set) "
			case PublicModifier => "public "
			case PublicSetModifier => "public(set) "
			case OpenModifier => "open "
			case OpenSetModifier => "open(set) "
		}
	}

	def printMutationMod(mod: MutationMod): String = {
		mod match {
			case MutatingModifier => "mutating "
			case NonMutatingModifier => "nonmutating "
		}
	}

	def printDeclarationMod(mod: DeclarationModifier): String = {
		mod match {
			case AccessLevelModifier(accessLevelMod) => printAccessLevelMod(accessLevelMod)
			case ActorIsolationModifier => "nonisolated "
			case MutationModifier(mutationMod) => printMutationMod(mutationMod)
			case ClassModifier => "class "
			case ConvenienceModifier => "convenience "
			case DynamicModifier => "dynamic "
			case FinalModifier => "final "
			case InfixModifier => "infix "
			case LazyModifier => "lazy "
			case OptionalModifier => "optional "
			case OverrideModifier => "override "
			case PostfixModifier => "postfix "
			case PrefixModifier => "prefix "
			case RequiredModifier => "required "
			case StaticModifier => "static "
			case UnownedModifier => "unowned "
			case UnownedSafeModifier => "unowned(safe) "
			case UnownedUnsafeModifier => "unowned(unsafe) "
			case WeakModifier => "weak "
		}
	}

	def printPatternInitializer(init: PatternInitializer): String = {
		init match {
			case PatternInitializer(pattern, optExp) => {
				optExp match {
					case Some(exp) => printPattern(pattern) + " = " + printExp(exp)
					case None => printPattern(pattern)
				}
			}
		}
	}

	def printGetterClause(clause: GetterClause): String = {
		clause match {
			case GetterClause(optAttributesList, optMutationMod, codeBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optMutationMod)(printMutationMod _) + "get " + printGetterSetterBlock(codeBlock)
			}
		}
	}

	def printSetterClause(clause: SetterClause): String = {
		clause match {
			case SetterClause(optAttributesList, optMutationMod, optSetterName, codeBlock) => {
				optSetterName match {
					case Some(name) => printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optMutationMod)(printMutationMod _) + "set " +
						 				"(" + name.name + ")" + printGetterSetterBlock(codeBlock)
					case None => printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optMutationMod)(printMutationMod _) + "set " + printGetterSetterBlock(codeBlock)
				}
			}
		}
	}

	def printGetterSetterBlock(block: GetterSetterBlock): String = {
		block match {
			case CodeBlock(optStmtList) => "{" + printOptionalThing(optStmtList)(printSemiColonSepList(_)(printStmt _)) + "}"
			case GetterSetterClauseBlock(getterClause, optSetterClause) => "{" + printGetterClause(getterClause) + printOptionalThing(optSetterClause)(printSetterClause _) + "}"
			case SetterGetterClauseBlock(setterClause, getterClause) => "{" + printSetterClause(setterClause) + printGetterClause(getterClause) + "}"
		}
	}

	def printGetterKeywordClause(clause: GetterKeywordClause): String = {
		clause match {
			case GetterKeywordClause(optAttributesList, optMutationMod) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optMutationMod)(printMutationMod _) + "get "
			}
		}
	}

	def printSetterKeywordClause(clause: SetterKeywordClause): String = {
		clause match {
			case SetterKeywordClause(optAttributesList, optMutationMod) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optMutationMod)(printMutationMod _) + "set "
			}
		}
	}

	def printKeywordBlock(block: KeywordBlock): String = {
		block match {
			case GetterSetterKeywordBlock(getterKeywordClause, optSetterKeywordClause) => {
				"{" + printGetterKeywordClause(getterKeywordClause) + printOptionalThing(optSetterKeywordClause)(printSetterKeywordClause _) + "}"
			}
			case SetterGetterKeywordBlock(setterKeywordClause, getterKeywordClause) => {
				"{" + printSetterKeywordClause(setterKeywordClause) + printGetterKeywordClause(getterKeywordClause) + "}"
			}
		}
	}
	
	def printSetterName(name: SetterName): String = { "(" + name.name + ")" }

	def printWillSetClause(clause: WillSetClause): String = {
		clause match {
			case WillSetClause(optAttributesList, optMod, optSetterName, codeBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optMod)(printMutationMod _) + "willSet " + printOptionalThing(optSetterName)(printSetterName _) + printGetterSetterBlock(codeBlock)
			}
		}
	}

	def printDidSetClause(clause: DidSetClause): String = {
		clause match {
			case DidSetClause(optAttributesList, optMod, optSetterName, codeBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optMod)(printMutationMod _) + "didSet " + printOptionalThing(optSetterName)(printSetterName _) + printGetterSetterBlock(codeBlock)
			}
		}
	}

	def printSetBlock(block: SetBlock): String = {
		block match {
			case WillDidSetBlock(willSetClause, optDidSetClause) => {
				"{" + printWillSetClause(willSetClause) + printOptionalThing(optDidSetClause)(printDidSetClause _) + "}"
			}
			case DidWillSetBlock(didSetClause, optWillSetBlock) => {
				"{" + printDidSetClause(didSetClause) + printOptionalThing(optWillSetBlock)(printWillSetClause _) + "}"
			}
		}
	}

	def printGenericParameter(param: GenericParameter): String = {
		param match {
			case SimpleGenericParameter(typeName) => typeName.name
			case AnnotatedGenericParameter(typeName, typ) => typeName.name + ":" + printType(typ)
			case ProtocolCompGenericParameter(typeName, typ) => typeName.name + ":" + printType(typ)
		}
	}

	def printGenericParameterClause(list: List[GenericParameter]): String = {
		"<" + printCommaSepList(list)(printGenericParameter _) + ">"
	}

	def printFuncHead(head: FunctionHead): String = {
		head match {
			case FunctionHead(optAttributesList, optDeclarationModList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) + "func "
			}
		}
	}

	def printFuncName(name: FunctionName): String = {
		name match {
			case IdentifierFunctionName(funcName) => funcName
			case OperatorFunctionName(op) => op.value
		}
	}
	
	def printExternalParamName(name: ExternalParamName): String = name.name + " "

	def printParameter(param: Parameter): String = {
		param match {
			case OptDefaultArgClauseParameter(optExternalParamName, localParamName, typeAnno, optExp) => {
				optExp match {
					case Some(exp) => printOptionalThing(optExternalParamName)(printExternalParamName _) + localParamName.name + printTypeAnnotation(typeAnno) + " = " + printExp(exp)
					case None => printOptionalThing(optExternalParamName)(printExternalParamName _) + localParamName.name + printTypeAnnotation(typeAnno)
				}
			}
			case ElipsesParameter(optExternalParamName, localParamName, typeAnno) => printOptionalThing(optExternalParamName)(printExternalParamName _) + localParamName.name + printTypeAnnotation(typeAnno) + "..."
		}
	}

	def printParamClause(clause: List[Parameter]): String = {
		clause match {
			case Nil => "()"
			case _ => "(" + printCommaSepList(clause)(printParameter _) + ")"
		}
	}

	def printFuncSig(sig: FunctionSignature): String = {
		sig match {
			case ThrowsFunctionSig(paramList, optAsync, optThrows, optFuncResult) => {
				printParamClause(paramList) + printOptionalThing(optAsync)(printAsync _) + printOptionalThing(optThrows)(printThrows _) + printOptionalThing(optFuncResult)(printFunctionResult _)
			}
			case RethrowsFunctionSig(paramList, optAsync, optFuncResult) => {
				printParamClause(paramList) + printOptionalThing(optAsync)(printAsync _) + "rethrows" + printOptionalThing(optFuncResult)(printFunctionResult _)
			}
		}
	}

	def printRequirement(req: Requirement): String = {
		req match {
			case ConformanceRequirementDoubleTypeID(type1, type2) => printType(type1) + ":" + printType(type2)
			case ConformanceRequirementTypeIDProtocolCompType(type1, type2) => printType(type1) + ":" + printType(type2)
			case SameTypeRequirement(type1, type2) => printType(type1) + "==" + printType(type2)
		}
	}

	def printGenericWhereClause(clause: List[Requirement]): String = {
		" where " + printCommaSepList(clause)(printRequirement _)
	}

	def printTypeInheritance(typeInheritance: TypeInheritance): String = {
		typeInheritance match {
			case TypeInheritance(optAttributesList, typ) => printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printType(typ)
		}
	}

	def printTypeInheritanceClause(clause: List[TypeInheritance]): String = {
		":" + printCommaSepList(clause)(printTypeInheritance _)
	}

	def printIndirect(mod: IndirectMod): String = { "indirect " }

	def printUnionStyleEnumCase(usCase: UnionStyleEnumCase): String = {
		usCase match {
			case UnionStyleEnumCase(enumCaseName, optType) => enumCaseName.name + printOptionalThing(optType)(printType _)
		}
	}

	def printUnionStyleEnumMember(member: UnionStyleEnumMember): String = {
		member match {
			case DeclarationUSEnumMember(decl) => printDeclaration(decl)
			case EnumCaseClauseUSEnumMember(optAttributesList, optIndirect, caseList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optIndirect)(printIndirect _) + "case " +printCommaSepList(caseList)(printUnionStyleEnumCase _)
			}
			case CompilerControlEnumMember(stmt) => printStmt(stmt)
		}
	}

	def printRawValue(value: RawValue): String = {
		value match {
			case NumericLiteralRawValue(num) => num
			case StaticStringLiteralRawValue(string) => "\"" + string + "\""
			case BooleanTrueLiteralRawValue => "true"
			case BooleanFalseLiteralRawValue => "false"
		}
	}

	def printRawValueStyleEnumCase(rvsCase: RawValueStyleEnumCase): String = {
		rvsCase match {
			case RawValueStyleEnumCase(enumCaseName, optRawValue) => {
				optRawValue match {
					case Some(value) => enumCaseName.name + "=" + printRawValue(value)
					case None => enumCaseName.name
				}
			}
		}
	}

	def printRawValueStyleEnumMember(member: RawValueStyleEnumMember): String = {
		member match {
			case DeclarationRVSEnumMember(decl) => printDeclaration(decl)
			case EnumCaseClauseRVSEnumMember(optAttributesList, caseList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + "case " + printCommaSepList(caseList)(printRawValueStyleEnumCase _)
			}
			case CompilerControlRVSEnumMember(stmt) => printStmt(stmt)
		}
	}

	def printStructMember(member: StructMember): String = {
		member match {
			case DeclarationStructMember(decl) => printDeclaration(decl)
			case CompilerControlStructMember(stmt) => printStmt(stmt)
		}
	}
	
	def printClassMember(member: ClassMember): String = {
		member match {
			case DeclarationClassMember(decl) => printDeclaration(decl)
			case CompilerControlClassMember(stmt) => printStmt(stmt)
		}
	}
	
	def printActorMember(member: ActorMember): String = {
		member match {
			case DeclarationActorMember(decl) => printDeclaration(decl)
			case CompilerControlActorMember(stmt) => printStmt(stmt)
		}
	}
	
	def printFinal(mod: FinalMod): String = { "final " }
	
	def printInitializerHead(head: InitializerHead): String = {
		head match {
			case RegInitHead(optAttributesList, optDeclarationModList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) + "init"
			}
			case QuestionInitHead(optAttributesList, optDeclarationModList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) + "init?"
			}
			case ExclamationInitHead(optAttributesList, optDeclarationModList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) + "init!"
			}
		}
	}
	
	def printSubscriptHead(head: SubscriptHead): String = {
		head match {
			case SubscriptHead(optAttributesList, optDeclarationModList, optGenericParamClause, paramClause) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) + "subscript" +
				printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printParamClause(paramClause)
			}
		}
	}
	
	def printSubscriptResult(result: SubscriptResult): String = {
		result match {
			case SubscriptResult(optAttributesList, typ) => {
				"->" + printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printType(typ)
			}
		}
	}
	
	def printProtocolMember(member: ProtocolMember): String = {
		member match {
			case ProtocolPropertyDeclaration(optAttributesList, optDeclarationModList, varName, typeAnno, keywordBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
				"var " + varName.name + printTypeAnnotation(typeAnno) + printKeywordBlock(keywordBlock)
			}
			case ProtocolMethodDeclaration(funcHead, funcName, optGenericParamClause, funcSig, optGenericWhereClause) => {
				printFuncHead(funcHead) + printFuncName(funcName) + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printFuncSig(funcSig) +
				printOptionalThing(optGenericWhereClause)(printGenericWhereClause _)
			}
			case ThrowsProtocolInitializerDeclaration(initHead, optGenericParamClause, paramClause, optThrows, optGenericWhereClause) => {
				printInitializerHead(initHead) + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printParamClause(paramClause) + printOptionalThing(optThrows)(printThrows _) +
				printOptionalThing(optGenericWhereClause)(printGenericWhereClause _)
			}
			case RethrowsProtocolInitializerDeclaration(initHead, optGenericParamClause, paramClause, optGenericWhereClause) => {
				printInitializerHead(initHead) + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printParamClause(paramClause) + "rethrows " + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _)
			}
			case ProtocolSubscriptDeclaration(subscriptHead, subscriptResult, optGenericWhereClause, keywordBlock) => {
				printSubscriptHead(subscriptHead) + printSubscriptResult(subscriptResult) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _ ) + printKeywordBlock(keywordBlock)
			}
			case ProtocolAssociatedTypeDeclaration(optAttributesList, optAccessMod, typeAliasName, optTypeInheritanceClause, optType, optGenericWhereClause) => {
				optType match {
					case Some(typ) => {
						printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + "associatedtype " + typeAliasName.name + printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) + "=" +
						printType(typ) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _)
					}
					case None => {
						printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + "associatedtype " + typeAliasName.name + printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) +
						printOptionalThing(optGenericWhereClause)(printGenericWhereClause _)
					}
				}
 			}
			case ProtocolTypeAliasDeclaration(decl) => printDeclaration(decl)
			case CompilerControlProtocolMember(stmt) => printStmt(stmt)
		}
	}
	
	def printExtensionMember(member: ExtensionMember): String = {
		member match {
			case DeclarationExtensionMember(decl) => printDeclaration(decl)
			case CompilerControlExtensionMember(stmt) => printStmt(stmt)
		}
	}
	
	def printAllowableSubscriptBlock(block: AllowableSubscriptBlock): String = {
		block match {
			case AllowableGetterSetterBlock(block) => printGetterSetterBlock(block)
			case AllowableKeywordBlock(block) => printKeywordBlock(block) 
		}
	}
	
	def printPrecedenceGroupName(name: PrecedenceGroupName): String = { name.name }
	
	def printPrecedenceGroupAttribute(attribute: PrecedenceGroupAttribute): String = {
		attribute match {
			case PrecedenceGroupRelationHigher(precedenceGroupNameList) => "higherThan: " + printCommaSepList(precedenceGroupNameList)(printPrecedenceGroupName _)
			case PrecedenceGroupRelationLower(precedenceGroupNameList) => "lowerThan: " + printCommaSepList(precedenceGroupNameList)(printPrecedenceGroupName _)
			case PrecedenceGroupAssignmentTrue => "assignment: true"
			case PrecedenceGroupAssignmentFalse => "assignment: false"
			case PrecedenceGroupLeftAssociative => "associativity: left"
			case PrecedenceGroupRightAssociative => "associativity: right"
			case PrecedenceGroupNotAssociative => "associativity: none"
		}
	}

	def printDeclaration(decl: Declaration): String = {
		decl match {
			case ImportDeclaration(optAttributesList, optImportKind, importPath) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + "import " + printOptionalThing(optImportKind)(printImportKind _) +
					printImportPath(importPath)
			}
			case ConstantDeclaration(optAttributesList, optDeclarationModList, patternInitList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
					"let " + printCommaSepList(patternInitList)(printPatternInitializer _)
			}
			case VariableDeclaration1(optAttributesList, optDeclarationModList, patternInitList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
					"var " + printCommaSepList(patternInitList)(printPatternInitializer _)
			}
			case VariableDeclaration23(optAttributesList, optDeclarationModList, varName, typeAnno, getterSetterBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
					"var " + varName.name + printTypeAnnotation(typeAnno) + printGetterSetterBlock(getterSetterBlock)
			}
			case VariableDeclaration4(optAttributesList, optDeclarationModList, varName, typeAnno, keywordBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
					"var " + varName.name + printTypeAnnotation(typeAnno) + printKeywordBlock(keywordBlock)
			}
			case VariableDeclaration5(optAttributesList, optDeclarationModList, varName, exp, setBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
					"var " + varName.name + " = " + printExp(exp) + printSetBlock(setBlock)
			}
			case VariableDeclaration6(optAttributesList, optDeclarationModList, varName, typeAnno, optExp, setBlock) => {
				optExp match {
					case Some(exp) => {
						printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
							"var " + varName.name + printTypeAnnotation(typeAnno) + " = " + printExp(exp) + printSetBlock(setBlock)
					}
					case None => {
						printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optDeclarationModList)(printNonSepList(_)(printDeclarationMod _)) +
							"var " + varName.name + printTypeAnnotation(typeAnno) + printSetBlock(setBlock)
					}
				}
			}
			case TypeAliasDeclaration(optAttributesList, optAccessMod, typeAliasName, optGenericParamList, typ) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + "typealias " + typeAliasName.name +
					printOptionalThing(optGenericParamList)(printGenericParameterClause _) + "=" + printType(typ)
			}
			case FunctionDeclaration(funcHead, funcName, optGenericParamClause, funcSig, optGenericWhereClause, optCodeBlock) => {
				printFuncHead(funcHead) + printFuncName(funcName) + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printFuncSig(funcSig) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) +
					printOptionalThing(optCodeBlock)(printGetterSetterBlock _)
			}
			case UnionStyleEnumDeclaration(optAttributesList, optAccessMod, optIndirect, enumName, optGenericParamClause, optTypeInheritanceClause, optGenericWhereClause, optUSEnumMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + printOptionalThing(optIndirect)(printIndirect _) +
					"enum " + enumName.name + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) +
					printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + "{" + printOptionalThing(optUSEnumMemberList)(printSemiColonSepList(_)(printUnionStyleEnumMember _)) + "}"
			}
			case RawValueStyleEnumDeclaration(optAttributesList, optAccessMod, enumName, optGenericParamClause, typeInheritanceClause, optGenericWhereClause, rvsEnumMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + "enum " + enumName.name +
				printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printTypeInheritanceClause(typeInheritanceClause) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) +
				"{" + printSemiColonSepList(rvsEnumMemberList)(printRawValueStyleEnumMember _) + "}"
			}
			case StructDeclaration(optAttributesList, optAccessMod, structname, optGenericParamClause, optTypeInheritanceClause, optGenericWhereClause, structMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + "struct " + structname.name + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) +
					printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + "{" + printSemiColonSepList(structMemberList)(printStructMember _) + "}"
			}
			case RegClassDeclaration(optAttributesList, optAccessMod, optFinal, classname, optGenericParamClause, optTypeInheritanceClause, optGenericWhereClause, classMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + printOptionalThing(optFinal)(printFinal _) + "class " + classname.name +
				printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) +
				"{" + printSemiColonSepList(classMemberList)(printClassMember _) + "}"
			}
			case ForcedFinalClassDeclaration(optAttributesList, optAccessMod, classname, optGenericParamClause, optTypeInheritanceClause, optGenericWhereClause, classMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + "final " + printOptionalThing(optAccessMod)(printAccessLevelMod _) + "class " + classname.name +
				printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) +
				"{" + printSemiColonSepList(classMemberList)(printClassMember _) + "}"
			}
			case ActorDeclaration(optAttributesList, optAccessMod, actorName, optGenericParamClause, optTypeInheritanceClause, optGenericWhereClause, actorMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printDeclarationMod _) + "actor " + actorName.name + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) +
				printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + "{" + printSemiColonSepList(actorMemberList)(printActorMember _) + "}"
			}
			case ProtocolDeclaration(optAttributesList, optAccessMod, protocolName, optTypeInheritanceClause, optGenericWhereClause, protocolMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + " protocol " + protocolName.name + printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) +
				printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + "{" + printSemiColonSepList(protocolMemberList)(printProtocolMember _) + "}"
			}
			case ThrowsInitializerDeclaration(initHead, optGenericParamClause, paramClause, optAsync, optThrows, optGenericWhereClause, codeBlock) => {
				printInitializerHead(initHead) + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printParamClause(paramClause) + printOptionalThing(optAsync)(printAsync _) + printOptionalThing(optThrows)(printThrows _) +
				printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + printGetterSetterBlock(codeBlock) }
			case RethrowsInitializerDeclaration(initHead, optGenericParamClause, paramClause, optAsync, optGenericWhereClause, codeBlock) => {
				printInitializerHead(initHead) + printOptionalThing(optGenericParamClause)(printGenericParameterClause _) + printParamClause(paramClause) + printOptionalThing(optAsync)(printAsync _) + "rethrows " +
				printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + printGetterSetterBlock(codeBlock)
			}
			case DeinitializerDeclaration(optAttributesList, codeBlock) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + "deinit" + printGetterSetterBlock(codeBlock)
			}
			case ExtensionDeclaration(optAttributesList, optAccessMod, typ, optTypeInheritanceClause, optGenericWhereClause, extensionMemberList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + printOptionalThing(optAccessMod)(printAccessLevelMod _) + "extension " + printType(typ) + printOptionalThing(optTypeInheritanceClause)(printTypeInheritanceClause _) +
				printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + "{" + printSemiColonSepList(extensionMemberList)(printExtensionMember _) + "}"
			}
			case SubscriptDeclaration(subscriptHead, subscriptResult, optGenericWhereClause, allowableSubscriptBlock) => {
				printSubscriptHead(subscriptHead) + printSubscriptResult(subscriptResult) + printOptionalThing(optGenericWhereClause)(printGenericWhereClause _) + printAllowableSubscriptBlock(allowableSubscriptBlock)
			}
			case PrefixOperatorDeclaration(op) => "prefix operator " + op.value
			case PostfixOperatorDeclaration(op) => "postfix operator " + op.value
			case InfixOperatorDeclaration(op, optPrecedenceGroupName) => {
				optPrecedenceGroupName match {
					case Some(name) => "infix operator " + op.value + " : " + name.name
					case None => "infix operator " + op.value
				}
			}
			case PrecedenceGroupDeclaration(precedenceGroupName, optPrecedenceGroupAttributeList) => {
				"precedencegroup " + precedenceGroupName.name + "{" + printOptionalThing(optPrecedenceGroupAttributeList)(printNonSepList(_)(printPrecedenceGroupAttribute _)) + "}"
			}
		}
	}
	
	def printCaseMod(mod: CaseMod): String = { "case " }
	
	def printWhereClause(clause: Exp): String = {
		" where" + printExp(clause) 
	}
	
	def printCondition(condition: Condition): String = {
		condition match {
			case ExpressionCondition(exp) => printExp(exp)
			case CaseCondition(pattern, exp) => "case " + printPattern(pattern) + " = " + printExp(exp)
			case OptionalBindingConditionLet(pattern, optExp) => {
				optExp match {
					case Some(exp) => "let " + printPattern(pattern) + " = " + printExp(exp)
					case None => "let " + printPattern(pattern)
				}
			}
			case OptionalBindingConditionVar(pattern, optExp) => {
				optExp match {
					case Some(exp) => "var " + printPattern(pattern) + " = " + printExp(exp)
					case None => "var " + printPattern(pattern)
				}
			}
		}
	}
	
	def printElseClause(elseClause: ElseClause): String = {
		elseClause match {
			case ElseCodeBlock(codeBlock) => "else" + printGetterSetterBlock(codeBlock)
			case ElseIfStmt(stmt) => "else " + printStmt(stmt)
		}
	}
	
	def printCaseItem(item: CaseItem): String = {
		item match {
			case CaseItem(pattern, optExp) => {
				printPattern(pattern) + printOptionalThing(optExp)(printWhereClause _)
			}
		}
	}
	
	def printCaseLabel(label: CaseLabel): String = {
		label match {
			case CaseLabel(optAttributesList, caseItemList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + "case " + printCommaSepList(caseItemList)(printCaseItem _) + ":"
			}
		}
	}
	
	def printSwitchCase(switchCase: SwitchCase): String = {
		switchCase match {
			case CaseLabelStmts(caseLabel, stmtList) => {
				printCaseLabel(caseLabel) + printSemiColonSepList(stmtList)(printStmt _)
			}
			case DefaultLabelStmts(optAttributesList, stmtList) => {
				printOptionalThing(optAttributesList)(printNonSepList(_)(printAttribute _)) + " default: " + printSemiColonSepList(stmtList)(printStmt _)
			}
		}
	}
	
	def printCatchPattern(pattern: CatchPattern): String = {
		pattern match {
			case CatchPattern(pattern, optExp) => {
				printPattern(pattern) + printOptionalThing(optExp)(printWhereClause _)
			}
		}
	}
	
	def printCatchClause(clause: CatchClause): String = {
		clause match {
			case CatchClause(optCatchPatternList, codeBlock) => {
				"catch " + printOptionalThing(optCatchPatternList)(printCommaSepList(_)(printCatchPattern _)) + printGetterSetterBlock(codeBlock)
			}
		}
	}
	
	def printCompilationCondition(compCond: CompilationCondition): String = {
		compCond match {
			case PlatformConditionOS(os) => "os(" + os + ")"
			case PlatformConditionArch(arch) => "arch(" + arch + ")"
			case PlatformConditionSwiftVer(op, version) => "swift(" + op.value + version + ")"
			case PlatformConditionCompilerVer(op, version) => "compiler(" + op.value + version + ")"
			case PlatformConditionImport(importPath) => "canImport(" + printImportPath(importPath) + ")"
			case PlatformConditionEnv(targetEnv) => "targetEnvironment(" + targetEnv + ")"
			case IdentifierCondition(idExp) => removeParens(idExp)
			case BooleanLiteralCondition(boolExp) => printExp(boolExp)
			case InParensCondition(cond) => "(" + printCompilationCondition(cond) + ")"
			case NotCondition(cond) => "!" + printCompilationCondition(cond)
			case AndCondition(cond1, cond2) => printCompilationCondition(cond1) + " && " + printCompilationCondition(cond2)
			case OrCondition(cond1, cond2) => printCompilationCondition(cond1) + " || " + printCompilationCondition(cond2)
		}
	}
	
	def printIfDirectiveClause(ifClause: IfDirectiveClause): String = {
		ifClause match {
			case IfDirectiveClause(compCond, optStmtList) => {
				"#if " + printCompilationCondition(compCond) + "\n" + printOptionalThing(optStmtList)(printSemiColonSepList(_)(printStmt _))
			}
		}
	}
	
	def printElseIfDirectiveClause(elseIfClause: ElseIfDirectiveClause): String = {
		elseIfClause match {
			case ElseIfDirectiveClause(compCond, optStmtList) => {
				"\n" + "#elseif " + printCompilationCondition(compCond) + "\n" + printOptionalThing(optStmtList)(printSemiColonSepList(_)(printStmt _))
			}
		}
	}
	
	def printElseDirectiveClause(elseClause: ElseDirectiveClause): String = {
		elseClause match {
			case ElseDirectiveClause(optStmtList) => "#else\n" + printOptionalThing(optStmtList)(printSemiColonSepList(_)(printStmt _))
		}
	}
	
	def printCompilerCtrl(compCtrl: CompilerCtrl): String = {
		compCtrl match {
			case ConditionalCompilationBlock(ifClause, optElseIfClauseList, optElseClause) => {
				printIfDirectiveClause(ifClause) + printOptionalThing(optElseIfClauseList)(printNonSepList(_)(printElseIfDirectiveClause _)) + printOptionalThing(optElseClause)(printElseDirectiveClause _) + "#endif"
			}
			case LineControlStmt(optFilePath, optLineNumber) => {
				optFilePath match {
					case Some(filePath) => optLineNumber match {
						case Some(lineNumber) => {
							val expStr = printExp(filePath)
							val expStr2 = printExp(lineNumber)
							"#sourceLocation ( file: " + expStr.substring(1, expStr.length-1) + ", line: " + expStr2.substring(1, expStr2.length-1) + ")"
						}
						case None => "" //not gonna reach this
					}
					case None => "#sourceLocation ()"
				}
			}
			case AvailabilityConditionAvailable(argList) => "#available (" + argList.map(x => print(s"$x,")) + ")"
			case AvailabilityConditionUnavailable(argList) => "unavailable (" + argList.map(x => print(s"$x,")) + ")"
		}
	}

	def printStmt(stmt: Stmt): String = {
		stmt match {
			case ExpressionStmt(exp) => printExp(exp)
			case DeclarationStmt(decl) => printDeclaration(decl)
			case ForInStmt(optCaseMod, pattern, exp, optWhereClause, codeBlock) => {
				"for " + printOptionalThing(optCaseMod)(printCaseMod _) + printPattern(pattern) + " in" + printExp(exp) + printOptionalThing(optWhereClause)(printWhereClause _) +
				printGetterSetterBlock(codeBlock)
			}
			case WhileStmt(conditionList, codeBlock) => {
				"while " + printCommaSepList(conditionList)(printCondition _) + printGetterSetterBlock(codeBlock)
			}
			case RepeatWhileStmt(codeBlock, exp) => {
				"repeat" + printGetterSetterBlock(codeBlock) + "while" + printExp(exp)
			}
			case IfStmt(conditionList, codeBlock, optElseClause) => {
				"if " + printCommaSepList(conditionList)(printCondition _) + printGetterSetterBlock(codeBlock) + printOptionalThing(optElseClause)(printElseClause _)
			}
			case GuardStmt(conditionList, codeBlock) => {
				"guard " + printCommaSepList(conditionList)(printCondition _) + " else" + printGetterSetterBlock(codeBlock)
			}
			case SwitchStmt(exp, optSwitchCaseList) => {
				"switch" + printExp(exp) + "{" + printOptionalThing(optSwitchCaseList)(printNonSepList(_)(printSwitchCase _)) + "}"
			}
			case LabeledStmt(labelName, stmt) => labelName.name + ":" + printStmt(stmt)
			case BreakStmt(optLabelName) => {
				optLabelName match {
					case Some(name) => "break " + name.name
					case None => "break"
				}
			}
			case ContinueStmt(optLabelName) => {
				optLabelName match {
					case Some(name) => "continue " + name.name
					case None => "continue"
				}
			}
			case FallthroughStmt => "fallthrough"
			case ReturnStmt(optExp) => "return" + printOptionalThing(optExp)(printExp _)
			case ThrowStmt(exp) => "throw" + printExp(exp)
			case DeferStmt(codeBlock) => "defer" + printGetterSetterBlock(codeBlock)
			case DoStmt(codeBlock, optCatchClauseList) => {
				"do" + printGetterSetterBlock(codeBlock) + printOptionalThing(optCatchClauseList)(printNonSepList(_)(printCatchClause _))
			}
			case CompilerControlStmt(compCtrl) => printCompilerCtrl(compCtrl)
		}
	}

}