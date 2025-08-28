package FuzzingSwift.parser

import FuzzingSwift.tokenizer._
import FuzzingSwift.typechecker._

case class Program(stmts: List[Stmt])

case class Operator(value: String)

//exps
sealed trait Exp {
	var expType: Option[Typ] = None
	var typeEnv: Option[Map[String, (Typ, Mutability)]] = None
	var functions: Option[Map[String, (Typ, Option[Throwing], List[Parameter])]] = None
}

case class TryExp(modifier: TryModifier, exp: Exp) extends Exp
case class AwaitExp(exp: Exp) extends Exp
case class PrefixExp(operator: Operator, expression: Exp) extends Exp
case class PostfixWithOpExp(exp: Exp, op: Operator) extends Exp
case class PostfixForcedValueExp(exp: Exp) extends Exp
case class PostfixOptionalChainingExp(exp: Exp) extends Exp
case class PostfixFunctionCallExp(exp: Exp, after: PostfixFunctionCall) extends Exp
case class PostfixSubscriptExp(exp: Exp, list: List[FunctionCallArgument]) extends Exp
case class PostfixInitializerExp(exp: Exp, argumentNames: Option[List[ArgumentName]]) extends Exp
case class PostfixSelfExp(exp: Exp) extends Exp
case class PostfixExplicitMemberExp(exp: Exp, after: ExplicitMember) extends Exp
case class InOutExp(exp: Exp) extends Exp
case class CastExp(exp: Exp, op: TypeCastingOp) extends Exp
case class AssignmentExp(prefix: Exp, exp: Exp) extends Exp
case class ConditionalExp(prefix: Exp, conditionalExp: Exp, exp: Exp) extends Exp
case class TrueInfixExp(exp1: Exp, op: Operator, exp2: Exp) extends Exp
case class GenericExp(genericName: GenericName, typs: List[Type]) extends Exp
//identifiers
case class VariableExp(name: String) extends Exp
case class ImplicitParameterExpOrPWP(name: String) extends Exp
case class PropertyWrapperProjectionExp(name: String) extends Exp 
//
//numbers
case class DecimalIntegerLiteralExp(value: String) extends Exp
case class BinaryIntegerLiteralExp(value: String) extends Exp
case class OctalIntegerLiteralExp(value: String) extends Exp
case class HexIntegerLiteralExp(value: String) extends Exp
case class DecimalFloatLiteralExp(value: String) extends Exp
case class HexFloatLiteralExp(value: String) extends Exp
//
case class CharStringExp(value: String) extends Exp
case class SingleLineStaticStringLiteralExp(value: String) extends Exp
case class MultiLineStaticStringLiteralExp(value: String) extends Exp
case class InterpolatedStringLiteralExp(value: String) extends Exp
case class TrueLiteralExp() extends Exp
case class FalseLiteralExp() extends Exp
case class NilExp() extends Exp
case class ArrayLiteralExp(exps: List[Exp]) extends Exp
case class DictionaryLiteralExp(exps: List[(Exp, Exp)]) extends Exp
case class ColorPlaygroundLiteralExp(exp1: Exp, exp2: Exp, exp3: Exp, exp4: Exp) extends Exp
case class FilePlaygroundLiteralExp(exp: Exp) extends Exp
case class ImagePlaygroundLiteralExp(exp: Exp) extends Exp
/* case object HashFileExp extends Exp
case object HashFileIDExp extends Exp
case object HashFilePathExp extends Exp
case object HashLineExp extends Exp
case object HashColumnExp extends Exp
case object HashFunctionExp extends Exp
case object HashDSOHandleExp extends Exp */
//self exps
case class SoloSelfExp() extends Exp
case class MethodSelfExp(selfMethodName: SelfMethodName) extends Exp
case class SubscriptSelfExp(functionCallArgList: List[FunctionCallArgument]) extends Exp
case class InitSelfExp() extends Exp
//
//super exps
case class MethodSuperExp(superMethodName: SuperMethodName) extends Exp
case class SubscriptSuperExp(functionCallArgList: List[FunctionCallArgument]) extends Exp
case class InitSuperExp() extends Exp
//
case class ClosureExp(attributeList: Option[List[Attribute]], closureSig: Option[ClosureSignature], stmts: Option[List[Stmt]]) extends Exp
case class ParenthesizedExp(exp: Exp) extends Exp
case class TupleExp(elementList: List[TupleElement]) extends Exp
case class IdentifierImplicitMemberExp(implicitMemberName: ImplicitMemberName) extends Exp
case class IdentifierDotPostfixImplicitMemberExp(implicitMemberName: ImplicitMemberName, postfixExp: Exp) extends Exp
case class WildcardExp() extends Exp
case class KeyPathExp(typ: Option[Type], keyPathComponents: List[KeyPathComponent]) extends Exp
case class SelectorExp(exp: Exp) extends Exp
case class SelectorGetterExp(exp: Exp) extends Exp
case class SelectorSetterExp(exp: Exp) extends Exp
case class KeyPathStringExp(exp: Exp) extends Exp
//end exps

//helpers for exps
case class InOutName(name: String) 
case class GenericName(name: String) 
case class SelfMethodName(name: String) 
case class SuperMethodName(name: String) 
case class ImplicitMemberName(name: String) 

case class InOutMod() 
//above are also used as helpers for types

sealed trait TryModifier 
case object RegTryModifier extends TryModifier
case object QuestionTryModifier extends TryModifier
case object ExclamationTryModifier extends TryModifier

sealed trait InfixExp 
case class WithOperatorInfixExpression(op: Operator, exp: Exp) extends InfixExp
case class AssignmentOpInfixExpression(theTry: Option[TryModifier], exp: Exp) extends InfixExp
case class ConditionalOpInfixExpression(conditionalExp: Exp, theTry: Option[TryModifier], exp: Exp) extends InfixExp
case class TypeCastInfixExpression(op: TypeCastingOp) extends InfixExp

sealed trait TypeCastingOp 
case class IsType(typ: Type) extends TypeCastingOp
case class AsType(typ: Type) extends TypeCastingOp
case class AsQuestionType(typ: Type) extends TypeCastingOp
case class AsExclamationType(typ: Type) extends TypeCastingOp

case class AttributeName(name: String) 

case class Attribute(attributeName: AttributeName, argClause: Option[List[BalancedToken]]) 

sealed trait BalancedToken
case class InParensBalancedToken(token: BalancedToken) extends BalancedToken
case class InBracketsBalancedToken(token: BalancedToken) extends BalancedToken
case class InBracesBalancedToken(token: BalancedToken) extends BalancedToken
case class IdentifierBalancedToken(identifierExp: String) extends BalancedToken
case class KeywordBalancedToken(name: String) extends BalancedToken
case class LiteralBalancedToken(name: Exp) extends BalancedToken
case class OperatorBalancedToken(op: String) extends BalancedToken
case class PunctuationBalancedToken(punc: String) extends BalancedToken


sealed trait ClosureSignature 
case class ClosureSignatureComplex(list: Option[List[CaptureListItem]], clause: ClosureParameterClause, asynch: Option[AsyncMod], throws: Option[ThrowsMod], functionResult: Option[FunctionResult]) extends ClosureSignature
case class ClosureSignatureSimple(list: List[CaptureListItem]) extends ClosureSignature

case class AsyncMod() 
case class ThrowsMod()

case class CaptureListItemName(name: String) 

sealed trait CaptureListItem 
case class CaptureListItemIdentifier(specifier: Option[CaptureSpecifier], captureListItemName: CaptureListItemName) extends CaptureListItem
case class CaptureListItemAssignment(specifier: Option[CaptureSpecifier], captureListItemName: CaptureListItemName, assignmentExp: Exp) extends CaptureListItem
case class CaptureListItemSelf(specifier: Option[CaptureSpecifier], selfExp: Exp) extends CaptureListItem

sealed trait CaptureSpecifier 
case object WeakCaptureSpecifier extends CaptureSpecifier
case object UnownedCaptureSpecifier extends CaptureSpecifier
case object UnownedSafeCaptureSpecifier extends CaptureSpecifier
case object UnownedUnsafeCaptureSpecifier extends CaptureSpecifier

sealed trait ClosureParameterClause 
case class CPCIdentifierList(identifierList: List[String]) extends ClosureParameterClause
case class CPCClosureParameterList(list: List[ClosureParameter]) extends ClosureParameterClause

case class ClosureParamName(name: String) 

sealed trait ClosureParameter 
case class ClosureParameterReg(closureParamName: ClosureParamName, typeAnnotation: Option[TypeAnnotation]) extends ClosureParameter
case class ClosureParameterElipses(closureParamName: ClosureParamName, typeAnnotation: TypeAnnotation) extends ClosureParameter

case class TupleElementName(name: String) 

sealed trait TupleElement 
case class ExpTuple(exp: Exp) extends TupleElement
case class IdentifierColonExpTuple(tupleElementName: TupleElementName, exp: Exp) extends TupleElement

case class FunctionCallArgName(name: String) 

sealed trait FunctionCallArgument 
case class ExpFunctionCallArgument(exp: Exp) extends FunctionCallArgument
case class IdentifierColonExpFunctionCallArgument(functionCallArgName: FunctionCallArgName, exp: Exp) extends FunctionCallArgument
case class OperatorFunctionCallArgument(op: Operator) extends FunctionCallArgument
case class IdentifierColonOperatorFunctionCallArgument(functionCallArgName: FunctionCallArgName, op: Operator) extends FunctionCallArgument

case class KeyPathComponentName(name: String) 

sealed trait KeyPathComponent 
case class IdentifierThenOptPostfixesKPC(keyPathComponentName: KeyPathComponentName, postfixes: Option[List[KeyPathPostfix]]) extends KeyPathComponent
case class PostfixesKPC(postfixes: List[KeyPathPostfix]) extends KeyPathComponent

sealed trait KeyPathPostfix 
case object QuestionKPP extends KeyPathPostfix
case object ExclamationKPP extends KeyPathPostfix
case object SelfKPP extends KeyPathPostfix
case class FuncCallArgListKPP(list: List[FunctionCallArgument]) extends KeyPathPostfix

sealed trait AfterPostfix 
case class AfterPostfixOperator(op: Operator) extends AfterPostfix
case object AfterPostfixForcedValue extends AfterPostfix
case object AfterPostfixOptionalChaining extends AfterPostfix
case class AfterPostfixFunctionCall(call: PostfixFunctionCall) extends AfterPostfix
case class AfterPostfixSubscript(list: List[FunctionCallArgument]) extends AfterPostfix
case class AfterPostfixInitializer(argumentNames: Option[List[ArgumentName]]) extends AfterPostfix
case object AfterPostfixSelf extends AfterPostfix
case class AfterPostfixExplicitMember(member: ExplicitMember) extends AfterPostfix

sealed trait PostfixFunctionCall 
case class SimpleFunctionCall(list: List[FunctionCallArgument]) extends PostfixFunctionCall
case class ComplexFunctionCall(list: Option[List[FunctionCallArgument]], trailing: TrailingClosure) extends PostfixFunctionCall

case class TrailingClosureLabel(name: String) 

case class TrailingClosure(closureExp: Exp, list: Option[List[LabeledTrailingClosure]]) 
case class LabeledTrailingClosure(trailingClosureLabel: TrailingClosureLabel, closureExp: Exp) 

case class ExplicitMemberName(name: String) 
case class ArgumentName(name: String) 

sealed trait ExplicitMember 
case class ExplicitMemberDecimalDigits(nums: String) extends ExplicitMember
case class ExplicitMemberIdentifierOptGeneric(explicitMemberName: ExplicitMemberName, genericArgList: Option[List[Type]]) extends ExplicitMember
case class ExplicitMemberIdentifierArgs(explicitMemberName: ExplicitMemberName, argumentIdentifier: List[ArgumentName]) extends ExplicitMember
//case class ExplicitMemberCondCompBlock

//end helpers for exps

//types
sealed trait Type  {
	var resolvedTyp: Option[Typ] = None
}
case class FunctionType(optAttributes: Option[List[Attribute]], argClause: List[FunctionTypeArg], async: Option[AsyncMod], throws: Option[ThrowsMod], typ: Type) extends Type
case class ArrayType(typ: Type) extends Type
case class DictionaryType(type1: Type, type2: Type) extends Type
case class NormalTypeIdentifier(typeName: TypeName) extends Type
case class GenericTypeIdentifier(typeName: TypeName, genericTypes: List[Type]) extends Type
case class NestedNormalTypeIdentifier(typeName: TypeName, nestedType: Type) extends Type
case class NestedGenericTypeIdentifier(typeName: TypeName, genericTypes: List[Type], nestedType: Type) extends Type
case class TupleType(list: List[TupleTypeElement]) extends Type
case class ProtocolCompositionType(typeIDs: List[Type]) extends Type
case class OpaqueType(typ: Type) extends Type
case object AnyType extends Type
case object SelfType extends Type
case class InParensType(typ: Type) extends Type
case class OptionalType(typ: Type) extends Type
case class ImplicitlyUnwrappedOptionalType(typ: Type) extends Type
case class MetatypeTypeType(typ: Type) extends Type
case class MetatypeProtocolType(typ: Type) extends Type
case class BoxedProtocolType(typ: Type) extends Type
//end types

//helpers for types
case class TypeName(name: String) 

sealed trait TrailorTypeThing 
case object OptionalTypeThing extends TrailorTypeThing
case object ImplicitlyUnwrappedOptionalTypeThing extends TrailorTypeThing
case object MetatypeTypeThing extends TrailorTypeThing
case object MetatypeProtocolThing extends TrailorTypeThing

case class TypeAnnotation(attributes: Option[List[Attribute]], inout: Option[InOutMod], typ: Type) 

case class ArgumentLabel(name: String) 

sealed trait FunctionTypeArg 
case class FunctionTypeArg1(attributesList: Option[List[Attribute]], inout: Option[InOutMod], typ: Type) extends FunctionTypeArg
case class FunctionTypeArg2(argumentLabel: ArgumentLabel, typeAnnotation: TypeAnnotation) extends FunctionTypeArg

case class ElementName(name: String) 

sealed trait TupleTypeElement 
case class TupleTypeElementNameAnnotation(elementName: ElementName, typeAnnotation: TypeAnnotation) extends TupleTypeElement
case class TupleTypeElementType(typ: Type) extends TupleTypeElement
//end helpers for types

//declarations
sealed trait Declaration 
case class ImportDeclaration(attributes: Option[List[Attribute]], kind: Option[ImportKind], path: ImportPath) extends Declaration
case class ConstantDeclaration(attributes: Option[List[Attribute]], modifier: Option[List[DeclarationModifier]], patternInitializerList: List[PatternInitializer]) extends Declaration
case class VariableDeclaration1(attributes: Option[List[Attribute]], modifiers: Option[List[DeclarationModifier]], patternInitList: List[PatternInitializer]) extends Declaration
case class VariableDeclaration23(attributes: Option[List[Attribute]], modifiers: Option[List[DeclarationModifier]], variableName: VariableName, typeAnno: TypeAnnotation, block: GetterSetterBlock) extends Declaration
case class VariableDeclaration4(attributes: Option[List[Attribute]], modifiers: Option[List[DeclarationModifier]], variableName: VariableName, typeAnno: TypeAnnotation, block: KeywordBlock) extends Declaration
case class VariableDeclaration5(attributes: Option[List[Attribute]], modifiers: Option[List[DeclarationModifier]], variableName: VariableName, initializer: Exp, block: SetBlock) extends Declaration
case class VariableDeclaration6(attributes: Option[List[Attribute]], modifiers: Option[List[DeclarationModifier]], variableName: VariableName, typeAnno: TypeAnnotation, initializer: Option[Exp], block: SetBlock) extends Declaration
case class TypeAliasDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], typeAliasName: TypeAliasName, paramClause: Option[List[GenericParameter]], typ: Type) extends Declaration
case class FunctionDeclaration(funcHead: FunctionHead, funcName: FunctionName, genericParamClause: Option[List[GenericParameter]], funcSig: FunctionSignature, genericWhereClause: Option[List[Requirement]], funcBody: Option[CodeBlock]) extends Declaration
case class UnionStyleEnumDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], indirect: Option[IndirectMod], enumName: EnumName, genericParamClause: Option[List[GenericParameter]], typeInheritanceClause: Option[List[TypeInheritance]],
						  genericWhereClause: Option[List[Requirement]], enumMembers: Option[List[UnionStyleEnumMember]]) extends Declaration
case class RawValueStyleEnumDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], enumName: EnumName, genericParamClause: Option[List[GenericParameter]], typeInheritanceClause: List[TypeInheritance],
								 genericWhereClause: Option[List[Requirement]], enumMembers: List[RawValueStyleEnumMember]) extends Declaration
case class StructDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], structName: Structname, genericParamClause: Option[List[GenericParameter]],
							 typeInheritanceClause: Option[List[TypeInheritance]], genericWhereClause: Option[List[Requirement]], body: List[StructMember]) extends Declaration
case class RegClassDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], finalMod: Option[FinalMod], className: Classname, genericParamClause: Option[List[GenericParameter]],
						typeInheritanceClause: Option[List[TypeInheritance]], genericWhereClause: Option[List[Requirement]], body: List[ClassMember]) extends Declaration
case class ForcedFinalClassDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], classname: Classname, genericParamClause: Option[List[GenericParameter]],
						typeInheritanceClause: Option[List[TypeInheritance]], genericWhereClause: Option[List[Requirement]], body: List[ClassMember]) extends Declaration
case class ActorDeclaration(attributes: Option[List[Attribute]], accessMod: Option[DeclarationModifier], actorName: ActorName, genericParamClause: Option[List[GenericParameter]],
							typeInheritanceClause: Option[List[TypeInheritance]], genericWhereClause: Option[List[Requirement]], body: List[ActorMember]) extends Declaration
case class ProtocolDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], protocolName: ProtocolName, typeInheritanceClause: Option[List[TypeInheritance]],
							   genericWhereClause: Option[List[Requirement]], body: List[ProtocolMember]) extends Declaration
case class ThrowsInitializerDeclaration(initHead: InitializerHead, genericParamClause: Option[List[GenericParameter]], paramClause: List[Parameter], asyncMod: Option[AsyncMod], throwsMod: Option[ThrowsMod],
										genericWhereClause: Option[List[Requirement]], initBody: CodeBlock) extends Declaration
case class RethrowsInitializerDeclaration(initHead: InitializerHead, genericParamClause: Option[List[GenericParameter]], paramClause: List[Parameter], asyncMod: Option[AsyncMod],
										genericWhereClause: Option[List[Requirement]], initBody: CodeBlock) extends Declaration
case class DeinitializerDeclaration(attributes: Option[List[Attribute]], codeBlock: CodeBlock) extends Declaration
case class ExtensionDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], typeID: Type, typeInheritanceClause: Option[List[TypeInheritance]],
							    genericWhereClause: Option[List[Requirement]], extensionBody: List[ExtensionMember]) extends Declaration
case class SubscriptDeclaration(head: SubscriptHead, result: SubscriptResult, genericWhereClause: Option[List[Requirement]], block: AllowableSubscriptBlock) extends Declaration
case class PrefixOperatorDeclaration(op: Operator) extends Declaration
case class PostfixOperatorDeclaration(op: Operator) extends Declaration
case class InfixOperatorDeclaration(op: Operator, precedenceGroupName: Option[PrecedenceGroupName]) extends Declaration
case class PrecedenceGroupDeclaration(precedenceGroupName: PrecedenceGroupName, precedenceGroupAttributes: Option[List[PrecedenceGroupAttribute]]) extends Declaration
//end declarations

//helpers for declarations
case class VariableName(name: String) 
case class Classname(name: String) 
case class Structname(name: String) 
case class ImportPathName(name: String) 
case class TypeAliasName(name: String) 
case class EnumName(name: String) 
case class ActorName(name: String) 
case class ProtocolName(name: String) 
case class PrecedenceGroupName(name: String) 

sealed trait ImportKind 
case object TypeAliasKind extends ImportKind
case object StructKind extends ImportKind
case object ClassKind extends ImportKind
case object EnumKind extends ImportKind
case object ProtocolKind extends ImportKind
case object LetKind extends ImportKind
case object VarKind extends ImportKind
case object FuncKind extends ImportKind

sealed trait ImportPath 
case class RegularPath(name: ImportPathName) extends ImportPath
case class NestedPath(name: ImportPathName, path: ImportPath) extends ImportPath

sealed trait DeclarationModifier 
case class AccessLevelModifier(mod: AccessLevelMod) extends DeclarationModifier
case object ActorIsolationModifier extends DeclarationModifier
case class MutationModifier(mod: MutationMod) extends DeclarationModifier
case object ClassModifier extends DeclarationModifier
case object ConvenienceModifier extends DeclarationModifier
case object DynamicModifier extends DeclarationModifier
case object FinalModifier extends DeclarationModifier
case object InfixModifier extends DeclarationModifier
case object LazyModifier extends DeclarationModifier
case object OptionalModifier extends DeclarationModifier
case object OverrideModifier extends DeclarationModifier
case object PostfixModifier extends DeclarationModifier
case object PrefixModifier extends DeclarationModifier
case object RequiredModifier extends DeclarationModifier
case object StaticModifier extends DeclarationModifier
case object UnownedModifier extends DeclarationModifier
case object UnownedSafeModifier extends DeclarationModifier
case object UnownedUnsafeModifier extends DeclarationModifier
case object WeakModifier extends DeclarationModifier

sealed trait AccessLevelMod 
case object PrivateModifier extends AccessLevelMod
case object PrivateSetModifier extends AccessLevelMod
case object FilePrivateModifier extends AccessLevelMod
case object FilePrivateSetModifier extends AccessLevelMod
case object InternalModifier extends AccessLevelMod
case object InternalSetModifier extends AccessLevelMod
case object PublicModifier extends AccessLevelMod
case object PublicSetModifier extends AccessLevelMod
case object OpenModifier extends AccessLevelMod
case object OpenSetModifier extends AccessLevelMod

sealed trait MutationMod 
case object MutatingModifier extends MutationMod
case object NonMutatingModifier extends MutationMod

case class PatternInitializer(thePattern: Pattern, initializer: Option[Exp]) 

sealed trait GetterSetterBlock 
case class CodeBlock(stmts: Option[List[Stmt]]) extends GetterSetterBlock
case class GetterSetterClauseBlock(getter: GetterClause, setter: Option[SetterClause]) extends GetterSetterBlock
case class SetterGetterClauseBlock(setter: SetterClause, getter: GetterClause) extends GetterSetterBlock

sealed trait KeywordBlock 
case class GetterSetterKeywordBlock(getter: GetterKeywordClause, setter: Option[SetterKeywordClause]) extends KeywordBlock
case class SetterGetterKeywordBlock(setter: SetterKeywordClause, getter: GetterKeywordClause) extends KeywordBlock

case class SetterName(name: String) 

case class GetterClause(attributes: Option[List[Attribute]], mutationMod: Option[MutationMod], codeBlock: CodeBlock) 
case class SetterClause(attributes: Option[List[Attribute]], mutationMod: Option[MutationMod], setterName: Option[SetterName], codeBlock: CodeBlock) 

case class GetterKeywordClause(attributes: Option[List[Attribute]], mutationMod: Option[MutationMod]) 
case class SetterKeywordClause(attributes: Option[List[Attribute]], mutationMod: Option[MutationMod]) 

sealed trait SetBlock 
case class WillDidSetBlock(will: WillSetClause, did: Option[DidSetClause]) extends SetBlock
case class DidWillSetBlock(did: DidSetClause, will: Option[WillSetClause]) extends SetBlock

case class WillSetClause(attributes: Option[List[Attribute]], mod: Option[MutationMod], setterName: Option[SetterName], codeBlock: CodeBlock) 
case class DidSetClause(attributes: Option[List[Attribute]], mod: Option[MutationMod], setterName: Option[SetterName], codeBlock: CodeBlock) 

sealed trait GenericParameter 
case class SimpleGenericParameter(typeName: TypeName) extends GenericParameter
case class AnnotatedGenericParameter(typeName: TypeName, typeID: Type) extends GenericParameter
case class ProtocolCompGenericParameter(typeName: TypeName, protocolCompTypeID: Type) extends GenericParameter

//function_declaration helpers
case class FunctionHead(attributes: Option[List[Attribute]], declModifiers: Option[List[DeclarationModifier]]) 

sealed trait FunctionName 
case class IdentifierFunctionName(name: String) extends FunctionName
case class OperatorFunctionName(name: Operator) extends FunctionName

sealed trait FunctionSignature 
case class ThrowsFunctionSig(paramClause: List[Parameter], asyncMod: Option[AsyncMod], throwsMod: Option[ThrowsMod], funcResult: Option[FunctionResult]) extends FunctionSignature
case class RethrowsFunctionSig(paramClause: List[Parameter], asyncMod: Option[AsyncMod], funcResult: Option[FunctionResult]) extends FunctionSignature

case class FunctionResult(attributes: Option[List[Attribute]], typ: Type) 

sealed trait ParamName 
case class ExternalParamName(name: String) extends ParamName
case class LocalParamName(name: String) extends ParamName

sealed trait Parameter 
case class OptDefaultArgClauseParameter(optExternalParamName: Option[ExternalParamName], localParamName: LocalParamName, typeAnno: TypeAnnotation, defaultArgClause: Option[Exp]) extends Parameter
case class ElipsesParameter(optExternalParamNameIdentifierExp: Option[ExternalParamName], localParamName: LocalParamName, typeAnno: TypeAnnotation) extends Parameter

sealed trait Requirement 
case class ConformanceRequirementDoubleTypeID(typeID1: Type, typeID2: Type) extends Requirement
case class ConformanceRequirementTypeIDProtocolCompType(typeID: Type, protocolCompType: Type) extends Requirement
case class SameTypeRequirement(typeID: Type, typ: Type) extends Requirement

case class IndirectMod()

case class TypeInheritance(attributes: Option[List[Attribute]], typeID: Type) 

case class EnumCaseName(name: String) 

//union style enum stuff
sealed trait UnionStyleEnumMember 
case class DeclarationUSEnumMember(decl: Declaration) extends UnionStyleEnumMember
case class EnumCaseClauseUSEnumMember(attributes: Option[List[Attribute]], indirect: Option[IndirectMod], caseList: List[UnionStyleEnumCase]) extends UnionStyleEnumMember
case class CompilerControlEnumMember(compCtrl: Stmt) extends UnionStyleEnumMember

case class UnionStyleEnumCase(enumCaseName: EnumCaseName, tupleType: Option[Type]) 

//raw value enum stuff
sealed trait RawValueStyleEnumMember 
case class DeclarationRVSEnumMember(decl: Declaration) extends RawValueStyleEnumMember
case class EnumCaseClauseRVSEnumMember(attributes: Option[List[Attribute]], caseList: List[RawValueStyleEnumCase]) extends RawValueStyleEnumMember
case class CompilerControlRVSEnumMember(compCtrl: Stmt) extends RawValueStyleEnumMember

case class RawValueStyleEnumCase(enumCaseName: EnumCaseName, rawValue: Option[RawValue]) 

sealed trait RawValue 
case class NumericLiteralRawValue(num: String) extends RawValue
case class StaticStringLiteralRawValue(string: String) extends RawValue
case object BooleanTrueLiteralRawValue extends RawValue
case object BooleanFalseLiteralRawValue extends RawValue

//struct_declaration helpers
sealed trait StructMember 
case class DeclarationStructMember(decl: Declaration) extends StructMember
case class CompilerControlStructMember(compCtrl: Stmt) extends StructMember

sealed trait ClassMember 
case class DeclarationClassMember(decl: Declaration) extends ClassMember
case class CompilerControlClassMember(compCtrl: Stmt) extends ClassMember

case class FinalMod()

//actor_declaration helpers
sealed trait ActorMember 
case class DeclarationActorMember(decl: Declaration) extends ActorMember
case class CompilerControlActorMember(compCtrl: Stmt) extends ActorMember

//protocol_declaration helpers
sealed trait ProtocolMember 
case class ProtocolPropertyDeclaration(attributes: Option[List[Attribute]], modifiers: Option[List[DeclarationModifier]], variableName: VariableName, typeAnno: TypeAnnotation, block: KeywordBlock) extends ProtocolMember
case class ProtocolMethodDeclaration(funcHead: FunctionHead, funcName: FunctionName, genericParamClause: Option[List[GenericParameter]], functionSig: FunctionSignature, genericWhereClause: Option[List[Requirement]]) extends ProtocolMember
case class ThrowsProtocolInitializerDeclaration(initHead: InitializerHead, genericParamClause: Option[List[GenericParameter]], paramClause: List[Parameter], throwsMod: Option[ThrowsMod], genericWhereClause: Option[List[Requirement]]) extends ProtocolMember
case class RethrowsProtocolInitializerDeclaration(initHead: InitializerHead, genericParamClause: Option[List[GenericParameter]], paramClause: List[Parameter], genericWhereClause: Option[List[Requirement]]) extends ProtocolMember
case class ProtocolSubscriptDeclaration(subscriptHead: SubscriptHead, subscriptResult: SubscriptResult, genericWhereClause: Option[List[Requirement]], block: KeywordBlock) extends ProtocolMember
case class ProtocolAssociatedTypeDeclaration(attributes: Option[List[Attribute]], accessMod: Option[AccessLevelMod], typeAliasName: TypeAliasName, typeInheritanceClause: Option[List[TypeInheritance]],
											 typeAliasAssignment: Option[Type], genericWhereClause: Option[List[Requirement]]) extends ProtocolMember
case class ProtocolTypeAliasDeclaration(typeAliasDecl: Declaration) extends ProtocolMember
case class CompilerControlProtocolMember(compilerCtrlStmt: Stmt) extends ProtocolMember

sealed trait InitializerHead 
case class RegInitHead(attributes: Option[List[Attribute]], declModifiers: Option[List[DeclarationModifier]]) extends InitializerHead
case class QuestionInitHead(attributes: Option[List[Attribute]], declModifiers: Option[List[DeclarationModifier]]) extends InitializerHead
case class ExclamationInitHead(attributes: Option[List[Attribute]], declModifiers: Option[List[DeclarationModifier]]) extends InitializerHead

//extension_declaration helpers
sealed trait ExtensionMember 
case class DeclarationExtensionMember(decl: Declaration) extends ExtensionMember
case class CompilerControlExtensionMember(compCtrlStmt: Stmt) extends ExtensionMember

//subscript_declaration helpers
case class SubscriptHead(attributes: Option[List[Attribute]], mods: Option[List[DeclarationModifier]], genericParamClause: Option[List[GenericParameter]], paramClause: List[Parameter]) 
case class SubscriptResult(attributes: Option[List[Attribute]], typ: Type) 

sealed trait AllowableSubscriptBlock 
case class AllowableGetterSetterBlock(block: GetterSetterBlock) extends AllowableSubscriptBlock
case class AllowableKeywordBlock(block: KeywordBlock) extends AllowableSubscriptBlock

//precedence_group_declaration helpers
sealed trait PrecedenceGroupAttribute 
case class PrecedenceGroupRelationHigher(precedenceGroupNames: List[PrecedenceGroupName]) extends PrecedenceGroupAttribute
case class PrecedenceGroupRelationLower(precedenceGroupNames: List[PrecedenceGroupName]) extends PrecedenceGroupAttribute
case object PrecedenceGroupAssignmentTrue extends PrecedenceGroupAttribute
case object PrecedenceGroupAssignmentFalse extends PrecedenceGroupAttribute
case object PrecedenceGroupLeftAssociative extends PrecedenceGroupAttribute
case object PrecedenceGroupRightAssociative extends PrecedenceGroupAttribute
case object PrecedenceGroupNotAssociative extends PrecedenceGroupAttribute

//end of helpers for declarations

//patterns
sealed trait Pattern 
case class WildcardPattern(typeAnnotation: Option[TypeAnnotation]) extends Pattern
case class IdentifierPattern(patternName: PatternName, typeAnno: Option[TypeAnnotation]) extends Pattern
case class ValueBindingPattern(modifier: MutableModifier, pattern: Pattern) extends Pattern
case class TuplePattern(list: List[TuplePatternElement], typeAnnotation: Option[TypeAnnotation]) extends Pattern
case class EnumCasePattern(typeID: Option[Type], enumCaseName: EnumCaseName, tuplePattern: Option[Pattern]) extends Pattern
case class OptionalPattern(identifierPattern: Pattern) extends Pattern
case class ExpressionPattern(exp: Exp) extends Pattern
case class IsTypeCastingPattern(typ: Type) extends Pattern
case class AsTypeCastingPattern(primaryPattern: Pattern, typ: Type) extends Pattern
//end patterns

//helpers for patterns
case class PatternName(name: String) 

sealed trait MutableModifier 
case object VarModifier extends MutableModifier
case object LetModifier extends MutableModifier

sealed trait TuplePatternElement 
case class PatternElement(pattern: Pattern) extends TuplePatternElement
case class IdentifierPatternElement(patternName: PatternName, pattern: Pattern) extends TuplePatternElement
//end helpers for patterns

//statements
sealed trait Stmt 
case class ExpressionStmt(exp: Exp) extends Stmt
case class DeclarationStmt(decl: Declaration) extends Stmt
//loops
case class ForInStmt(caseMod: Option[CaseMod], pattern: Pattern, exp: Exp, whereClause: Option[Exp], codeBlock: CodeBlock) extends Stmt
case class WhileStmt(conditionList: List[Condition], codeBlock: CodeBlock) extends Stmt
case class RepeatWhileStmt(codeBlock: CodeBlock, exp: Exp) extends Stmt
//end loops
//branches
case class IfStmt(conditionList: List[Condition], codeBlock: CodeBlock, elseClause: Option[ElseClause]) extends Stmt
case class GuardStmt(conditionList: List[Condition], codeBlock: CodeBlock) extends Stmt
case class SwitchStmt(exp: Exp, switchCases: Option[List[SwitchCase]]) extends Stmt
//end branches
case class LabeledStmt(labelName: LabelName, allowableStmt: Stmt) extends Stmt
//control transfer stmts
case class BreakStmt(labelName: Option[LabelName]) extends Stmt
case class ContinueStmt(labelName: Option[LabelName]) extends Stmt
case object FallthroughStmt extends Stmt
case class ReturnStmt(exp: Option[Exp]) extends Stmt
case class ThrowStmt(exp: Exp) extends Stmt
//end control transfer stmts
case class DeferStmt(codeBlock: CodeBlock) extends Stmt
case class DoStmt(codeBlock: CodeBlock, catchClauses: Option[List[CatchClause]]) extends Stmt
case class CompilerControlStmt(compctrl: CompilerCtrl) extends Stmt
//end statements

//helpers for statements
//this compiler control stuff is v simplified bcuz i only care about parsing it
//not actually replacing anything later
sealed trait CompilerCtrl 
case class ConditionalCompilationBlock(ifClause: IfDirectiveClause, optElseIfClauses: Option[List[ElseIfDirectiveClause]],
										optElseClause: Option[ElseDirectiveClause]) extends CompilerCtrl
case class LineControlStmt(filePath: Option[Exp], lineNumber: Option[Exp]) extends CompilerCtrl
case class AvailabilityConditionAvailable(args: List[AvailabilityArgument]) extends CompilerCtrl
case class AvailabilityConditionUnavailable(args: List[AvailabilityArgument]) extends CompilerCtrl

case class AvailabilityArgument(str: String) 

case class IfDirectiveClause(compCond: CompilationCondition, optStmts: Option[List[Stmt]]) 
case class ElseIfDirectiveClause(compCond: CompilationCondition, optStmts: Option[List[Stmt]]) 
case class ElseDirectiveClause(optStmts: Option[List[Stmt]]) 

sealed trait CompilationCondition 
case class PlatformConditionOS(os: String) extends CompilationCondition
case class PlatformConditionArch(arch: String) extends CompilationCondition
case class PlatformConditionSwiftVer(op: Operator, version: String) extends CompilationCondition
case class PlatformConditionCompilerVer(op: Operator, version: String) extends CompilationCondition
case class PlatformConditionImport(path: ImportPath) extends CompilationCondition
case class PlatformConditionEnv(targetEnv: String) extends CompilationCondition
case class IdentifierCondition(identifier: Exp) extends CompilationCondition
case class BooleanLiteralCondition(boolean: Exp) extends CompilationCondition
case class InParensCondition(cond: CompilationCondition) extends CompilationCondition
case class NotCondition(cond: CompilationCondition) extends CompilationCondition
case class AndCondition(cond1: CompilationCondition, cond2: CompilationCondition) extends CompilationCondition
case class OrCondition(cond1: CompilationCondition, cond2: CompilationCondition) extends CompilationCondition

sealed trait ConditionTrailer 
case class AndTrailer(cond: CompilationCondition) extends ConditionTrailer
case class OrTrailer(cond: CompilationCondition) extends ConditionTrailer

case class LabelName(name: String) 
 
case class CaseMod()

sealed trait Condition 
case class ExpressionCondition(exp: Exp) extends Condition
//skipping availability-condition for now
case class CaseCondition(pattern: Pattern, initializer: Exp) extends Condition
case class OptionalBindingConditionLet(pattern: Pattern, initializer: Option[Exp]) extends Condition
case class OptionalBindingConditionVar(pattern: Pattern, initializer: Option[Exp]) extends Condition

sealed trait ElseClause 
case class ElseCodeBlock(codeBlock: CodeBlock) extends ElseClause
case class ElseIfStmt(ifStmt: Stmt) extends ElseClause

sealed trait SwitchCase 
case class CaseLabelStmts(caseLabel: CaseLabel, stmts: List[Stmt]) extends SwitchCase
case class DefaultLabelStmts(attributes: Option[List[Attribute]], stmts: List[Stmt]) extends SwitchCase
//skipping condition-switch-case for now

case class CaseLabel(attributes: Option[List[Attribute]], caseItemList: List[CaseItem]) 
case class CaseItem(pattern: Pattern, whereClause: Option[Exp]) 

case class CatchClause(catchPatternList: Option[List[CatchPattern]], codeBlock: CodeBlock) 
case class CatchPattern(pattern: Pattern, whereClause: Option[Exp]) 
//end helpers for statements