package FuzzingSwift.tokenizer

import scala.util.parsing.input.Positional

sealed trait Token extends Positional {
	def isComment: Boolean = false
}

//reserved words
case object AsToken extends Token
case object AlphaToken extends Token
case object BreakToken extends Token
case object CaseToken extends Token
case object CatchToken extends Token
case object ClassToken extends Token
case object ContinueToken extends Token
case object DefaultToken extends Token
case object DeferToken extends Token
case object DoToken extends Token
case object GuardToken extends Token
case object ElseToken extends Token
case object EnumToken extends Token
case object ForToken extends Token
case object FallthroughToken extends Token
case object FuncToken extends Token
case object InToken extends Token
case object IfToken extends Token
case object ImportToken extends Token
case object InternalToken extends Token
case object FinalToken extends Token
case object OpenToken extends Token
case object PrivateToken extends Token
case object PublicToken extends Token
case object WhereToken extends Token
case object WhileToken extends Token
case object LetToken extends Token
case object VarToken extends Token
case object ProtocolToken extends Token
case object GetToken extends Token
case object SetToken extends Token
case object WillSetToken extends Token
case object DidSetToken extends Token
case object RepeatToken extends Token
case object SwitchToken extends Token
case object StructToken extends Token
case object ReturnToken extends Token
case object ThrowToken extends Token
case object ThrowsToken extends Token
case object RethrowsToken extends Token
case object IndirectToken extends Token
case object InitToken extends Token
case object DeinitToken extends Token
case object AssociatedTypeToken extends Token
case object ExtensionToken extends Token
case object SubscriptToken extends Token
case object PrefixToken extends Token
case object InfixToken extends Token
case object LeftToken extends Token
case object RightToken extends Token
case object NoneToken extends Token
case object PrecedenceGroupToken extends Token
case object HigherThanToken extends Token
case object LowerThanToken extends Token
case object AssignmentToken extends Token
case object AssociativityToken extends Token
case object PostFixToken extends Token
case object OperatorToken extends Token
case object TypeAliasToken extends Token
case object OSToken extends Token
case object ArchToken extends Token
case object SwiftToken extends Token
case object CompilerToken extends Token
case object CanImportToken extends Token
case object TargetEnvironmentToken extends Token
case object ConvenienceToken extends Token
case object DynamicToken extends Token
case object LazyToken extends Token
case object OptionalToken extends Token
case object OverrideToken extends Token
case object RequiredToken extends Token
case object StaticToken extends Token
case object WeakToken extends Token
case object UnownedToken extends Token
case object SafeToken extends Token
case object UnsafeToken extends Token
case object MutatingToken extends Token
case object NonmutatingToken extends Token
case object FilePrivateToken extends Token
case object IsToken extends Token
case object TryToken extends Token
case object SuperToken extends Token
case object AnyToken extends Token
case object SmallAnyToken extends Token
case object FalseToken extends Token
case object RedToken extends Token
case object BlueToken extends Token
case object GreenToken extends Token
case object ResourceNameToken extends Token
case object TrueToken extends Token
case object NilToken extends Token
case object InOutToken extends Token
case object SomeToken extends Token
case object TypeToken extends Token
case object PrecedenceToken extends Token
case object SelfToken extends Token
case object SelfBigToken extends Token
case object AsyncToken extends Token
case object AwaitToken extends Token
case object NonIsolatedToken extends Token
case object ActorToken extends Token

case object MacOSToken extends Token
case object IOSToken extends Token
case object OSXToken extends Token
case object WatchOSToken extends Token
case object TVOSToken extends Token
case object LinuxToken extends Token
case object WindowsToken extends Token

case object I386Token extends Token
case object X86_64Token extends Token
case object ArmToken extends Token
case object Arm64Token extends Token

case object SimulatorToken extends Token
case object MacCatalystToken extends Token

case object IOSApplicationExtensionToken extends Token
case object MacCatalystApplicationExtensionToken extends Token
case object MacOSApplicationExtensionToken extends Token
case object WatchOSApplicationExtensionToken extends Token
case object TVOSApplicationExtensionToken extends Token
case object SourceLocationToken extends Token

case object FileToken extends Token
case object LineToken extends Token
//case object ErrorToken extends Token
//case object WarningToken extends Token
case object AvailableToken extends Token
case object UnavailableToken extends Token

case object HashIfToken extends Token
case object HashElseIfToken extends Token
case object HashElseToken extends Token
case object HashEndIfToken extends Token
//case object HashFileToken extends Token
//case object HashFileIDToken extends Token
//case object HashFilePathToken extends Token
//case object HashLineToken extends Token
//case object HashColumnToken extends Token
//case object HashFunctionToken extends Token
//case object HashDSOHandleToken extends Token
case object HashSelectorToken extends Token
case object HashKeyPathToken extends Token
case object HashColorLiteralToken extends Token
case object HashFileLiteralToken extends Token
case object HashImageLiteralToken extends Token
case object GetterToken extends Token
case object SetterToken extends Token

//operator
case class InfixOperatorLiteralToken(operator: String) extends Token
case class PostfixOperatorLiteralToken(operator: String) extends Token
case class PrefixOperatorLiteralToken(operator: String) extends Token
case class InfixDotOperatorLiteralToken(operator: String) extends Token
case class PostfixDotOperatorLiteralToken(operator: String) extends Token
case class PrefixDotOperatorLiteralToken(operator: String) extends Token

//identifiers (variables)
case class VariableToken(name: String) extends Token
case class PropertyWrapperProjectionToken(name: String) extends Token
//there's an ambiguity here in the grammar where we would need further information to disambiguate something like:
// $0 as an implicit param or a property wrapper projection. This would be handled later in the compiler.
case class ImplicitParameterOrPropertyWrapperProjectionToken(name: String) extends Token

// reserved symbols
case object LeftCurlyToken extends Token
case object LeftParenToken extends Token
case object LeftParenNewLineToken extends Token
case object LeftBracketToken extends Token
case object RightCurlyToken extends Token
case object RightParenToken extends Token
case object RightBracketToken extends Token
case object CommaToken extends Token
case object ColonToken extends Token
case object SemicolonToken extends Token
case object UnderscoreToken extends Token
case object AtToken extends Token
case object HashToken extends Token
case object BackTickToken extends Token
case object DollarToken extends Token
case object BackSlashToken extends Token


//numerical literals
//ints
case class BinaryIntegerLiteralToken(value: String) extends Token
case class OctalIntegerLiteralToken(value: String) extends Token
case class DecimalIntegerLiteralToken(value: String) extends Token
case class HexIntegerLiteralToken(value: String) extends Token

//floats
case class FloatDecimalLiteralToken(value: String) extends Token
case class FloatHexLiteralToken(value: String) extends Token

//char literals
case class CharLiteralToken(value: String) extends Token
//string literals
case class SingleLineStringLiteralToken(value: String) extends Token
case class MultiLineStringLiteralToken(value: String) extends Token
case class InterpolatedStringLiteralToken(value: String) extends Token

//comments
case class SingleLineCommentToken(comment: String) extends Token {
	override def isComment: Boolean = true
}
case class MultiLineCommentToken(comment: String) extends Token {
	override def isComment: Boolean = true
}
//trailing ws
case class TrailingWSToken(ws: String) extends Token {
	override def isComment: Boolean = true
}

