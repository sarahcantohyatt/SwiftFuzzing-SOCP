package FuzzingSwift.tokenizer

import scala.util.matching.Regex
import scala.util.parsing.combinator._

class TokenizerException(message: String) extends Exception(message)

case class Location(line: Int, column: Int) {
	override def toString = s"$line:$column"
}

object Tokenizer extends RegexParsers {
	override def skipWhitespace = false
	val generalWS = "[\\s]+".r
	val unmatchedGeneralWS = "(?=[\\s]+)".r
	val after_rw = "(?=[\\s\\W+]+)".r
	val unmatched_notWS = "(?=[\\S])".r
	val unmatched_left_unbound = "(?=[,;:\\[\\(\\{])".r
	val unmatched_right_unbound = "(?=[,;:\\]\\)\\}])".r
	
	def reserved_words: Parser[Token] = positioned {
		opt(generalWS) ~ "associatedtype".r ~ after_rw ^^^ AssociatedTypeToken |
		opt(generalWS) ~ "alpha".r ~ after_rw ^^^ AlphaToken |
		opt(generalWS) ~ "break".r ~ after_rw ^^^ BreakToken |
		opt(generalWS) ~ "case".r ~ after_rw ^^^ CaseToken  |
		opt(generalWS) ~ "catch".r ~ after_rw ^^^ CatchToken |
		opt(generalWS) ~ "class".r ~ after_rw ^^^ ClassToken |
		opt(generalWS) ~ "continue".r ~ after_rw ^^^ ContinueToken |
		opt(generalWS) ~ "default".r ~ after_rw ^^^ DefaultToken |
		opt(generalWS) ~ "defer".r ~ after_rw ^^^ DeferToken |
		opt(generalWS) ~ "do".r ~ after_rw ^^^ DoToken |
		opt(generalWS) ~ "guard".r ~ after_rw ^^^ GuardToken |
		opt(generalWS) ~ "else".r ~ after_rw ^^^ ElseToken |
		opt(generalWS) ~ "enum".r ~ after_rw ^^^ EnumToken |
		opt(generalWS) ~ "for".r ~ after_rw ^^^ ForToken |
		opt(generalWS) ~ "fallthrough".r ~ after_rw ^^^ FallthroughToken |
		opt(generalWS) ~ "func".r ~ after_rw ^^^ FuncToken |
		opt(generalWS) ~ "internal".r ~ after_rw ^^^ InternalToken |
		opt(generalWS) ~ "if".r ~ after_rw ^^^ IfToken |
		opt(generalWS) ~ "import".r ~ after_rw ^^^ ImportToken |
		opt(generalWS) ~ "indirect".r ~ after_rw ^^^ IndirectToken |
		opt(generalWS) ~ "final".r ~ after_rw ^^^ FinalToken |
		opt(generalWS) ~ "open".r ~ after_rw ^^^ OpenToken |
		opt(generalWS) ~ "private".r ~ after_rw ^^^ PrivateToken |
		opt(generalWS) ~ "public".r ~ after_rw ^^^ PublicToken |
		opt(generalWS) ~ "where".r ~ after_rw ^^^ WhereToken |
		opt(generalWS) ~ "while".r ~ after_rw ^^^ WhileToken |
		opt(generalWS) ~ "let".r ~ after_rw ^^^ LetToken |
		opt(generalWS) ~ "var".r ~ after_rw ^^^ VarToken |
		opt(generalWS) ~ "protocol".r ~ after_rw ^^^ ProtocolToken |
		opt(generalWS) ~ "getter".r ~ after_rw ^^^ GetterToken |
		opt(generalWS) ~ "setter".r ~ after_rw ^^^ SetterToken |
		opt(generalWS) ~ "willSet".r ~ after_rw ^^^ WillSetToken |
		opt(generalWS) ~ "didSet".r ~ after_rw ^^^ DidSetToken |
		opt(generalWS) ~ "repeat".r ~ after_rw ^^^ RepeatToken |
		opt(generalWS) ~ "switch".r ~ after_rw ^^^ SwitchToken |
		opt(generalWS) ~ "struct".r ~ after_rw ^^^ StructToken |
		opt(generalWS) ~ "return".r ~ after_rw ^^^ ReturnToken |
		opt(generalWS) ~ "throws".r ~ after_rw ^^^ ThrowsToken |
		opt(generalWS) ~ "throw".r ~ after_rw ^^^ ThrowToken | 
		opt(generalWS) ~ "rethrows".r ~ after_rw ^^^ RethrowsToken |
		opt(generalWS) ~ "init".r ~ after_rw ^^^ InitToken |
		opt(generalWS) ~ "infix".r ~ after_rw ^^^ InfixToken |
		opt(generalWS) ~ "deinit".r ~ after_rw ^^^ DeinitToken |
		opt(generalWS) ~ "assignment".r ~ after_rw ^^^ AssignmentToken |
		opt(generalWS) ~ "extension".r ~ after_rw ^^^ ExtensionToken |
		opt(generalWS) ~ "subscript".r ~ after_rw ^^^ SubscriptToken |
		opt(generalWS) ~ "prefix".r ~ after_rw ^^^ PrefixToken |
		opt(generalWS) ~ "inout".r ~ after_rw ^^^ InOutToken |
		opt(generalWS) ~ "left".r ~ after_rw ^^^ LeftToken |
		opt(generalWS) ~ "right".r ~ after_rw ^^^ RightToken |
		opt(generalWS) ~ "none".r ~ after_rw ^^^ NoneToken |
		opt(generalWS) ~ "precedencegroup".r ~ after_rw ^^^ PrecedenceGroupToken |
		opt(generalWS) ~ "higherThan".r ~ after_rw ^^^ HigherThanToken |
		opt(generalWS) ~ "lowerThan".r ~ after_rw ^^^ LowerThanToken |
		opt(generalWS) ~ "associativity".r ~ after_rw ^^^ AssociativityToken |
		opt(generalWS) ~ "as".r ~ after_rw ^^^ AsToken |
		opt(generalWS) ~ "postfix".r ~ after_rw ^^^ PostFixToken |
		opt(generalWS) ~ "operator".r ~ after_rw ^^^ OperatorToken |
		opt(generalWS) ~ "typealias".r ~ after_rw ^^^ TypeAliasToken |
		opt(generalWS) ~ "os".r ~ after_rw ^^^ OSToken |
		opt(generalWS) ~ "arch".r ~ after_rw ^^^ ArchToken |
		opt(generalWS) ~ "swift".r ~ after_rw ^^^ SwiftToken |
		opt(generalWS) ~ "compiler".r ~ after_rw ^^^ CompilerToken |
		opt(generalWS) ~ "canImport".r ~ after_rw ^^^ CanImportToken |
		opt(generalWS) ~ "targetEnvironment".r ~ after_rw ^^^ TargetEnvironmentToken |
		opt(generalWS) ~ "convenience".r ~ after_rw ^^^ ConvenienceToken |
		opt(generalWS) ~ "dynamic".r ~ after_rw ^^^ DynamicToken |
		opt(generalWS) ~ "lazy".r ~ after_rw ^^^ LazyToken |
		opt(generalWS) ~ "optional".r ~ after_rw ^^^ OptionalToken |
		opt(generalWS) ~ "override".r ~ after_rw ^^^ OverrideToken |
		opt(generalWS) ~ "required".r ~ after_rw ^^^ RequiredToken |
		opt(generalWS) ~ "static".r ~ after_rw ^^^ StaticToken |
		opt(generalWS) ~ "weak".r ~ after_rw ^^^ WeakToken |
		opt(generalWS) ~ "unowned".r ~ after_rw ^^^ UnownedToken |
		opt(generalWS) ~ "safe".r ~ after_rw ^^^ SafeToken |
		opt(generalWS) ~ "unsafe".r ~ after_rw ^^^ UnsafeToken |
		opt(generalWS) ~ "mutating".r ~ after_rw ^^^ MutatingToken |
		opt(generalWS) ~ "nonmutating".r ~ after_rw ^^^ NonmutatingToken |
		opt(generalWS) ~ "fileprivate".r ~ after_rw ^^^ FilePrivateToken |
		opt(generalWS) ~ "is".r ~ after_rw ^^^ IsToken |
		opt(generalWS) ~ "try".r ~ after_rw ^^^ TryToken |
		opt(generalWS) ~ "super".r ~ after_rw ^^^ SuperToken |
		opt(generalWS) ~ "Any".r ~ after_rw ^^^ AnyToken |
		opt(generalWS) ~ "any".r ~ after_rw ^^^ SmallAnyToken |
		opt(generalWS) ~ "false".r ~ after_rw ^^^ FalseToken |
		opt(generalWS) ~ "red".r ~ after_rw ^^^ RedToken |
		opt(generalWS) ~ "blue".r ~ after_rw ^^^ BlueToken |
		opt(generalWS) ~ "green".r ~ after_rw ^^^ GreenToken |
		opt(generalWS) ~ "resourceName".r ~ after_rw ^^^ ResourceNameToken |
		opt(generalWS) ~ "true".r ~ after_rw ^^^ TrueToken |
		opt(generalWS) ~ "nil".r ~ after_rw ^^^ NilToken |
		opt(generalWS) ~ "in".r ~ after_rw ^^^ InToken |
		opt(generalWS) ~ "some".r ~ after_rw ^^^ SomeToken |
		opt(generalWS) ~ "Type".r ~ after_rw ^^^ TypeToken |
		opt(generalWS) ~ "precedence".r ~ after_rw ^^^ PrecedenceToken |
		opt(generalWS) ~ "self".r ~ after_rw ^^^ SelfToken |
		opt(generalWS) ~ "Self".r ~ after_rw ^^^ SelfBigToken |
		opt(generalWS) ~ "macOSApplicationExtension".r ~ after_rw ^^^ MacOSApplicationExtensionToken |
		opt(generalWS) ~ "iOSApplicationExtension".r ~ after_rw ^^^ IOSApplicationExtensionToken |
		opt(generalWS) ~ "OSX".r ~ after_rw ^^^ OSXToken |
		opt(generalWS) ~ "watchOSApplicationExtension".r ~ after_rw ^^^ WatchOSApplicationExtensionToken |
		opt(generalWS) ~ "watchOS".r ~ after_rw ^^^ WatchOSToken |
		opt(generalWS) ~ "watchOSApplicationExtension".r ~ after_rw ^^^ TVOSApplicationExtensionToken |
		opt(generalWS) ~ "tvOS".r ~ after_rw ^^^ TVOSToken |
		opt(generalWS) ~ "Linux".r ~ after_rw ^^^ LinuxToken |
		opt(generalWS) ~ "Windows".r ~ after_rw ^^^ WindowsToken |
		opt(generalWS) ~ "i386".r ~ after_rw ^^^ I386Token |
		opt(generalWS) ~ "x86_64".r ~ after_rw ^^^ X86_64Token |
		opt(generalWS) ~ "arm64".r ~ after_rw ^^^ Arm64Token |
		opt(generalWS) ~ "arm".r ~ after_rw ^^^ ArmToken |
		opt(generalWS) ~ "simulator".r ~ after_rw ^^^ SimulatorToken |
		opt(generalWS) ~ "macCatalystApplicationExtension".r ~ after_rw ^^^ MacCatalystApplicationExtensionToken |
		opt(generalWS) ~ "iOS".r ~ after_rw ^^^ IOSToken |
		opt(generalWS) ~ "macCatalyst".r ~ after_rw ^^^ MacCatalystToken |
		opt(generalWS) ~ "macOS".r ~ after_rw ^^^ MacOSToken |
		opt(generalWS) ~ "#sourceLocation".r ~ after_rw ^^^ SourceLocationToken |
		opt(generalWS) ~ "file".r ~ after_rw ^^^ FileToken |
		opt(generalWS) ~ "line".r ~ after_rw ^^^ LineToken |
		//#error => ErrorToken
		//#warning => WarningToken
		opt(generalWS) ~ "#available".r ~ after_rw ^^^ AvailableToken |
		opt(generalWS) ~ "#unavailable".r ~ after_rw ^^^ UnavailableToken |
		opt(generalWS) ~ "#if".r ~ after_rw ^^^ HashIfToken |
		opt(generalWS) ~ "#elseif".r ~ after_rw ^^^ HashElseIfToken |
		opt(generalWS) ~ "#else".r ~ after_rw ^^^ HashElseToken |
		opt(generalWS) ~ "#endif".r ~ after_rw ^^^ HashEndIfToken |
		//#fileID => HashFileIDToken
		//#filePath => HashFilePathToken
		opt(generalWS) ~ "#fileLiteral".r ~ after_rw ^^^ HashFileLiteralToken |
		//#line => HashLineToken
		//#column => HashColumnToken
		//#function => HashFunctionToken
		//#dsohandle => HashDSOHandleToken
		opt(generalWS) ~ "#selector".r ~ after_rw ^^^ HashSelectorToken |
		opt(generalWS) ~ "#keyPath".r ~ after_rw ^^^ HashKeyPathToken |
		opt(generalWS) ~ "#colorLiteral".r ~ after_rw ^^^ HashColorLiteralToken |
		//#file => HashFileToken
		opt(generalWS) ~ "#imageLiteral".r ~ after_rw ^^^ HashImageLiteralToken |
		opt(generalWS) ~ "get".r ~ after_rw ^^^ GetToken |
		opt(generalWS) ~ "set".r ~ after_rw ^^^ SetToken |
		opt(generalWS) ~ "async".r ~ after_rw ^^^ AsyncToken |
		opt(generalWS) ~ "await".r ~ after_rw ^^^ AwaitToken |
		opt(generalWS) ~ "nonisolated".r ~ after_rw ^^^ NonIsolatedToken |
		opt(generalWS) ~ "actor".r ~ after_rw ^^^ ActorToken
	}

	//example:
	//	as a variable			: var _ = "hi"
	//	as an underscore token	: case(_ , 1) 
	//verdict: going to tokenize all single underscore characters as underscore tokens and disambiguate in the parser.
	def underscore = positioned { opt(generalWS) ~ "_".r ~ after_rw ^^^ UnderscoreToken }
	
	//tokenize a variable
	def variable: Parser[VariableToken] = positioned {
		opt(generalWS) ~ "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { case _ ~ str => VariableToken(str) } |
		opt(generalWS) ~ "[`][a-zA-Z_][a-zA-Z0-9_]*[`]".r ^^ { case _ ~ str => VariableToken(str) }
	}
	
	//tokenize either an implicit parameter or a property wrapper projection
	def implicit_parameter_OR_property_wrapper_projection = positioned {
		opt(generalWS) ~ "[$][a-zA-Z_][a-zA-Z0-9_]*".r ^^ { case _ ~ str => PropertyWrapperProjectionToken(str) } |
		opt(generalWS) ~ "[$][0-9]+[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { case _ ~ str => PropertyWrapperProjectionToken(str) } |
		opt(generalWS) ~ "[$][0-9]+".r ^^ { case _ ~ str => ImplicitParameterOrPropertyWrapperProjectionToken(str) }
	}
	
	def float_literal = positioned {
		opt(generalWS) ~ "[0][x][0-9a-fA-F][0-9a-fA-F_]*[.][0-9a-fA-F][0-9a-fA-F_]*[Pp][+-]?[0-9a-fA-F][0-9a-fA-F_]*".r ^^ { case _ ~ str => FloatHexLiteralToken(str) } |
		opt(generalWS) ~ "[0][x][0-9a-fA-F][0-9a-fA-F_]*[.][0-9a-fA-F][0-9a-fA-F_]*".r ^^ { case _ ~ str => FloatHexLiteralToken(str) } |
		opt(generalWS) ~ "[0][x][0-9a-fA-F][0-9a-fA-F_]*[Pp][+-]?[0-9a-fA-F][0-9a-fA-F_]*".r ^^ { case _ ~ str => FloatHexLiteralToken(str) } |
		opt(generalWS) ~ "[0-9][0-9_]*[.][0-9][0-9_]*[Ee][+-]?[0-9][0-9_]*".r ^^ { case _ ~ str => FloatDecimalLiteralToken(str) } |
		opt(generalWS) ~ "[0-9][0-9_]*[.][0-9][0-9_]*".r ^^ { case _ ~ str => FloatDecimalLiteralToken(str) } |
		opt(generalWS) ~ "[0-9][0-9_]*[Ee][+-]?[0-9][0-9_]*".r ^^ { case _ ~ str => FloatDecimalLiteralToken(str) }
	}
	
	def integer_literal = positioned {
		opt(generalWS) ~ "[0][b][01][01_]*".r ^^ { case _ ~ str => BinaryIntegerLiteralToken(str) } |
		opt(generalWS) ~ "[0][o][0-7][0-7_]*".r ^^ { case _ ~ str => OctalIntegerLiteralToken(str) } |
		opt(generalWS) ~ "[0][x][0-9a-fA-F][0-9a-fA-F_]*".r ^^ { case _ ~ str => HexIntegerLiteralToken(str) } |
		opt(generalWS) ~ "[0-9][0-9_]*".r ^^ { case _ ~ str => DecimalIntegerLiteralToken(str) }
	}
	
	def comments = positioned {
		opt(generalWS) ~ """[/][/].*([\n]|$)""".r ^^ { case _ ~ str => SingleLineCommentToken(str) } |
		opt(generalWS) ~ "[/][*](.|\n)*?[*][/]".r ^^ { case _ ~ str => MultiLineCommentToken(str) }
	}
	
	def operators: Parser[Token] = positioned {
		//regular operators
		//weird hacky special case for ==<
		(generalWS | unmatched_left_unbound) ~ "==".r ~ "(?=[<])".r ^^ { case _ ~ str ~ _ => InfixOperatorLiteralToken(str) } |
		//not bound on either side(infix)
		(generalWS | unmatched_left_unbound) ~ """[/=+!*%<>&|^~?-]+""".r ~ (unmatchedGeneralWS | unmatched_right_unbound) ^^ { case _ ~ str ~ _ => InfixOperatorLiteralToken(str) } |
		//weird hacky special case for ? in postfix expressions
		"[?]".r ~ "(?=[./=+!*%<>&|^~?-])".r ^^ { case str ~ _ => PostfixOperatorLiteralToken(str) } |
		//weird hacky special case for multiple > in a nested generic
		"[>]".r ~ "(?=[>])".r ^^ { case str ~ _ => PostfixOperatorLiteralToken(str) } |
		//weird hacky special case for >? and >!
		"[>]".r ~ "(?=[!?])".r ^^ { case str ~ _ => InfixOperatorLiteralToken(str)} |
		//bound on left side(postfix)
		"""[/=+!*%<>&|^~?-]+""".r ~ (unmatchedGeneralWS | unmatched_right_unbound) ^^ { case str ~ _ => PostfixOperatorLiteralToken(str) } |
		//very partial patch for issue with multi line comments in the middle of an operator that's reg on left side and dot on the other
		"""[=+!%<>&|^~?-]+""".r ~ comments ~ """(?=[.][=+!%<>&|^~?-]*)""".r ~ unmatched_notWS ^^ { case str ~ _ ~ _ ~ _ => InfixOperatorLiteralToken(str) } |		
		//bound on both sides(infix)
		"""[/=+!*%<>&|^~?-]+""".r ~ unmatched_notWS ^^ { case str ~ _ => InfixOperatorLiteralToken(str) } |		
		//bound on the right side(prefix)
		(generalWS | unmatched_left_unbound) ~  """[/=+!*%<>&|^~?-]+""".r ~ unmatched_notWS ^^ { case _ ~ str ~ _ => PrefixOperatorLiteralToken(str) } |
		//dot operators
		//not bound on either side(infix)
		(generalWS | unmatched_left_unbound) ~ """[.][./=+!*%<>&|^~?-]*""".r ~ (unmatchedGeneralWS | unmatched_right_unbound) ^^ { case _ ~ str ~ _ => InfixDotOperatorLiteralToken(str) } |
		//bound on left side(postfix)
		"""[.][./=+!*%<>&|^~?-]*""".r ~ (unmatchedGeneralWS | unmatched_right_unbound) ^^ { case str ~ _ => PostfixDotOperatorLiteralToken(str) } |
		//bound on both sides(infix)
		"""[.][./=+!*%<>&|^~?-]*""".r ~ unmatched_notWS ^^ { case str ~ _ => InfixDotOperatorLiteralToken(str) } |
		//bound on the right side(prefix)
		(generalWS | unmatched_left_unbound) ~ """[.][./=+!*%<>&|^~?-]*""".r ~ unmatched_notWS ^^ { case _ ~ str ~ _ => PrefixDotOperatorLiteralToken(str) }
	}
	
	def reservedSymbols: Parser[Token] = positioned {
		opt(generalWS) ~ """[\\]""".r ^^^ BackSlashToken |
		opt(generalWS) ~ """[{]""".r ^^^ LeftCurlyToken |
		opt("\\t\\r\\f".r) ~ "\n".r ~ """[(]""".r ^^^ LeftParenNewLineToken |
		opt(generalWS) ~ """[(]""".r ^^^ LeftParenToken |
		opt(generalWS) ~ "[\\[]".r ^^^ LeftBracketToken |
		opt(generalWS) ~ "[\\}]".r ^^^ RightCurlyToken |
		opt(generalWS) ~ "[\\)]".r ^^^ RightParenToken |
		opt(generalWS) ~ "[\\]]".r ^^^ RightBracketToken |
		opt(generalWS) ~ "[,]".r ^^^ CommaToken |
		opt(generalWS) ~ "[:]".r ^^^ ColonToken |
		opt(generalWS) ~ "[;]".r ^^^ SemicolonToken |
		opt(generalWS) ~ "[@]".r ^^^ AtToken |
		opt(generalWS) ~ "[#]".r ^^^ HashToken |
		opt(generalWS) ~ "[`]".r ^^^ BackTickToken |
		opt(generalWS) ~ "[$]".r ^^^ DollarToken
	}
	
	def strings = positioned {
		opt(generalWS) ~ """["][^"]["]""".r ^^ { case _ ~ charString => CharLiteralToken(charString.substring(1, charString.length-1)) } |
		opt(generalWS) ~ """["][^\x00-\x7F]+["]""".r ^^ { case _ ~ unicodeChar => CharLiteralToken(unicodeChar.substring(1, unicodeChar.length-1)) } |
		opt(generalWS) ~ """["]["]["]([^"]|\n)*["]["]["]""".r ^^ { case _ ~ str => MultiLineStringLiteralToken(str.substring(3, str.length-3)) } |
		opt(generalWS) ~ """["][^"]*[\\][(][^"]*["]""".r ^^ { case _ ~ str => InterpolatedStringLiteralToken(str.substring(1, str.length-1)) } |
		opt(generalWS) ~ """["][^"]*["]""".r ^^ { case _ ~ str => SingleLineStringLiteralToken(str.substring(1, str.length-1)) }
	}
	
	def tokens: Parser[List[Token]] = {
		phrase((rep(reserved_words | underscore  | variable | implicit_parameter_OR_property_wrapper_projection
				| float_literal | integer_literal | comments | strings | reservedSymbols | operators) ~ rep(generalWS)).map(_._1))
	}
	
	
	def apply(fileName: String, code: String): Seq[Token] = {
		parse(tokens, code) match {
			case NoSuccess(message, next) => {
				val theLocation = Location(next.pos.line, next.pos.column)
				throw new TokenizerException(s"$fileName has this message: $message in position $theLocation")
			}
			case Success(result, next) => result
		}
	} 
}