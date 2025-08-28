package FuzzingSwift.tokenizer

import org.scalatest.FlatSpec

class TokenizerTest extends FlatSpec {
	import FuzzingSwift.tokenizer._
//assertThrows[TokenizerException]
	
	//reserved words testing
	
	"Tokenizer" should "handle an as token" in {
		assertResult(List(AsToken, VariableToken("x"))) { Tokenizer("test", "as x") }
	}
	
	"Tokenizer" should "handle an alpha token" in {
		assertResult(List(AlphaToken, VariableToken("x"))) { Tokenizer("test", "alpha x") }
	}
	
	"Tokenizer" should "handle an alpha token followed by an as token" in {
		assertResult(List(AlphaToken, AsToken, VariableToken("x"))) { Tokenizer("test", "alpha as x") }
	}
	
	"Tokenizer" should "handle a break token" in {
		assertResult(List(BreakToken, VariableToken("x"))) { Tokenizer("test", "break x") }
	}
	
	"Tokenizer" should "handle a case token" in {
		assertResult(List(CaseToken, VariableToken("x"))) { Tokenizer("test", "case x") }
	}
	
	"Tokenizer" should "handle a catch token" in {
		assertResult(List(CatchToken, VariableToken("x"))) { Tokenizer("test", "catch x") }
	}
	
	"Tokenizer" should "handle a class token" in {
		assertResult(List(ClassToken, VariableToken("x"))) { Tokenizer("test", "class x") }
	}
	
	"Tokenizer" should "handle a continue token" in {
		assertResult(List(ContinueToken, VariableToken("x"))) { Tokenizer("test", "continue x") }
	}
	
	"Tokenizer" should "handle a default token" in {
		assertResult(List(DefaultToken, VariableToken("x"))) { Tokenizer("test", "default x") }
	}
	
	"Tokenizer" should "handle a defer token" in {
		assertResult(List(DeferToken, VariableToken("x"))) { Tokenizer("test", "defer x") }
	}
	
	"Tokenizer" should "handle a do token" in {
		assertResult(List(DoToken, VariableToken("x"))) { Tokenizer("test", "do x") }
	}
	
	"Tokenizer" should "handle a guard token" in {
		assertResult(List(GuardToken, VariableToken("x"))) { Tokenizer("test", "guard x") }
	}
	
	"Tokenizer" should "handle an else token" in {
		assertResult(List(ElseToken, VariableToken("x"))) { Tokenizer("test", "else x") }
	}
	
	"Tokenizer" should "handle an enum token" in {
		assertResult(List(EnumToken, VariableToken("x"))) { Tokenizer("test", "enum x") }
	}
	
	"Tokenizer" should "handle a for token" in {
		assertResult(List(ForToken, VariableToken("x"))) { Tokenizer("test", "for x") }
	}
	
	"Tokenizer" should "handle a fallthrough token" in {
		assertResult(List(FallthroughToken, VariableToken("x"))) { Tokenizer("test", "fallthrough x") }
	}
	
	"Tokenizer" should "handle a func token" in {
		assertResult(List(FuncToken, VariableToken("x"))) { Tokenizer("test", "func x") }
	}
	
	"Tokenizer" should "handle an in token" in {
		assertResult(List(InToken, VariableToken("x"))) { Tokenizer("test", "in x") }
	}
	
	"Tokenizer" should "handle an if token" in {
		assertResult(List(IfToken, VariableToken("x"))) { Tokenizer("test", "if x") }
	}
	
	"Tokenizer" should "handle an import token" in {
		assertResult(List(ImportToken, VariableToken("x"))) { Tokenizer("test", "import x") }
	}
	
	"Tokenizer" should "handle an internal token" in {
		assertResult(List(InternalToken, VariableToken("x"))) { Tokenizer("test", "internal x") }
	}
	
	"Tokenizer" should "handle a final token" in {
		assertResult(List(FinalToken, VariableToken("x"))) { Tokenizer("test", "final x") }
	}
	
	"Tokenizer" should "handle an open token" in {
		assertResult(List(OpenToken, VariableToken("x"))) { Tokenizer("test", "open x") }
	}
	
	"Tokenizer" should "handle a private token" in {
		assertResult(List(PrivateToken, VariableToken("x"))) { Tokenizer("test", "private x") }
	}
	
	"Tokenizer" should "handle a public token" in {
		assertResult(List(PublicToken, VariableToken("x"))) { Tokenizer("test", "public x") }
	}
	
	"Tokenizer" should "handle a where token" in {
		assertResult(List(WhereToken, VariableToken("x"))) { Tokenizer("test", "where x") }
	}
	
	"Tokenizer" should "handle a while token" in {
		assertResult(List(WhileToken, VariableToken("x"))) { Tokenizer("test", "while x") }
	}
	
	"Tokenizer" should "handle a let token" in {
		assertResult(List(LetToken, VariableToken("x"))) { Tokenizer("test", "let x") }
	}
	
	"Tokenizer" should "handle a var token" in {
		assertResult(List(VarToken, VariableToken("x"))) { Tokenizer("test", "var x") }
	}
	
	"Tokenizer" should "handle a protocol token" in {
		assertResult(List(ProtocolToken, VariableToken("x"))) { Tokenizer("test", "protocol x") }
	}
	
	"Tokenizer" should "handle a get token" in {
		assertResult(List(GetToken, VariableToken("x"))) { Tokenizer("test", "get x") }
	}
	
	"Tokenizer" should "handle a set token" in {
		assertResult(List(SetToken, VariableToken("x"))) { Tokenizer("test", "set x") }
	}
	
	"Tokenizer" should "handle a willSet token" in {
		assertResult(List(WillSetToken, VariableToken("x"))) { Tokenizer("test", "willSet x") }
	}
	
	"Tokenizer" should "handle a didSet token" in {
		assertResult(List(DidSetToken, VariableToken("x"))) { Tokenizer("test", "didSet x") }
	}
	
	"Tokenizer" should "handle a repeat token" in {
		assertResult(List(RepeatToken, VariableToken("x"))) { Tokenizer("test", "repeat x") }
	}
	
	"Tokenizer" should "handle a switch token" in {
		assertResult(List(SwitchToken, VariableToken("x"))) { Tokenizer("test", "switch x") }
	}
	
	"Tokenizer" should "handle a struct token" in {
		assertResult(List(StructToken, VariableToken("x"))) { Tokenizer("test", "struct x") }
	}
	
	"Tokenizer" should "handle a return token" in {
		assertResult(List(ReturnToken, VariableToken("x"))) { Tokenizer("test", "return x") }
	}
	
	"Tokenizer" should "handle a throw token" in {
		assertResult(List(ThrowToken, VariableToken("x"))) { Tokenizer("test", "throw x") }
	}
	
	"Tokenizer" should "handle a throws token" in {
		assertResult(List(ThrowsToken, VariableToken("x"))) { Tokenizer("test", "throws x") }
	}
	
	"Tokenizer" should "handle a rethrows token" in {
		assertResult(List(RethrowsToken, VariableToken("x"))) { Tokenizer("test", "rethrows x") }
	}
	
	"Tokenizer" should "handle an indirect token" in {
		assertResult(List(IndirectToken, VariableToken("x"))) { Tokenizer("test", "indirect x") }
	}
	
	"Tokenizer" should "handle an init token" in {
		assertResult(List(InitToken, VariableToken("x"))) { Tokenizer("test", "init x") }
	}
	
	"Tokenizer" should "handle a deinit token" in {
		assertResult(List(DeinitToken, VariableToken("x"))) { Tokenizer("test", "deinit x") }
	}
	
	"Tokenizer" should "handle an associatedType token" in {
		assertResult(List(AssociatedTypeToken, VariableToken("x"))) { Tokenizer("test", "associatedtype x") }
	}
	
	"Tokenizer" should "handle an extension token" in {
		assertResult(List(ExtensionToken, VariableToken("x"))) { Tokenizer("test", "extension x") }
	}
	
	"Tokenizer" should "handle a subscript token" in {
		assertResult(List(SubscriptToken, VariableToken("x"))) { Tokenizer("test", "subscript x") }
	}
	
	"Tokenizer" should "handle a prefix token" in {
		assertResult(List(PrefixToken, VariableToken("x"))) { Tokenizer("test", "prefix x") }
	}
	
	"Tokenizer" should "handle an infix token" in {
		assertResult(List(InfixToken, VariableToken("x"))) { Tokenizer("test", "infix x") }
	}
	
	"Tokenizer" should "handle a left token" in {
		assertResult(List(LeftToken, VariableToken("x"))) { Tokenizer("test", "left x") }
	}
	
	"Tokenizer" should "handle a right token" in {
		assertResult(List(RightToken, VariableToken("x"))) { Tokenizer("test", "right x") }
	}
	
	"Tokenizer" should "handle a none token" in {
		assertResult(List(NoneToken, VariableToken("x"))) { Tokenizer("test", "none x") }
	}
	
	"Tokenizer" should "handle a precedencegroup token" in {
		assertResult(List(PrecedenceGroupToken, VariableToken("x"))) { Tokenizer("test", "precedencegroup x") }
	}
	
	"Tokenizer" should "handle a higherThan token" in {
		assertResult(List(HigherThanToken, VariableToken("x"))) { Tokenizer("test", "higherThan x") }
	}
	
	"Tokenizer" should "handle a lowerThan token" in {
		assertResult(List(LowerThanToken, VariableToken("x"))) { Tokenizer("test", "lowerThan x") }
	}
	
	"Tokenizer" should "handle an assignment token" in {
		assertResult(List(AssignmentToken, VariableToken("x"))) { Tokenizer("test", "assignment x") }
	}
	
	"Tokenizer" should "handle an associativity token" in {
		assertResult(List(AssociativityToken, VariableToken("x"))) { Tokenizer("test", "associativity x") }
	}
	
	"Tokenizer" should "handle a postfix token" in {
		assertResult(List(PostFixToken, VariableToken("x"))) { Tokenizer("test", "postfix x") }
	}
	
	"Tokenizer" should "handle an operator token" in {
		assertResult(List(OperatorToken, VariableToken("x"))) { Tokenizer("test", "operator x") }
	}
	
	"Tokenizer" should "handle a typealias token" in {
		assertResult(List(TypeAliasToken, VariableToken("x"))) { Tokenizer("test", "typealias x") }
	}
	
	"Tokenizer" should "handle an os token" in {
		assertResult(List(OSToken, VariableToken("x"))) { Tokenizer("test", "os x") }
	}
	
	"Tokenizer" should "handle an arch token" in {
		assertResult(List(ArchToken, VariableToken("x"))) { Tokenizer("test", "arch x") }
	}
	
	"Tokenizer" should "handle a swift token" in {
		assertResult(List(SwiftToken, VariableToken("x"))) { Tokenizer("test", "swift x") }
	}
	
	"Tokenizer" should "handle a compiler token" in {
		assertResult(List(CompilerToken, VariableToken("x"))) { Tokenizer("test", "compiler x") }
	}
	
	"Tokenizer" should "handle a canImport token" in {
		assertResult(List(CanImportToken, VariableToken("x"))) { Tokenizer("test", "canImport x") }
	}
	
	"Tokenizer" should "handle a targetEnvironment token" in {
		assertResult(List(TargetEnvironmentToken, VariableToken("x"))) { Tokenizer("test", "targetEnvironment x") }
	}
	
	"Tokenizer" should "handle a convenience token" in {
		assertResult(List(ConvenienceToken, VariableToken("x"))) { Tokenizer("test", "convenience x") }
	}
	
	"Tokenizer" should "handle a dynamic token" in {
		assertResult(List(DynamicToken, VariableToken("x"))) { Tokenizer("test", "dynamic x") }
	}
	
	"Tokenizer" should "handle a lazy token" in {
		assertResult(List(LazyToken, VariableToken("x"))) { Tokenizer("test", "lazy x") }
	}
	
	"Tokenizer" should "handle an optional token" in {
		assertResult(List(OptionalToken, VariableToken("x"))) { Tokenizer("test", "optional x") }
	}
	
	"Tokenizer" should "handle an override token" in {
		assertResult(List(OverrideToken, VariableToken("x"))) { Tokenizer("test", "override x") }
	}
	
	"Tokenizer" should "handle a required token" in {
		assertResult(List(RequiredToken, VariableToken("x"))) { Tokenizer("test", "required x") }
	}
	
	"Tokenizer" should "handle a static token" in {
		assertResult(List(StaticToken, VariableToken("x"))) { Tokenizer("test", "static x") }
	}
	
	"Tokenizer" should "handle a weak token" in {
		assertResult(List(WeakToken, VariableToken("x"))) { Tokenizer("test", "weak x") }
	}
	
	"Tokenizer" should "handle an unowned token" in {
		assertResult(List(UnownedToken, VariableToken("x"))) { Tokenizer("test", "unowned x") }
	}
	
	"Tokenizer" should "handle a safe token" in {
		assertResult(List(SafeToken, VariableToken("x"))) { Tokenizer("test", "safe x") }
	}
	
	"Tokenizer" should "handle an unsafe token" in {
		assertResult(List(UnsafeToken, VariableToken("x"))) { Tokenizer("test", "unsafe x") }
	}
	
	"Tokenizer" should "handle a mutating token" in {
		assertResult(List(MutatingToken, VariableToken("x"))) { Tokenizer("test", "mutating x") }
	}
	
	"Tokenizer" should "handle a nonmutating token" in {
		assertResult(List(NonmutatingToken, VariableToken("x"))) { Tokenizer("test", "nonmutating x") }
	}
	
	"Tokenizer" should "handle a fileprivate token" in {
		assertResult(List(FilePrivateToken, VariableToken("x"))) { Tokenizer("test", "fileprivate x") }
	}
	
	"Tokenizer" should "handle a is token" in {
		assertResult(List(IsToken, VariableToken("x"))) { Tokenizer("test", "is x") }
	}
	
	"Tokenizer" should "handle a try token" in {
		assertResult(List(TryToken, VariableToken("x"))) { Tokenizer("test", "try x") }
	}
	
	"Tokenizer" should "handle a super token" in {
		assertResult(List(SuperToken, VariableToken("x"))) { Tokenizer("test", "super x") }
	}
	
	"Tokenizer" should "handle an Any token" in {
		assertResult(List(AnyToken, VariableToken("x"))) { Tokenizer("test", "Any x") }
	}
	
	"Tokenizer" should "handle a false token" in {
		assertResult(List(FalseToken, VariableToken("x"))) { Tokenizer("test", "false x") }
	}
	
	"Tokenizer" should "handle a red token" in {
		assertResult(List(RedToken, VariableToken("x"))) { Tokenizer("test", "red x") }
	}
	
	"Tokenizer" should "handle a blue token" in {
		assertResult(List(BlueToken, VariableToken("x"))) { Tokenizer("test", "blue x") }
	}
	
	"Tokenizer" should "handle a green token" in {
		assertResult(List(GreenToken, VariableToken("x"))) { Tokenizer("test", "green x") }
	}
	
	"Tokenizer" should "handle a resourceName token" in {
		assertResult(List(ResourceNameToken, VariableToken("x"))) { Tokenizer("test", "resourceName x") }
	}
	
	"Tokenizer" should "handle a true token" in {
		assertResult(List(TrueToken, VariableToken("x"))) { Tokenizer("test", "true x") }
	}
	
	"Tokenizer" should "handle a nil token" in {
		assertResult(List(NilToken, VariableToken("x"))) { Tokenizer("test", "nil x") }
	}
	
	"Tokenizer" should "handle a inout token" in {
		assertResult(List(InOutToken, VariableToken("x"))) { Tokenizer("test", "inout x") }
	}
	
	"Tokenizer" should "handle a some token" in {
		assertResult(List(SomeToken, VariableToken("x"))) { Tokenizer("test", "some x") }
	}
	
	"Tokenizer" should "handle a Type token" in {
		assertResult(List(TypeToken, VariableToken("x"))) { Tokenizer("test", "Type x") }
	}
	
	"Tokenizer" should "handle a precedence token" in {
		assertResult(List(PrecedenceToken, VariableToken("x"))) { Tokenizer("test", "precedence x") }
	}
	
	"Tokenizer" should "handle a self token" in {
		assertResult(List(SelfToken, VariableToken("x"))) { Tokenizer("test", "self x") }
	}
	
	"Tokenizer" should "handle a Self token" in {
		assertResult(List(SelfBigToken, VariableToken("x"))) { Tokenizer("test", "Self x") }
	}
	
	"Tokenizer" should "handle a macOS token" in {
		assertResult(List(MacOSToken, VariableToken("x"))) { Tokenizer("test", "macOS x") }
	}
	
	"Tokenizer" should "handle an iOS token" in {
		assertResult(List(IOSToken, VariableToken("x"))) { Tokenizer("test", "iOS x") }
	}
	
	"Tokenizer" should "handle an OSX token" in {
		assertResult(List(OSXToken, VariableToken("x"))) { Tokenizer("test", "OSX x") }
	}
	
	"Tokenizer" should "handle a watchOS token" in {
		assertResult(List(WatchOSToken, VariableToken("x"))) { Tokenizer("test", "watchOS x") }
	}
	
	"Tokenizer" should "handle a tvOS token" in {
		assertResult(List(TVOSToken, VariableToken("x"))) { Tokenizer("test", "tvOS x") }
	}
	
	"Tokenizer" should "handle a Linux token" in {
		assertResult(List(LinuxToken, VariableToken("x"))) { Tokenizer("test", "Linux x") }
	}
	
	"Tokenizer" should "handle a Windows token" in {
		assertResult(List(WindowsToken, VariableToken("x"))) { Tokenizer("test", "Windows x") }
	}
	
	"Tokenizer" should "handle an i386 token" in {
		assertResult(List(I386Token, VariableToken("x"))) { Tokenizer("test", "i386 x") }
	}
	
	"Tokenizer" should "handle an x86_64 token" in {
		assertResult(List(X86_64Token, VariableToken("x"))) { Tokenizer("test", "x86_64 x") }
	}
	
	"Tokenizer" should "handle an arm token" in {
		assertResult(List(ArmToken, VariableToken("x"))) { Tokenizer("test", "arm x") }
	}
	
	"Tokenizer" should "handle an arm64 token" in {
		assertResult(List(Arm64Token, VariableToken("x"))) { Tokenizer("test", "arm64 x") }
	}
	
	"Tokenizer" should "handle a simulator token" in {
		assertResult(List(SimulatorToken, VariableToken("x"))) { Tokenizer("test", "simulator x") }
	}
	
	"Tokenizer" should "handle a macCatalyst token" in {
		assertResult(List(MacCatalystToken, VariableToken("x"))) { Tokenizer("test", "macCatalyst x") }
	}

	"Tokenizer" should "handle an iOSApplicationExtension token" in {
		assertResult(List(IOSApplicationExtensionToken, VariableToken("x"))) { Tokenizer("test", "iOSApplicationExtension x") }
	}
	
	"Tokenizer" should "handle a macCatalystApplicationExtension token" in {
		assertResult(List(MacCatalystApplicationExtensionToken, VariableToken("x"))) { Tokenizer("test", "macCatalystApplicationExtension x") }
	}
	
	"Tokenizer" should "handle a macOSApplicationExtension token" in {
		assertResult(List(MacOSApplicationExtensionToken, VariableToken("x"))) { Tokenizer("test", "macOSApplicationExtension x") }
	}
	
	"Tokenizer" should "handle a #sourceLocation token" in {
		assertResult(List(SourceLocationToken, VariableToken("x"))) { Tokenizer("test", "#sourceLocation x") }
	}
	
	"Tokenizer" should "handle a file token" in {
		assertResult(List(FileToken, VariableToken("x"))) { Tokenizer("test", "file x") }
	}
	
	"Tokenizer" should "handle a line token" in {
		assertResult(List(LineToken, VariableToken("x"))) { Tokenizer("test", "line x") }
	}
	
	"Tokenizer" should "handle a #available token" in {
		assertResult(List(AvailableToken, VariableToken("x"))) { Tokenizer("test", "#available x") }
	}
	
	"Tokenizer" should "handle a #if token" in {
		assertResult(List(HashIfToken, VariableToken("x"))) { Tokenizer("test", "#if x") }
	}
	
	"Tokenizer" should "handle a #elseif token" in {
		assertResult(List(HashElseIfToken, VariableToken("x"))) { Tokenizer("test", "#elseif x") }
	}
	
	"Tokenizer" should "handle a #else token" in {
		assertResult(List(HashElseToken, VariableToken("x"))) { Tokenizer("test", "#else x") }
	}
	
	"Tokenizer" should "handle a #endif token" in {
		assertResult(List(HashEndIfToken, VariableToken("x"))) { Tokenizer("test", "#endif x") }
	}
	
/* 	"Tokenizer" should "handle a #file token" in {
		assertResult(List(HashFileToken, VariableToken("x"))) { Tokenizer("test", "#file x") }
	}
	
	"Tokenizer" should "handle a #fileID token" in {
		assertResult(List(HashFileIDToken, VariableToken("x"))) { Tokenizer("test", "#fileID x") }
	}
	
	"Tokenizer" should "handle a #filePath token" in {
		assertResult(List(HashFilePathToken, VariableToken("x"))) { Tokenizer("test", "#filePath x") }
	}
	
	"Tokenizer" should "handle a #line token" in {
		assertResult(List(HashLineToken, VariableToken("x"))) { Tokenizer("test", "#line x") }
	}
	
	"Tokenizer" should "handle a #column token" in {
		assertResult(List(HashColumnToken, VariableToken("x"))) { Tokenizer("test", "#column x") }
	}
	
	"Tokenizer" should "handle a #function token" in {
		assertResult(List(HashFunctionToken, VariableToken("x"))) { Tokenizer("test", "#function x") }
	}
	
	"Tokenizer" should "handle a #dsohandle token" in {
		assertResult(List(HashDSOHandleToken, VariableToken("x"))) { Tokenizer("test", "#dsohandle x") }
	} */
	
	"Tokenizer" should "handle a #selector token" in {
		assertResult(List(HashSelectorToken, VariableToken("x"))) { Tokenizer("test", "#selector x") }
	}
	
	"Tokenizer" should "handle a #keyPath token" in {
		assertResult(List(HashKeyPathToken, VariableToken("x"))) { Tokenizer("test", "#keyPath x") }
	}
	
	"Tokenizer" should "handle a #colorLiteral token" in {
		assertResult(List(HashColorLiteralToken, VariableToken("x"))) { Tokenizer("test", "#colorLiteral x") }
	}
	
	"Tokenizer" should "handle a #fileLiteral token" in {
		assertResult(List(HashFileLiteralToken, VariableToken("x"))) { Tokenizer("test", "#fileLiteral x") }
	}
	
	"Tokenizer" should "handle a #imageLiteral token" in {
		assertResult(List(HashImageLiteralToken, VariableToken("x"))) { Tokenizer("test", "#imageLiteral x") }
	}
	
	"Tokenizer" should "handle a getter token" in {
		assertResult(List(GetterToken, VariableToken("x"))) { Tokenizer("test", "getter x") }
	}
	
	"Tokenizer" should "handle a setter token" in {
		assertResult(List(SetterToken, VariableToken("x"))) { Tokenizer("test", "setter x") }
	}
	
	//variable testing
	
	"Tokenizer" should "handle a variable literal with all lowercase letters" in {
		assertResult(List(VariableToken("hello"))) { Tokenizer("test", "hello") }
	}
	
	"Tokenizer" should "handle a variable literal that contains digits" in {
		assertResult(List(VariableToken("h3ll0"))) { Tokenizer("test", "h3ll0") }
	}
	
	"Tokenizer" should "handle a variable literal that starts with an underscore" in {
		assertResult(List(VariableToken("_h3ll0"))) { Tokenizer("test", "_h3ll0") }
	}
	
	"Tokenizer" should "handle this problem" in {
		assertResult(List(VariableToken("var_thing")))  { Tokenizer("test", "var_thing") }
	}

	
	//implicit param and property wrapper projection testing
	
	"Tokenizer" should "handle an implicit parameter literal with one digit" in {
		assertResult(List(ImplicitParameterOrPropertyWrapperProjectionToken("$0"))) { Tokenizer("test", "$0") }
	}
	
	"Tokenizer" should "handle an implicit parameter literal with multiple digits" in {
		assertResult(List(ImplicitParameterOrPropertyWrapperProjectionToken("$032"))) { Tokenizer("test", "$032") }
	}
	
	"Tokenizer" should "handle property wrapper projection with one letter" in {
		assertResult(List(PropertyWrapperProjectionToken("$a"))) { Tokenizer("test", "$a") }
	}
	
	"Tokenizer" should "handle property wrapper projection that starts with a number and has multiple letters" in {
		assertResult(List(PropertyWrapperProjectionToken("$2abc"))) { Tokenizer("test", "$2abc") }
	}
	
	"Tokenizer" should "handle property wrapper projection that has multiple letter letters" in {
		assertResult(List(PropertyWrapperProjectionToken("$hello"))) { Tokenizer("test", "$hello") }
	}
	
	
	//number testing
	
	//decimal integers
	"Tokenizer" should "handle a decimal integer literal with one digit" in {
		assertResult(List(DecimalIntegerLiteralToken("2"))) { Tokenizer("test", "2") }
	}
	
	"Tokenizer" should "handle a decimal integer literal with multiple digits" in {
		assertResult(List(DecimalIntegerLiteralToken("463456"))) { Tokenizer("test", "463456") }
	}
	
	"Tokenizer" should "handle a decimal integer literal with multiple digits and delimeters" in {
		assertResult(List(DecimalIntegerLiteralToken("123_345"))) { Tokenizer("test", "123_345") }
	}
	
	//binary integers
	"Tokenizer" should "handle a binary integer literal with one digit" in {
		assertResult(List(BinaryIntegerLiteralToken("0b1"))) { Tokenizer("test", "0b1") }
	}
	
	"Tokenizer" should "handle a binary integer literal with multiple digits" in {
		assertResult(List(BinaryIntegerLiteralToken("0b1011"))) { Tokenizer("test", "0b1011") }
	}
	
	"Tokenizer" should "handle a binary integer literal with multiple digits and delimeters" in {
		assertResult(List(BinaryIntegerLiteralToken("0b1_011"))) { Tokenizer("test", "0b1_011") }
	}
	
	//octal integers
	"Tokenizer" should "handle an octal integer literal with one digit" in {
		assertResult(List(OctalIntegerLiteralToken("0o5"))) { Tokenizer("test", "0o5") }
	}
	
	"Tokenizer" should "handle an octal integer literal with multiple digits" in {
		assertResult(List(OctalIntegerLiteralToken("0o347"))) { Tokenizer("test", "0o347") }
	}
	
	"Tokenizer" should "handle an octal integer literal with multiple digits and delimeters" in {
		assertResult(List(OctalIntegerLiteralToken("0o34_7"))) { Tokenizer("test", "0o34_7") }
	}
	
	//hex integers
	"Tokenizer" should "handle a hex integer literal with one digit" in {
		assertResult(List(HexIntegerLiteralToken("0x8"))) { Tokenizer("test", "0x8") }
	}
	
	"Tokenizer" should "handle a hex integer literal with multiple digits" in {
		assertResult(List(HexIntegerLiteralToken("0xA4b3"))) { Tokenizer("test", "0xA4b3") }
	}
	
	"Tokenizer" should "handle a hex integer literal with multiple digits and delimeters" in {
		assertResult(List(HexIntegerLiteralToken("0xF_A4b3"))) { Tokenizer("test", "0xF_A4b3") }
	}
	
	//decimal floats
	"Tokenizer" should "handle a decimal float literal with a decimal point" in {
		assertResult(List(FloatDecimalLiteralToken("12.3"))) { Tokenizer("test", "12.3") }
	}
	
	"Tokenizer" should "handle a decimal float literal with a decimal point and delimeters" in {
		assertResult(List(FloatDecimalLiteralToken("12.3_5"))) { Tokenizer("test", "12.3_5") }
	}
	
	"Tokenizer" should "handle a decimal float literal with a floating point e" in {
		assertResult(List(FloatDecimalLiteralToken("12e3"))) { Tokenizer("test", "12e3") }
	}
	
	"Tokenizer" should "handle a decimal float literal with a floating point e and delimeters" in {
		assertResult(List(FloatDecimalLiteralToken("12e3_5"))) { Tokenizer("test", "12e3_5") }
	}
	
	//hex floats
	"Tokenizer" should "handle a hex float literal with a decimal point" in {
		assertResult(List(FloatHexLiteralToken("0x1.2"))) { Tokenizer("test", "0x1.2") }
	}
	
	"Tokenizer" should "handle a hex float literal with a decimal point and delimeters" in {
		assertResult(List(FloatHexLiteralToken("0x1A.3_D"))) { Tokenizer("test", "0x1A.3_D") }
	}
	
	"Tokenizer" should "handle a hex float literal with a floating point p" in {
		assertResult(List(FloatHexLiteralToken("0x12p3"))) { Tokenizer("test", "0x12p3") }
	}
	
	"Tokenizer" should "handle a hex float literal with a floating point p and sign" in {
		assertResult(List(FloatHexLiteralToken("0x12p+3"))) { Tokenizer("test", "0x12p+3") }
	}
	
	"Tokenizer" should "handle a hex float literal with a floating point p, delimeters and sign" in {
		assertResult(List(FloatHexLiteralToken("0x12p+3_4"))) { Tokenizer("test", "0x12p+3_4") }
	}
	
	
	//comment testing
	
	//single line comments
	"Tokenizer" should "handle a single line comment" in {
		assertResult(List(SingleLineCommentToken("//iamacomment\n"))) { Tokenizer("test", "//iamacomment\n") }
	}
	
	//char testing
	"Tokenizer" should "handle a char: a" in {
		assertResult(List(CharLiteralToken("a"))) { Tokenizer("test", "\"a\"") }
	}
	
	//string testing
	
	//single line string
	"Tokenizer" should "handle a regular single line string" in {
		assertResult(List(SingleLineStringLiteralToken("helloiamstring"))) { Tokenizer("test", "\"helloiamstring\"") }
	}
	
	//multi line string
	"Tokenizer" should "handle a regular multi line (actually single line) string" in {
		assertResult(List(MultiLineStringLiteralToken("helloiamstring"))) { Tokenizer("test", "\"\"\"helloiamstring\"\"\"") }
	}
	
	"Tokenizer" should "handle a regular multi line string" in {
		assertResult(List(MultiLineStringLiteralToken("hello\niamstring"))) { Tokenizer("test", "\"\"\"hello\niamstring\"\"\"") }
	}
	
	"Tokenizer" should "handle an interpolated string" in {
		assertResult(List(InterpolatedStringLiteralToken("\\($0)"))) { Tokenizer("test", "\"\\($0)\"")}
	}
	
	//operators
	
	//regular operators
	
	//infix operators (not bound)
	"Tokenizer" should "handle an unbounded infix less than token" in {
		assertResult(List(InfixOperatorLiteralToken("<"), VariableToken("x"))) { Tokenizer("test", " < x") }
	}
	
	"Tokenizer" should "handle an unbounded infix greater than token" in {
		assertResult(List(InfixOperatorLiteralToken(">"), VariableToken("x"))) { Tokenizer("test", " > x") }
	}
	
	"Tokenizer" should "handle an unbounded infix exclamation token" in {
		assertResult(List(InfixOperatorLiteralToken("!"), VariableToken("x"))) { Tokenizer("test", " ! x") }
	}
	
	"Tokenizer" should "handle an unbounded infix question token" in {
		assertResult(List(InfixOperatorLiteralToken("?"), VariableToken("x"))) { Tokenizer("test", " ? x") }
	}
	
	"Tokenizer" should "handle an unbounded infix and token" in {
		assertResult(List(InfixOperatorLiteralToken("&"), VariableToken("x"))) { Tokenizer("test", " & x") }
	}
	
	"Tokenizer" should "handle an unbounded infix minus token" in {
		assertResult(List(InfixOperatorLiteralToken("-"), VariableToken("x"))) { Tokenizer("test", " - x") }
	}

	"Tokenizer" should "handle an unbounded infix equal token" in {
		assertResult(List(InfixOperatorLiteralToken("="), VariableToken("x"))) { Tokenizer("test", " = x") }
	}
	
	"Tokenizer" should "handle an unbounded infix or token" in {
		assertResult(List(InfixOperatorLiteralToken("|"), VariableToken("x"))) { Tokenizer("test", " | x") }
	}
	
	"Tokenizer" should "handle an unbounded infix division token" in {
		assertResult(List(InfixOperatorLiteralToken("/"), VariableToken("x"))) { Tokenizer("test", " / x") }
	}
	
	"Tokenizer" should "handle an unbounded infix addition token" in {
		assertResult(List(InfixOperatorLiteralToken("+"), VariableToken("x"))) { Tokenizer("test", " + x") }
	}
	
	"Tokenizer" should "handle an unbounded infix multiplication token" in {
		assertResult(List(InfixOperatorLiteralToken("*"), VariableToken("x"))) { Tokenizer("test", " * x") }
	}
	
	"Tokenizer" should "handle an unbounded infix mod token" in {
		assertResult(List(InfixOperatorLiteralToken("%"), VariableToken("x"))) { Tokenizer("test", " % x") }
	}
	
	"Tokenizer" should "handle an unbounded infix caret token" in {
		assertResult(List(InfixOperatorLiteralToken("^"), VariableToken("x"))) { Tokenizer("test", " ^ x") }
	}
	
	"Tokenizer" should "handle an unbounded infix tilde token" in {
		assertResult(List(InfixOperatorLiteralToken("~"), VariableToken("x"))) { Tokenizer("test", " ~ x") }
	}
	
	"Tokenizer" should "handle an unbounded infix custom operator token" in {
		assertResult(List(InfixOperatorLiteralToken("->"), VariableToken("x"))) { Tokenizer("test", " -> x") }
	}
	
	//infix operators (bound)
	"Tokenizer" should "handle a bound infix less than token" in {
		assertResult(List(VariableToken("x"),InfixOperatorLiteralToken("<"), VariableToken("x"))) { Tokenizer("test", "x<x") }
	}
	
	"Tokenizer" should "handle a bound infix greater than token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken(">"), VariableToken("x"))) { Tokenizer("test", "x>x") }
	}
	
	"Tokenizer" should "handle a bound infix exclamation token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("!"), VariableToken("x"))) { Tokenizer("test", "x!x") }
	}
	
	"Tokenizer" should "handle a bound infix question token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("?"), VariableToken("x"))) { Tokenizer("test", "x?x") }
	}
	
	"Tokenizer" should "handle a bound infix and token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("&"), VariableToken("x"))) { Tokenizer("test", "x&x") }
	}
	
	"Tokenizer" should "handle a bound infix minus token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("-"), VariableToken("x"))) { Tokenizer("test", "x-x") }
	}

	"Tokenizer" should "handle a bound infix equal token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("="), VariableToken("x"))) { Tokenizer("test", "x=x") }
	}
	
	"Tokenizer" should "handle a bound infix or token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("|"), VariableToken("x"))) { Tokenizer("test", "x|x") }
	}
	
	"Tokenizer" should "handle a bound infix division token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("/"), VariableToken("x"))) { Tokenizer("test", "x/x") }
	}
	
	"Tokenizer" should "handle a bound infix addition token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("+"), VariableToken("x"))) { Tokenizer("test", "x+x") }
	}
	
	"Tokenizer" should "handle a bound infix multiplication token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("*"), VariableToken("x"))) { Tokenizer("test", "x*x") }
	}
	
	"Tokenizer" should "handle a bound infix mod token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("%"), VariableToken("x"))) { Tokenizer("test", "x%x") }
	}
	
	"Tokenizer" should "handle a bound infix caret token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("^"), VariableToken("x"))) { Tokenizer("test", "x^x") }
	}
	
	"Tokenizer" should "handle a bound infix tilde token" in {
		assertResult(List(VariableToken("x"), InfixOperatorLiteralToken("~"), VariableToken("x"))) { Tokenizer("test", "x~x") }
	}
	
	//prefix (bound on right side)
	"Tokenizer" should "handle a prefix less than token" in {
		assertResult(List(PrefixOperatorLiteralToken("<"), VariableToken("x"))) { Tokenizer("test", " <x") }
	}
	
	"Tokenizer" should "handle a prefix greater than token" in {
		assertResult(List(PrefixOperatorLiteralToken(">"), VariableToken("x"))) { Tokenizer("test", " >x") }
	}
	
	//postfix (bound on left side)
	"Tokenizer" should "handle a postfix less than token" in {
		assertResult(List(VariableToken("num5"), PostfixOperatorLiteralToken("<"), VariableToken("x"))) { Tokenizer("test", "num5< x") }
	}
	
	"Tokenizer" should "handle a postfix greater than token" in {
		assertResult(List(DecimalIntegerLiteralToken("5"), PostfixOperatorLiteralToken(">"), VariableToken("x"))) { Tokenizer("test", "5> x") }
	}
	
	//dot operators
	
	//infix
	"Tokenizer" should "handle an infix dot token" in {
		assertResult(List(InfixDotOperatorLiteralToken("."), VariableToken("x"))) { Tokenizer("test", " . x") }
	}
	
	"Tokenizer" should "handle a custom infix dot operator" in {
		assertResult(List(InfixDotOperatorLiteralToken(".!."), VariableToken("x"))) { Tokenizer("test", " .!. x") }
	} 
	
	//prefix
	"Tokenizer" should "handle a prefix dot token" in {
		assertResult(List(PrefixDotOperatorLiteralToken("."), VariableToken("x"))) { Tokenizer("test", " .x") }
	}
	
	"Tokenizer" should "handle a custom prefix dot operator" in {
		assertResult(List(PrefixDotOperatorLiteralToken(".!."), VariableToken("x"))) { Tokenizer("test", " .!.x") }
	}
	
	//postfix
	"Tokenizer" should "handle a postfix dot token" in {
		assertResult(List(PostfixDotOperatorLiteralToken("."), VariableToken("x"))) { Tokenizer("test", ". x") }
	}
	
	"Tokenizer" should "handle a custom postfix dot operator" in {
		assertResult(List(VariableToken("x"), PostfixDotOperatorLiteralToken(".!."), VariableToken("x"))) { Tokenizer("test", "x.!. x") }
	}
	
	
	//reserved symbol testing
	"Tokenizer" should "handle a left curly token" in {
		assertResult(List(LeftCurlyToken)) { Tokenizer("test", "{") }
	}
	
	"Tokenizer" should "handle a left paren token" in {
		assertResult(List(LeftParenToken)) { Tokenizer("test", "\t(") }
	}
	
	"Tokenizer" should "handle a left paren token that is directly following a newline" in {
		assertResult(List(LeftParenNewLineToken)) { Tokenizer("test", "\n(") }
	}
	
	"Tokenizer" should "handle a left bracket token" in {
		assertResult(List(LeftBracketToken)) { Tokenizer("test", "[") }
	}
	
	"Tokenizer" should "handle a right curly token" in {
		assertResult(List(RightCurlyToken)) { Tokenizer("test", "}") }
	}
	
	"Tokenizer" should "handle a right paren token" in {
		assertResult(List(RightParenToken)) { Tokenizer("test", ")") }
	}
	
	"Tokenizer" should "handle a right bracket token" in {
		assertResult(List(RightBracketToken)) { Tokenizer("test", "]") }
	}
	
	"Tokenizer" should "handle a comma token" in {
		assertResult(List(CommaToken)) { Tokenizer("test", ",") }
	}
	
	"Tokenizer" should "handle a colon token" in {
		assertResult(List(ColonToken)) { Tokenizer("test", ":") }
	}
	
	"Tokenizer" should "handle a semicolon token" in {
		assertResult(List(SemicolonToken)) { Tokenizer("test", ";") }
	}
	
	"Tokenizer" should "handle an underscore token" in {
		assertResult(List(UnderscoreToken, VariableToken("x"))) { Tokenizer("test", "_ x") }
	}
	
	"Tokenizer" should "handle an at token" in {
		assertResult(List(AtToken)) { Tokenizer("test", "@") }
	}
	
	"Tokenizer" should "handle a hash token" in {
		assertResult(List(HashToken)) { Tokenizer("test", "#") }
	}
	
	"Tokenizer" should "handle a backtick token" in {
		assertResult(List(BackTickToken)) { Tokenizer("test", "`") }
	}
	
	"Tokenizer" should "handle a backslash token" in {
		assertResult(List(BackSlashToken)) { Tokenizer("test", "\\") }
	}
	
	
	
	//complex testing
	"Tokenizer" should "handle a complex test" in {
		assertResult(List(FuncToken, VariableToken("factorial"), LeftParenToken, VariableToken("n"), ColonToken,  VariableToken("Int"), RightParenToken, InfixOperatorLiteralToken("->"),  VariableToken("Int")))							
			{ Tokenizer("test", "func factorial(n: Int) -> Int") }
	}
	
	"Tokenizer" should "handle a second complex test" in {
		assertResult(List(LeftCurlyToken, IfToken, VariableToken("n"), InfixOperatorLiteralToken("<="), DecimalIntegerLiteralToken("1"), LeftCurlyToken, ReturnToken, VariableToken("n"), RightCurlyToken))							
			{ Tokenizer("test", "{ if n <= 1 { return n }") }
	}	
	
	"Tokenizer" should "handle a third complex test" in {
		assertResult(List(ReturnToken, VariableToken("n"), InfixOperatorLiteralToken("*"), VariableToken("factorial"), LeftParenToken, VariableToken("n"), ColonToken, VariableToken("n"), InfixOperatorLiteralToken("-"), DecimalIntegerLiteralToken("1"), RightParenToken, RightCurlyToken))							
			{ Tokenizer("test", "return n * factorial(n: n - 1) }") }
	}
	
	"Tokenizer" should "handle a combination of the above complex tests" in {
		assertResult(List(FuncToken, VariableToken("factorial"), LeftParenToken, VariableToken("n"), ColonToken,  VariableToken("Int"), RightParenToken, InfixOperatorLiteralToken("->"),  VariableToken("Int"), 
							LeftCurlyToken, IfToken, VariableToken("n"), InfixOperatorLiteralToken("<="), DecimalIntegerLiteralToken("1"), LeftCurlyToken, ReturnToken, VariableToken("n"), RightCurlyToken,
							ReturnToken, VariableToken("n"), InfixOperatorLiteralToken("*"), VariableToken("factorial"), LeftParenToken, VariableToken("n"), ColonToken, VariableToken("n"), InfixOperatorLiteralToken("-"), DecimalIntegerLiteralToken("1"), RightParenToken, RightCurlyToken))							
			{ Tokenizer("test", "func factorial(n: Int) -> Int { if n <= 1 { return n } return n * factorial(n: n - 1) }") }
	}
	
	"Tokenizer" should "handle a custom operator definition" in {
		val input = "infix operator +-: AdditionPrecedence"
		val expected = List(InfixToken, OperatorToken, InfixOperatorLiteralToken("+-"), ColonToken, VariableToken("AdditionPrecedence"))
		assertResult(expected) { Tokenizer("test", input) }
	}
	
	"Tokenizer" should "handle break + 5" in {
		val input = "break + 5"
		val expected = List(BreakToken, InfixOperatorLiteralToken("+"), DecimalIntegerLiteralToken("5"))
		assertResult(expected) { Tokenizer("test", input) }
	}
	
	"Tokenizer" should "handle 5)+(5" in {
		val input = "5)+(5"
		val expected = List(DecimalIntegerLiteralToken("5"), RightParenToken, InfixOperatorLiteralToken("+"), LeftParenToken, DecimalIntegerLiteralToken("5"))
		assertResult(expected) { Tokenizer("test", input) }
	}
	
	"Tokenizer" should "handle let z = x + y print(z)" in {
		val input = "let z = x + y\nprint(z)"
		val expected = Seq(LetToken, VariableToken("z"), InfixOperatorLiteralToken("="), VariableToken("x"), InfixOperatorLiteralToken("+"),
							VariableToken("y"), VariableToken("print"), LeftParenToken, VariableToken("z"), RightParenToken)
		assertResult(expected) { Tokenizer("test", input) }
	}
	
 	"Tokenizer" should "handle //comment\nlet x = 1\n//comment" in {
		val input = "//comment\nlet x = 1 //comment"
		val expected = Seq(SingleLineCommentToken("//comment\n"), LetToken, VariableToken("x"), InfixOperatorLiteralToken("="), 
						   DecimalIntegerLiteralToken("1"), SingleLineCommentToken("//comment"))
		assertResult(expected) { Tokenizer("test", input) }
	}
	
	// "Tokenizer" should "handle this interpolated string" in {
	// 	assertResult(List(InterpolatedStringLiteralToken("@\\(position)"))) { Tokenizer("test", "\"@\\(position)\"") }
	// }
}