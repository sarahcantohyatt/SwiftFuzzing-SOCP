package FuzzingSwift.typechecker

sealed trait Typ
case object IntType extends Typ
case object DoubleType extends Typ
case object BoolType extends Typ
case object StringType extends Typ
case object CharType extends Typ
case object VoidType extends Typ
case class NamedType(name: String) extends Typ
case class ArrayTyp(typ: Typ) extends Typ
case class DictionaryTyp(typ1: Typ, typ2: Typ) extends Typ
case class OpaqueTyp(typ: Typ) extends Typ
case object AnyTyp extends Typ
//case object SelfTyp extends Typ
case class OptionalTyp(typ: Typ) extends Typ
case class ImplicitlyUnwrappedOptionalTyp(typ: Typ) extends Typ
case class MetatypeTypeTyp(typ: Typ) extends Typ
case class MetatypeProtocolTyp(typ: Typ) extends Typ
case class FunctionTyp(typList: List[Typ], returnTyp: Typ) extends Typ
case class BoxedProtocolTyp(typ: Typ) extends Typ

case object UnknownType extends Typ

sealed trait Mutability
case object Mutable extends Mutability
case object Immutable extends Mutability

sealed trait Throwing
case object Throws extends Throwing
case object Rethrows extends Throwing