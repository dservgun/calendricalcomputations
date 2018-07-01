package lambda;

object Binding {
  import Syntax._
  sealed trait Binding
  case object NameBinding extends Binding 
  case object TyVarBinding extends Binding 
  case class VarBinding(Type : Type) extends Binding 
  case class TmAbbBind (term : Term, Type : Option[Type]) extends Binding 
  case class TyAbbBind (Type : Type) extends Binding
}
