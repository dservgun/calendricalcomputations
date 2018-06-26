package lambda;


/**
* All of the code is copy of the ml implementation from Benjamin Pierce's
* book on types and programming languages.
*/
object Syntax {
  import java.io._
  case class FileInfo(file : File, line : Int, char : Int)

  sealed trait Tipe 
  case class TyVar (fileInfo : Int, dbIndex : Int) extends Tipe
  case class TyId (identifier : String) extends Tipe 
  case class TyArr (funType : (TyArr, TyArr)) extends Tipe
  case object TUnit extends Tipe
  case class TyRecord (fields : List[(String, Tipe)]) extends Tipe
  case class TyVariant (sumTypes : List[(String, Tipe)]) extends Tipe
  case object TyBool extends Tipe
  case object TyString extends Tipe 
  case object TyFloat extends Tipe
  case object TyNat extends Tipe

  sealed trait Term 
  case class TmTrue (info : FileInfo) extends Term
  case class TmFalse (info : FileInfo) extends Term
  case class TmIf (info : FileInfo, cond : Term, choiceTrue : Term, choiceFalse : Term) extends Term
  case class CaseChoice(c1 : String, namedPair : (String, Tipe))
  //TODO: Understand these case classes from the text.
  case class TmCase(info : FileInfo, cTerm : Term, choices : List[CaseChoice]) extends Term
  case class TmTag (info : FileInfo, tag : String, tmTerm : Term, tipe : Tipe) extends Term 
  case class TmVar (info : FileInfo, index1 : Int, index2 : Int) extends Term
  case class TmAbs (info : FileInfo, aString : String, aType : Tipe, term : Term) extends Term
  case class TmApp (info : FileInfo, lTerm : Term, args : Term) extends Term 
  case class TmLet (info : FileInfo, astring : String, term1 : Term, term2 : Term) extends Term 
  case class TmFix (info : FileInfo, term : Term) extends Term 
  case class TmString (info : FileInfo, aString : String) extends Term 
  case class TmUnit (info : FileInfo) extends Term 
  case class TmAscribe (info : FileInfo, term : Term , tipe : Tipe) extends Term 
  case class TmRecord (info : FileInfo, fields : List[(String, Term)]) extends Term 
  case class TmProj (info : FileInfo, term : Term, proj : String) extends Term 
  case class TmFloat (info : FileInfo, aValue : Float) extends Term 
  case class TmTimesFloat (info : FileInfo, mult : Term, flTerm : Term) extends Term 
  case class TmZero (info : FileInfo) extends Term 
  case class TmSucc (info : FileInfo, term : Term) extends Term 
  case class TmPred (info : FileInfo, term : Term) extends Term 
  case class TmIsZero (info : FileInfo, term : Term) extends Term 
  case class TmInert (info : FileInfo, tipe : Tipe) extends Term 
} 

object Binding {
  import Syntax._
  sealed trait Binding
  case object NameBinding extends Binding 
  case object TyVarBinding extends Binding 
  case class VarBinding(tipe : Tipe) extends Binding 
  case class TmAbbBind (term : Term, tipe : Option[Tipe]) extends Binding 
  case class TyAbbBind (tipe : Tipe) extends Binding
}

object Context {
  import Binding._
  import Syntax._
  type ContextPair = (String, Binding)
  case class Context(context : List[ContextPair]) {
    def length = context.length
    def addBinding(aName : String, aBinding : Binding) =
      (aName, aBinding) :: context
    def addName (aName : String) = addBinding(aName, NameBinding)
    def isNameBound(aName : String) : Boolean = {
      val nameExists = context.filter {
        case (name, _) => 
          if(name == aName) true else false
      }
      nameExists.nonEmpty      
    }
    def pickFreshName(aName : String) : Context = 
      if (isNameBound(aName)) pickFreshName (aName ++ "_")
      else Context((aName, NameBinding) :: context)
    def index2Name(fileInfo : FileInfo, anIndex : Int) : String = {
      try {
        context(anIndex)
      }catch {
        case(Exception(e)) => {
          val message = s"Variable lookup failure ${anIndex} : context size : ${context.length}"
          error(fileInfo, message)
        }
      }
    def name2Index (fileInfo : FileInfo, aName : String, aContext : List[ContextPair]) : Int = {
      context match {
        case (List()) => error(fileInfo, s"Identifier ${aName} is unbound")
        case aList@((h, _) :: t) => 
          if (h == aName) 0 
          else (1 + name2Index(fileInfo, aName, t))
      }
    }
    }
  }
  sealed trait Command 
  case class Eval (info : FileInfo, aTerm : Term) extends Command 
  case class Bind (info : FileInfo, name : String, aBinding : Binding) extends Command 


}
object Core {

}

object Parser {
  def parse(aString : String) = aString
}

