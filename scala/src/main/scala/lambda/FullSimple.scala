package lambda;


object Support {
  import java.io._
  case class FileInfo(file : File, line : Int, char : Int)  
}

import Support._
/**
* All of the code is copy of the ml implementation from Benjamin Pierce's
* book on types and programming languages.
*/
object Syntax {

  sealed trait Type 
  case class TyVar (fileInfo : Int, dbIndex : Int) extends Type
  case class TyId (identifier : String) extends Type 
  case class TyArr (funType : (Type, Type)) extends Type
  case object TyUnit extends Type
  case class TyRecord (fields : List[(String, Type)]) extends Type
  case class TyVariant (sumTypes : List[(String, Type)]) extends Type
  case object TyBool extends Type
  case object TyString extends Type 
  case object TyFloat extends Type
  case object TyNat extends Type

  sealed trait Term 
  case class TmTrue (info : FileInfo) extends Term
  case class TmFalse (info : FileInfo) extends Term
  case class TmIf (info : FileInfo, cond : Term, choiceTrue : Term, choiceFalse : Term) extends Term
  case class CaseChoice(c1 : String, namedPair : (String, Term))
  //TODO: Understand these case classes from the text.
  case class TmCase(info : FileInfo, cTerm : Term, choices : List[CaseChoice]) extends Term
  case class TmTag (info : FileInfo, tag : String, tmTerm : Term, Type : Type) extends Term 
  case class TmVar (info : FileInfo, index1 : Int, index2 : Int) extends Term
  case class TmAbs (info : FileInfo, aString : String, aType : Type, term : Term) extends Term
  case class TmApp (info : FileInfo, lTerm : Term, args : Term) extends Term 
  case class TmLet (info : FileInfo, astring : String, term1 : Term, term2 : Term) extends Term 
  case class TmFix (info : FileInfo, term : Term) extends Term 
  case class TmString (info : FileInfo, aString : String) extends Term 
  case class TmUnit (info : FileInfo) extends Term 
  case class TmAscribe (info : FileInfo, term : Term , Type : Type) extends Term 
  case class TmRecord (info : FileInfo, fields : List[(String, Term)]) extends Term 
  case class TmProj (info : FileInfo, term : Term, proj : String) extends Term 
  case class TmFloat (info : FileInfo, aValue : Float) extends Term 
  case class TmTimesFloat (info : FileInfo, mult : Term, flTerm : Term) extends Term 
  case class TmZero (info : FileInfo) extends Term 
  case class TmSucc (info : FileInfo, term : Term) extends Term 
  case class TmPred (info : FileInfo, term : Term) extends Term 
  case class TmIsZero (info : FileInfo, term : Term) extends Term 
  case class TmInert (info : FileInfo, Type : Type) extends Term 
} 

object Binding {
  import Syntax._
  sealed trait Binding
  case object NameBinding extends Binding 
  case object TyVarBinding extends Binding 
  case class VarBinding(Type : Type) extends Binding 
  case class TmAbbBind (term : Term, Type : Option[Type]) extends Binding 
  case class TyAbbBind (Type : Type) extends Binding
}

object Error {
  def error(fileInfo : FileInfo, message : String) : Int = {    
    println (s"${fileInfo} : ${message}");
    -1
  }

}
object Context {
  import Binding._
  import Syntax._
  import Error._
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
        context(anIndex)._1
      }catch {
        case ex : Exception => {
          val message = s"Variable lookup failure ${anIndex} : context size : ${context.length}"
          error(fileInfo, message)
          throw ex
        }
      }
    }
    def name2Index (fileInfo : FileInfo, 
        aName : String, aContext : List[ContextPair]) : Int = {
      context match {
        case (List()) => 
          error(fileInfo, s"Identifier ${aName} is unbound")
        case aList@((h, _) :: t) => 
          if (h == aName) 0 
          else (1 + name2Index(fileInfo, aName, t))
      }
    }

    //Shifting operation: the operation of shifting a nameless
    //lambda terms based on de-Breujin indices.
    //Walk the type map, here it seems to be a simple 
    //walk with no shifting of types.
    def tymap (onvar : Int => Int => Int => Type) (cutoff : Int) (typeT : Type) = {
      def walk(cutoff : Int , typeT : Type) : Type = {
        typeT match {
          // case (TyVar(x, n)) => ???//what is the type of onvar
          case (tyT@(TyId(b))) => tyT 
          case (TyString) => TyString 
          case (TyUnit) => TyUnit 
          case (TyRecord (fieldTypes)) => 
              TyRecord (fieldTypes.map((ele) => (ele._1, walk(cutoff, ele._2))))
          case TyFloat => TyFloat 
          case TyBool => TyBool 
          case TyNat => TyNat 
          case TyArr (typePair) => 
              TyArr((walk(cutoff, typePair._1), 
                      walk(cutoff, typePair._2)))
          case TyVariant(fieldTypes) => 
              TyVariant (fieldTypes.map((ele) => (ele._1, walk(cutoff, ele._2))))
          case TyVar(x, n) => onvar (cutoff) (x) (n)
        }
      }
      walk(cutoff, typeT)
    }

    def tmmap (onvar : FileInfo => Int => Int => Int => Term)
                (ontype : Int => Type => Type)
                (cutoff : Int)
                (term : Term) = {
      def walk(cutoff : Int, term : Term) : Term = 
        term match {
          case (TmInert(fi, aType)) => TmInert(fi, ontype(cutoff)(aType))
          case (TmVar(fi, x, n)) => onvar(fi)(cutoff)(x)(n)
          case (TmAbs(fi, x, tyT1, t2)) => 
              TmAbs(fi, x, ontype(cutoff)(tyT1), 
                      walk(cutoff + 1, t2))
          case (TmApp(fi, t1, t2)) => 
              TmApp (fi, walk(cutoff, t1), walk(cutoff, t2))
          case (TmLet(fi, x, t1, t2)) => 
              TmLet(fi, x, walk(cutoff, t1), walk(cutoff + 1, t2))
          case (TmFix(fi,t1)) => TmFix(fi, walk(cutoff, t1))
          case (t@TmTrue(fi)) => t
          case (f@TmFalse(fi)) => f 
          case (TmIf(fi, t1, t2, t3)) => 
              TmIf(fi, walk(cutoff, t1), walk(cutoff, t2), walk(cutoff, t3))
          case (t@TmString(fi, aString)) => t
          case (t@TmUnit(fi)) => t 
          case (t@TmProj(fi, t1, l)) => 
                TmProj(fi, walk(cutoff, t1), l)
          case (t@TmRecord(fi, fields)) => 
              TmRecord(fi, fields.map(f => (f._1, walk(cutoff, f._2))))
          case (t@TmAscribe(fi, t1, tyT1)) => 
              TmAscribe (fi, walk(cutoff, t1), ontype(cutoff)(tyT1))
          case (t@TmFloat(fi, f)) => t 
          case (t@TmTimesFloat(fi, t1, t2)) => 
              TmTimesFloat(fi, walk(cutoff, t1), walk(cutoff, t2))
          case (t@TmZero(fi)) => TmZero(fi)
          case (t@TmSucc(fi, t1)) => TmSucc(fi, walk(cutoff, t1))
          case (t@TmPred(fi, t1)) => TmPred(fi, walk(cutoff, t1)) 
          case (t@TmIsZero(fi, t1)) => TmIsZero(fi, walk(cutoff, t1))
          case (t@TmTag(fi, l, t1, tyT)) =>
              TmTag(fi, l, walk(cutoff, t1), ontype(cutoff)(tyT))
          case (t12@TmCase(fi, t, cases)) => 
              TmCase(fi
                  , walk(cutoff, t)
                  , cases.map
                      (a => 
                        CaseChoice(a.c1, 
                          (a.namedPair._1, 
                            walk(cutoff + 1 , a.namedPair._2)))))
        }
    }

    def typeShiftAbove (d : Int) (cutoff : Int) (tipe : Type) = {
      def anony (cut : Int) (x : Int) (n : Int) = {
        if (x >= cut) TyVar (x + d, n + d) 
        else TyVar(x, n + d)
      }
      tymap (anony) (cutoff) (tipe)
    }

    def termShiftAbove (d : Int) (cutoff : Int) (term : Term) = {
      def anony (fileInfo : FileInfo) (cut : Int) (x : Int) (n : Int) = {
        if (x >= cut) TmVar(fileInfo, x + d, n + d) 
        else TmVar (fileInfo, x, n + d)
      }
      tmmap (anony) (typeShiftAbove (d))(cutoff)(term)
    }
    def termShift (d : Int)(term :Term) = termShiftAbove (d) (0) (term)
    def typeShift (d : Int) (tipe : Type) = typeShiftAbove (d) (0) (tipe)
    def bindingShift (d : Int) (binding : Binding) = 
      binding match {
        case NameBinding => NameBinding 
        case TyVarBinding => TyVarBinding
        case TmAbbBind (t, tytOpt) => 
            val tyt_1 = tytOpt match {
              case None => None 
              case (Some(tyT)) => Some(typeShift (d) (tyT))
            }
        case VarBinding(tyT) => VarBinding(typeShift (d) (tyT)) 
        case TyAbbBind(tyT) => TyAbbBind(typeShift (d) (tyT))
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

