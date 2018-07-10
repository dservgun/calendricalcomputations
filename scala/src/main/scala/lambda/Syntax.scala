package lambda;

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

  def extractFileInfo (aTerm : Term) : FileInfo = ???
  def associate (tag : String, fields : List[(String, Term)]) : Term = {
    val result = 
      fields.filter((a : (String, Term)) => tag == a._1)
    if (result.length == 1) {
      result.head._2
    }else {
      throw new NoRuleApplies
    }

  }
  //Find the branch that corresponds to the tag.
  //if a tag is not found, raise an error?
  def associate(tag : String, branches : List[CaseChoice]) : (String, Term) = {
    val result = branches.filter((a : CaseChoice) => tag == a.c1)
    if (result.length == 1) {
      result.head.namedPair
    }else {
      throw new NoRuleApplies()
    }    
  }

} 
