package lambda

import Support._
import Syntax._
import Context._

object Evaluation {
  class NoRuleApplies extends Exception 
  def isNumericVal (ctx : Context) (t : Term) : Boolean = 
    t match {
      case (TmZero(_)) => true 
      case TmSucc(_, t1) => isNumericVal (ctx) (t1)
      case _ => false
    }
  def isVal (ctx : Context) (t : Term) : Boolean = 
    t match {
      case (TmTrue(_)) => true 
      case (TmFalse(_)) => true 
      case (TmTag(_, l, t1, _)) => isVal (ctx)(t1)
      case TmString(_, _) => true 
      case TmUnit(_) => true 
      case TmFloat (_, _) => true
      case (TmAbs(_, _, _, _)) => true 
      case TmRecord(_, fields) => fields.forall(x => isVal(ctx)(x._2))
      case TmZero(_) => isNumericVal (ctx) (t) 
      case TmSucc(_, t1) => isNumericVal (ctx) (t1)
      case _ => false
    }
  
}