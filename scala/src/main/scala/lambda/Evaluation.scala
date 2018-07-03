package lambda

import Support._
import Syntax._
import Context._
import Substitution._
import Binding._

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

    

  def eval1 (ctx : Context) (t : Term) : Term = 
    t match {
      case TmIf(_, TmTrue(_), t2, t3) => t2
      case TmIf(_, TmFalse(_), t2, t3) => t3
      case TmIf(fi, t1, t2, t3) => {
        val t1_ = eval1 (ctx)(t1)
        TmIf(fi, t1_, t2, t3)
      }
      case TmTag(fi, l, t1, tyT) => 
        val t1_ = eval1 (ctx) (t1) 
        TmTag(fi, l, t1_, tyT)
      case TmCase(fi, TmTag(_,li, v11,_), branches) if (isVal(ctx)(v11)) =>
          val (x, body) = associate(li, branches) 
          termSubstTop(v11)(body)    
      case TmCase (fi, t1, branches) => 
        val t1_ = eval1 (ctx) (t1) 
        TmCase(fi, t1_, branches)
      case TmApp (fi, TmAbs(_, x, tyT11, t12), v2) if (isVal(ctx)(v2)) =>
        termSubstTop (v2)(t12) 
      case TmApp (fi, v1, t2) if (isVal(ctx)(v1)) => 
        val t2_ = eval1 (ctx)(t2) 
        TmApp (fi, v1, t2_)
      case TmApp (fi, t1, t2) => 
        val t1_ = eval1 (ctx) (t1) 
        TmApp (fi, t1_, t2)
      case TmLet (fi, x, v1, t2) if (isVal(ctx)(v1)) => 
        termSubstTop(v1)(t2)
      case TmLet(fi, x, t1, t2) => 
        val t1_ = eval1 (ctx)(t1) 
        TmLet (fi, x, t1_, t2)            
      case (t@TmFix(fi, v1)) if (isVal (ctx) (v1)) => 
        v1 match {
          case TmAbs(_, _, _, t12) => termSubstTop (t) (t12)
          case _ => throw new NoRuleApplies()
        }
      case TmFix(fi, t1) => 
        val t1_ = eval1 (ctx)(t1) 
        TmFix (fi, t1_)
      case TmVar(fi, n, _) => 
        val binding = getBinding(fi) (ctx)(n) 
        binding match {
          case (TmAbbBind(t, _)) => t 
          case _ => throw new NoRuleApplies ()
        }
      case TmAscribe (fi, v1, tyT) if (isVal(ctx)(v1)) => v1 
      case TmAscribe (fi, t1, tyT) => 
        val t1_ = eval1 (ctx)(t1)
        TmAscribe(fi, t1_, tyT)
      case TmRecord (fi, fields) => {
        def evalAField (l : List[(String, Term)]) : List[(String, Term)] = 
          l match {
            case List() => throw new NoRuleApplies() 
            case (l, vi) :: rest if (isVal(ctx)(vi)) => 
                val rest_ = evalAField(rest)
                (l, vi) :: rest_
            case (l, ti) :: rest => 
                val ti_ = eval1(ctx)(ti) 
                (l, ti_) :: rest 
          }
        val fields_ = evalAField(fields) 
        TmRecord(fi, fields_)
      }

      case (TmProj(fi, v1@(TmRecord(fi1, fields)), l)) if(isVal(ctx)(v1)) => 
        try {
          associate(l, fields)
        }catch {
          case e : Exception => throw new NoRuleApplies() 
        }
      case TmProj (fi, t1, l) => 
        val t1_ = eval1 (ctx)(t1) 
        TmProj(fi, t1_, l) 
      /*
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1

      */
      case TmTimesFloat(fi, TmFloat(_, f1), TmFloat(_, f2)) => 
        TmFloat(fi, f1 * f2)
      case TmTimesFloat(fi, t1@TmFloat(_, f1), t2) => 
        val t2_ = eval1 (ctx)(t2) 
        TmTimesFloat(fi, t1, t2_)
      case TmTimesFloat(fi, t1, t2) => 
        val t1_ = eval1 (ctx) (t1) 
        TmTimesFloat(fi, t1_, t2) 
      case TmSucc (fi, t1) => 
        val t1_ = eval1 (ctx) (t1) 
        TmSucc(fi, t1_) 
      case TmPred(_, TmZero(_)) => 
        TmZero(dummyInfo)
      case TmPred(_, TmSucc(_, nv1)) if (isNumericVal(ctx)(nv1)) => 
        nv1
      /*
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | _ -> 
      raise NoRuleApplies

      */    

      case TmPred(fi, t1) => 
        val t1_ = eval1 (ctx) (t1) 
        TmPred(fi, t1_) 
      case TmIsZero(_, TmZero(_)) => 
        TmTrue(dummyInfo)
      case TmIsZero(_, TmSucc(_, nv1)) if (isNumericVal(ctx)(nv1)) => 
        TmFalse(dummyInfo)
      case TmIsZero (fi, t1) => 
        val t1_ = eval1 (ctx)(t1) 
        TmIsZero (fi, t1_)

      case _ => throw new NoRuleApplies()
    }


}