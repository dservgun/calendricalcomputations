package lambda

import Support._
import Syntax._
import Context._
import Substitution._

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

  //Find the branch that corresponds to the tag.
  //if a tag is not found, raise an error?
  def associate(tag : String, branches : List[CaseChoice]) : (String, Term) = {
    val result = branches.filter((a : CaseChoice) => tag == a.c1)
    if (result.length == 1) {
      result.head.namedPair
    }else {
      throw new NoRuleApplies
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
      case TmCase (fi, t1, branches) => {
        t1 match {
          case TmTag(_, li, v11, tyT) => {
            if(isVal(ctx)(v11)) {
              val x = associate(li, branches) 
              termSubstTop(x._2)(tyT)
            }else {
              val t1_ = eval1(ctx)(t1)
              TmCase(fi, t1_,branches)
            }          
          }
          case _ => {
            val t1_ = eval1(ctx) (t1)
            TmCase(fi, t1_, branches)
          }
        }
      }
      case TmApp(fi, TmAbs(_, x, tyT11, t12),v2) => ???
      case _ => throw new NoRuleApplies
    }

/*
  | TmTag(fi,l,t1,tyT) ->
      let t1' = eval1 ctx t1 in
      TmTag(fi, l, t1',tyT)
  | TmCase(fi,TmTag(_,li,v11,_),branches) when isval ctx v11->
      (try 
         let (x,body) = List.assoc li branches in
         termSubstTop v11 body
       with Not_found -> raise NoRuleApplies)
  | TmCase(fi,t1,branches) ->
      let t1' = eval1 ctx t1 in
      TmCase(fi, t1', branches)
  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2 
  | TmLet(fi,x,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2) 
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi,t1')
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t 
        | _ -> raise NoRuleApplies)
  | TmAscribe(fi,v1,tyT) when isval ctx v1 ->
      v1
  | TmAscribe(fi,t1,tyT) ->
      let t1' = eval1 ctx t1 in
      TmAscribe(fi,t1',tyT)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest -> 
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesfloat(fi,t1,t2') 
  | TmTimesfloat(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesfloat(fi,t1',t2) 
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1
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
}