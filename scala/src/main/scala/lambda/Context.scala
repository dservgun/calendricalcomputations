package lambda

import Syntax._
import Support._
import Binding._
import Error._
/*
type context = (string * binding) list

type command =
  | Eval of info * term
  | Bind of info * string * binding

(* ---------------------------------------------------------------------- *)

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

*/
object Context {
    type Context = List[(String, Binding)]
    val emptyContext : Context = List() 
    def contextLength (ctx : Context) : Int = ctx.length

    def addBinding(aName : String, aBinding : Binding) (ctx : Context) : Context =
      (aName, aBinding) :: ctx

    def addName (aName : String) (ctx : Context) = addBinding(aName, NameBinding) (ctx)
    def isNameBound(aName : String) (ctx : Context) : Boolean = {
      val nameExists = ctx.filter {
        case (name, _) => 
          if(name == aName) true else false
      }
      nameExists.nonEmpty      
    }

    def pickFreshName(aName : String) (ctx : Context) : Context = 
      if (isNameBound(aName)(ctx)) pickFreshName (aName ++ "_") (ctx)
      else (aName, NameBinding) :: ctx
    def index2Name(fileInfo : FileInfo, anIndex : Int) (context : Context) : String = {
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
        aName : String, context : Context) : Int = {
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
                (term : Term) : Term = {
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
        walk (cutoff, term)
    }

    def typeShiftAbove (d : Int) (cutoff : Int) (tipe : Type) : Type = {
      def anony (cut : Int) (x : Int) (n : Int) : Type = {
        if (x >= cut) TyVar (x + d, n + d) 
        else TyVar(x, n + d)
      }
      tymap (anony) (cutoff) (tipe)
    }
    def termShiftAbove (d : Int) (cutoff : Int) (term : Term) : Term = {
      def anony (fileInfo : FileInfo) (cut : Int) (x : Int) (n : Int) : Term = {
        if (x >= cut) 
          TmVar(fileInfo, x + d, n + d) 
        else 
          TmVar (fileInfo, x, n + d)
      }
      val fileInfo : FileInfo = extractFileInfo(term)
      tmmap (anony) (typeShiftAbove (d)) (cutoff) (term)
    }

    def termShift (d : Int)(term :Term) : Term = termShiftAbove (d) (0) (term)
    
    def typeShift (d : Int) (tipe : Type) : Type = typeShiftAbove (d) (0) (tipe)
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