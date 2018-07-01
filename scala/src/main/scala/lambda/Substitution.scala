package lambda

import Support._
import Syntax._
import Context._

object Substitution {

  def termSubst (fileInfo : FileInfo) (j : Int) (s : Term) (tyT : Type) : Term = {
    def anony1 (fileInfo : FileInfo) (j : Int) (x : Int) (n : Int) : Term = {
      if (x == j) {
        termShift (j) (s) 
      }
      else TmVar(fileInfo, x, n)
    }

    def anony2 (j : Int) (tyT : Type) : Type = tyT 
    
    tmmap (anony1) (anony2) (j) (s)
  }
/*
(* ---------------------------------------------------------------------- *)
(* Substitution *)


(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

let termSubst j s t =
  tmmap
    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
    (fun j tyT -> tyT)
    j t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT

let typeSubstTop tyS tyT = 
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let rec tytermSubst tyS j t =
  tmmap (fun fi c x n -> TmVar(fi,x,n))
        (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t = 
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
*/

}