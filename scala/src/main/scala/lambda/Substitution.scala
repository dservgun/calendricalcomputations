package lambda

import Support._
import Syntax._
import Context._

object Substitution {

  def termSubst (j : Int) (s : Term) (tyT : Type) : Term = {
    def anony1 (fileInfo : FileInfo) (j : Int) (x : Int) (n : Int) : Term = {
      if (x == j) {
        termShift (j) (s) 
      }
      else TmVar(fileInfo, x, n)
    }

    def anony2 (j : Int) (tyT : Type) : Type = tyT 
    
    tmmap (anony1) (anony2) (j) (s)
  }

  def termSubstTop (s : Term) (t : Type) = 
    termShift (-1) (termSubst (0) (termShift (1) (s)) (t))

/*let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT
*/
  def typeSubSt (tyS : Type) (j : Int) (tyT : Type) = {
    def anony1 (j : Int) (x : Int) (n : Int) : Type = {
      if (x == j) (typeShift (j) (tyS)) else (TyVar (x, n))
    }
    tymap (anony1) (j) (tyT)
  }

  def typeSubStTop (tyS : Type) (tyT : Type) : Type = 
    typeShift (-1) (typeSubSt (typeShift (1)(tyS)) (0) (tyT))

  def tytermSubst (tyS : Type) (j : Int) (t : Term) = {
    def anony1 (fileInfo : FileInfo) (c : Int) (x : Int) (n : Int) : Term = 
        TmVar(fileInfo, x, n)
    def anony2 (j : Int) (tyT : Type) = typeSubSt (tyS) (j) (tyT)
    tmmap (anony1) (anony2) (j) (t)    
  }
  def tytermSubstTop (tyS : Type) (t : Term) : Term = 
    termShift (-1) (tytermSubst (typeShift (1) (tyS))(0) (t))
}

