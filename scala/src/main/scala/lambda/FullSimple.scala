package lambda;

import Syntax._
import Context._
import Support._
import Binding._



sealed trait Command 
case class Eval (info : FileInfo, aTerm : Term) extends Command 
case class Bind (info : FileInfo, name : String, aBinding : Binding) extends Command 

object Core {

}

object Parser {
  def parse(aString : String) = aString
}

