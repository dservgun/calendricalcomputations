package lambda
import lambda.Parser._
import scala.io.Source
object Driver extends scala.App {
  def help = "Driver : <filename>"
  val inFile = if (args.isEmpty) help else args(0) 
  val input = Source.fromFile(inFile).mkString("") 
  parse(input)
}