package lambda
import Support._
object Error {
  def error(fileInfo : FileInfo, message : String) : Int = {    
    println (s"${fileInfo} : ${message}");
    -1
  }
}
