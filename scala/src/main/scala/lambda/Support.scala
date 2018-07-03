package lambda


object Support {
  import java.io._
  type File = String
  case class FileInfo(file : File, line : Int, char : Int)
  val dummyInfo = FileInfo(file = "dummyinfo" ,line = -1, char = -1)  
}
