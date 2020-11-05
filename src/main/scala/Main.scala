import alsiola.Util
import java.io.FileReader
import alsiola.Util.JSONParser

object Program extends App {
  val reader = new FileReader("input.json")

  val j = new JSONParser();

  println(j.parseAll(j.obj, reader))
}
