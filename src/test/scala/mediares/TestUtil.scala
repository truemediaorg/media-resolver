package mediares

import scala.collection.JavaConverters._

object TestUtil {

  def resourceText (path :String) :String = {
    import java.io._
    new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(path))).
      lines.collect(java.util.stream.Collectors.joining("\n"))
  }

  def resourceLines (path :String) :Iterator[String] = {
    import java.io._
    new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(path))).
      lines.iterator.asScala
  }
}
