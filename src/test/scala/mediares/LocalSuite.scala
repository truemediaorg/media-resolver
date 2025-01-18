package mediares

import java.nio.file.{Files, Paths}

class LocalSuite extends org.scalatest.funsuite.AsyncFunSuite {

  test("Extract duration from video") {
    for (duration <- Local.extractVideoDuration(Paths.get("src/test/resources/arianna.mp4")))
    yield assert(duration == 7.36)
  }

  test("Extract thumbnail video") {
    val source = Paths.get("src/test/resources/arianna.mp4")
    val temp = Files.createTempFile("arianna", s".jpg")
    for (file <- Local.extractVideoThumbnail(source, "test://url", temp))
    yield {
      val size = Files.size(file.path)
      Files.delete(temp)
      assert(size == 13059)
    }
  }
}
