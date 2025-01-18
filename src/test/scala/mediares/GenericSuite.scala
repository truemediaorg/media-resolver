package mediares

class GenericSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Extract video from BitChute post") {
    val urls = Generic.extractVideos(resourceText("/bitchute-post.html"))
    val url = "https://seed305.bitchute.com/QFESlQgqDfPO/hLTrv5iA1wYf.mp4"
    assert(urls == Seq(Media.video("video/mp4", url)))
  }
}
