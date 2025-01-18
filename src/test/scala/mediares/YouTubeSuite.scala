package mediares

class YouTubeSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Extract post id from URL") {
    for (url <- resourceLines("/youtube-urls.txt") if (url != "" && !(url startsWith "//"))) {
      val res = YouTube.videoIdFromUrl(url)
      assert(res != None && res.get != null, s"Unhandled url: $url")
    }
  }

  test("Invalid JSON") {
    try {
      YouTube.decodeMedia("foo", "this is not valid json")
      fail("Got result from invalid json!?")
    } catch {
      case e :Exception =>  // expected
    }
    try {
      YouTube.decodeMedia("foo", "{\"error\": \"Some random error\"}")
      fail("Got result from invalid json!?")
    } catch {
      case e :Exception => // expected
    }
  }

  test("File suffixes for YouTube's detailed types") {
    assert(Util.fileSuffixFromMimeType("audio/mp4; codecs=\"mp4a.40.2\"") == Some(".m4a"))
    assert(Util.fileSuffixFromMimeType("audio/webm; codecs=\"opus\"") == Some(".opus"))
    assert(Util.fileSuffixFromMimeType("video/webm; codecs=\"vp9\"") == Some(".webm"))
    val mp4s = Seq(
      "video/mp4; codecs=\"av01.0.00M.08\"",
      "video/mp4; codecs=\"av01.0.01M.08\"",
      "video/mp4; codecs=\"av01.0.04M.08\"",
      "video/mp4; codecs=\"av01.0.05M.08\"",
      "video/mp4; codecs=\"av01.0.08M.08\"",
      "video/mp4; codecs=\"avc1.42001E, mp4a.40.2\"",
      "video/mp4; codecs=\"avc1.4d400c\"",
      "video/mp4; codecs=\"avc1.4d4015\"",
      "video/mp4; codecs=\"avc1.4d401e\"",
      "video/mp4; codecs=\"avc1.4d401f\"",
      "video/mp4; codecs=\"avc1.4d401f\"",
      "video/mp4; codecs=\"avc1.64001F, mp4a.40.2\"",
      "video/mp4; codecs=\"avc1.640028\"",
    )
    for (url <- mp4s) assert(Util.fileSuffixFromMimeType(url) == Some(".mp4"))
  }

  test("Parse post JSON & extract media URLs") {
    val media = YouTube.decodeMedia("Q0FFU0FnZ0M=", resourceText("/youtube-response.json"))
    assert(media.size == 1)
    val mm = media(0)
    assert(mm.id == "CjztaAzWCoVQ0t0kMdzzjpuYMYU.mp4")
    assert(mm.width == 1280)
    assert(mm.height == 720)
    assert(mm.bitrate == 1359017)
    assert(Option(mm.audio).map(_.id) == Some("QNIntHwWOz-gsUmWSdlStRRyZD4.m4a"))
    assert(mm.videoThumbnailUrl == Some("https://i.ytimg.com/vi/Lp64xua_A-8/maxresdefault.jpg"))
  }

  test("Handle unavaialble response") {
    try {
      YouTube.decodeMedia("Kb-VK31Vajc", resourceText("/youtube-unavailable-response.json"))
    } catch {
      case re :Util.ReportException => // expected
    }
  }
}
