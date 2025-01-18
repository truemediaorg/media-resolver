package mediares

import java.nio.file.Paths

class YTDLPSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._
  import YTDLP._

  test("Parse output from YouTube download") {
    val output = Seq(
      "[youtube] Extracting URL: https://youtube.com/v/P6FORpg0KVo",
      "[youtube] P6FORpg0KVo: Downloading webpage",
      "[youtube] P6FORpg0KVo: Downloading ios player API JSON",
      "[youtube] P6FORpg0KVo: Downloading m3u8 information",
      "[info] P6FORpg0KVo: Downloading 1 format(s): 616+251",
      "[info] Downloading video thumbnail 41 ...",
      "[info] Writing video thumbnail 41 to: P6FORpg0KVo.webp",
      "[info] Writing video metadata as JSON to: P6FORpg0KVo.info.json",
      "[hlsnative] Downloading m3u8 manifest",
      "[hlsnative] Total fragments: 141",
      "[download] Destination: P6FORpg0KVo.f616.mp4",
      "[download] 100% of   80.98MiB in 00:00:06 at 11.89MiB/s",
      "[download] Destination: P6FORpg0KVo.f251.webm",
      "[download] 100% of   10.21MiB in 00:00:00 at 32.70MiB/s",
      "[Merger] Merging formats into \"P6FORpg0KVo.webm\"",
      "[ExtractAudio] Destination: P6FORpg0KVo.opus",
    )
    val parser = new OutputParser()
    output.foreach(line => {
      parser.onOutput(line) match {
        case Some(GotMetadata(videoId, videoFormat, audioFormat, metadataFile)) =>
          assert(videoId == "P6FORpg0KVo")
          assert(videoFormat == "616")
          assert(audioFormat == Some("251"))
          assert(metadataFile == "P6FORpg0KVo.info.json")
        case None =>
      }
    })
  }

  test("Parse JSON from YouTube download") {
    val json = resourceText("/ytdlp-0xWyP1UxOR8.info.json")
    val info = YTDLP.parseInfo(json, "137", Some("251"))
    assert(info.isInstanceOf[Result.Resolved])
    val res = info.asInstanceOf[Result.Resolved]
    assert(res.media.size == 1)
    val media = res.media(0)
    assert(media.isCachedByResolver)
    assert(media.mimeType == "video/mp4")
    assert(media.duration == 124)
    assert(media.audio != null)
    assert(media.audio.mimeType == "audio/webm")
  }

  test("Parse output from TikTok download") {
    val output = Seq(
      "[TikTok] Extracting URL: https://www.tiktok.com/@kellycookstexas/video/7348189445943807262",
      "[TikTok] 7348189445943807262: Downloading webpage",
      "[info] 7348189445943807262: Downloading 1 format(s): bytevc1_1080p_1651507-1",
      "[info] Downloading video thumbnail 2 ...",
      "[info] Writing video thumbnail 2 to: 7348189445943807262.image",
      "[info] Writing video metadata as JSON to: 7348189445943807262.info.json",
      "[download] Destination: 7348189445943807262.mp4",
      "[download] 100% of   21.92MiB in 00:00:01 at 17.99MiB/s",
    )
    val parser = new OutputParser()
    var gotMeta = false
    output.foreach(line => {
      parser.onOutput(line) match {
        case Some(GotMetadata(videoId, videoFormat, audioFormat, metadataFile)) =>
          assert(videoId == "7348189445943807262")
          assert(videoFormat == "bytevc1_1080p_1651507-1")
          assert(audioFormat == None)
          assert(metadataFile == "7348189445943807262.info.json")
          gotMeta = true
        case None =>
      }
    })
    assert(gotMeta)
  }

  test("Parse JSON from TikTok download") {
    val json = resourceText("/ytdlp-7348189445943807262.info.json")
    val info = YTDLP.parseInfo(json, "bytevc1_1080p_1651507-1", None)
    assert(info.isInstanceOf[Result.Resolved])
    val res = info.asInstanceOf[Result.Resolved]
    assert(res.media.size == 1)
    val media = res.media(0)
    assert(media.isCachedByResolver)
    assert(media.mimeType == "video/mp4")
    assert(media.duration == 111)
    assert(media.audio != null)
    assert(media.audio.mimeType == "audio/mp3") // extracted audio mime-type
  }

  test("Parse error from failed download") {
    val output = Seq(
      "[generic] Extracting URL: http://samskivert.com/",
      "[generic] samskivert: Downloading webpage",
      "WARNING: [generic] Falling back on generic information extractor",
      "[generic] samskivert: Extracting information",
      "ERROR: Unsupported URL: http://samskivert.com/",
    )
    val parser = new OutputParser()
    output.foreach(parser.onOutput)
    assert(parser.error == "Unsupported URL: http://samskivert.com/")
  }
}
