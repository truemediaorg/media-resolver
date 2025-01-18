package mediares

class InstagramSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Parse url-resolver") {
    val video = Instagram.parsePost(resourceText("/instagram-video-url.json"))
    assert(video.media_id == "3305366994705717156_1744984635")
  }

  test("Parse media") {
    val video = Instagram.decodePostMedia(
      Instagram.parsePostMedia(resourceText("/instagram-media-info-video.json")))
    assert(video.size == 1)
    assert(video(0).id == "2Svh9TzLceEV2LnIxz6n5m7xA_U.mp4")
    assert(video(0).mimeType == "video/mp4")
    assert(video(0).width == 720)
    assert(video(0).height == 1280)

    val photos = Instagram.decodePostMedia(
      Instagram.parsePostMedia(resourceText("/instagram-media-info-photos.json")))
    assert(photos.size == 10)
    photos foreach(p => {
      assert(p.mimeType == "image/webp")
      assert(p.width == 1080)
      assert(p.height == 608)
    })

    val reel = Instagram.decodePostMedia(
      Instagram.parsePostMedia(resourceText("/instagram-media-info-reel.json")))
    assert(reel.size == 1)
    assert(reel(0).id == "HijNmURHocsQJDi5S2ji9GJvxdQ.mp4")
    assert(reel(0).mimeType == "video/mp4")
    assert(reel(0).width == 720)
    assert(reel(0).height == 1280)

    val carousel = Instagram.decodePostMedia(
      Instagram.parsePostMedia(resourceText("/instagram-media-info-carousel.json")))
    assert(carousel.size == 6)
    assert(carousel(0).mimeType == "video/mp4")
    assert(carousel(1).mimeType == "image/jpeg")
  }
}
