package mediares

class TikTokPhotoSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Invalid JSON") {
    assert(TikTokPhoto.V2.decode(Right("this is not valid json")) == None)
    assert(TikTokPhoto.V2.decode(Right("{\"error\": \"Some random error\"}")) == None)
  }

  test("URL matching") {
    assert(TikTokPhoto.isTikTokUrl("https://www.tiktok.com/@karelia_moncada/photo/7298557158436654341"))
    assert(!TikTokPhoto.isTikTokUrl("https://www.tiktok.com/@deeptomcruise/video/7157421288976387333?lang=en"))
    assert(!TikTokPhoto.isTikTokUrl("https://www.tiktok.com/@khvioff/video/7307576115076369707"))
    assert(!TikTokPhoto.isTikTokUrl("https://www.google.com/@deeptomcruise/video/7157421288976387333?lang=en"))
    assert(!TikTokPhoto.isTikTokUrl("https://www.reddit.com/r/TheTrumpZone/comments/18klukv/barack_obama_thinks_joe_biden_could_lose_the/"))
  }

  test("Parse V2 response JSON") {
    val json = resourceText("/tikly-photo-response-v2.json")
    val v2media = TikTokPhoto.V2.decode(Right(json))
    val asMedia = v2media.get.result.toMedia
    println(asMedia)
    assert(asMedia.length == 4)
  }
}
