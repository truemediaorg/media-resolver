package mediares

class TikTokVideoSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Invalid JSON") {
    assert(TikTokVideo.V1.decode(Right("this is not valid json")) == None)
    assert(TikTokVideo.V1.decode(Right("{\"error\": \"Some random error\"}")) == None)
  }

  test("URL matching") {
    assert(TikTokVideo.isTikTokUrl("https://www.tiktok.com/@deeptomcruise/video/7157421288976387333?lang=en"))
    assert(TikTokVideo.isTikTokUrl("https://www.tiktok.com/@karelia_moncada/video/7298557158436654341"))
    assert(TikTokVideo.isTikTokUrl("https://www.tiktok.com/@khvioff/video/7307576115076369707"))
    assert(!TikTokVideo.isTikTokUrl("https://www.google.com/@deeptomcruise/video/7157421288976387333?lang=en"))
    assert(!TikTokVideo.isTikTokUrl("https://www.reddit.com/r/TheTrumpZone/comments/18klukv/barack_obama_thinks_joe_biden_could_lose_the/"))
  }

  test("Extract mime-type") {
    val url = "https://v16m-default.akamaized.net/efb9a49b0c755f205998ad88db07d1ab/65bc335f/video/tos/useast2a/tos-useast2a-ve-0068c001-euttp/oEGi7ALp2JQGRDEi1aebBBQczIXBvFIfRFnXEU/?a=0&ch=0&cr=0&dr=0&lr=all&cd=0%7C0%7C0%7C0&cv=1&br=3818&bt=1909&bti=Ozk3QGo4dik3OjlmMzAuYCM6bTQ0MDo%3D&cs=0&ds=3&ft=pK~tdMZj8Zmo08GBo94jVNUJ2CFrKsd.&mime_type=video_mp4&qs=0&rc=aDhkaDw5OjZkNjVpOGQ2ZUBpam03a2U6Zm9wPDMzNzczM0BfYjYvLi00NTIxLi41X2E2YSMva2ZzcjQwa2lgLS1kMTZzcw%3D%3D&l=2024020118115615C16C3555E0C7727557&btag=e00088000"
    assert(Util.guessMimeType(url, "") == "video/mp4")
  }

  test("Parse V1 response JSON") {
    val videoUrl = "https://v16m-default.akamaized.net/db9c4953336111ae4a673c993b461bfc/65a61c37/video/tos/useast2a/tos-useast2a-ve-0068-euttp/oANim5R1EIzoQwghT9ftBIq4B7ExEQBJeGQJqA/?a=0&ch=0&cr=0&dr=0&lr=all&cd=0%7C0%7C0%7C0&cv=1&br=3264&bt=1632&bti=OHYpOTY0Zik3OjlmOm01MzE6ZDQ0MDo%3D&cs=0&ds=3&ft=pK~tdMZj8Zmo0.8QU94jVk_Zr5WrKsd.&mime_type=video_mp4&qs=0&rc=ZTNkNjY2NTs0ZGdnNjVlaEBpajk2Zjw6ZjN3ZDMzNzczM0BgXzAxNF41NjYxMS4vY2IzYSNxZl5pcjRnYWVgLS1kMTZzcw%3D%3D&l=20240116000313942F09071F6A8E2B44FE&btag=e00088000"
    val coverUrl = "https://p16-sign-va.tiktokcdn.com/tos-maliva-p-0068/f88d95c0ca3e4916a244c38723cdb157_1654904495~noop.webp?lk3s=d05b14bd&x-expires=1705449600&x-signature=LC7H0PBk0wgDfaV64CUgDFPXVss%3D&s=FEED&se=false&sh=&sc=cover&l=20240116000313942F09071F6A8E2B44FE"
    val json = resourceText("/tikly-response.json")
    val v1media = TikTokVideo.V1.decode(Right(json)).get.video.toMedia
    assert(v1media == Media.video("video/mp4", videoUrl, 1080, 1920, 0, 19, Some(coverUrl)))
    assert(TikTokVideo.V1.decode(Left(resourceText("/tikly-failure.json"))) == None)
  }

  test("Parse V2 response JSON") {
    val videoUrl = "https://dl.muscdn.app/cG9saXRfaWNzMjQ=/hd/aHR0cHM6Ly92MTZtLWRlZmF1bHQuYWthbWFpemVkLm5ldC83YjIyYThlNTY3MGNhNzY1NjdkYjcxNzBkNDVkOWM1My82NjRiZTMwNC92aWRlby90b3MvYWxpc2cvc2RlL3Rvcy1hbGlzZy1wdi0wMDM3YzAwMS9vY0pBSHZBbmlBTUJrVTBrQll6NVp4RGlFQXlCTGpuVVBWRUFFLz9hPTAmYnRpPU9VQnpPVGc3UUdvNk9qWkFMM0FqTFRBellDTXhORE5nJmNoPTAmY3I9MCZkcj0wJmxyPWFsbCZjZD0wJTdDMCU3QzAlN0MxJmN2PTEmYnI9NTA3MiZidD0yNTM2JmRzPTImZnQ9WEU1YkNxVDBtN2pQRDEybEFwdzczd1VLOUF5S01lRn5PNSZtaW1lX3R5cGU9dmlkZW9fbXA0JnFzPTEzJnJjPWFtdHNiMjQ1Y21scWN6TXpPRGN6TkVCcGFtdHNiMjQ1Y21scWN6TXpPRGN6TkVBd1l5OW5NbVEwYlRGZ0xTMWtNVEZ6WVNNd1l5OW5NbVEwYlRGZ0xTMWtNVEZ6Y3clM0QlM0QmbD0yMDI0MDUyMDE3NTQwNjIwNTg4ODlERkMwNTgzRUI1MUZDJmJ0YWc9ZTAwMDUwMDAwJmRwaz1oRCUyRnFRWnRQVEh6SmlJVVlJbDY1ODdYYmMlMkYxZmRJdjVSUmVISHIyWVg1dVhuTnBxV2F1cEdTOGNGNiUyRmJlY3FtTU1KREt0RmhlZVRrd3l1SWVYbkFCZiUyQnAyVjN4Y3F0UERQQnElMkJnJTNEJTNEJmRwbT1hZXMtMjU2LWNmYiZsPTIwMjQwNTIwMTc1NDA2MjA1ODg4OURGQzA1ODNFQjUxRkM="
    val audioUrl = "https://sf16-ies-music-sg.tiktokcdn.com/obj/tiktok-obj/7370437437106981648.mp3"
    val json = resourceText("/tikly-response-v2.json")
    val v2media = TikTokVideo.V2.decode(Right(json))
    println(v2media.get.result.toMedia)
    // assert(v1media == Media.video("video/mp4", videoUrl, 1080, 1920, 0, 19, Some(coverUrl)))
    // assert(TikTok.V1.decode(Left(resourceText("/tikly-failure.json"))) == None)
  }
}
