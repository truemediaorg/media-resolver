package mediares

class MastodonSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Extract info from URL") {
    assert(Mastodon.serverInfoFromUrl(
      "https://mastodon.gamedev.place/@RustyBertrand@mastodon.social/111728190835688257"
    ) == Some(("mastodon.gamedev.place", "111728190835688257")))
    assert(Mastodon.serverInfoFromUrl(
      "https://mastodon.gamedev.place/@badlogic/111772886977661740"
    ) == Some(("mastodon.gamedev.place", "111772886977661740")))
    assert(Mastodon.serverInfoFromUrl(
      "https://mastodon.gamedev.place/@gregeganSF@mathstodon.xyz/111774983643968115"
    ) == Some(("mastodon.gamedev.place", "111774983643968115")))

    assert(Mastodon.serverInfoFromUrl("https://mastodon.gamedev.place/") == None)
    assert(Mastodon.serverInfoFromUrl("https://mastodon.social/@RustyBertrand") == None)
  }

  test("Invalid JSON") {
    try {
      Mastodon.decodeMedia("this is not valid json")
      fail("Got result from invalid json!?")
    } catch {
      case e :Throwable =>  // expected
    }
    try {
      Mastodon.decodeMedia("{\"error\": \"Some random error\"}")
      fail("Got result from invalid json!?")
    } catch {
      case e :Throwable => // expected
    }
  }

  test("Parse post JSON & extract media URLs") {
    val url1 = "https://files.mastodon.online/media_attachments/files/111/772/534/435/027/419/original/40b9616c3ee628ad.jpg"
    assert(Mastodon.decodeMedia(resourceText("/mastodon-image.json")) ==
           Seq(Media.image("image/jpeg", url1, 1600, 1600)))
    val url2 = "https://files.mastodon.online/media_attachments/files/111/761/732/971/959/771/original/2b5fda785a84f77d.mp4"
    assert(Mastodon.decodeMedia(resourceText("/mastodon-video.json")) ==
           Seq(Media.video("video/mp4", url2, 558, 338, 1362766, 105)))
    // make sure we use the remote_url when looking at a federated post
    val url3 = "https://media.mathstodon.xyz/media_attachments/files/111/774/978/279/611/284/original/9e360832d9087b2e.jpg"
    assert(Mastodon.decodeMedia(resourceText("/mastodon-indirect.json")) ==
           Seq(Media.image("image/jpeg", url3, 3325, 2494)))
  }
}
