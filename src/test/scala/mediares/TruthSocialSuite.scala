package mediares

class TruthSocialSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Extract info from URL") {
    assert(TruthSocial.postIdFromUrl(
      "https://truthsocial.com/@TruthSports/posts/113154381896339455"
    ) == Some(("TruthSports", "113154381896339455")))
  }

  test("Parse post JSON & extract media URLs") {
    val url1 = "https://static-assets-1.truthsocial.com/tmtg:prime-ts-assets/media_attachments/files/113/154/513/560/527/076/original/d98476507f100a31.jpg"
    assert(Mastodon.decodeMedia(resourceText("/truthsocial-image.json")) ==
           Seq(Media.image("image/jpeg", url1, 796, 782)))
    // make sure a non-media post returns the empty list
    assert(Mastodon.decodeMedia(resourceText("/truthsocial-article.json")) == Seq())
  }
}
