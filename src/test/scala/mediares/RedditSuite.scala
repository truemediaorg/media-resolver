package mediares

class RedditSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Extract post id from URL") {
    assert(Reddit.postIdFromUrl(
      "https://www.reddit.com/r/TheTrumpZone/comments/18klukv/barack_obama_thinks_joe_biden_could_lose_the/"
    ) == Some("18klukv"))
    assert(Reddit.postIdFromUrl(
      "https://reddit.com/r/TheTrumpZone/comments/18klukv/barack_obama_thinks_joe_biden_could_lose_the/"
    ) == Some("18klukv"))
    assert(Reddit.postIdFromUrl(
      "reddit.com/r/TheTrumpZone/comments/18klukv/barack_obama_thinks_joe_biden_could_lose_the/"
    ) == Some("18klukv"))

    assert(Reddit.postIdFromUrl(
      "https://www.reddit.com/r/science/comments/18z5syo/neptune_is_known_for_being_a_rich_blue_and_uranus/"
    ) == Some("18z5syo"))

    assert(Reddit.postIdFromUrl(
      "https://www.reddit.com//r/Scams/comments/17qvqyx/the_beginning_was_a_deep_fake_of_joe_biden_and_im/"
    ) == Some("17qvqyx"))

    assert(Reddit.postIdFromUrl("https://www.reddit.com/") == None)
    assert(Reddit.postIdFromUrl("https://reddit.com/") == None)
    assert(Reddit.postIdFromUrl("reddit.com/") == None)

    assert(Reddit.postIdFromUrl("https://www.reddit.com/18klukv") == Some("18klukv"))
    assert(Reddit.postIdFromUrl("https://reddit.com/18z5syo") == Some("18z5syo"))
    assert(Reddit.postIdFromUrl("reddit.com/18z5syo") == Some("18z5syo"))

    assert(Reddit.postIdFromUrl("https://redd.it/18klukv") == Some("18klukv"))
    assert(Reddit.postIdFromUrl("redd.it/18z5syo") == Some("18z5syo"))
  }

  test("Invalid JSON") {
    try {
      Reddit.parseInfo("this is not valid json")
      fail("Got result from invalid json!?")
    } catch {
      case e :Exception => // expected
    }
    try {
      Reddit.parseInfo("{\"error\": \"Some random error\"}")
      fail("Got result from invalid json!?")
    } catch {
      case e :Exception => // expected
    }
  }

  test("Parse post JSON & extract media URLs") {
    val videoInfo = Reddit.parseInfo(resourceText("/reddit-video-post.json"))
    assert(Reddit.decodeMedia(videoInfo, false) ==
           Seq(Media.video("video/mp4", "https://v.redd.it/y9b1mwu9sx9c1/DASH_480.mp4?source=fallback", 480, 854, 1200, 89)))
    assert(videoInfo.canonicalUrl == Some("https://www.reddit.com/r/TikTokCringe/comments/18wdun7/skywriter_spells_ur_taxes_killed_10k_gaza_kids/"))

    val imageInfo = Reddit.parseInfo(resourceText("/reddit-image-post.json"))
    assert(Reddit.decodeMedia(imageInfo, false) ==
           Seq(Media.image("image/jpeg", "https://i.redd.it/jslnz8aptgbc1.jpeg")))
    assert(imageInfo.canonicalUrl == Some("https://www.reddit.com/r/mildlyinteresting/comments/192n27s/i_boiled_down_5_gallons_of_seawater_and_this_all/"))

    val carouselInfo = Reddit.parseInfo(resourceText("/reddit-carousel-post.json"))
    assert(Reddit.decodeMedia(carouselInfo, false).map(_.id) ==
           Seq("2KkNYsEZ6X1fbb6d_VNTKyOXyho.png", "f2xBI_yptKVtUvbo9DQIxWE26ic.png"))

    try {
      val nonVideoInfo = Reddit.parseInfo(resourceText("/reddit-non-video-post.json"))
      assert(Reddit.decodeMedia(nonVideoInfo, false) == Seq())
    } catch {
      case re :Util.ReportException => // expected
    }
  }

  test("Parse DASH MPD") {
    var dashIn = getClass.getResourceAsStream("/reddit-dash-playlist.mpd")
    val video = Reddit.extractMedia("https://reddit.com/foo/", 0, dashIn).get
    assert(video.id == "sjhDH8dn_t5oPPKJLjrm692xUCo.mp4")
    assert(video.bitrate == 1018606)
    assert(video.audio != null)
  }

  test("Parse media URL") {
    val mediaUrl = "https://www.reddit.com/media?url=https%3A%2F%2Fpreview.redd.it%2Fnever-knew-jesus-was-a-dr-pepper-guy-from-a-piggly-wiggly-v0-nm6leo8ezpoc1.jpeg%3Fwidth%3D1080%26crop%3Dsmart%26auto%3Dwebp%26s%3D7a07812b5d162b8ad38a0ca04615327d17f39496&xpromo_edp=enabled"
    assert(Reddit.decodeMediaUrl(mediaUrl) == Some("https://preview.redd.it/never-knew-jesus-was-a-dr-pepper-guy-from-a-piggly-wiggly-v0-nm6leo8ezpoc1.jpeg?width=1080&crop=smart&auto=webp&s=7a07812b5d162b8ad38a0ca04615327d17f39496"))
  }
}
