package mediares

class FacebookSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Extract media id") {
    val data = Seq(("https://www.facebook.com/reel/7267334893381498", "7267334893381498"),
                   ("https://www.facebook.com/photo/?fbid=717077590580168&set=a.573638464924082", "717077590580168"),
                   ("https://www.facebook.com/watch/?v=7058523417565227", "7058523417565227"))
    data.foreach { pair =>
      assert(Facebook.extractMediaId(pair._1) == Some(pair._2))
    }
  }

  test("Parse url-resolver") {
    val reel = Facebook.parseResolvedUrl(resourceText("/facebook-reel-url.json"))
    assert(reel.data.urlResolver.id == "NjQyMTgzOTU5MjA4MTA3Omh0dHBzXGEvL3d3dy5mYWNlYm9vay5jb20vcmVlbC83MjY3MzM0ODkzMzgxNDk4Ojo6Og==")
  }

  test("Parse post") {
    val post = Facebook.parsePost(resourceText("/facebook-post.json"))
    assert(post.media.length == 1)
    assert(post.media.head.id == "bvoGEWMdL9pJmeI5zaGzee3pxDM.jpg")
  }

  test("Parse media") {
    val video = Facebook.parseMedia(resourceText("/facebook-video.json"))
    assert(video.data.nodes.head.__typename == "Video")
    assert(video.data.nodes.head.id == "7058523417565227")
    assert(video.media.length == 1)
    assert(video.media.head.id == "kgd_wPEddL0dM4ygtL_f4MCiHgE.mp4")

    val photo = Facebook.parseMedia(resourceText("/facebook-photo.json"))
    assert(photo.data.nodes.head.__typename == "Photo")
    assert(photo.data.nodes.head.id == "717077590580168")
    assert(photo.media.length == 1)
    assert(photo.media.head.id == "w7ncB_MoTWwt5tTxKNxI_R38U4I.jpg")
  }
}
