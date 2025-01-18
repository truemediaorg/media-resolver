package mediares

class TwitterSuite extends org.scalatest.funsuite.AnyFunSuite {
  import TestUtil._

  test("Extract post id from URL") {
    assert(Twitter.tweetIdFromUrl("https://twitter.com/elonmusk/status/1747047100502999141") ==
           Some(("elonmusk", "1747047100502999141")))
    assert(Twitter.tweetIdFromUrl("twitter.com/elonmusk/status/1747047100502999141") ==
           Some(("elonmusk", "1747047100502999141")))
    assert(Twitter.tweetIdFromUrl("https://twitter.com/MrBeast/status/1747044525116108854") ==
           Some(("MrBeast", "1747044525116108854")))

    assert(Twitter.tweetIdFromUrl("https://www.twitter.com/") == None)
    assert(Twitter.tweetIdFromUrl("https://twitter.com/") == None)
    assert(Twitter.tweetIdFromUrl("twitter.com/") == None)
  }
}
