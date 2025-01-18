package mediares

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import java.util.logging.{Logger, Level}

import upickle.default._
import sttp.client4._

import com.twitter.clientlib.ApiException
import com.twitter.clientlib.TwitterCredentialsBearer
import com.twitter.clientlib.model.{Media => TMedia, Video, Photo, AnimatedGif, Variant}
import com.twitter.clientlib.api.TwitterApi

object Twitter {

  import Pool.ctx
  private val log = Logger.getLogger("twitter")

  import scala.jdk.CollectionConverters._
  private def fields (fields :String*) = fields.toSet.asJava

  private val urlRegexes = Seq(
    ("^(https?://)?(www.|mobile.)?(twitter|x).com/(\\S+)/status/(\\d+)".r, 4, 5))
  private val ignoreTypes = Set("application/x-mpegURL")

  private lazy val apiInstance = {
    val Seq(token) = Env.getSecrets("TWITTER_BEARER_TOKEN")
    new TwitterApi(new TwitterCredentialsBearer(token))
  }

  def createPlaceholderSDKProperties () :Unit = {
    import java.nio.file._
    val path = Paths.get("sdk.properties")
    Files.write(path, Seq("# Temporary placeholder to silence Twitter SDK").asJava)
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run () = Files.delete(path)
    })
  }

  def resolveMedia (username :String, tweetId :String) :Future[Result] = Future {
    try {
      val result = apiInstance.tweets.findTweetById(tweetId).
        tweetFields(fields("author_id", "created_at", "in_reply_to_user_id", "lang", "source",
                           "public_metrics", "possibly_sensitive", "geo", "attachments")).
        mediaFields(fields("media_key", "type", "url", "variants", "width", "height", "duration_ms",
                           "preview_image_url")).
        expansions(fields("attachments.media_keys", "author_id")).
        execute()

      def pickVariant (
        vvs :java.util.List[Variant], width :Int, height :Int, duration :Int, thumbUrl :Option[String]
      ) :Seq[Media] = Util.pickBestMedia(
        for (vv <- vvs.asScala if (!ignoreTypes(vv.getContentType)) ; url = vv.getUrl.toString)
        yield Media.video(vv.getContentType, url, width, height, vv.getBitRate, duration, thumbUrl))

      // avoid choking on all the nulls Twitter might throw at us
      val incls = Option(result.getIncludes)
      val tweetMedia = incls.flatMap(incls => Option(incls.getMedia)).
        map(_.iterator.asScala.toSeq) getOrElse Seq()
      val tweetUsers = incls.flatMap(incls => Option(incls.getUsers)).
        map(_.asScala.toSeq) getOrElse Seq()

      val media = Seq() ++ tweetMedia.flatMap(_ match {
        case vid :Video =>
          val thumb = Option(vid.getPreviewImageUrl).map(_.toString)
          pickVariant(vid.getVariants, vid.getWidth, vid.getHeight, vid.getDurationMs / 1000, thumb)

        case gif :AnimatedGif =>
          pickVariant(gif.getVariants, gif.getWidth, gif.getHeight, 0, None)

        case photo :Photo =>
          Seq(Media.image(Util.guessMimeType(photo.getUrl.toString, "image/unknown"),
                          photo.getUrl.toString, photo.getWidth, photo.getHeight))

        case media :TMedia =>
          println(s"Unhandled media type: $media")
          Seq()
      })

      if (result.getData == null) Result.failure("That tweet has been deleted or hidden.")
      else if (media.size == 0) {
        // if the text contains what looks like a URL, give a special error message
        if (result.getData.getText.contains("https://")) Result.failure(
          "This tweet is previewing an image linked from another website.")
        else Result.failure("Unable to identify any image or video in that tweet.")
      }
      else {
        val json = result.toJson
        val id = result.getData.getId
        val user = tweetUsers.find(user => user.getId == result.getData.getAuthorId)
        val canonicalUsername = user.map(_.getUsername) getOrElse username
        Result.resolved(media, json, s"https://twitter.com/$canonicalUsername/status/$id")
      }

    } catch {
      case e :ApiException =>
        if (e.getCode == 403 || e.getCode == 429) {
          log.warning("Twitter rejected API request " +
                      s"[tweet=${tweetId}, code=${e.getCode}, msg=${e.getMessage}, body=${e.getResponseBody}]")
          Result.failure("We are temporarily unable to resolve Twitter posts. Please try again in a few hours.")
        } else {
          log.log(Level.WARNING, s"findTweetById failure (id: $tweetId)")
          val rsp = e.getResponseBody()
          System.err.println("Status code: " + e.getCode())
          System.err.println("Reason: " + rsp)
          System.err.println("Response headers: " + e.getResponseHeaders())
          throw Exception(rsp) // TODO: parse and extract "title" or "detail"
        }
    }
  }

  /** Fetches the metadata for a tweet & extracts the media URL(s) therein.
    * @return `None` if `url` is not a tweet URL, otherwise a list of media URLs for the tweet
    * or a failed future with error explanation.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] =
    tweetIdFromUrl(url).map(resolveMedia) orElse checkInvalidUrl(url)

  /** Extracts the tweet id from a Twitter URL.
    * @return the tweet id or None if `url` is not a Twitter URL.
    */
  def tweetIdFromUrl (url :String) :Option[(String, String)] =
    urlRegexes.flatMap((r, u, g) => r.findFirstMatchIn(url) map { m => (m.group(u), m.group(g)) }).headOption

  private def checkInvalidUrl (url :String) =
    if (url.startsWith("https://x.com/") || url.startsWith("https://www.x.com/") ||
        url.startsWith("https://twitter.com/") || url.startsWith("https://www.twitter.com/")) Some(
      Future.successful(Result.failure("We cannot process that Twitter link.")))
    else None
}
