package mediares

import scala.concurrent.Future

object TruthSocial {

  import Pool.ctx

  private val urlRegexes = Seq(
    ("^(https?://)?(www.)?truthsocial.com/@(\\S+)(/posts)?/(\\d+)".r, 3, 5))

  /** Resolves the media metadata for the specified TruthSocial post.
    * @return metadata for the media, or a failed future with an error message.
    */
  def resolveMedia (userId :String, postId :String) :Future[Result] = {
    Local.runCommand(
      // if testing locally and you don't have curl-impersonate installed, you can uncomment these
      // lines and it will run it from inside a Docker image
      // "docker",
      // "run",
      // "--rm",
      // "lwthiker/curl-impersonate:0.6-chrome",
      "curl_chrome116",
      "-s",
      s"https://truthsocial.com/api/v1/statuses/${postId}"
    ).map(lines => {
      lines.find(ll => ll.startsWith("{")) match {
        case Some(line) => Mastodon.decodeResponse("Truth Social", line)
        case None => Result.failure("Failed to resolve Truth Social post.", lines.mkString("\n"))
      }
    })
  }

  /** Fetches the metadata for a Mastodon post & extracts the media therein.
    * @return `None` if `url` is not a Mastodon post, otherwise the media metadata or a failed
    * future with error explanation.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] = postIdFromUrl(url).map(resolveMedia)

  /** Extracts the post id from a TruthSocial URL.
    * @return `(userId, postId)` or None if `url` is not a TruthSocial URL.
    */
  def postIdFromUrl (url :String) :Option[(String, String)] =
    urlRegexes.flatMap((r, u, g) => r.findFirstMatchIn(url) map { m => (m.group(u), m.group(g)) }).headOption
}
