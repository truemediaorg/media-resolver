package mediares

object Env {

  private val log = java.util.logging.Logger.getLogger("mediares")

  /** Gets an env var. */
  def getEnv (name :String) = Option(System.getenv(name)).map(_.trim)

  /** Gets an env var. Uses `defval` if the env var is not set. */
  def getString (name :String, defval :String) = getEnv(name) getOrElse defval

  /** Gets an env var and converts its value into an int. Uses `defval` if the env var is not set
    * or is invalid. */
  def getInt (name :String, defval :Int) = getEnv(name) match {
    case None => defval
    case Some(str) =>
      try str.toInt
      catch {
        case e :Throwable =>
          log.warning(s"Invalid $name: '$str', using default: $defval")
          defval
      }
  }

  /** Gets an env var and converts its value (`true` or `false`, case insensitive) into a boolean.
    * Uses `defval` if the env var is not set or is invalid. */
  def getBool (name :String, defval :Boolean) = getEnv(name) match {
    case None => defval
    case Some(str) =>
      if (str.equalsIgnoreCase("true")) true
      else if (str.equalsIgnoreCase("false")) false
      else {
        log.warning(s"Invalid $name: '$str', using default: $defval")
        defval
      }
  }

  /** Obtains the specified secrets from the `SECRETS` environment variable. `SECRETS` should
    * contain a JSON object. This is the format used by the AWS Secrets Manager.
    */
  def getSecrets (keys :String*) :Seq[String] = {
    val secrets = System.getenv("SECRETS")
    if (secrets == null) throw Exception("Missing SECRETS environment variable.")
    try ujson.read(secrets) match {
      case ujson.Obj(values) =>
        val missing = keys.filter(kk => !values.contains(kk))
        if (!missing.isEmpty) throw new Exception(
          s"Missing required SECRETS: ${missing.mkString(", ")}")
        keys.map(kk => values(kk).str)
      case _ => throw Exception("SECRETS environment variable is not a JSON object.")
    }
    catch {
      case e :Throwable =>
        throw Exception(s"Failed to parse SECRETS environment variable: ${e.getMessage}")
    }
  }
}
