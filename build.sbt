val scala3Version = "3.4.2"

Compile / run / fork := true

lazy val root = project.in(file(".")).settings(
  name := "mediares",
  organization := "org.global",
  version := "0.1.0",
  scalaVersion := scala3Version,
  libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.3",
  libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M8",
  libraryDependencies += "com.softwaremill.sttp.client4" %% "upickle" % "4.0.0-M8",
  libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "3.1.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  libraryDependencies += "io.undertow" % "undertow-core" % "2.1.0.Final",
  libraryDependencies += "com.twitter" % "twitter-api-java-sdk" % "2.0.3",
  libraryDependencies += "software.amazon.awssdk" % "s3" % "2.23.11",
  libraryDependencies += "software.amazon.awssdk" % "s3-transfer-manager" % "2.23.11",
  libraryDependencies += "io.lindstrom" % "mpd-parser" % "0.9",
)

enablePlugins(DockerPlugin)

docker / dockerfile := {
  val jarFile = (Compile / packageBin / sbt.Keys.`package`).value
  val classpath = (Compile / managedClasspath).value
  val mainclass = (Compile / packageBin / mainClass).value.getOrElse(
    sys.error("Expected exactly one main class"))
  val jarTarget = s"/app/${jarFile.getName}"
  val classpathString = classpath.files.map("/app/" + _.getName).mkString(":") + ":" + jarTarget
  val ytDlpUrl = "https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp"
  new Dockerfile {
    from("amazoncorretto:21-alpine-jdk") // Base image
    customInstruction("RUN", "apk add ffmpeg") // Install ffmpeg
    customInstruction("RUN", "apk add python3") // Install python
    customInstruction("WORKDIR", "bin")
    customInstruction("RUN", s"wget $ytDlpUrl") // "Install" yt-dlp
    customInstruction("RUN", "chmod a+rx yt-dlp")
    customInstruction("WORKDIR", "/usr/local/bin")
    customInstruction("COPY", "--from=lwthiker/curl-impersonate:0.6-chrome /usr/local/bin/curl-impersonate .")
    customInstruction("COPY", "--from=lwthiker/curl-impersonate:0.6-chrome /usr/local/bin/curl-impersonate-chrome .")
    customInstruction("COPY", "--from=lwthiker/curl-impersonate:0.6-chrome /usr/local/bin/curl_chrome116 .")
    customInstruction("WORKDIR", "/")
    add(classpath.files, "/app/") // Add all files on the classpath
    add(jarFile, jarTarget) // Add the JAR file
    entryPoint("java", "-cp", classpathString, mainclass) // Main command
  }
}

docker / buildOptions := BuildOptions(
  platforms = List("linux/amd64"),
)

docker / imageNames := Seq(
  ImageName(namespace = Some("OPEN-TODO-PLACEHOLDER.dkr.ecr.us-east-2.amazonaws.com/OPEN-TODO-PLACEHOLDER"),
            repository = name.value,
            tag = Some("latest"))
)
