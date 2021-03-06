resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  "bintray" at "http://jcenter.bintray.com"
)

libraryDependencies ++= Seq(
  "org.macroid" %% "macroid" % "2.0.0-M3",
  "com.hanhuy" %% "android-common" % "0.3-SNAPSHOT",
  "ch.acra" % "acra" % "4.5.0",
  "com.android.support" % "support-v4" % "21.0.2",
  "com.android.support" % "appcompat-v7" % "21.0.2"
)

proguardCache in Android += ProguardCache("macroid") % "org.macroid"
