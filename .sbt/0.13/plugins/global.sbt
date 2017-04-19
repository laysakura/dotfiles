// build.sbtを編集したときにsbt reloadを実行しないと設定が反映されないが、よく実行し忘れるので実行必要だけどされてないですよというのを教えてくれるプラグイン
// http://tototoshi.hatenablog.com/entry/2015/01/31/001535
addSbtPlugin("com.github.tototoshi" % "sbt-build-files-watcher" % "0.1.1")
 
// sbtのライブラリ解決＆ダウンロードを爆速にしてくれるもの（依存解決も速いので既にダウンロード済みでも速くなる）
// まだ未完成なので、これをつけていると依存解決に失敗するものもあるので https://gist.github.com/matsu-chara/5856ec0ee322fb7ce20fb40e93db8c35 と併用すると便利
// https://gist.github.com/gakuzzzz/49b78acfe1401fd8047c7a0f0d981cfa
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC1")
 
// sbt dependencyBrowseGraphによって依存関係を表示する。誰がどのライブラリに依存しているか確認するのに使える。
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
 
 
// sbt dependencyUpdatesによって依存ライブラリにアップデートがあるかどうか教えてくれる。
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0")
