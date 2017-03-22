// sbt 0.13.13でtestOnly時のテストクラス名のtab補完が効かないデグレのワークアラウンド
// http://d.hatena.ne.jp/xuwei/20170123/1485152011
inConfig(Test){
  definedTestNames <<= {
    import sbinary.DefaultProtocol.StringFormat
    import Cache.seqFormat
    (definedTests map (_.map(_.name).distinct) storeAs definedTestNames triggeredBy compile)
  }
}
