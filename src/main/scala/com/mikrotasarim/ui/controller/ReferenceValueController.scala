package com.mikrotasarim.ui.controller

import scala.io.Source

object ReferenceValueController {

  val input = Source fromFile "ref.conf" getLines()

  val referenceValues = for (i <- 0 until 7) yield input.next().toDouble

  val margin = input.next().toDouble

  def checkPower(currentSeq: Seq[Double]): (Boolean, String) = {
    var verdict = true
    val report = new StringBuilder
    for (i <- 0 until 7) {
      if (math.abs(referenceValues(i) - currentSeq(i)) > margin) verdict = false
      report.append("Stage " + (i + 1) + " expected " + referenceValues(i) + " measured " + currentSeq(i) + "\n")
    }
    (verdict, report.toString())
  }
}
