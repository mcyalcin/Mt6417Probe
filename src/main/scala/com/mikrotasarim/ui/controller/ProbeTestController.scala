package com.mikrotasarim.ui.controller

import javafx.embed.swing.SwingFXUtils
import javafx.scene.image.Image

import com.mikrotasarim.api.command.ApiConstants.{NucMode, TriggerMode}
import com.mikrotasarim.ui.model.{Frame, A1FrameProvider, ProbeTestCase}
import spire.implicits._

import scalafx.beans.property.ObjectProperty

object ProbeTestController {

  def fc = FpgaController
  def dc = fc.deviceController
  def psc = PowerSourceController
  def rvc = ReferenceValueController

  val testCases: Seq[ProbeTestCase] = Seq(
    new ProbeTestCase("Power Consumption Test", powerConsumptionTest),
    new ProbeTestCase("Memory Test", memoryTest),
    new ProbeTestCase("Image Test", imageTest)
  )

  private def powerConsumptionTest(): (Boolean, String) = {
    fc.deployBitfile()
    dc.putFpgaOnReset()
    dc.takeFpgaOffReset()
    dc.setReset()
    val currentS1 = psc.measureCurrent()
    dc.clearReset()
    val currentS2 = psc.measureCurrent()
    dc.initializeRoic()
    dc.writeToRoicMemory(0x5f, 0x0000)
    dc.writeToRoicMemory(0x0e, 0x0003)
    dc.writeToRoicMemory(0x17, 0x0003)
    dc.writeToRoicMemory(0x0c, 0x003f)
    dc.writeToRoicMemory(0x11, 0x0004)
    val currentS3 = psc.measureCurrent()
    dc.writeToRoicMemory(0x5f, 0xa8a0)
    val currentS4 = psc.measureCurrent()
    dc.writeToRoicMemory(0x0c, 0x0000)
    val currentS5 = psc.measureCurrent()
    dc.writeToRoicMemory(0x0e, 0x0000)
    val currentS6 = psc.measureCurrent()
    dc.writeToRoicMemory(0x17, 0x0000)
    val currentS7 = psc.measureCurrent()
    val currentSeq = Seq(currentS1, currentS2, currentS3, currentS4, currentS5, currentS6, currentS7)
    val results = rvc.checkPower(currentSeq)
    if (results.contains(false)) {
      (false, "Not Implemented Yet\n") // TODO: Specify error output format
    } else {
      (true, "")
    }
  }

  private def memoryTest(): (Boolean, String) = {
    fc.deployBitfile()
    dc.putFpgaOnReset()
    dc.takeFpgaOffReset()
    dc.setReset()
    dc.clearReset()
    val errors = new StringBuilder
    for (i <- (4 to 95).filter(_!=7)) {
      dc.writeToRoicMemory(i, 0)
      dc.writeToRoicMemory(i, 1)
      if (dc.readFromRoicMemory(i) % 2 != 1) {
        errors.append("Address " + i.toHexString + " bit 0 failed to toggle from 0 to 1\n")
      }
      for (j <- 1 until 16) {
        val testData = 2 pow j
        dc.writeToRoicMemory(i, testData)
        val read = dc.readFromRoicMemory(i)
        if ((read / testData / 2) % 2 != 0) {
          errors.append("Address " + i.toHexString + " bit " + (j - 1) + " failed to toggle from 1 to 0\n")
        }
        if ((read / testData) % 2 != 1) {
          errors.append("Address " + i.toHexString + " bit " + j + " failed to toggle from 0 to 1\n")
        }
      }
      dc.writeToRoicMemory(i, 0)
      if ((dc.readFromRoicMemory(i) / 2 pow 15) % 2 != 0) {
        errors.append("Address " + i.toHexString + " bit 15 failed to toggle from 1 to 0\n")
      }
    }
    dc.setReset()
    dc.clearReset()
    dc.initializeRoic()
    dc.setTriggerMode(TriggerMode.Slave_Software)
    dc.setNucMode(NucMode.Fixed, 0xff)
    dc.sendReferenceDataToRoic()
    // TODO: Pending specification
    (errors.toString().isEmpty, errors.toString())
  }

  private def imageTest(): (Boolean, String) = {
    fc.deployBitfile()
    dc.putFpgaOnReset()
    dc.takeFpgaOffReset()
    dc.setReset()
    dc.clearReset()
    dc.initializeRoic()
    dc.setTriggerMode(TriggerMode.Slave_Software)
    dc.setNucMode(NucMode.Enabled)
    dc.enableImagingMode()
    val frameProvider = new A1FrameProvider(dc, 640, 480)
    val frame = frameProvider.getFrame.getGrayscale
    val convertedFrame = SwingFXUtils.toFXImage(frame, null)
    currentImage.set(convertedFrame)
    (false, "")
  }

  def runAll(): Unit = {
    for (testCase <- testCases) testCase.runTest()
  }

  val diagonalData = Array.ofDim[Int](640 * 480)
  for (i <- 0 until 640) for (j <- 0 until 480) diagonalData(j * 640 + i) = 8192 * i / 640 + 8192 * j / 480
  val diagonalFrame = Frame.createFrom14Bit(640, 480, diagonalData)

  val currentImage = ObjectProperty[Image](SwingFXUtils.toFXImage(diagonalFrame.getGrayscale, null))
}
