package com.mikrotasarim.ui.controller

import javafx.embed.swing.SwingFXUtils
import javafx.scene.image.Image

import com.mikrotasarim.api.command.ApiConstants.{NucMode, TriggerMode}
import com.mikrotasarim.ui.model.{A1FrameProvider, Frame, ProbeTestCase}
import spire.implicits._

import scalafx.beans.property.{StringProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer

object ProbeTestController {

  def fc = FpgaController

  def dc = fc.deviceController

  def psc = PowerSourceController

  def rvc = ReferenceValueController

  def fp = new A1FrameProvider(dc, xSize(selectedSystem.value), ySize(selectedSystem.value))

  val systemOptions = ObservableBuffer(List(
    "MT3817BA",
    "MT6417BA"
  ))

  val selectedSystem = StringProperty("MT3817BA")

  val xSize = Map("MT3817BA" -> 384, "MT6417BA" -> 640)
  val ySize = Map("MT3817BA" -> 288, "MT6417BA" -> 480)
  val depth = Map("MT3817BA" -> 8192, "MT6417BA" -> 8192)

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
    rvc.checkPower(currentSeq)
  }

  private def memoryTest(): (Boolean, String) = {
    fc.deployBitfile()
    dc.putFpgaOnReset()
    dc.takeFpgaOffReset()
    dc.setReset()
    dc.clearReset()
    val errors = new StringBuilder
    for (i <- (5 to 95).filter(_ != 10)) {
      dc.writeToRoicMemory(i, 0)
      dc.writeToRoicMemory(i, 1)
      if (dc.readFromRoicMemory(i) % 2 != 1) {
        errors.append("Address " + i + " bit 0 failed to toggle from 0 to 1\n")
      }
      for (j <- 1 until 16) {
        val testData = 2 pow j
        dc.writeToRoicMemory(i, testData)
        val read = dc.readFromRoicMemory(i)
        if ((read / testData / 2) % 2 != 0) {
          errors.append("Address " + i + " bit " + (j - 1) + " failed to toggle from 1 to 0\n")
        }
        if ((read / testData) % 2 != 1) {
          errors.append("Address " + i + " bit " + j + " failed to toggle from 0 to 1\n")
        }
      }
      dc.writeToRoicMemory(i, 0)
      if ((dc.readFromRoicMemory(i) / 2 pow 15) % 2 != 0) {
        errors.append("Address " + i + " bit 15 failed to toggle from 1 to 0\n")
      }
    }
    dc.setReset()
    dc.clearReset()
    dc.initializeRoic()
    dc.setTriggerMode(TriggerMode.Slave_Software)
    dc.setNucMode(NucMode.Fixed, 0xff)
    dc.sendReferenceDataToRoic()
    val refOnes = dc.readReferenceData(xSize(selectedSystem.value))
    for (i <- refOnes.indices) {
      if (refOnes(i) != 0xf) {
        errors.append("Reference byte " + i + " expected 0xf read " + refOnes(i).toInt.toHexString + ".\n")
      }
    }
    dc.setNucMode(NucMode.Fixed, 0x00)
    dc.sendReferenceDataToRoic()
    val refZeroes = dc.readReferenceData(xSize(selectedSystem.value))
    for (i <- refZeroes.indices) {
      if (refZeroes(i) != 0x0) {
        errors.append("Reference byte " + i + " expected 0x0 read " + refZeroes(i).toInt.toHexString + ".\n")
      }
    }
    dc.enableImagingMode()
    dc.sendFsync()
    dc.disableImagingMode()
    Thread.sleep(1000)
    dc.setNucMode(NucMode.Fixed, 0xff)
    dc.enableImagingMode()
    dc.sendFsync()
    Thread.sleep(1000)
    dc.disableImagingMode()
    val nucOnes = dc.readNuc(xSize(selectedSystem.value))
    for (i <- nucOnes.indices) {
      if (nucOnes(i) != 0xf) {
        errors.append("Nuc byte " + i + " expected 0xf read " + nucOnes(i).toInt.toHexString + ".\n")
      }
    }
    dc.setNucMode(NucMode.Fixed, 0x00)
    dc.enableImagingMode()
    dc.sendFsync()
    Thread.sleep(1000)
    dc.disableImagingMode()
    val nucZeroes = dc.readNuc(xSize(selectedSystem.value))
    for (i <- nucZeroes.indices) {
      if (nucZeroes(i) != 0x0) {
        errors.append("Reference byte " + i + " expected 0x0 read " + nucZeroes(i).toInt.toHexString + ".\n")
      }
    }
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
    dc.writeToRoicMemory(0x3B, 521)
    dc.updateRoicMemory()
    dc.writeToRoicMemory(0x11, 0x045F)
    dc.updateRoicMemory()
    dc.enableImagingMode()
    val frameProvider = fp
    var frame = fp.getFrame.getGrayscale
    Thread.sleep(100)
    frame = frameProvider.getFrame.getGrayscale
    // TODO: Why is frame taken twice? Solve the problem and remove.
    val convertedFrame = SwingFXUtils.toFXImage(frame, null)
    currentImage.set(convertedFrame)
    (false, "")
  }

  def runAll(): Unit = {
    for (testCase <- testCases) testCase.runTest()
  }

  val diagonalData = Array.ofDim[Int](xSize(selectedSystem.value) * ySize(selectedSystem.value))
  for (i <- 0 until xSize(selectedSystem.value)) for (j <- 0 until ySize(selectedSystem.value)) diagonalData(j * xSize(selectedSystem.value) + i) = depth(selectedSystem.value) * i / xSize(selectedSystem.value) + depth(selectedSystem.value) * j / ySize(selectedSystem.value)
  val diagonalFrame = Frame.createFrom14Bit(xSize(selectedSystem.value), ySize(selectedSystem.value), diagonalData)

  def resetImage(): Unit = {
    currentImage.set(SwingFXUtils.toFXImage(diagonalFrame.getGrayscale, null))
  }

  val currentImage = ObjectProperty[Image](SwingFXUtils.toFXImage(diagonalFrame.getGrayscale, null))

  def refreshImage(): Unit = {
    fc.deployBitfile()
    dc.setTriggerMode(TriggerMode.Slave_Software)
    dc.setNucMode(NucMode.Enabled)
    dc.enableImagingMode()
    val frame = fp.getFrame.getGrayscale
    val convertedFrame = SwingFXUtils.toFXImage(frame, null)
    dc.disableImagingMode()
    currentImage.set(convertedFrame)
  }

  def initializeRoic(): Unit = {
    fc.deployBitfile()
    dc.putFpgaOnReset()
    dc.takeFpgaOffReset()
    dc.setReset()
    dc.clearReset()
    dc.initializeRoic()
    dc.setNucMode(NucMode.Fixed, 255, 255) // TODO: replace magic numbers
    dc.sendReferenceDataToRoic()
    dc.setNucMode(NucMode.Enabled)
    dc.setSamplingDelay(4) // TODO: replace magic numbers
    dc.setAdcDelay(3) // TODO: replace magic numbers
  }
}
