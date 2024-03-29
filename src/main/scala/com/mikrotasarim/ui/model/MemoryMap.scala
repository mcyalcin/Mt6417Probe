package com.mikrotasarim.ui.model

import com.mikrotasarim.ui.controller.FpgaController

import scalafx.beans.property.StringProperty

object MemoryMap {

  def deviceController = FpgaController.deviceController

  val minMemoryIndex = 4
  val maxMemoryIndex = 95

  val memoryLocations = for (i <- minMemoryIndex to maxMemoryIndex) yield new MemoryLocation(i)

  def readRoicMemory(): Unit = {
    if (!FpgaController.bitfileDeployed) {
      FpgaController.deployBitfile()
      FpgaController.deviceController.takeFpgaOffReset()
      FpgaController.deviceController.setReset()
      FpgaController.deviceController.clearReset()
    }
    for (memoryLocation <- memoryLocations) {
      memoryLocation.read()
    }
  }

  class MemoryLocation(val address: Int) {
    val text = StringProperty("0000000000000000")

    def read(): Unit = {
      val value = deviceController.readFromRoicMemory(address)
      text.value = String.format("%16s", value.toBinaryString).replace(' ', '0')
    }

    def commit(): Unit = {
      val value = java.lang.Long.parseLong(text.value, 2)
      deviceController.writeToRoicMemory(address, value)
      read()
    }
  }
}
