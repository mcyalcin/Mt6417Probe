package com.mikrotasarim.ui.controller

import com.mikrotasarim.api.command.DeviceController
import com.mikrotasarim.api.device.{ConsoleMockDeviceInterface, DeviceInterface, OpalKellyInterface}

import scalafx.beans.property.BooleanProperty

object FpgaController {

  var device: DeviceInterface = _
  var deviceController: DeviceController = _

  var bitfileDeployed = false

  def deployBitfile(): Unit = {
    if (!bitfileDeployed) {
      if (testMode.value) {
        device = new ConsoleMockDeviceInterface()
      } else {
        device = new OpalKellyInterface("mt3817ba.bit")
      }
      deviceController = new DeviceController(device)
      bitfileDeployed = true
    }
  }

  val testMode = BooleanProperty(value = false)
}
