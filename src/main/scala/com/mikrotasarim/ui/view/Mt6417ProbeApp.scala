package com.mikrotasarim.ui.view

import com.mikrotasarim.ui.controller._
import com.mikrotasarim.ui.model.MemoryMap
import org.controlsfx.dialog.Dialogs

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.image.ImageView
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.{Node, Scene}

object Mt6417ProbeApp extends JFXApp {

  val model = ProbeTestController

  stage = new PrimaryStage {
    title = "Mikro-Tasarim MT3817BA Probe Test App"
    width = 1100
  }

  stage.scene = new Scene {
    root = new ScrollPane {
      content = new HBox {
        padding = Insets(10)
        spacing = 20
        content = List(
          createControlColumn,
          createTestColumn
        )
      }
    }
  }

  private def createTestColumn: Node = new VBox {
    padding = Insets(10)
    spacing = 10
    content = createTests :+ new ImageView() {
      image <== ProbeTestController.currentImage
    }
  }

  private def createTests: Seq[Node] = {
    for (testCase <- model.testCases.zipWithIndex) yield {
      new HBox {
        spacing = 10
        content = Seq(
          new Label((testCase._2 + 1) + ". " + testCase._1.name) {
            prefWidth = 300
          },
          passFailControl(testCase._1.testPassed, testCase._1.testFailed),
          new Button("Run") {
            onAction = handle {
              testCase._1.runTest()
            }
          },
          new Label() {
            text <== testCase._1.shortMessage
          }
        )
      }
    }
  }

  private def passFailControl(pass: BooleanProperty, fail: BooleanProperty): Node = {
    val tog = new ToggleGroup()

    new HBox {
      spacing = 10
      prefWidth = 120
      content = List(
        new RadioButton("Pass") {
          toggleGroup = tog
          selected <==> pass
        },
        new RadioButton("Fail") {
          toggleGroup = tog
          selected <==> fail
        }
      )
    }
  }

  private def createControlColumn: Node = new VBox {
    spacing = 10
    content = List(
      new ChoiceBox(ProbeTestController.systemOptions) {
        value <==> ProbeTestController.selectedSystem
      },
      new Button("Run All Tests") {
        onAction = handle {
          ProbeTestController.runAll()
        }
      },
      new Button("Save Result and Proceed") {
        onAction = handle {
          OutputController.saveAndProceed()
        }
      },
      createCommentControl(),
      new Separator,
      createLabelControls(),
      new Separator,
      new ChoiceBox(PowerSourceController.comPortList) {
        value <==> PowerSourceController.selectedComPort
      },
      new HBox {
        spacing = 10
        content = List(
          new Label("Output") {
            prefWidth = 85
          },
          new Button("On") {
            onAction = handle {
              PowerSourceController.outputOn()
            }
          },
          new Button("Off") {
            onAction = handle {
              PowerSourceController.outputOff()
            }
          }
        )
      },
      new HBox {
        spacing = 10
        content = List(
          new Label("Mode") {
            prefWidth = 70
          },
          new Button("Local") {
            onAction = handle {
              PowerSourceController.setLocal()
            }
          },
          new Button("Remote") {
            onAction = handle {
              PowerSourceController.setRemote()
            }
          }
        )
      },
      new Separator,
      new Button("Memory Map") {
        onAction = handle {
          MemoryMap.readRoicMemory()
          MemoryMapStage.show()
        }
      },
      new Button("Initialize") {
        onAction = handle {
          ProbeTestController.initializeRoic()
        }
      },
      new Button("Refresh Image") {
        onAction = handle {
          ProbeTestController.refreshImage()
        }
      },
      new Separator,
      new CheckBox("Self Test Mode") {
        selected <==> FpgaController.testMode
      }
    )
  }

  private def createLabelControls(): Node = {
    new VBox {
      spacing = 10
      content = List(
        createPathControl,
        createWaferControl,
        createDieControl
      )
    }
  }

  private def createPathControl = new HBox {
    spacing = 10
    content = List(
      new Label("Output path") {
        prefWidth = 85
      },
      new TextField {
        prefWidth = 150
        text <==> OutputController.outputPath
      }
    )
  }

  private def createWaferControl = new HBox {
    spacing = 10
    content = List(
      new Label("Wafer Id") {
        prefWidth = 85
      },
      new TextField {
        prefWidth = 150
        text <==> OutputController.waferId
      }
    )
  }

  private def createDieControl = new HBox {
    spacing = 10
    content = List(
      new Label("Die #") {
        prefWidth = 85
      },
      new TextField {
        prefWidth = 150
        text.onChange({
          text.value = text.value.replaceAll("[^0-9]", "")
        })
        text <==> OutputController.dieNumber
      }
    )
  }

  private def createCommentControl(): Node = {
    new HBox {
      spacing = 10
      content = List(
        new Label("Comment:") {
          prefWidth = 85
        },
        new TextField {
          prefWidth = 150
          text <==> OutputController.comment
        }
      )
    }
  }

  object UncaughtExceptionHandler extends Thread.UncaughtExceptionHandler {
    def uncaughtException(thread: Thread, e: Throwable): Unit = e match {
      case e: UnsatisfiedLinkError => Dialogs.create()
        .title("Error")
        .masthead("Unsatisfied Link")
        .message("Opal Kelly driver not in java library path.")
        .showException(e)
      case e: Exception => Dialogs.create()
        .title("Exception")
        .masthead("Unhandled Exception")
        .message(e.getMessage)
        .showException(e)
      case e: Error => Dialogs.create()
        .title("Error")
        .masthead("Unhandled Error")
        .message(e.getMessage)
        .showException(e)
      case _ => Dialogs.create()
        .title("Problem")
        .masthead("Unhandled Problem")
        .message(e.getMessage)
        .showException(e)
    }
  }

  Thread.currentThread().setUncaughtExceptionHandler(UncaughtExceptionHandler)
}
