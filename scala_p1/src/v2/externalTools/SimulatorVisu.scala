package v2.externalTools

import scala.swing.SimpleSwingApplication
import scala.swing.BorderPanel
import scala.swing.MainFrame
import scala.swing._
import scala.swing.event.MouseClicked
import scala.swing.BorderPanel.Position._
import java.awt.Color
import scala.swing.event.MouseDragged
import scala.swing.event.MouseClicked
import v2.GameState4P

/**
 * @author Jahan
 */
object SimulatorVisu extends SimpleSwingApplication {

  val PER_SEC = 0.00001
  object Timer {
    def apply(interval: Int, repeats: Boolean = true)(op: => Unit) {
      val timeOut = new javax.swing.AbstractAction() {
        def actionPerformed(e: java.awt.event.ActionEvent) = op
      }
      val t = new javax.swing.Timer(interval, timeOut)
      t.setRepeats(repeats)
      t.start()
    }
  }

  val model = SimulatorModel.getModel;
  val canvas = new Panel {
        val cz = 20

    listenTo(mouse.moves)
        listenTo(mouse.clicks)
    reactions += {
          case e: MouseClicked => { if(e.peer.getButton==3) (model.doBackward()) else model.turn(); repaint(); Console.err.println(""+model.getState.scores)  }
      case e: MouseDragged => { model.turn(); repaint(); }
    }

    //Timer.apply((1000 / PER_SEC).toInt, true) { model.turn(); repaint(); System.err.println("Auto Turn done"); }

    preferredSize = new Dimension(GameState4P.W*cz +1, GameState4P.H*cz +1)


    val allCol = Array(Color.black, Color.pink, Color.green, Color.blue, Color.yellow)

    opaque = true
    background = Color.darkGray
    override def paintComponent(g: java.awt.Graphics2D) {

      super.paintComponent(g);
      g.setColor(Color.lightGray)

      for (i <- 0 until GameState4P.W; j <- 0 until GameState4P.H) {
        val v= model(i)(j).toInt match{
          case 1 => 1
          case 2 => 2
          case 4 => 3
          case 8 => 4
          case _ => 0
        } 
        g.setColor(allCol(v))

        g.fillRect(i * cz, j * cz, cz - 1, cz - 1)
      }

      for (c <- 0 until model.agents.size) {

        val is = model.getState.pos(c).forAllSet{ (i,j) =>
          g.setColor(Color.DARK_GRAY)
          g.fillRect(i * cz + cz / 4, j * cz + cz / 4, cz / 2, cz / 2)
          g.setColor(allCol(c+1))
          g.fillRect(i * cz + cz / 4 + cz / 6, j * cz + cz / 4 + cz / 6, cz / 3, cz / 3)          
        }

      }

    }
  }

  val ui = new BorderPanel {
    layout(canvas) = Center
  }

  override def top = new MainFrame {
    title = "title"
    contents = ui
  }

  //( model : ServWorld)
}

