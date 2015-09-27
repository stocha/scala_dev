package servengine

import scala.swing.SimpleSwingApplication
import scala.swing.BorderPanel
import scala.swing.MainFrame
import scala.swing._
import scala.swing.event.MouseClicked
import scala.swing.BorderPanel.Position._
import java.awt.{ Graphics2D, Color }

/**
 * @author Jahan
 */
object  ServVisu extends SimpleSwingApplication {

              val model=Scenarios.getModel;
  val canvas = new Panel {
    
  listenTo(mouse.clicks)
  reactions += {
    case e: MouseClicked => {model.applyTurn();repaint();System.err.println("Turn done");}
  }    
    
    preferredSize = new Dimension(600,400)
    val cz=20

    val allCol= Array(Color.black,Color.pink,Color.green,Color.blue,Color.yellow)
    
    opaque = true
    background = Color.darkGray
    override def paintComponent(g: java.awt.Graphics2D) {

      super.paintComponent(g);
      g.setColor(Color.lightGray)
      
      for(i <- 0 until ServWorld.W; j <- 0 until ServWorld.H){
        val p=(model.officialMap.dat)(i)(j)+1
        g.setColor(allCol(p))
        
        
        g.fillRect(i*cz, j*cz, cz-1, cz-1)
      }
      
      var id=0
      for(c <- model.rawTurn.head){
          
          id=id +1
          val i=c.x
          val j=c.y
          g.setColor(Color.DARK_GRAY)
          g.fillRect(i*cz +cz/4, j*cz + cz/4, cz/2, cz/2)
          g.setColor(allCol(id))
          g.fillRect(i*cz +cz/4+cz/6, j*cz +cz/4+cz/6, cz/3, cz/3)
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

