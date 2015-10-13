package cod.mars

/**
 * @author Jahan
 */
object geometric  {

  type StructCoord = { def getCoord : point }

  object point {
    def apply(dx: Double, dy: Double) = {
      new point(dx, dy)
    }
    
    def normedY()={
      point(0,1)
    }
    
    def normedX()={
      point(1,0)
    }
    
    implicit def richPoint(x : StructCoord)={
      new pointManip(x)
    }
    

  }

  class point(val x: Double, val y: Double) {
    override def toString={
      val res="("+x+","+y+")"
      res
    }
    
    def getCoord : point={
      this
    }    
  }

  class pointManip(val p : StructCoord) {
    def *( s : Double)={
      point(p.getCoord.x*s,p.getCoord.y*s)
    }
  }
  
  

}

import geometric.point

object testGeometric extends App {
  
     override def main(args: Array[String]) {
     Console.err.println(" Hello there "+(point(4,6.5)*5.0));
     Console.err.println(" Hello there "+(point.normedY()*5.0));
   
   }
}