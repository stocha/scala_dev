package cod.mars

/**
 * @author Jahan
 */
object geometric {

  type StructCoord = { def getCoord: point }

  object point {
    def apply(dx: Double, dy: Double) = {
      new point(dx, dy)
    }

    def normedY() = {
      point(0, 1)
    }

    def normedX() = {
      point(1, 0)
    }

    def angulNorm(ang: Double) = {
      point(Math.cos(ang), Math.sin(ang))

    }
    
    def angulNormDegree(ang : Double)={
      angulNorm(ang*Math.PI/180)
    }

    implicit def richPoint(x: StructCoord) = {
      new pointManip(x)
    }

  }

  class point(val x: Double, val y: Double) {
    override def toString = {
      val res = "(" + x + "," + y + ")"
      res
    }

    def getCoord: point = {
      this
    }
  }

  class pointManip(val p: StructCoord) {
    def *(s: Double) = {
      point(p.getCoord.x * s, p.getCoord.y * s)
    }

    def ||= {
      val x = p.getCoord.x;
      val y = p.getCoord.y;

      Math.sqrt(x * x + y * y)
    }

    def *( other: StructCoord) = {
      val x = p.getCoord.x;
      val y = p.getCoord.y;   
      
      val xp = other.getCoord.x;
      val yp = other.getCoord.y;      
      
      point(x*xp-y*yp,y*xp+yp*x)
    }
    
    def+(other : StructCoord)={
      val x = p.getCoord.x;
      val y = p.getCoord.y;   
      
      val xp = other.getCoord.x;
      val yp = other.getCoord.y;      
      
      point(x+xp,y+yp)      
    }

  }

}

import geometric.point

object testGeometric extends App {

  override def main(args: Array[String]) {
    Console.err.println(" Hello there " + (point(4, 6.5) * 5.0));
    Console.err.println(" Hello there " + (point.normedY() * 5.0));
    Console.err.println(" Rot 90 " + (point.normedY() * 5.0)*point.angulNormDegree(-45));
Console.err.println(" Rot 90 " + (point.normedY() * 5.0)*point.angulNormDegree(-45)*point.angulNormDegree(-45));    

  }
}