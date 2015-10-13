package cod.mars

/**
 * @author Jahan
 */
object geometric {

  type StructCoord = { def getCoord: point }
  
  def sign(dx : Double)=if(dx ==0) 1 else (dx/dx.abs)

  object point {
    def apply(dx: Double, dy: Double) = {
      new point(dx, dy)
    }
    
    def apply(x : Tuple2[Double,Double]) = {
      new point(x._1, x._2)
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
    
    def ++(other : StructCoord) : point={
      val x = p.getCoord.x;
      val y = p.getCoord.y;   
      
      val xp = other.getCoord.x;
      val yp = other.getCoord.y;      
      
      point(x+xp,y+yp)      
    }
    
    def center(other : StructCoord) : point ={
      val x = p.getCoord.x;
      val y = p.getCoord.y;   
      
      val xp = other.getCoord.x;
      val yp = other.getCoord.y;         
      
      point((x+xp)/2 ,( y+yp)/2)
    }

  }

}

import geometric.point
import geometric.sign

object mars{
  val GRAV = point(0,-3.711)
  
  def mxtStableAngleVect = {
    val l=for(i <- 0 to 90) yield{
      val p=(point.angulNormDegree(90-i)*4)
      val pg= p ++ GRAV
      (pg,i)
    }
    
    l.filter { x => x._1.y > 0 }.maxBy { x => Math.abs(x._1.x) }
    //l
  }
  
  def stableHVel=mxtStableAngleVect._1.x
  
  def stableHAngle=mxtStableAngleVect._2
  
  class terrain(val g : Seq[point]){
    def findLanding = {
      val l=g.toList
      val there=(g zip g.tail).filter { x => (x._1.y-x._2.y ==0) }.head
      
      there._1 center there._2
    }
  }
  
  class vehicule(coord : point, val velocity : point){
    def getCoord = coord
    
    def gotoH(c : point) : (Double,Double)= {
      val dx =c.x-coord.x
      
      val nbStepToSpeed0=if (velocity.x==0) 0 else (dx/velocity.x).abs
      
      val distToSpeed0=nbStepToSpeed0*velocity.x.abs - stableHVel * nbStepToSpeed0
      
      val dirDest=if(dx ==0) 1 else (dx/dx.abs)
      val dirVel=sign(velocity.x)
      
     
      
      
      if(distToSpeed0>0){
        ( stableHAngle*dirVel,4)
      }else{
        (0,4)
      }
    }
  }
  
  
}


object testGeometric extends App {

  override def main(args: Array[String]) {

    Console.err.println(" mxtStable "+mars.stableHVel+" "+mars.stableHAngle );    
    var v = new mars.vehicule(point(200,200),point(100,0))
    
    Console.err.println(" goto 0 0 "+v.gotoH(point(0,0) ));  
    Console.err.println(" goto 200 0 "+v.gotoH(point(200,0) ));
    
  }
}