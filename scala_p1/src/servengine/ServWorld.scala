

package servengine
/**
 * @author Jahan
 */

object servMap{
  val W = 35
  val H =  20
  
}

class servMap{
  val dat=Array.fill[Int](servMap.W,servMap.H) { 0}
  
}

class servCoord(
  val x : Int,val y : Int,val back : Int
){
  
}

abstract class servBot{
  def init(nbPlay : Int)
  
  def input(coords : List[servCoord])
}

class ServWorld(seed : Int,sameSquare : Boolean, symetrical : Boolean, bots : List[servBot] ) {
  
  
  
}