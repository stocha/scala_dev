package servengine

/**
 * @author Jahan
 */
object Testeur extends App {
    override def main(args: Array[String]) {
      println("Test, world!")
      val serv= new ServWorld(1154,false, false,List(new randBot(), new randBot()))
      
      for( i <- 0 until 100){
        serv.applyTurn()
        
      }
    }  
}