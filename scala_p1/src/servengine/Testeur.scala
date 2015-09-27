package servengine

/**
 * @author Jahan
 */
object Testeur extends App {
  
    def testBm{
      
      println("Test, Bm!")
      val b=BitMap.zero
      
      println(""+b);
      
      println(""+BitMap.full);
      
    }
    
    def testServ{
      println("Test, world!")
      val serv= new ServWorld(1154,false, false,List(new randBot(), new randBot()))
      
      for( i <- 0 until 100){
        serv.applyTurn()
        
      }      
    }
  
    override def main(args: Array[String]) {
      testBm
    }  
}