package servengine

/**
 * @author Jahan
 */
object Scenarios {
  def getModel = {
          val serv= new ServWorld(1154,false, false,List(new randBot(), new randBot(), new randBot(), new randBot()))
          
          
          serv
  }
}