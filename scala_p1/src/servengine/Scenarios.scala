package servengine

/**
 * @author Jahan
 */
object Scenarios {
  def getModel = {
    val serv4R = new ServWorld(1154, false, false, List(new randBot(), new randBot(), new randBot(), new randBot()))
    val serv2R_same = new ServWorld(0x4545FF99, true, false, List(new randBot(), new randBot()))

    val serv4R_2 = new ServWorld(1154, false, false, List(new randBot(), new Bot002(), new randBot(), new randBot()))
    
    val serv4R_3_2 = new ServWorld(1154, false, false, List(new Bot002(), new Bot003(), new Bot002(), new Bot002()))
     val serv4R_3_3 = new ServWorld(1154, false, false, List(new Bot003(), new Bot003(), new Bot003(), new Bot002()))
    
    val serv4R_4_2 = new ServWorld(1154, false, false, List(new randBot(), new Bot004(), new randBot(), new randBot())) 
    
    val serv4R_4_4 = new ServWorld(1154, false, false, List(new Bot004(), new Bot004(), new Bot004(), new Bot004()))
    
    val serv4R_5_4 = new ServWorld(1154, false, false, List(new Bot004(), new Bot005(), new Bot004(), new Bot004()))
    
    val serv4R_5_5 = new ServWorld(1154, false, false, List(new Bot005(), new Bot005(), new Bot005(), new Bot005()))
    
    val serv4R_6_5 = new ServWorld(1154, false, false, List(new Bot005(), new Bot006(), new Bot005(), new Bot005()))

    val serv = serv4R_6_5
    serv
  }
}