package servengine

/**
 * @author Jahan
 */
object Scenarios {
  def getModel = {
    val serv4R = new ServWorld(1154, false, false, List(new randBot(), new randBot(), new randBot(), new randBot()))
    val serv2R_same = new ServWorld(0x4545FF99, true, false, List(new randBot(), new randBot()))

    val serv4R_2 = new ServWorld(1154, false, false, List(new randBot(), new Bot008_DefShortcut(), new randBot(), new randBot()))
    
    val serv4R_3_2 = new ServWorld(1154, false, false, List(new Bot002(), new Bot003(), new Bot002(), new Bot002()))
     val serv4R_3_3 = new ServWorld(1154, false, false, List(new Bot003(), new Bot003(), new Bot003(), new Bot002()))
    
    val serv4R_4_2 = new ServWorld(1154, false, false, List(new randBot(), new Bot004(), new randBot(), new randBot())) 
    
    val serv4R_4_4 = new ServWorld(1154, false, false, List(new Bot004(), new Bot004(), new Bot004(), new Bot004()))
    
    val serv4R_5_4 = new ServWorld(1154, false, false, List(new Bot004(), new Bot005(), new Bot004(), new Bot004()))
    
    val serv4R_5_5 = new ServWorld(1154, false, false, List(new Bot005(), new Bot005(), new Bot005(), new Bot005()))
    
    val serv4R_6_5 = new ServWorld(1154, false, false, List(new Bot005(), new Bot006(), new Bot005(), new Bot005()))
    
    val serv4R_7_6 = new ServWorld(1154, false, false, List(new Bot006(), new Bot007Defender(), new Bot006(), new Bot006()))
    
     val serv4R_8d_7d = new ServWorld(1154, false, false, List(new Bot007Defender(), new Bot006(), new Bot007Defender(), new Bot007Defender()))
    
     val serv4R_9_6 = new ServWorld(1154, false, false, List(new Bot006(), new Bot009(), new Bot006(), new Bot006()))
    
    val serv2R_9_6 = new ServWorld(1154, false, false, List(new Bot006(), new Bot009()))

    val serv = serv2R_9_6
    serv
  }
}