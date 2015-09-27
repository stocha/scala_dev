package servengine

/**
 * @author Jahan
 */
object Scenarios {
  def getModel = {
    val serv4R = new ServWorld(1154, false, false, List(new randBot(), new randBot(), new randBot(), new randBot()))
    val serv2R_same = new ServWorld(0x4545FF99, true, false, List(new randBot(), new randBot()))

    val serv4R_2 = new ServWorld(1154, false, false, List(new randBot(), new Bot002(), new randBot(), new randBot()))

    val serv = serv4R_2
    serv
  }
}