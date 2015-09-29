package servengine

import scala.util.Random

/**
 * @author Jahan
 */
class Bot009 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var rand: Random = null
  var coords: Array[servCoord] = null
  var dat: servMap = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    this.coords = coords
    this.dat = dat
  }
  
    def goTarget(pos : servCoord,targs : List[BitMap], void : BitMap) : servCoord ={
    
    if(targs.isEmpty){
      pos
    }else
    if(!targs.head.isNull){
        val currMap=BitMap.zero.set(pos.x)(pos.y)(1)
         //Console.err.println("currMap\n"+currMap);
        //Console.err.println("dest\n"+targs.head);
        //Console.err.println("void\n"+void);
          val possibi = BitMap.firstDirToThrough(currMap, targs.head,void)
         // Console.err.println("possibi Through void "+possibi);
          if(possibi.nonEmpty){
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)
          }else{
              val possibi = BitMap.firstDirTo(BitMap.zero.set(pos.x)(pos.y)(1), targs.head)
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)            
          }
          
    }else{
      goTarget(pos,targs.tail,void)
    }

  }

  override def turn: servCoord = {
    strat
  }

  def strat = {

    val posPlay: Array[BitMap] = coords.map { x => BitMap.zero.set(x.x)(x.y)(1) }
    val crossMe = BitMap.zero.paintCrossAt(coords(idP).x, coords(idP).y)

    val territories = dat.extractBm(coords.size);
    val void = BitMap.voidArea(territories)

    val splitVoid = void.extractZones
    //------

    val firstZone = BitMap.firstArea(territories, coords, idP)
    val enemyLine = ~void ^ territories(idP)

    //Console.err.println("enemyLine\n"+enemyLine);
    //Console.err.println("firstZone\n" + firstZone);

    def getConqTarget = {
      val vwf=splitVoid.filter { x =>( !(x & firstZone).isNull) }
      val connexe=splitVoid.filter { x =>( !(x & posPlay(idP).scramble).isNull) }
      if(connexe.nonEmpty){
         connexe.maxBy { x => x.countBitset }
      }else
      if(vwf.nonEmpty){
        vwf.maxBy { x => x.countBitset }
      }else{
        if(splitVoid.nonEmpty){
          splitVoid.maxBy { x => x.countBitset }
        }else BitMap.zero
      }
    }
    
    val toConqRaw=getConqTarget | territories(idP)
    val toConqSpots=BitMap.closeDiag(toConqRaw.frontierMap, void) |toConqRaw.frontierMap
    Console.err.println("toConq\n" + toConqRaw);
    Console.err.println("toConqSpots\n" + toConqSpots);

    goTarget(coords(idP), List(toConqSpots,void), void)
    
    //new servCoord(coords(idP).x, coords(idP).y, 0);
  }

  override def name: String = {
    "Bot009 (" + idP + ")";
  }

}