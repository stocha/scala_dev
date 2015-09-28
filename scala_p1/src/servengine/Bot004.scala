package servengine

import scala.util.Random

/**
 * @author Jahan
 */
class Bot004 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
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
    coord = coords(idP)

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
       // Console.err.println("dest\n"+targs.head);
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
  
  def stratBase ={
    
    
        val bms = dat.extractBm(coords.size);
        val terri= bms(idP)
        val void = BitMap.voidArea(bms)
        val firstZone = BitMap.firstArea(bms, coords, idP)
        
        val terriDiag=BitMap.closeDiag(terri, void)
        
       // Console.err.println("terriDiag\n"+terriDiag);
        
        val bordFirst = (( firstZone | terri).frontierMap) &  (~terri)
        
        goTarget(coord, List(terriDiag,bordFirst,void), void)
        
  }

  override def turn: servCoord = {
    stratBase

  }
  override def name: String = {
    "Bot004 (" + idP + ")";
  }

}