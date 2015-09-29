package v2

/**
 * @author Jahan



 */
import scala.util.Random

class score(val x0 : Int,val x1 : Int,val x2 : Int,val x3 : Int){
  override def toString={
    val res="("+x0+" / "+x1+" / "+x2+" / "+x3+")"
    
    res
  }
  
}


  abstract class agentAbstract{
    def genMove (ref : GameState4P) : Int    
    def backMove(){}
  }  

  class stupidAgent extends agentAbstract{
    private val rand = new Random(0x4575752);  
    
    def genMove (ref : GameState4P) ={
        val d=rand.nextInt(4)
        d    
    }
  }

class SimulBot (seed : Long , val  ref : GameState4P,val agents : Array[agentAbstract]) {
  private val rand = new Random(seed);  
  private var sim=ref
 // private val agents = Array(new stupidAgent,new stupidAgent,new stupidAgent,new stupidAgent)
  
  var log=List[Int]()
  
  def apply(i : Int )(j : Int) ={
    sim.tr.get(i)(j)
  }  
  
  def reset(){
    sim=ref
  }
  
  def turn()={
      val n = rand.nextInt(GameState4P.moveMax)
      val d0=agents(0).genMove(sim.swap(0))
      val d1=      if(agents.size>1) agents(1).genMove(sim.swap(1)) else 4
      val d2=      if(agents.size>2) agents(2).genMove(sim.swap(2)) else 4
      val d3=       if(agents.size>3) agents(3).genMove(sim.swap(3)) else 4
      val move =GameState4P.m(d0, d1, d2, d3)
      sim= sim.transition(move)
      //System.err.println("Turn "+i);
      //System.err.println(""+game);    
      log = move :: log
      move
  }
  
  def doBackward(){
    
    if(!log.isEmpty){
      log= log.tail
      sim=ref
      
      for(i <- log.reverse){
        //System.err.println(""+i);
        sim=sim.transition(i)
      }
    }
    
    for(b <- agents){
      //System.err.println("backing up");
      b.backMove()
    }
  }
  
  def eval()={
    for(i<-0 until 700){
      turn()
    }
    
    new score(sim.tr(0).countBitset,sim.tr(1).countBitset,sim.tr(2).countBitset,sim.tr(3).countBitset)
  }
    
  
  def getState={
    
    sim
  }
    
}
