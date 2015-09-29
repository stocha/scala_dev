package v2

import scala.util.Random

/**
 * @author Jahan
 */



class Simulateur001(seed : Long , val  ref : GameState4P) {
  private val rand = new Random(seed);  
  private var sim=ref
  private val agents = Array(new agent,new agent,new agent,new agent)
  
  class agent{
    var currdir=4
    
    def turn={
        val r=rand.nextInt(1000)
        if(r<500){
          (currdir+1)&3
        }else{
          (currdir-1)&3
        }      
    }
    
    def choice={
      if(currdir==4){
        val d=rand.nextInt(4)
        d
        
      }else{
        val goingStraightProb=700
        val r=rand.nextInt(1000)
        if(r<goingStraightProb){
          currdir
        }else{
          val r=rand.nextInt(1000)
          if(r<800){
            turn
          }else{
            (currdir-2)&3
          }
        }
        
      }      
    }
    
    def genMove()={
      currdir=choice
      currdir
    }
    
  }  
  
  
  def reset(){
    sim=ref
  }
  
  def turn()={
      val n = rand.nextInt(GameState4P.moveMax)
      val d0=agents(0).genMove()
      val d1=agents(1).genMove()
      val d2=agents(2).genMove()
      val d3=agents(3).genMove()
      val move =GameState4P.m(d0, d1, d2, d3)
      sim= sim.transition(move)
      //System.err.println("Turn "+i);
      //System.err.println(""+game);    
    
      move
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

