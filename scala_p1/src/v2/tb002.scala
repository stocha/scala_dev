package v2

/**
 * @author Jahan
 */
class tb002 extends agentAbstract{
  
  
   var currPlan : agentAbstract=null
  
  def doPlan(ref : GameState4P) ={
  
    val bv = new BotVocabulary(ref)
    
    val area=   bv.simpleSquareRuleZone 
    Console.err.println("target area\n"+area);
    Console.err.println("target border\n"+area.border);
    
    val toPlan : agentAbstract=new bv_followTrail(area.border) (x => x)
    
    val futur=bv.forsee_withSquarers(area.border^area,toPlan)
    //val futur=ref   
    
    if((futur.tr.pos0 & area) ==area)
      Console.err.println("future success !\n"+futur);
    else{
      Console.err.println("future success !\n"+futur);
      Console.err.println("future failure !\n"+(futur.tr.pos0 & area));
    }
    
    currPlan=new bv_followTrail(area.border) (x => x)
    
    currPlan.genMove(ref)
  }
  
  
  
    var logMove : log = new log
    override def backMove(){
      //System.err.println("backing "+logMove);
      logMove.undo()
    }
    
      def genMove (ref : GameState4P) ={
        logMove.blockControl{
          
          if(currPlan==null){
           doPlan(ref )
          }else{
            val m = currPlan.genMove(ref)
            if(m!=4) m else{
              currPlan=null
              doPlan(ref)
              
            }
          }
          
        }
      
      }
  
}