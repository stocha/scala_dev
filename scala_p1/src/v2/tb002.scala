package v2

/**
 * @author Jahan
 */
class tb002 extends agentAbstract{
  
  
   var currPlan : agentAbstract=null
  
  def doPlan(ref : GameState4P) ={
  
    val bv = new BotVocabulary(ref)
    
    val area=   bv.simpleSquareRuleZone 
    //Console.err.println("target area\n"+area);
    
    val consolBorder=bv.border(area)
        //Console.err.println("target border\n"+consolBorder);
    
    val toPlan : agentAbstract=new bv_followTrail(consolBorder) (x => x)
    
    val futur=bv.forsee_withZerger(consolBorder,toPlan)
    //val futur=ref   
    
    val success : Boolean = !area.isNull && ((futur.tr.pos0 & area) ==area)  
    
    
    if(success){
      currPlan=new bv_followTrail(consolBorder) (x => x)    
      val move=currPlan.genMove(ref)       
      move         
    }
    else
    bv.goToElseGo(if(success) consolBorder else BMap.zero){
      
      val specialVoid=ref.tr.void|ref.tr.pos0
      //Console.err.println("specialVoid\n"+specialVoid)
      val targNoBorder= bv.border( ref.tr.void) & ~BMap.border
      val targ=if(targNoBorder.isNull) (BMap.border & ref.tr.void) else targNoBorder
      //Console.err.println("frontier\n"+targ)
      val resp=bv.goTo(targ)
      
      if(resp.size>0) resp(0) else {
        //Console.err.println("Nowhere to go !\n");
        val lastChance = bv.goTo(ref.tr.void)
        
        if(lastChance.size>0){
          lastChance(0)
        }else
          4
        
      }
      
    }

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