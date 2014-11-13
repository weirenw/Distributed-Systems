package bonus

import akka.actor._
import akka.actor.{Actor, PoisonPill}
import scala.math._
import akka.pattern._
class ComputeActor extends Actor{
  def receive = {//react
    case (start:  Long, end:Long, len: Long, endConstant: Long, superboss: ActorRef) => {   
      var cur: Long = start
      var ans0 = len*(cur+(len-1l)/2l)*(cur+(len-1l)/2l)+len*(cur+(len-1l)/2l)+endConstant 
      while(cur<=end){
          var root = math.sqrt(ans0)
          var rdroot = round(root)
          if( rdroot == root){
          //  println("root: "+root)
            sender ! (cur, true, superboss)
            context.stop(self)
          }
          else{ 
            sender ! false
          }
          ans0+=2l*(cur+(len-1l)/2l+1l)*len
          cur=cur+1l
        }
      exit(0) 
      }
    } 
  }  

class SubSysBoss() extends Actor{
  var countfinish: Long = 0l
  var endConstant: Long = 0l
  var flag: Boolean = false
  var rangep: Long = 0l;
    
  
  
  def receive = {
      case (cur: Long, flag: Boolean, superboss: ActorRef) =>

         //  println("SubSysBoss root: "+root)
           superboss ! cur
           exit(0) 

      case(start: Long, end: Long, len: Long, endConstant: Long, superboss: ActorRef) => {
           println("SubSysBoss receiving")
           val range = end-start+1l 
           val subRangeSize: Long = range/4l
           var xstart: Long = start 
           var xend: Long = start+subRangeSize-1
           while(xstart<=end){
               var computeActor = context.actorOf(Props[ComputeActor]) 
               println("xstart: "+xstart+" xend: "+xend)
               computeActor ! (xstart, xend, len, endConstant, superboss)
               if(xend + subRangeSize<=end){
                 xstart+=subRangeSize
                 xend+=subRangeSize
               }
               else{
                 xstart+=subRangeSize
                 xend=end
               }
           }
      }

    }
}
    
