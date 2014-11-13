import java.util.Random
import akka.actor.Actor; 
import akka.actor._; 
import akka.actor.{ ActorRef, Props, Actor, ActorSystem}; 
import scala.math._ 


abstract class Node extends Actor{
  var id: Int = -1
  var neighbors: Array[Int] = Array.empty
  var count: Int = 0 
  def init(iid: Int, neighborlist: Array[Int]) = { 
    id = iid
    neighbors = neighborlist
  } 

} 



class GossipNode extends Node{ 
  var countmax: Int = 10 //arbitrary 10 
   def receive() =  {
     case(iid:Int, neighborlist: Array[Int]) => {
       init(iid,neighborlist)
     }

     case("Rumor") => {
       count += 1
  val rand = new Random();
  val random_index = rand.nextInt(neighbors.length)//-1?? 
  val random_neighbor = neighbors(random_index)
  context.parent ! ("Rumor",random_neighbor)
  if(count==10){
   context.parent ! ("Stop",0.0) 
 } 
     }
  
  }
} 



class PushSumNode extends Node{
 var s: Double = 0 
 var w: Double = 1
 var value: Double = 0
 def receive = {
   case(iid: Int, neighbors: Array[Int]) => {
       s = iid
       value = iid
       init(iid,neighbors) 
     } 

     case ("Start") => {
       s = s/2.0
       w = w/2.0
       value = s/w
       val rand = new Random();
       val random_index = rand.nextInt(neighbors.length)//-1?? 
       val random_neighbor = neighbors(random_index) 
       context.parent ! (w,s,random_neighbor) 
     }

     case(senderw: Double, senders: Double) => {
       w += senderw
       s += senders
       if(s/w == value){ 
         count += 1 
         if(count==3){
           //println("exit id: " + id +  " value: " + value)
           context.parent ! ("Stop",value)
        //   context.system.shutdown()
         
         }
        //   context.parent ! ("Exit", id)  
      }

       s = s/2.0 
       w = w/2.0
       value = s/w
       val rand = new Random()
       val random_index = rand.nextInt(neighbors.length)//-1?? 
       val random_neighbor = neighbors(random_index) 
       //println("id: " + id + " neighbor: " + random_neighbor + " value: " + value) 
       context.parent ! (w,s,random_neighbor) 
     } 

     case("Exit", deleteneighbor: Int) => {
       neighbors filter(_ != deleteneighbor) 
     }

   } 
 }

abstract class NetworkBuilder extends Actor{
 
  def neighbor(x: Int):Array[Int]
  var count = 0
  var nodenum: Int = 0
  var nodes: Array[ActorRef] = Array.empty  
  var gossipexitcount: Int = 0  
  var timestart: Long = 0
  def receive = {
    case("Exit", id:Int) => {
        println("Exit: " + id)
        nodes.foreach( e => e ! ("Exit", id) )
      }

    //pushsum
    case(senderw:Double, senders: Double, randomneighbor_id: Int) => { 
      nodes(randomneighbor_id) ! (senderw, senders)
    }
    
    // gossip&&pushsum
    case (act_nodenum: Int, algorithm: String) => {
      nodenum = act_nodenum
      nodes = new Array[ActorRef](nodenum)
      for(index <- 0 to nodenum-1){
        if(algorithm=="gossip")
      nodes(index) =  context.actorOf(Props[GossipNode])
        else if(algorithm=="push-sum") 
      nodes(index) =  context.actorOf(Props[PushSumNode])
      nodes(index) ! (index,neighbor(index))
      } 
      val rand = new Random()
      val random_index = rand.nextInt(nodenum-1)
      var random_seed = nodes(random_index)
      if(algorithm == "push-sum"){
        timestart = System.currentTimeMillis
        random_seed ! ("Start")
      }
      else if(algorithm == "gossip"){
        timestart = System.currentTimeMillis
        random_seed ! ("Rumor") 
      } 
      
      self ! ("Fail") 

      }
    
      case("Fail") => { 
      count+=1 
      if(count==nodenum/10){ 
        val rand = new Random()
        val random_index = rand.nextInt(nodenum-1)
        var random_seed = nodes(random_index)
        random_seed ! ("Exit") 
      } 
      self ! ("Fail") 
    } 



    case("Rumor", randomneighbor_id: Int) => {
      nodes(randomneighbor_id) ! ("Rumor") 
    } 
    
    case("Stop",value: Double) => {
      println("stop")
      println("converge value: " + value) 
      println(System.currentTimeMillis-timestart)
      context.system.shutdown
    } 

    case("Try another", id: Int, senderid: Int) => {
      gossipexitcount+=1
      if(gossipexitcount == nodenum-1){ 
        context.system.shutdown
      } 
      nodes(id) ! ("Try another", senderid) 
    }
    
  }
} 



class Full extends NetworkBuilder{ 
  def neighbor(x:Int):Array[Int] = {
   var list: List[Int] = Nil
   for( index <- 0 to nodenum-1){
     if(index != x)
     list = list ::: List(index)
     //list.filter(element => element==x)
   }
   list.toArray 
 } 
}



class Grid extends NetworkBuilder{
  def neighbor(x: Int) : Array[Int] = {
    var list: List[Int] = Nil
    var edge = round(math.sqrt(nodenum)).toInt
    if(((x-1)/edge == x/edge)&&(x-1 >= 0)) list = list ::: List(x - 1) 
    if(((x+1)/edge == x/edge)&&(x+1 < nodenum) ) list = list ::: List(x + 1) 
    if(x-edge >= 0) list = list ::: List(x - edge) 
    if(x+edge <= nodenum-1) list = list ::: List(x + edge)
    list.toArray 
  } 
}

class ImpGrid extends NetworkBuilder{
  def hasElement(element: Int, list: List[Int]) = {
    list exists {x => (x==element) }
  }

  def neighbor(x: Int) : Array[Int] = {
    var list : List[Int] = Nil
    var edge = round(math.sqrt(nodenum)).toInt
    if(((x-1)/edge == x/edge)&&(x-1 >= 0)) list = list ::: List(x - 1) 
    if(((x+1)/edge == x/edge)&&(x+1 < nodenum) ) list = list ::: List(x + 1) 
    if(x-edge >= 0) list = list ::: List(x - edge) 
    if(x+edge <= nodenum-1) list = list ::: List(x + edge)
    var flag: Boolean = true
    var random_index: Int = 0
    while (flag) {
      var rand = new Random()
      random_index = rand.nextInt(nodenum-1)
      flag = hasElement(random_index, list) 
    } 
    list = list ::: List(random_index) 
    list.toArray 
  } 
}
    


class Line extends NetworkBuilder{
  def neighbor(x: Int) :Array[Int] = {
    var list: List[Int] = Nil
    if(x-1>=0) list = list ::: List(x - 1)
    if(x+1<nodenum) list = list ::: List(x + 1)
    list.toArray 
  }
} 




object project2bonus{
  def main(args: Array[String]): Unit = {
  val system = ActorSystem("project2")
  var network = system.actorOf(Props[ImpGrid])
  network ! (2000,"push-sum")/*
  if(args(1)=="full"){
  var network = system.actorOf(Props[Full])
  network ! (args(0).toInt,args(2))
}
  if(args(1)=="2D"){
  var network = system.actorOf(Props[Grid])
  network ! (args(0).toInt,args(2))
}
  if(args(1)=="imp2D"){
  var network = system.actorOf(Props[ImpGrid])
  network ! (args(0).toInt,args(2))
}
  if(args(1)=="line"){
  var network = system.actorOf(Props[Line])
  network ! (args(0).toInt,args(2))
}*/
  }
}
