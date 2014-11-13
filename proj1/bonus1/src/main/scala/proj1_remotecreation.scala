package bonus
import akka.actor.Actor
import akka.actor._;
import akka.actor.{Actor, PoisonPill};
import scala.math._;
//import scala.util.Random;
//import scala.concurrent.duration._;
import com.typesafe.config.ConfigFactory;
import akka.actor.{ ActorRef, Props, Actor, ActorSystem };
import akka.kernel.Bootable;
import akka.actor.ReceiveTimeout; 
import akka.actor.{ Props, Deploy, Address, AddressFromURIString }
import akka.remote.RemoteScope

class CreatApplication(host: String, port: Int, system: ActorSystem) extends Bootable{
  val address = Address("akka.tcp", "SubBossListen", host, port)
  println("address finished")

// val system = ActorSystem("CreatApplication", ConfigFactory.load.getConfig("remotecreation"))
// val system = ActorSystem("sys") 
  println("system finished")
  val remoteActor = system.actorOf(Props[SubSysBoss].withDeploy(Deploy(scope = RemoteScope(address))))
  val localActor = system.actorOf(Props(classOf[CreationActor]))
  
  
  def doSomething(start: Long, end: Long, len: Long, endConstant: Long): Unit = 
      localActor ! (start,end,len,endConstant,remoteActor)
  def startup(){
  }
  def shutdown(){
    system.shutdown()
  }
}



class CreationActor() extends Actor{
  var pport: Int = 0
  var count: Long = 0l
  def receive = { 
      case (start: Long, end: Long, len: Long, endConstant: Long, remoteActor: ActorRef) => 
//      pport = port
      println("sending msg to remoteActor")
      remoteActor ! (start: Long, end: Long, len: Long, endConstant: Long, self) 
      case (cur: Long) =>
//      println(pport+" cur: "+cur)
        println("cur: "+cur)
    }

  }

class SuperBoss(range: Long, len: Long, subSysNum: Long, system: ActorSystem) {
      val hosts = new Array[String](10)
      val ports = new Array[Int](10)
      var ipcount: Int = 0
      hosts(0) = "127.0.0.1"   ;     ports(0) = 2553 ; 
      hosts(1) = "127.0.0.1"   ;     ports(1) = 2552 ; 
      hosts(2) = "127.0.0.1"   ;     ports(2) = 2555 ; 
      hosts(3) = "127.0.0.1"   ;     ports(3) = 2556 ; 
      hosts(4) = "127.0.0.1"   ;     ports(4) = 2557 ; 

      if(range < subSysNum)
             println("error:range is smaller than subSysNum") 
        var subRangeSize: Long = range/subSysNum
        var start: Long = 1l
        var end: Long = subRangeSize
        var endConstant: Long = 0l
        for(cons <-1l to (len/2l))
           endConstant += 2l*cons*cons
         endConstant -= (len/2l)*(len/2l) 
          while(start<=range){
             println(hosts(ipcount)+": "+ports(ipcount)) 
             val app = new CreatApplication(hosts(ipcount), ports(ipcount), system)
             println("CreatApplication created")
             app.doSomething(start,end,len,endConstant)
             ipcount+=1
              
             if(end + subRangeSize<=range){
                start+=subRangeSize
                end+=subRangeSize
             }
             else{
              start+=subRangeSize
              end=range
             }
           }
  }
object proj1_remotecreation{
  def main(args: Array[String]) : Unit = {
  val system = ActorSystem("CreatApplication", ConfigFactory.load.getConfig("remotecreation"))
      var superBoss = new SuperBoss(40L,24L,2l,system)
    }
}
