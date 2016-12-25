/**
  * Created by wowgs on 25.12.2016.
  */
package Barbers
import scala.collection.mutable
import scala.io.Source

class CheckLog(clients : Int, barbers : Int, sofa_size : Int) {

  var dict = mutable.Map[Long, Long]()
  var now_at_sofa = 0

  def getInfo() : List[String] = {
    Source.fromFile("log.txt").getLines().toList
  }

  def parseStr(str : String) : Unit = {
    val info = str.split(' ')
    val actor = info(0)
    val id = info(1).toLong
    
    def client_to_do() : Unit = {
      if (info(2) == "at")
        now_at_sofa += 1
    }
    
    def other_to_do() : Unit = {
      println("Unknown log")
      System.exit(0)
    }
    
    def barber_to_do() : Unit = {
      if (info(2) == "take") {
        if (dict.contains(id)) {
          if (dict.apply(id) == -1) {
            dict(id) = info(3).toLong
            now_at_sofa -= 1
          }
          else {
            println("Double clients")
            System.exit(0)
          }
        }
        else {
          dict update(id, info(3).toLong)
          now_at_sofa -= 1
        }
      }
      else {
        if (dict.apply(id) != -1) {
          dict(id) = -1
        }
        else {
          println("Free barber done work")
          System.exit(0)
        }
      }
    }

    actor match {
      case "Client" => client_to_do()
      case "Barber"  => barber_to_do()
      case _ => other_to_do()
    }

    if (now_at_sofa > sofa_size) {
      println("Sofa overflow")
      System.exit(0)
    }

    if (now_at_sofa < 0) {
      println("Taking from empty sofa")
      System.exit(0)
    }

  }

  def main(): Unit = {
    println("Errors in log:")
    val g = getInfo()
    for (x <- g) parseStr(x)
  }

}

/* object gege extends App {
  new CheckLog(30, 2, 5).main()
} */
