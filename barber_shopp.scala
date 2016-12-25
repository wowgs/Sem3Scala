/**
  * Created by wowgs on 25.12.2016.
  */

package Barbers
import java.io.{File, PrintWriter}

class My_log(file_name: String) {
  private val file = new PrintWriter(new File(file_name))

  def close(): Unit = {
    file.close()
  }

  def write(msg: String): Unit = {
    file.write(msg)
    file.write("\n")
  }
}


class Barber(id : Int, divan : Sofa) extends Thread{
  var my_client_id : Long = -2

  override def run() = {
    while(divan.already_done <= divan.num_of_clients) {
      while (!take_client()) {}
      Thread.sleep(1000)
      work()
      if (divan.already_done == divan.num_of_clients) {
        divan.log.close()
        System.exit(0)
      }
    }
  }

  def work() : Unit = divan.lock.synchronized {
    divan.already_done += 1
    divan.log.write(s"Barber $id done $my_client_id")
  }

  def take_client() : Boolean = divan.lock.synchronized {
    if (divan.now_on_sofa > 0) {
      my_client_id = divan.give_client()
      divan.log.write(s"Barber $id take $my_client_id")
      true
    }
    else false
  }
}


class Client(id : Int, divan : Sofa) extends Thread{
  override def run() = divan.lock.synchronized {
    if (divan.now_on_sofa < divan.size_of_sofa) {
      divan.sit_client(id)
      divan.log.write(s"Client $id at sofa")
    }
    else {
      divan.log.write(s"Client $id go home")
      divan.already_done += 1
    }
  }
}

class Sofa(Clients : Int, barbers : Int, newsize_of_sofa : Int) {
  val r = scala.util.Random
  var lock : AnyRef = new Object()
  val log = new My_log("log.txt")
  
  val num_of_clients = Clients
  val num_of_barbers = barbers
  val size_of_sofa = newsize_of_sofa
  var already_done = 0

  var ar = Array.fill[Long](size_of_sofa)(-1)
  var head = 0
  var tail = 0
  var now_on_sofa = 0

  def sit_client(x : Long) : Unit = lock.synchronized {
    ar(head) = x
    now_on_sofa += 1
    head = (head + 1) % size_of_sofa
  }

  def give_client() : Long = lock.synchronized {
    val res = ar(tail)
    now_on_sofa -= 1
    tail = (tail + 1) % size_of_sofa
    res
  }

  def main() = {
    for(i <- 0 until barbers) new Barber(i, this).start()

    for (i <- 0 until Clients) {
      new Client(i, this).start()
      Thread.sleep(r.nextInt(300))
    }
  }
}

object GoGo {
  def main(args: Array[String]): Unit = {
    println("Enter Clients number")
    val Clients = scala.io.StdIn.readInt()
    println("Enter barbers number")
    val barbers = scala.io.StdIn.readInt()
    println("Enter sofa size")
    val size = scala.io.StdIn.readInt()

    new Sofa(Clients, barbers, size).main()

    new CheckLog(Clients, barbers, size).main()
  }
}