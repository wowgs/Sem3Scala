/**
  * Created by wowgs on 23.12.2016.
  */
import pref._
import bigints._
import org.scalameter._
object bigints{
  def xor(x : Char, y : Char) : Char = {
    y match {
      case 'M' => x
      case _ => y
    }
  }

  def rek_su(ar : Array[Int], ar2 : Array[Int], res : Array[Char], res2 : Array[Int], left : Int, right : Int, h : Int, ma : Int) : Unit = {
    if (right > left) {
      val mid = (left + right) / 2
      if ((1 << (h + 1)) <= ma) {
        val tr = new Thread {
          override def run(): Unit = rek_su(ar, ar2, res, res2, left, mid, h + 1, ma)
        }
        tr.start()
        rek_su(ar, ar2, res, res2, mid + 1, right, h + 1, ma)
        tr.join()
      }
      else {
        rek_su(ar, ar2, res, res2, left, mid, h + 1, ma)
        rek_su(ar, ar2, res, res2, mid + 1, right, h + 1, ma)
      }
    }
    else {
      res(left) = inttochar(ar(left), ar2(left))
      res2(left) = ar(left) + ar2(left)
    }
  }

  def rek_su2(ar : Array[Int], ar2 : Array[Char], res : Array[Int], left : Int, right : Int, h : Int, ma : Int) : Unit = {
    if (right > left) {
      val mid = (left + right) / 2
      if ((1 << (h + 1)) <= ma) {
        val tr = new Thread {
          override def run(): Unit = rek_su2(ar, ar2, res, left, mid, h + 1, ma)
        }
        tr.start()
        rek_su2(ar, ar2, res, mid + 1, right, h + 1, ma)
        tr.join()
      }
      else {
        rek_su2(ar, ar2, res, left, mid, h + 1, ma)
        rek_su2(ar, ar2, res, mid + 1, right, h + 1, ma)
      }
    }
    else {
      res(left) = (ar(left) + new_ch_to_int(ar2, left - 1)) % 10
    }
  }

  def inttochar(a : Int, b : Int) : Char = {
    if (a + b >= 10) 'C'
    else {
      if (a + b == 9) 'M'
      else 'N'
    }
  }

  def new_ch_to_int(ar : Array[Char], i : Int) : Int = {
    if (i == -1) 0
    else {
      if (ar(i) == 'C') 1
      else 0
    }
  }

  /*def chartoint(ar : Array[Char]) : Array[Int] = {
    var i = 0
    val tmp = new Array[Int](ar.length + 1)
    tmp(i) = 0
    for (i <- ar.indices) {
      if (ar(i) == 'C') tmp(i + 1) = 1
      else tmp(i + 1) = 0
    }
    tmp
  }*/



  def parbigints(a : Array[Int], b : Array[Int], ma : Int) : Array[Int] = {
    val ar = new Array[Char](a.length)
    val ar2 = new Array[Int](a.length)
    rek_su(a, b, ar, ar2, 0, a.length - 1, 0, ma)
    val pref = prefix_scan(ar, xor, ma, 'M')
    val otv = new Array[Int](ar.length + 1)
    rek_su2(ar2, pref, otv, 0, ar2.length - 1, 0, ma)
    otv(ar.length) = new_ch_to_int(pref, pref.length - 1)
    otv
  }

  def solobigints(a : Array[Int], b : Array[Int]) : Array[Int] = {
    val ar = new Array[Int](a.length + 1)
    var car : Int = 0
    for (i <- a.indices) {
      ar(i) = (a(i) + b(i) + car) % 10
      car = (a(i) + b(i) + car) / 10
    }
    ar(a.length) = car
    ar

  }

}

/*
      sequential time:       8,7880878 ms
      parallel  1 time:      95,0633053 ms
      parallel  2 time:      64,2468450 ms
      parallel  4 time:      39,9133762 ms
      parallel  8 time:      34,7646584 ms
      parallel 16 time:      35,1428677 ms
      parallel 32 time:      40,0589946 ms

    */

object ints_run {
  type BigInteger = Array[Int]
  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15,
    Key.exec.benchRuns -> 20
  ) withWarmer new Warmer.Default

  def printTime(prefix: String, time: Double): Unit =
    println(f"$prefix%12s time: $time%15.7f ms")

  def timeall(): Unit = {
    val num_of_ma = 5
    val n = 2000000
    val x = new BigInteger(n)
    val y = new BigInteger(n)
    for (i <- 0 until n) {
      x(i) = (1 + i) % 10
      y(i) = (1 + i) % 10
    }
    val solotime = standardConfig measure {
      solobigints(x, y)
    }
    printTime("sequential", solotime.value)

    for (maPow <- 0 to num_of_ma; ma = 1 << maPow) {
      val partime = standardConfig measure {
        parbigints(x, y, ma)
      }

      printTime(f"parallel $ma%2d", partime.value)
    }


  }

  def main(args: Array[String]): Unit = {
    timeall()
  }
}
