/**
  * Created by wowgs on 24.12.2016.
  */
package parentheses

import pref._
import org.scalameter.{Key, Warmer, config}
import pars._

object parsrun {
  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15,
    Key.exec.benchRuns -> 5
  ) withWarmer new Warmer.Default

  def printTime(prefix: String, time: Double): Unit =
    println(f"$prefix%12s time: $time%15.7f ms")

  def randsk(n : Int) : Array[Char] = {
    val tmp = new Array[Char](n)
    for (i <- 0 until n) tmp(i) = {
      if (scala.util.Random.nextBoolean()) '(' else ')'
    }
    tmp
  }

  def timeall(): Unit = {
    val num_of_ma = 5
    val strLen = (1 << 19) + 29

    val par = randsk(strLen)

    val solotime = standardConfig measure {
      solopar(par)
    }
    printTime("sequential", solotime.value)

    for (maPow <- 0 to num_of_ma; ma = 1 << maPow) {
      val partime = standardConfig measure {
        parpar(par, ma)
      }

      printTime(f"parallel $ma%2d", partime.value)
    }

  }

  def main(args: Array[String]): Unit = {
    timeall()
  }
}

/*
  sequential time:       2,7701554 ms
 parallel  1 time:      17,0048692 ms
 parallel  2 time:      16,8719360 ms
 parallel  4 time:      12,5444408 ms
 parallel  8 time:      12,8569434 ms
 parallel 16 time:      13,0815910 ms
 parallel 32 time:      13,1693642 ms
 */

object pars {

  def xor(p: (Int, Int), q: (Int, Int)): (Int, Int) = {
    val m = math.min(p._1, q._2)
    (p._1 + q._1 - m, p._2 + q._2 - m)
  }
  def fun(x : Char) : (Int, Int) = {
    if (x == '(') (1, 0)
    else (0, 1)
  }

  def solopar(pars: Array[Char]): Boolean = {
    var tmp = 0
    var bool = true
    for (c <- pars) {
      if (c == '(') tmp += 1
      else tmp -= 1
      if (tmp < 0) bool = false
    }

    (tmp == 0) && bool
  }

  def parpar(pars: Array[Char], ma: Int): Boolean = {
    prefix_scan.collect_only(pars.map(fun).toArray, xor, ma).last == (0, 0)
  }
}