/**
  * Created by wowgs on 24.12.2016.
  */
import pref._
import org.scalameter.{Key, Warmer, config}
import Lines._
object linesrun {
  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15,
    Key.exec.benchRuns -> 20
  ) withWarmer new Warmer.Default

  def printTime(prefix: String, time: Double): Unit =
    println(f"$prefix%12s time: $time%15.7f ms")

  def timeall(): Unit = {
    val num_of_ma = 5
    val n = 1 << 20
    val a = new Array[Double](n)
    val b = new Array[Double](n)
    a(0) = 0
    b(0) = 13
    for (i <- 1 until n) {
      a(i) = 1
      b(i) = 1
    }
    val ab = a zip b

    val solotime = standardConfig measure {
      sololines(ab, n)
    }
    printTime("sequential", solotime.value)

    for (maPow <- 0 to num_of_ma; ma = 1 << maPow) {
      val partime = standardConfig measure {
        parlines(ab, n, ma)
      }

      printTime(f"parallel $ma%2d", partime.value)
    }


  }

  def main(args: Array[String]): Unit = {
    timeall()
  }
}

/*sequential time:       7,3248843 ms
  parallel  1 time:      23,5628249 ms
  parallel  2 time:      14,7816545 ms
  parallel  4 time:      13,9993512 ms
  parallel  8 time:      13,7545263 ms
  parallel 16 time:      13,9069594 ms
  parallel 32 time:      13,2135174 ms */

object Lines {

  type My = (Double, Double)

  def xor(a: My, b: My): My = (a._1 * b._1, b._1 * a._2 + b._2)

  def sololines(ab: Array[My], n: Int): Double = {
    var x = ab(0)._2
    for (i <- 1 until n) {
      x = ab(i)._1 * x + ab(i)._2
    }
    x
  }

  def parlines(ab: Array[My], n: Int, ma: Int): Double = {
    prefix_scan.collect_only(ab, xor, ma).last._2
  }
}

