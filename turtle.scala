/**
  * Created by wowgs on 24.12.2016.
  */
import pref._

import math.{Pi, sqrt}
import org.scalameter.{Key, Warmer, config}
import Turtle._
import scala.util.Random

object turtle_run {
  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15,
    Key.exec.benchRuns -> 10
  ) withWarmer new Warmer.Default

  def printTime(prefix: String, time: Double): Unit =
    println(f"$prefix%12s time: $time%15.7f ms")

  def timeall(): Unit = {
    val num_of_ma = 5

    val n = 1 << 18

    val r = new Random
    val moves = new Array[move_Type](n)

    for (i <- 0 until n)
      moves(i) = (r.nextDouble() % 360, r.nextDouble())

    val solotime = standardConfig measure {
      soloTurtle(moves)
    }
    printTime("sequential", solotime.value)

    for (maPow <- 0 to num_of_ma; ma = 1 << maPow) {
      val partime = standardConfig measure {
        parTurtle(moves, ma)
      }

      printTime(f"parallel $ma%2d", partime.value)
    }

  }

  def main(args: Array[String]): Unit = {
    timeall()
  }
}

/*
  sequential time:      30,4722845 ms
 parallel  1 time:      61,4025665 ms
 parallel  2 time:      38,6251857 ms
 parallel  4 time:      23,7331575 ms
 parallel  8 time:      18,0225138 ms
 parallel 16 time:      17,4856926 ms
 parallel 32 time:      20,1078897 ms
 */

object Turtle {
  type my_Type = (Double, Double, Double)
  type move_Type = (Double, Double)

  def res_pos(move: my_Type): move_Type = (move._1 * cos(move._2), move._1 * sin(move._2))

  def xor(p: my_Type, q: my_Type): my_Type = {
    val (a, ang_1, alpha) = p
    val (b, ang_2,  beta)  = q
    val tmp = sqrt(a * a + b * b + 2 * a * b * cos(ang_2 + alpha - ang_1))

    if (tmp == 0) {
      (tmp, ang_1, (alpha + beta) % 360)
    }
    else {
      (tmp, (ang_1 + asin(b * sin(ang_2 + alpha - ang_1) / tmp)) % 360, (alpha + beta) % 360)
    }
  }

  def map_fun(x : move_Type) : my_Type = {
    (x._2, x._1, x._1)
  }

  def asin(x: Double): Double =
    180 * Math.asin(x) / Pi

  def cos(x: Double): Double =
    Math.cos(x * Pi / 180)

  def sin(x: Double): Double =
    Math.sin(x * Pi / 180)

  def soloTurtle(moves: Array[move_Type]): move_Type = {
    var (x, y) = (0.0, 0.0)
    var directionAngle = 0.0 // east

    for ((angle, move) <- moves) {
      directionAngle = (directionAngle + angle) % 360
      x += move * cos(directionAngle)
      y += move * sin(directionAngle)
    }

    (x, y)
  }

  def parTurtle(moves: Array[move_Type], ma: Int): move_Type = {
    res_pos(prefix_scan.collect_only(moves.map(map_fun), xor, ma).last)
  }
}
