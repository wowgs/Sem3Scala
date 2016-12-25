/**
  * Created by wowgs on 23.12.2016.
  */
package kek

import scala.util.Random
import scala.math._
import java.lang.System.currentTimeMillis
import scala.collection.GenSeq
import kmeans._
import org.scalameter.{Key, Warmer, config}

object kmeans {

  type Point = (Double, Double)

  def rast(p1 : Point, p2 : Point) : Double = {
    sqrt(pow((p1._1 - p2._1), 2) + pow((p1._2 - p2._2), 2))
  }

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val r = new Random

    for (i <- 0 until num) yield (k * r.nextDouble, k * r.nextDouble)
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    Random.shuffle(points).take(k)
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    means.minBy((x : Point) => rast(x, p))
  }

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenSeq[(Point, GenSeq[Point])] = {
    val tmp = means.map((x : Point) => points.filter(findClosest(_,means) == x))
    means.zip(tmp)
  }

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = {
    if (points.isEmpty)
    oldMean
    else { val tmp = points.reduce((x : Point, y : Point) => (x._1 + y._1, x._2 + y._2)); (tmp._1 / points.length, tmp._2 / points.length) }
  }

  def update(classified: GenSeq[(Point, GenSeq[Point])]): GenSeq[Point] = {
    classified.map(x => findAverage(x._1, x._2))
  }

  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    !oldMeans.map(x => rast(x, newMeans.minBy(rast(x,_))) < eta).exists(!_)
  }

  @annotation.tailrec
  def rek(s : GenSeq[Point], mean : GenSeq[Point], eta : Double) : GenSeq[Point] = {
    val tmp = update(classify(s, mean))
    if (converged(eta)(mean, tmp)) tmp else rek(s, tmp, eta)
  }

}

object mama extends App {

  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def timeall(): Unit = {
    val k = 1 << 3
    val numOfPoints = 1 << 11
    val eta = 1.0 / (1 << 10)

    val points = generatePoints(100, numOfPoints)
    val startingMeans = initializeMeans(k, points)



    val partime = standardConfig measure {
      rek(points.par, startingMeans.par, eta)
    }

    val seqtime = standardConfig measure {
      rek(points, startingMeans , eta)
    }

    println(s"seq time: $seqtime")
    println(s"par time: $partime")
    println(s" speedup: ${seqtime.value / partime.value}")


    /*
    seq time: 116.32868196666668 ms
    par time: 560.5280379666667 ms
    speedup: 0.20753410014716245
     */
  }

  timeall()






}
