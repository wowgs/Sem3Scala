/**
  * Created by wowgs on 25.12.2016.
  */
package Blur_filter

import java.awt.{Color, Dimension, Graphics2D}
import java.io.FileInputStream
import scala.swing.{Image => _, _}
import java.awt.image._
import javax.imageio._
import org.scalameter._
import java.io._
import mama._
object mama {

  type RGBA = Int

  class Image(var ar: Array[Array[RGBA]], private val width: Int, private val height: Int) {
    def get_width(): Int = width

    def get_height(): Int = height

    def copy(): Image = {
      val tmp = new Array[Array[RGBA]](this.width)
      for (i <- 0 until this.width) {
        tmp(i) = new Array[Int](this.height)
      }
      for (i <- 0 until this.width) {
        for (j <- 0 until this.height) {
          tmp(i)(j) = this.ar(i)(j)
        }
      }
      new Image(tmp, this.width, this.height)
    }

    def toARGB(): Unit = {
      for (i <- 0 until this.width) {
        for (j <- 0 until this.height) {
          this.ar(i)(j) = Image.RGBA2ARGB(this.ar(i)(j))
        }
      }
    }
  }

  object Image {
    def apply(s: String): Image = {
      val stream = new FileInputStream(s)
      val tmp = ImageIO.read(stream)
      val height = tmp.getHeight
      val width = tmp.getWidth
      val ar = new Array[Array[RGBA]](width)

      for (i <- 0 until width)
        ar(i) = new Array(height)

      for {
        x <- 0 until width
        y <- 0 until height
      } ar(x)(y) = ARGB2RGBA(tmp.getRGB(x, y))
      val img: Image = new Image(ar, width, height)
      img
    }


    def ARGB2RGBA(pix: Int): RGBA = {
      red(pix) | ((pix & 0x00ffffff) << 8)
    }

    def RGBA2ARGB(pix: RGBA): Int = {
      (alpha(pix) << 24) | ((pix & 0xffffff00) >>> 8)
    }

    def red(c: RGBA): Int = (0xff000000 & c) >>> 24

    def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

    def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

    def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

    def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
      (r << 24) | (g << 16) | (b << 8) | (a << 0)
    }
  }

  object Blur {
    def parRange(num: Int, ma: Int): Array[(Int, Int)] = {
      val div = num / ma
      val mod = num % ma
      val ar = new Array[(Int, Int)](ma)
      var left = 0
      var right = div
      for (i <- 0 until (ma - mod)) {
        ar(i) = (left, right)
        left += div
        right += div
      }
      right += 1
      for (i <- (ma - mod) until ma) {
        ar(i) = (left, right)
        left += (div + 1)
        right += (div + 1)
      }

      ar
    }

    def apply(dst: Image, src: Image, kak: String, rad: Int, ma: Int): Unit = {
      kak match {
        case "v" => gor(dst, src, rad, ma)
        case "h" => vert(dst, src, rad, ma)
        case _ => println("Wrong type g or v")
      }
    }

    def average_pix(src: Image, x: Int, y: Int, rad: Int): RGBA = {
      var num = 0
      var r = 0
      var g = 0
      var b = 0
      var a = 0
      for (i <- (x - rad) until (x + rad + 1)) {
        for (j <- (y - rad) until (y + rad + 1)) {
          if (i >= 0 && i < src.get_width() && j >= 0 && j < src.get_height()) {
            num += 1
            r += Image.red(src.ar(i)(j))
            g += Image.green(src.ar(i)(j))
            b += Image.blue(src.ar(i)(j))
            a += Image.alpha(src.ar(i)(j))
          }
        }
      }
      Image.rgba(r / num, g / num, b / num, a / num)
    }

    class Vert2(dst: Image, src: Image, rad: Int, left: Int, right: Int) {

      val me = new Thread {
        override def run(): Unit = {
          for (i <- left until right) {
            for (j <- 0 until dst.get_width()) {
              dst.ar(j)(i) = average_pix(src, j, i, rad)
            }
          }
        }
      }
    }

    class Gor2(dst: Image, src: Image, rad: Int, left: Int, right: Int) {
      val me = new Thread {
        override def run(): Unit = {
          for (i <- left until right) {
            for (j <- 0 until dst.get_height()) {
              dst.ar(i)(j) = average_pix(src, i, j, rad)
            }
          }
        }
      }
    }


    def vert(dst: Image, src: Image, rad: Int, ma: Int): Unit = {
      var left = 0
      var right = src.get_height() / ma
      val ar = parRange(src.get_height(), ma)
      val mas = new Array[Vert2] (ma - 1)
      for (i <- 1 until ar.length) {
        mas(i - 1) = new Vert2(dst, src, rad, ar(i)._1, ar(i)._2)
        mas(i - 1).me.start()
      }
      for (i <- left until right) {
        for (j <- 0 until dst.get_width()) {
          dst.ar(j)(i) = average_pix(src, j, i, rad)
        }
      }
      for (i <- 0 until ma - 1) mas(i).me.join()
    }

    def gor(dst: Image, src: Image, rad: Int, ma: Int): Unit = {
      var left = 0
      var right = src.get_width() / ma
      val mas = new Array[Gor2] (ma - 1)
      val ar = parRange(src.get_width(), ma)
      for (i <- 1 until ar.length) {
        mas(i - 1) = new Gor2(dst, src, rad, ar(i)._1, ar(i)._2)
        mas(i - 1).me.start()
      }
      for (i <- left until right) {
        for (j <- 0 until dst.get_height()) {
          dst.ar(i)(j) = average_pix(src, i, j, rad)
        }
      }
      for (i <- 0 until ma - 1) mas(i).me.join()
    }
  }

  class go_go(dst: Image, src: Image) {
    dst.toARGB()
    src.toARGB()

    object render extends SimpleSwingApplication {

      def top = new MainFrame {
        contents = new BoxPanel(Orientation.Vertical) {
          contents += new DataPanel(dst, Color.WHITE) {
            preferredSize = new Dimension(dst.get_width(), dst.get_height())
          }
          contents += new DataPanel(src, Color.WHITE) {
            preferredSize = new Dimension(src.get_width(), src.get_height())
          }
        }
      }


      class DataPanel(img: Image, BGColor: Color) extends Panel {
        override def paintComponent(gcan: Graphics2D) = {
          val bufferedImage = new BufferedImage(img.get_width(), img.get_height(), BufferedImage.TYPE_INT_ARGB)
          for (x <- 0 until img.get_width(); y <- 0 until img.get_height()) bufferedImage.setRGB(x, y, img.ar(x)(y))

          gcan.drawImage(bufferedImage, 0, 0, null)
        }
      }

    }

  }

  def give_info() : (Int, Int, String) = {
    println("Choose number of threads")
    val numOfThreads = scala.io.StdIn.readInt()
    println("Choose radius of Blur")
    val rad = scala.io.StdIn.readInt()
    println("Choose h or v")
    val blurType = scala.io.StdIn.readLine()
    (numOfThreads, rad, blurType)
  }

  def start(ma : Int, kak : String) = {
    val img1 = Image("pic1.bmp")
    val img2 = img1.copy()
    Blur(img2, img1, kak, 3, ma)
    //new go_go(img1, img2).render.startup(new Array[String](1))
  }
}
/*
Radius = 3

Parallel - 1, type - h : 88.43394286666667
Parallel - 2, type - h : 50.51476163333334
Parallel - 4, type - h : 34.59178696666667
Parallel - 8, type - h : 26.669526800000007
Parallel - 16, type - h : 26.3448722
Parallel - 32, type - h : 26.693789666666657
Parallel - 64, type - h : 27.915568500000006

Parallel - 1, type - v : 76.68623576666666
Parallel - 2, type - v : 45.39517153333331
Parallel - 4, type - v : 30.973580366666667
Parallel - 8, type - v : 24.940759133333337
Parallel - 16, type - v : 24.8934649
Parallel - 32, type - v : 25.81237543333334
Parallel - 64, type - v : 26.565838099999993
 */
object BlurRunner {
  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 15,
    Key.exec.maxWarmupRuns -> 20,
    Key.exec.benchRuns -> 30
  ) withWarmer new Warmer.Default


  def timeall(): Unit = {
    val ma = 6

    for (kak <- Array("h", "v")) {
      for (numTasksPow <- 0 to ma; numTasks = 1 << numTasksPow) {
        val partime = standardConfig measure {
          start(numTasks, kak)
        }
        println("Parallel - " + numTasks.toString + ", type - " + kak + " : " + partime.value.toString)
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    timeall()
  }
}