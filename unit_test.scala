/**
  * Created by wowgs on 26.12.2016.
  */

package Blur_filter

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import mama.Image._
import mama._

@RunWith(classOf[JUnitRunner])
class unit_test extends FunSuite {
  test("Color functions tests") {
    val testColor: RGBA = 0x0a0b0c0d
    assert(red(testColor) == 0x0a, "red test")
    assert(green(testColor) == 0x0b, "green test")
    assert(blue(testColor) == 0x0c, "blue test")
    assert(alpha(testColor) == 0x0d, "alpha test")
  }

  test("Colors one pixel") {
    val r = 0xfa
    val g = 0xfb
    val b = 0xfc
    val a = 0xfd
    assert(rgba(r, g, b, a) == 0xfafbfcfd)
  }
  def eq(I:Image, I1:Image): Boolean =
    (for {
      x <- 0 until 20
      y <- 0 until 20
    } yield I1.ar(x)(y) == I.ar(x)(y)).forall(identity)

  test("Blur test") {
    val src = new Image(new Array[Array[RGBA]](20), 20, 20)
    for (i <- 0 until 20)
      src.ar(i) = new Array(20)
    for {
      x <- 0 until 20
      y <- 0 until 20
    } src.ar(x)(y) = 0x00ff00ff

    val dst = src.copy()

    Blur.vert(dst, src, 0, 8)
    assert(eq(src, dst), "zero rad")

    Blur.gor(dst, src, 3, 8)
    assert(eq(src, dst), "nonzero rad")
  }
}
