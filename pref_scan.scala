/**
  * Created by wowgs on 23.12.2016.
  */
package pref

import scala.math
import scala.reflect.ClassTag
object prefix_scan {
  def apply[A : ClassTag](ar : Array[A], func : (A, A) => A, ma : Int, neut : A) : Array[A] = {
    val tmp = new Array[A](ar.length)
    ar.copyToArray(tmp)
    collect_phase(tmp, func, 0, ar.length - 1, 0, ma)
    tmp(tmp.length - 1) = neut
    distribute_phase(tmp, func, 0, ar.length - 1, 0, ma)
    rek_sum(tmp, ar, func, 0, ar.length - 1, 0, ma)
    tmp
  }

  private def rek_sum[A](ar : Array[A], ar2 : Array[A], func : (A, A) => A, left : Int, right : Int, h : Int, ma : Int) : Unit = {
    if (right > left) {
      val mid = (left + right) / 2
      if ((1 << (h + 1)) <= ma) {
        val tr = new Thread {
          override def run(): Unit = rek_sum(ar, ar2, func, left, mid, h + 1, ma)
        }
        tr.start()
        rek_sum(ar, ar2, func, mid + 1, right, h + 1, ma)
        tr.join()
      }
      else {
        rek_sum(ar, ar2, func, left, mid, h + 1, ma)
        rek_sum(ar, ar2, func, mid + 1, right, h + 1, ma)
      }
    }
    else {ar(left) = func(ar(left), ar2(left))}
  }
  //Медленнее, чем copyToArray
  /* private def rek_copy[A](ar : Array[A], ar2 : Array[A], left : Int, right : Int, h : Int, ma : Int) : Unit = {
    if (right > left) {
      val mid = (left + right) / 2
      if ((1 << (h + 1)) <= ma) {
        val tr = new Thread {
          override def run(): Unit = rek_copy(ar, ar2, left, mid, h + 1, ma)
        }
        tr.start()
        rek_copy(ar, ar2, mid + 1, right, h + 1, ma)
        tr.join()
      }
      else {
        rek_copy(ar, ar2, left, mid, h + 1, ma)
        rek_copy(ar, ar2, mid + 1, right, h + 1, ma)
      }
    }
    else {ar(left) = ar2(left)}
  }  */

  def collect_only[A : ClassTag](ar : Array[A], func : (A, A) => A, ma : Int) : Array[A] = {
    val tmp = new Array[A](ar.length)
    ar.copyToArray(tmp)
    collect_phase(tmp, func, 0, ar.length - 1, 0, ma)
    tmp
  }

  private def collect_phase[A](ar : Array[A], func : (A, A) => A, left : Int, right : Int, h : Int, ma : Int) : Unit = {
    if (right > left) {
      val mid = (left + right) / 2
      if ((1 << (h + 1)) <= ma) {
        val tr = new Thread {
          override def run(): Unit = collect_phase(ar, func, left, mid, h + 1, ma)
        }
        tr.start()
        collect_phase(ar, func, mid + 1, right, h + 1, ma)
        tr.join()
        ar(right) = func(ar(mid), ar(right))
      }
      else {
        collect_phase(ar, func, left, mid, h + 1, ma)
        collect_phase(ar, func, mid + 1, right, h + 1, ma)
        ar(right) = func(ar(mid), ar(right))
      }
    }
    else {}
  }

  private def distribute_phase[A](ar : Array[A], func : (A, A) => A, left : Int, right : Int, h : Int, ma : Int) : Unit = {
    if (right > left) {
      val mid = (left + right) / 2
      val tmp = ar(mid)
      ar(mid) = ar(right)
      ar(right) = func(tmp, ar(right))

      if ((1 << (h + 1)) <= ma) {
        val tr = new Thread {
          override def run(): Unit = distribute_phase(ar, func, left, mid, h + 1, ma)
        }
        tr.start()
        distribute_phase(ar, func, mid + 1, right, h + 1, ma)
        tr.join()
      }
      else {
        distribute_phase(ar, func, left, mid, h + 1, ma)
        distribute_phase(ar, func, mid + 1, right, h + 1, ma)
      }
    }
    else {}


  }


}
