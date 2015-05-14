package org.kuleuven.mai.vision.image

import breeze.linalg
import breeze.linalg._
import breeze.numerics.{ceil, abs, floor}
import scala.collection.mutable.{MutableList => ML}
/**
 * @author @koldh, @mandar2812
 */
class Histogram( x: ML[Int],r:Int) {

  private val _bins = 2 * r

  def +( his1 : Histogram): Histogram =
    new Histogram(this.x ++ his1.binedge._2, r)

  def ++(hismat : ML[Histogram]): Histogram =
    new Histogram(this.x ++ hismat.map(_.binedge._2).reduce(_++_), r)

  def binedge: (DenseVector[Double], ML[Int], Int ,Int ) = {

    var a: Int = max(x)
    var b: Int = min(x)
    if (a == b) {
      a = a + 1
      b = b - 1
    }
    val edge1 = DenseVector.tabulate(_bins+1){i => a + i*(b-a)/_bins.toDouble}
    (edge1,x,b,a)
  }

  def indicator(edgebin: DenseVector[Double])(valeur: Double): DenseVector[Double] = {
    DenseVector.tabulate(edgebin.length-1){i =>
      if(valeur >= edgebin(i) && valeur <= edgebin(i+1)) 1.0 else 0.0
    }
  }

  def histo: DenseVector[Double] =
    x.map(indicator(this.binedge._1)(_)).reduce(_+_)

  def recomphisto(hisnew : Histogram): DenseVector[Double]= {
    actualizebinedge(hisnew.binedge._3,hisnew.binedge._4)
  }

  def extract(va : Int ): DenseVector[Double]={
    if(va.toDouble > this.binedge._3 || va.toDouble < this.binedge._4){
      this.histo - indicator(actualizebinedge(va,va))(va.toDouble)
    } else {
      this.histo - indicator(this.binedge._1)(va)
    }
  }

  def adda(va : Int ): DenseVector[Double] = {
    if(va > this.binedge._3 || va < this.binedge._4 ){
      this.histo + indicator(actualizebinedge(va,va))(va.toDouble)
    } else {
      this.histo + indicator(this.binedge._1)(va)
    }
  }

  def addahist(va : Int ): Histogram = new Histogram(this.binedge._2 ++ ML(va), r)

  def extracthist(va : Int ): Histogram = new Histogram(x diff ML(va) ,r)

  def actualizebinedge(vamin: Int, vamax: Int) : DenseVector[Double] = {
    var a: Int = max(x)
    var b: Int = min(x)
    if(vamax>a){a=vamax}
    if(vamin<b){b=vamin}
    val edge: DenseVector[Double] = DenseVector.fill(2 * r)(0)
    if (a == b) {
      a = a + 1
      b = b - 1
    }
    edge(0) = b.toDouble
    edge(edge.length - 1) = a.toDouble
    (1 to 2 * r - 2).foreach(i =>
      if (i % 2 != 0) {
        edge(i) = edge(i - 1) + (a.toDouble - b.toDouble) / r.toDouble
      } else {
        edge(i) = edge(i - 1)
      })
    edge
  }
}
