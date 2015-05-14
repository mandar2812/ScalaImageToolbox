package org.kuleuven.mai.vision.image

import breeze.linalg
import breeze.linalg._
import breeze.numerics.{ceil, abs, floor}
import scala.collection.mutable.{MutableList => ML}
/**
 * Created by Romain on 08/05/2015.
 */
class Histogram( x: ML[Int],r:Int) {


  def +( his1 : Histogram): Histogram =
    new Histogram(this.x ++ his1.binedge._2, r)

  def ++(hismat : ML[Histogram]): Histogram =
    new Histogram(this.x ++ hismat.map(_.binedge._2).reduce(_++_), r)

  def binedge: (DenseVector[Double], ML[Int], Int ,Int ) = {

    var a: Int = max(x)

    var b: Int = min(x)
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
    (edge,x,b,a)
  }

  def indicator(edgebin: DenseVector[Double])(valeur: Double): DenseVector[Double] = {
    val histovaleur: DenseVector[Double] = DenseVector.zeros(r)
    (0 to edgebin.length - 1).foreach(i => if (i % 2 == 0) {
      if (i != edgebin.length - 2) {
        if (valeur < edgebin(i + 1) && valeur >= edgebin(i)) {
          histovaleur(i / 2) = 1.0
        }
        else histovaleur(i / 2) = 0.0
      }
      else {
        if (valeur <= edgebin(i+1 ) && valeur >= edgebin(i)) {
          histovaleur(i / 2) = 1.0
        }
        else {
          histovaleur(i / 2) = 0.0
        }
      }
    }
    )
    histovaleur
  }

  def histo: DenseVector[Double] = {
    val edge: DenseVector[Double] = this.binedge._1
    val histogr = indicator(edge) _
    var histogram: DenseVector[Double] = DenseVector.zeros(r)
    (0 to x.length - 1).foreach(i =>
      histogram = histogr(x(i).toDouble) + histogram
    )
    histogram
  }

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

  def addahist(va : Int ): Histogram = {
   var newx : ML[Int]= ML.tabulate( this.binedge._2.length){i=> this.binedge._2(i)}
       newx=newx+=va
       new Histogram(newx,r)
  }

  def extracthist(va : Int ): Histogram = {
    var xvalnew :ML[Int]= ML.tabulate(x.length)(k=>x(k))
    var newx : ML[Int]= ML.tabulate( this.binedge._2.length){i=> this.binedge._2(i)}
    var comp : Int=x.length
    (0 to x.length-1).foreach(i=> if(x(i)==va) comp=i)
    if(comp<x.length) {
      xvalnew= ML.tabulate(comp)(k=>x(k))
      xvalnew=xvalnew++x.drop(comp+1)
    }
    new Histogram(xvalnew ,r)
  }

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
