package org.kuleuven.mai.vision.image

import breeze.linalg._
import breeze.numerics.{ceil, abs, floor}
import scala.collection.mutable.{MutableList => ML}
/**
 * Created by Romain on 08/05/2015.
 */
class Histogram( x: ML[Int],r:Int) {


  def binedge: DenseVector[Double]= {

    var a : Int= max(x)

    var b : Int= min(x)
    val edge: DenseVector[Double]= DenseVector.fill(2*r)(0)
    if (a==b ) {a=a+1
      b=b-1}
    edge(0)=b.toDouble
    edge(edge.length-1)=a.toDouble
    (1 to 2*r-2).foreach(i=> if(i%2!=0) edge(i)=edge(i-1) + (a.toDouble-b.toDouble)/r.toDouble
    else edge(i)=edge(i-1) )
    edge
  }

  def indicator( edgebin : DenseVector[Double] )( valeur :Double ) : DenseVector[Double] ={
    val histovaleur : DenseVector[Double]= DenseVector.zeros(r)
    (0 to  edgebin.length-1).foreach(i=> if( i%2==0){
      if ( i != edgebin.length-1){
      if( valeur < edgebin(i+1) && valeur >= edgebin(i) ){
        histovaleur(i/2)=1.0}
      else histovaleur(i/2)=0.0 }
    else {
      if (valeur <= edgebin(i + 1) && valeur >= edgebin(i)){
        histovaleur(i / 2) = 1.0}
      else {histovaleur(i / 2) = 0.0
      }
     }
    }
    )
    histovaleur
  }

  def histo: DenseVector[Double] = {
    val edge: DenseVector[Double] = binedge
    val histogr = indicator(edge)_
    var histogram : DenseVector[Double]= DenseVector.zeros(r)
    (0 to x.length-1).foreach(i=> histogram = histogr(x(i).toDouble) + histogram)
    histogram
  }


  def freqcumsum( y:DenseVector[Double] ) : DenseVector[Double] = {
    val freqvec: DenseVector[Double]= DenseVector.zeros(y.length)
    (0 to y.length-1).foreach(i=> freqvec(i) = y(i)/sum(y) +freqvec(i-1) )
    println(freqvec)
    freqvec

  }

  def extractmedian( x : DenseVector[Double]) : Int ={
    var median: Int =0
    val z: DenseVector[Double]= x
    println(x)
    (0 to x.length-1).find(i  => z(i)>= 0.5).foreach(i=> median =i)
    median
  }

  def histmedian: Int = { val median: Int= extractmedian( freqcumsum( histo ))
  median }


}
