package org.kuleuven.mai.vision.image

import breeze.linalg.{max, min, DenseVector}
import breeze.numerics.abs
import breeze.stats.hist

import scala.collection.mutable.{MutableList => ML}
import scala.collection.parallel.mutable

/**
 * @author koldh
 */
class RadioGraph (imag: List[ML[Int]], r: Int) {


  def getColumnIm(n: Int): ML[Int] = imag(n).slice(0, 2 * r + 1)

  // create a kernel matrix based on the column
  def kernel(borninf: Int): ML[ML[Int]] = {
    val sizekernel: Int = 2 * r + 1
    val ker: ML[ML[Int]] =
      ML.fill(sizekernel)(ML.fill(sizekernel)(0))
    (0 to sizekernel - 1).foreach(i => ker(i) = this.getColumnIm(i + borninf))
    ker
  }

  // shift the column : (i,j)=>(i+1,j)
  def shift(toshiftcol: ML[Int], i: Int, j: Int): ML[Int] = {
    val shiftedcol: ML[Int] = toshiftcol.drop(1) += imag(i)(j)
    shiftedcol
  }


  def kernelactualize(kernel: ML[ML[Int]],i:Int,j:Int): ML[ML[Int]]= {
    val columntoshift: ML[Int]= kernel(kernel.size-1)
    val shiftedcolumn: ML[Int]= shift(columntoshift,i,j)
    kernel.drop(1) += shiftedcolumn
  }

  def freqcumsum(y: ML[Int]): DenseVector[Double] = {
    val z: DenseVector[Double]= DenseVector.zeros(y.length)
    val l: DenseVector[Double]= DenseVector(y.map(_.toDouble).toArray)
    val x: DenseVector[Double]= DenseVector(hist(abs(l),r,(0.0 ,abs(min(imag.flatten)).toDouble)).hist.toArray.reverse)
    println(min(imag.flatten))
    println(max(imag.flatten))
    println(l)
    (0 to x.length-1).foreach(i=> z(i) = x(i)/(x.length+1) + z(i-1))
    z
  }

  def extractmedian(x : ML[Int]) : Int ={
    var median: Int=0
    var z: DenseVector[Double]= DenseVector.zeros(x.length)
    z= freqcumsum(x)
    (0 to x.length-1).find(i  => z(i) >= 0.5).foreach(i=> median=i)
    median
  }

}


