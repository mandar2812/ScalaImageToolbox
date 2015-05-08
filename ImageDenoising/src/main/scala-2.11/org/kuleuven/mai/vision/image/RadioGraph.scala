package org.kuleuven.mai.vision.image

import scala.collection.mutable.{MutableList => ML}

/**
 * @author koldh
 */
class RadioGraph (imag: List[ML[Int]]) {

  def getColumnIm(n: Int, r: Int): ML[Int] = imag(n).slice(0, 2 * r + 1)

  // create a kernel matrix based on the column
  def kernel(borninf: Int, r: Int): ML[ML[Int]] = {
    val sizekernel: Int = 2 * r + 1
    val ker: ML[ML[Int]] =
      ML.fill(sizekernel)(ML.fill(sizekernel)(0))
    (0 to sizekernel - 1).foreach(i => ker(i) = this.getColumnIm(i + borninf, r))
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

}


