package org.kuleuven.mai.vision.image

import scala.collection.mutable

/**
 * Created by Romain on 07/05/2015.
 */
class RadioGraph (imag: List[mutable.MutableList[Int]]) {

  def getColumnIm(n: Int, r: Int): mutable.MutableList[Int] = imag(n).slice(0, 2 * r + 1)

  // create a kernel matrix based on the column
  def kernel(borninf: Int, r: Int): mutable.MutableList[mutable.MutableList[Int]] = {
    val sizekernel: Int = 2 * r + 1
    val ker: mutable.MutableList[mutable.MutableList[Int]] =
      mutable.MutableList.fill(sizekernel)(mutable.MutableList.fill(sizekernel)(0))
    (0 to sizekernel - 1).foreach(i => ker(i) = this.getColumnIm(i + borninf, r))
    ker
  }

  // shift the column : (i,j)=>(i+1,j)
  def shift(toshiftcol: mutable.MutableList[Int], i: Int, j: Int): mutable.MutableList[Int] = {
    val shiftedcol: mutable.MutableList[Int] = toshiftcol.drop(1) += imag(i)(j)
    shiftedcol
  }


  def kernelactualize(kernel: mutable.MutableList[mutable.MutableList[Int]],i:Int,j:Int): mutable.MutableList[mutable.MutableList[Int]]= {
    val columntoshift: mutable.MutableList[Int]= kernel(kernel.size-1)
    val shiftedcolumn: mutable.MutableList[Int]= shift(columntoshift,i,j)
    kernel.drop(1) += shiftedcolumn
  }

}


