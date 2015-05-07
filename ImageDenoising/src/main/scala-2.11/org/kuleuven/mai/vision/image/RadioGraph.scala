package org.kuleuven.mai.vision.image

import scala.collection.mutable

/**
 * Created by Romain on 07/05/2015.
 */
class RadioGraph (imag: List[mutable.MutableList[Int]]){
  
  def getColumnIm(n: Int,r: Int): mutable.MutableList[Int] = imag(n).slice(0,2*r+1)

  def kernel(r: Int): mutable.MutableList[mutable.MutableList[Int]] = {
    val sizekernel: Int = (2*r +1)
    val ker: mutable.MutableList[mutable.MutableList[Int]] =
      mutable.MutableList.fill(sizekernel)(mutable.MutableList.fill(sizekernel)(0))
    (0 to sizekernel-1).foreach(i => ker(i) = this.getColumnIm(i, r))
    ker
  }

}
