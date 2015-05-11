package org.kuleuven.mai.vision.image


import breeze.linalg.{sum, max, min, DenseVector}
import scala.collection.mutable.{MutableList => ML}


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
    val shiftedcol: ML[Int] = toshiftcol.drop(1) += imag(i+r)(j+r)  // drop(1) : cancel head of list , += : add at the last position of the list
    shiftedcol
  }


 // def kernelactualize(kernel: ML[ML[Int]],i:Int,j:Int): ML[ML[Int]]= {
  //  val columntoshift: ML[Int]= kernel(kernel.size-1)
   // val shiftedcolumn: ML[Int]= shift(columntoshift,i,j)
   // kernel.drop(1) += shiftedcolumn
  //}



  def freqcumsum( y:DenseVector[Double] ) : DenseVector[Double] = {
    val freqvec: DenseVector[Double]= DenseVector.zeros(y.length)
    (0 to y.length-1).foreach(i=> freqvec(i) = y(i)/sum(y) +freqvec(i-1) )
    freqvec

  }

  def extractmedian( x : DenseVector[Double]) : Int ={
    var median: Int =0
    val z: DenseVector[Double]= x
    (0 to x.length-1).find(i  => z(i)>= 0.5).foreach(i=> median =i)
    median
  }

  def getaveragemedian(edge : DenseVector[Double],median : Int) : Int ={
    var avmed: Int=0
    avmed =  (edge(2*median)/2+edge(2*median+1)/2).toInt
    avmed

  }

  def getmedhist(k: DenseVector[Double], binedges : DenseVector[Double]): Int = {

    val median: Int= getaveragemedian(  binedges,  extractmedian( freqcumsum( k)))
    median }


  // access to (histogram,median) kernel's couple
  def histandmedkernel( kernel : ML[ML[Int]]) : (DenseVector[Double] ,  Int) ={

    var his = new Histogram( kernel.head ,r)
    var med : Int = getaveragemedian(his.binedge ,extractmedian(freqcumsum(his.histo)))
    var histokernel: DenseVector[Double] = his.histo
    (1 to 2*r).foreach{ k=>
      his= new Histogram (kernel(k),r)
      val binedge= his.binedge
      val actuhist=his.histo
      histokernel = histokernel + actuhist
     med=med+getaveragemedian(binedge ,extractmedian(freqcumsum(actuhist)))
    }

    (histokernel,med/(2*r+1))


  }

  //access to the median of the histogram kernel
  def medkernel( kernel : ML[ML[Int]]) :   Int ={

    var his = new Histogram( kernel.head ,r)
    var med : Int = getaveragemedian(his.binedge ,extractmedian(freqcumsum(his.histo)))
    var histokernel: DenseVector[Double] = his.histo
    (1 to 2*r).foreach{ k =>
      his= new Histogram (kernel(k),r)
      val binedge= his.binedge
      val actuhist=his.histo
      histokernel = histokernel + actuhist
      med=med+getaveragemedian(binedge ,extractmedian(freqcumsum(actuhist)))
    }
    var avmed: Int =0
    avmed=med/(2*r+1)
    avmed
  }
  //access to the Histogram of the kernel
  def histkernel( kernel : ML[ML[Int]]) : DenseVector[Double]  ={

    var his = new Histogram( kernel.head ,r)
    var med : Int = getaveragemedian(his.binedge ,extractmedian(freqcumsum(his.histo)))
    var histokernel: DenseVector[Double] = his.histo
    (1 to 2*r).foreach{ k=>
      his= new Histogram (kernel(k),r)
      val binedge= his.binedge
      val actuhist=his.histo
      histokernel = histokernel + actuhist
      med=med+getaveragemedian(binedge ,extractmedian(freqcumsum(actuhist)))
    }
    histokernel
  }



 def denoising: List[ML[Int]]= {

   var actucol: ML[Int] = ML.fill(2*r+1)(0)
   val firstcolumn: ML[ML[Int]] = ML.fill(imag.length)(ML.fill(2*r+1)(0))

  (0 to imag.length -1).foreach { i =>
    actucol = getColumnIm(i)
    firstcolumn(i) = actucol}

   var med: Int =0
   var ker: ML[ML[Int]] = kernel(0)
   var initker: ML[ML[Int]] = kernel(0)

   List.tabulate(imag.head.length-(2*r+1)) { i =>
     if (i != 0) {
       (0 to ker.length - 1).foreach{e =>
         ker(e) = shift(initker(e),e,  i+ r+1)
         initker(e)=ker(e)}
     }

     ML.tabulate(imag.length-(2*r+1)){ j =>
       if (j != 0 && i!=0 ) {
         var latent: ML[Int] = shift(firstcolumn(2*r+j), j+r, i +r)
         var lat: ML[ML[Int]]= ker.drop(1)+= latent
         ker = lat
       }
       med = medkernel(ker)
       med
     }
   }
 }





}


