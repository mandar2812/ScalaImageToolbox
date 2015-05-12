package org.kuleuven.mai.vision.image


import breeze.linalg.{sum, max, min, DenseVector}
import scala.collection.mutable.{MutableList => ML}


/**
 * @author koldh
 */
class RadioGraph (imag: List[ML[Int]], r: Int) {

  def apply(x: Int)(y: Int): Int = imag(x)(y)

  def getColumnIm(n: Int): ML[Int] = imag(n).slice(0, 2 * r + 1)

  // create a kernel matrix based on the column
  def kernel(borninf: Int): ML[ML[Int]] =
    ML.tabulate(2 * r + 1)(i => this.getColumnIm(i + borninf))

  def shift(toshiftcol: ML[Int], i: Int, j: Int): ML[Int] =
    toshiftcol.drop(1) += imag(i + this.r)(j + this.r)

  def freqcumsum(y: DenseVector[Double]): DenseVector[Double] =
    DenseVector.tabulate(y.length)(i => sum(y(0 to i)) / sum(y))

  def extractmedian(x: DenseVector[Double]): Int =
    (0 to x.length - 1).find(i => x(i) >= 0.5).get

  def getaveragemedian(edge: DenseVector[Double], median: Int): Int =
    edge(2 * median).toInt

  def getmedhist(k: DenseVector[Double], binedges: DenseVector[Double]): Int =
    getaveragemedian(binedges, extractmedian(freqcumsum(k)))

  // access to (histogram,median) kernel's couple
  def histandmedkernel(kernel: ML[ML[Int]]): (DenseVector[Double], Int) = {
    var his = new Histogram(kernel.head, r)
    var med: Int = getaveragemedian(his.binedge._1, extractmedian(freqcumsum(his.histo)))
    var histokernel: DenseVector[Double] = his.histo
    (1 to 2 * r).foreach { k =>
      his = new Histogram(kernel(k), r)
      val binedge = his.binedge
      val actuhist = his.histo
      histokernel = histokernel + actuhist
      med += getaveragemedian(binedge._1, extractmedian(freqcumsum(actuhist)))
    }
    (histokernel, med / (2 * r + 1))
  }


  def denoising: List[ML[Int]] = {
    var actucol: ML[Int] = ML.fill(2 * r + 1)(0)
    val firstcolumn: ML[ML[Int]] = ML.fill(imag.length)(ML.fill(2 * r + 1)(0))

    (0 to imag.length - 1).foreach { i =>
      actucol = getColumnIm(i)
      firstcolumn(i) = actucol
    }

    var ker: ML[ML[Int]] = kernel(0)
    val initker: ML[ML[Int]] = kernel(0)

    List.tabulate(imag.head.length - (2 * r + 1)) { i =>
      if (i != 0) {
        (0 to ker.length - 1).foreach { e =>
          ker(e) = shift(initker(e), e, i + r + 1)
          initker(e) = ker(e)
        }
      }

      ML.tabulate(imag.length - (2 * r + 1)) { j =>
        if (j != 0 && i != 0) {
          var latent: ML[Int] = shift(firstcolumn(2 * r + j), j + r, i + r)
          firstcolumn(2 * r + j) = latent
          ker = ker.drop(1) += latent
        }
        histandmedkernel(ker)._2
      }
    }
  }

  def denoisingfast: List[ML[Int]] = {

    var actucol: ML[Int] = ML.fill(2 * r + 1)(0)
    val firstcolumn: ML[ML[Int]] = ML.fill(imag.length)(ML.fill(2 * r + 1)(0))

    (0 to imag.length - 1).foreach { i =>
      actucol = getColumnIm(i)
      firstcolumn(i) = actucol
    }
    var ker: ML[ML[Int]] = kernel(0)
    val initker: ML[ML[Int]] = kernel(0)
    var initkerhist: DenseVector[Double] = histandmedkernel(ker)._1
    var histfirstcolumn: List[Histogram] = List.tabulate(imag.length)(i => new Histogram(firstcolumn(i), r))
    List.tabulate(imag.head.length - (2 * r + 1)) { i => if (i != 0) {
      (0 to ker.length - 1).foreach { e =>
        ker(e) = shift(initker(e), e, i + r )
        initker(e) = ker(e)
      }
    initkerhist=histandmedkernel(initker)._1}
      ML.tabulate(imag.length - (2 * r + 1)) { j =>
        if (j >r) {
          if (i == 0) {
            initkerhist.+=(histfirstcolumn(j + 2 * r).histo)
            initkerhist.-=(histfirstcolumn(j - r).histo)
            getaveragemedian(histfirstcolumn(j + 2 * r).actualizebinedge(imag(j + 2 * r)(i + 2 * r)), extractmedian(freqcumsum(initkerhist)))
          }

          else {
            var latenthisto: DenseVector[Double]= histfirstcolumn(j + 2 * r).extract(imag(j + 2 * r)(i - 1))
            latenthisto=latenthisto+histfirstcolumn(j + 2 * r).adda(imag(j + 2 * r)(i + 2 * r))

            initkerhist.+=(latenthisto)
            initkerhist.-=(histfirstcolumn(j - r-1).histo)
            getaveragemedian(histfirstcolumn(j + 2 * r).actualizebinedge(imag(j + 2 * r)(i + 2 * r)), extractmedian(freqcumsum(initkerhist)))

          }
        }
        else { if(j==0){
          histandmedkernel(ker)._2 }
               else {
          initkerhist.+=(histfirstcolumn(j + 2 * r).histo)
          initkerhist.-=(histfirstcolumn(j-1).histo)
          getaveragemedian(histfirstcolumn(j + 2 * r).actualizebinedge(imag(j + 2 * r)(i + 2 * r)), extractmedian(freqcumsum(initkerhist)))
        }
        }

      }
    }

  }
}



