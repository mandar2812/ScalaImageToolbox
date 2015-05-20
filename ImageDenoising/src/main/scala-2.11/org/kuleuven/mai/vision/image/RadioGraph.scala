package org.kuleuven.mai.vision.image

import org.kuleuven.mai.vision.utils.median
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

  def getaveragemedian(edge: DenseVector[Double], median: Int): Int ={
    (edge(2*median).toInt)}

  def getmedhist(k: DenseVector[Double], binedges: DenseVector[Double]): Int =
    getaveragemedian(binedges, extractmedian(freqcumsum(k)))


  def getwindowbin(k: DenseVector[Double],binedges: DenseVector[Double]): DenseVector[Double]= {
    var z:DenseVector[Double]= DenseVector.fill(2)(0)
    if (extractmedian(freqcumsum(k)) != k.length - 1){
      z= DenseVector(getaveragemedian(binedges, extractmedian(freqcumsum(k))),
        getaveragemedian(binedges, extractmedian(freqcumsum(k)) + 1))
    }
    else{z=DenseVector(getaveragemedian(binedges, extractmedian(freqcumsum(k))),
      binedges(2*k.length-1))
    }
    z
  }

  def getvalues( hist: Histogram ): ML[Int] ={
    hist.binedge._2.filter( i=>
      getwindowbin(hist.histo,hist.binedge._1)(0)<=i &&
        i<=getwindowbin(hist.histo,hist.binedge._1)(1))
  }

  def medianvalue(hist: Histogram): Int ={
    val valuesdb : List[Double] = List.tabulate(getvalues(hist).length){i=> getvalues(hist).toList(i).toDouble}
    median(valuesdb).toInt
  }
  def medianup(hist:Histogram): Int= median(hist.binedge._2.toList.map{_.toDouble}).toInt

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
          ker(e) = shift(initker(e), e, i + r )
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
  def binedgecompute(x :ML[Int]): (DenseVector[Double],ML[Int], Int ,Int ) = {

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

  def align(hismat : ML[Histogram]): Histogram = new Histogram(hismat.map(_.binedge._2).reduce(_++_),r)


  def denoisingfast: List[ML[Int]] = {

    var actucol: ML[Int] = ML.fill(2 * r + 1)(0)
    val firstcolumn: ML[ML[Int]] = ML.fill(imag.length)(ML.fill(2 * r + 1)(0))
    (0 to imag.length - 1).foreach { i =>
      actucol = getColumnIm(i)
      firstcolumn(i) = actucol
    }
    val ker: ML[ML[Int]] = kernel(0)
    val initker: ML[ML[Int]] = kernel(0)
    var initkerhist: DenseVector[Double] = histandmedkernel(ker)._1
    var histfirstcolumn: List[Histogram] = List.tabulate(imag.length)(i => new Histogram(firstcolumn(i), r))
    var histoofhistfirstcolumn: ML[DenseVector[Double]] = ML.tabulate(imag.length)( j =>histfirstcolumn(j).histo)
    var binmaxold: ML[Int]= ML.tabulate(2*r+1)(i=> max( histfirstcolumn(i).binedge._1 ).toInt)
    var binmaxxold: Int= max(binmaxold)
    var binminold:  ML[Int]= ML.tabulate(2*r+1)(i=> min( histfirstcolumn(i).binedge._1).toInt)
    var binminnold: Int= min(binminold)
    List.tabulate(imag.head.length - (2 * r + 1)) { i =>
      if (i != 0) {
        (0 to ker.length - 1).foreach { e =>
          ker(e) = shift(initker(e), e, i + r )
          initker(e) = ker(e)
        }
        (0 to ker.length-1).foreach{k=>
         binmaxold(k)= max(histfirstcolumn(k).actualizebinedge(imag(k)(i+2*r),imag(k)(i+2*r))).toInt
         binminold(k)= min(histfirstcolumn(k).actualizebinedge(imag(k)(i+2*r),imag(k)(i+2*r))).toInt
         histoofhistfirstcolumn(k) = histfirstcolumn(k).extract(imag(k)(0))
          histoofhistfirstcolumn(k)=histfirstcolumn(k).adda(imag(k)(i+2*r))
        }

        initkerhist=histandmedkernel(initker)._1
      }
      var binmaxnew: Int= max(binmaxold)
      var binminnew: Int= min(binminold)
      ML.tabulate(imag.length - (2 * r + 1)) { j =>
        if (j!=0){
          if (i == 0) {
            binmaxxold=max(ML(binmaxxold,max(histfirstcolumn(j+2*r).binedge._1).toInt))
            binminnold=min(ML(binminnold,min(histfirstcolumn(j+2*r).binedge._1).toInt))
            initkerhist += histfirstcolumn(j + 2 * r).histo
            initkerhist -= histfirstcolumn(j - 1).histo
            var binedge: DenseVector[Double]= histfirstcolumn(j).actualizebinedge(binminnold,binmaxxold)

            getaveragemedian(
              binedge,
              extractmedian(freqcumsum(initkerhist))
            )
          } else {
            val latenthisto = histfirstcolumn(j + 2 * r).extract(imag(j + 2 * r)(i - 1))
            latenthisto :+= histfirstcolumn(j + 2 * r).adda(imag(j + 2 * r)(i + 2 * r))

            initkerhist :+= latenthisto
            initkerhist :-= histfirstcolumn(j - 1).histo
            getaveragemedian(histfirstcolumn(j+r).actualizebinedge(imag(j + 2 * r)(i + 2 * r),imag(j + 2 * r)(i + 2 * r)),
              extractmedian(freqcumsum(initkerhist)))
          }
        } else{
          getaveragemedian(histfirstcolumn(j+r).actualizebinedge(binminnew,binminnew),
            extractmedian(freqcumsum(initkerhist)))

        }
      }
    }
  }
  def denoisingfast2: List[ML[Int]] = {

    var actucol: ML[Int] = ML.fill(2 * r + 1)(0)
    val firstcolumn: ML[ML[Int]] = ML.fill(imag.length)(ML.fill(2 * r + 1)(0))
    (0 to imag.length - 1).foreach { i =>
      actucol = getColumnIm(i)
      firstcolumn(i) = actucol
    }
    var binedge: DenseVector[Double]= DenseVector.fill(2*r)(0)
    val ker: ML[ML[Int]] = kernel(0)
    val initker: ML[ML[Int]] = kernel(0)
    var initkerhist: DenseVector[Double] = histandmedkernel(ker)._1
    val histfirstcolumn: List[Histogram] = List.tabulate(imag.length)(i => new Histogram(firstcolumn(i), r))

    List.tabulate(imag.head.length - (2 * r + 1)) { i =>
      if (i != 0) {
        (0 to ker.length - 1).foreach { e =>
          ker(e) = shift(initker(e), e, i + r )
          initker(e) = ker(e)
        }
        initkerhist=histandmedkernel(initker)._1
      }
      ML.tabulate(imag.length - (2 * r + 1)) { j =>
        if (j!=0){
          if (i == 0) {
            initkerhist += histfirstcolumn(j + 2 * r).histo
            initkerhist -= histfirstcolumn(j - 1).histo

             binedge= histfirstcolumn(j+2*r).recomphisto(histfirstcolumn(j+2*r-1))

            getaveragemedian(
              binedge,
              extractmedian(freqcumsum(initkerhist))
            )
          } else {
            val latenthisto = histfirstcolumn(j + 2 * r).extract(imag(j + 2 * r)(i - 1))
            latenthisto :+= histfirstcolumn(j + 2 * r).adda(imag(j + 2 * r)(i + 2 * r))

            initkerhist :+= latenthisto
            initkerhist :-= histfirstcolumn(j - 1).histo
            getaveragemedian(histfirstcolumn(j+r).actualizebinedge(imag(j + 2 * r)(i + 2 * r),imag(j + 2 * r)(i + 2 * r)),
              extractmedian(freqcumsum(initkerhist)))
          }
        } else{
          histandmedkernel(initker)._2


          /* else if(j==0){
             histandmedkernel(ker)._2
           } else {
             initkerhist.+=(histfirstcolumn(j + 2 * r).histo)
             initkerhist.-=(histfirstcolumn(j+r-1).histo)
             getaveragemedian(histfirstcolumn(j + 2 * r).actualizebinedge(imag(j + 2 * r)(i + 2 * r)),
               extractmedian(freqcumsum(initkerhist)))*/
        }
      }
    }
  }
  def denoisingfast3: ML[ML[Int]] = {

    var actucol: ML[Int] = ML.fill(2 * r + 1)(0)
    val firstcolumn: ML[ML[Int]] = ML.fill(imag.length)(ML.fill(2 * r + 1)(0))
    (0 to imag.length - 1).foreach { i =>
      actucol = this.getColumnIm(i)
      firstcolumn(i) = actucol
    }
    val ker: ML[ML[Int]]=ML.tabulate(2*r+1){i=>firstcolumn(i)}

    var kerhist: ML[Histogram]=ML.tabulate(ker.length){i=> new Histogram(ker(i),r)}
    var kerhistinit: ML[Histogram]=ML.tabulate(ker.length){i=> new Histogram(ker(i),r)}
    var kerline: Histogram = align(kerhist)
    val histcol: ML[Histogram] = ML.tabulate(imag.length)(i => new Histogram(firstcolumn(i), r))

    ML.tabulate(imag.head.length - (2 * r+1)) { i =>
      if(i!=0){
        (0 to 2*r).foreach{l=> kerhist(l)= kerhistinit(l).extracthist(imag(l)(i-1))
                                kerhist(l)=kerhistinit(l).addahist(imag(l)(i+2*r))

      }
        kerhistinit=kerhist

      }
      ML.tabulate(imag.length - (2 * r+1)) { j =>
        if(j!=0){
          if(i==0){
            kerhist= kerhist.drop(1)+=histcol(j+2*r)

            }
          else {
              histcol(j+2*r)=histcol(j+2*r).extracthist(imag(j+2*r)(i-1)).addahist(imag(j+2*r)(i+2*r) )
              kerhist = kerhist.drop(1) += histcol(j+2*r)

          }
          }
        kerline= align(kerhist)
        println("length: " +kerhist.length+" lengthvalue: " +kerhist.head.binedge._2.length)
         println("i:" +i +" j: "+j)
         medianup(kerline)
      }
      }
    }

}





