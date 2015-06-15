package org.kuleuven.mai.vision.asm

import java.io.File

import breeze.linalg.DenseVector
import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import com.sksamuel.scrimage.Format.PNG
import com.sksamuel.scrimage.PixelTools

/**
 * @author mandar2812
 *
 * Program to conduct leave one out validation
 * of Incisor Active Shape Model on the training
 * data set of 14 radiographs.
 */
object EvaluateASM {

  val dataRoot = "data/Landmarks/original/"
  val imageRoot = "data/Radiographs/"

  def main(args: Array[String]): Unit = {

    val levels = args(0).toInt
    val nsmax = args(1).toInt
    val kmax = args(2).toInt

    /*val w = for(k <- 1 to kmax; ns <- k+1 to nsmax) yield (ns,k)
    val writer = CSVWriter.open(new File("data/results.csv"))
    var net_error = Seq[Double]()

    w.foreach((windows) => {
      net_error = EvaluateASM.atKernelBandwidth(windows._1,
        windows._2, levels).toArray.toSeq
      writer.writeRow(Seq(windows._2, windows._1) ++ net_error)
    })

    writer.close()*/
    val net_error = EvaluateASM.atKernelBandwidth(nsmax,
      kmax, levels).toArray.toSeq
  }

  def readLandmarks(file: String): DenseVector[Double] = {
    val points = Array.fill(80)(0.0)
    val reader = CSVReader.open(new File(file))
    val it = reader.iterator

    (1 to 40).foreach{point =>
      points.update(point-1, it.next().head.toDouble)
      points.update(point+39, it.next().head.toDouble)
    }
    reader.close()
    DenseVector(points)
  }

  def atKernelBandwidth(searchBandwidth: Int,
                        pixelGradBandwidth: Int,
                        levels: Int): DenseVector[Double] = {

    val ns = searchBandwidth
    val k = pixelGradBandwidth
    val net_error = DenseVector.zeros[Double](8)
    /*
     * First copy the original images into the data/processed directory
     * Then for each fold i, calculate the image indexes for training
     * and test.
     *
     * For each fold i, train a List of 8 Active shape models
     * one for each incisor. Use the code from IncisorsAlign.
     */
    //Get the original image files
    val image_files = new java.io.File(imageRoot)
      .listFiles
      .filter(_.getName.endsWith(".tif")).toList
    //Get the processed multilevel image files
    val more_image_files = new java.io.File(imageRoot+"processed/")
      .listFiles
      .filter(_.getName.endsWith(".png")).toList

    val net_images = image_files ::: more_image_files
    val folds = image_files.length

    (1 to folds).foreach{fold =>
      //for this fold calculate the training and test split
      val testimagebasic = if(fold < 10) "0"+fold+".tif" else fold+".tif"
      val testImageNames = List(testimagebasic) ::: (1 to levels).map(level => {
        fold+"_level_"+level+".png"
      }).toList

      val training_images = net_images.filter(file =>
        !testImageNames.contains(file.getName))

      val training_indices = (1 to folds).filter(_ != fold)

      val test_images = (0 to levels).map(l =>{
        val name = if(l == 0) {
          if(fold < 10) "0"+fold+".tif" else fold+".tif"
        } else {
          fold+"_level_"+l+".png"
        }
        (l, net_images.filter(_.getName == name).head)
      }).toMap

      val test_landmarks = List.tabulate(8){j =>
        readLandmarks(dataRoot+"landmarks"+fold+"-"+(j+1)+".txt")
      }

      println("Fold: "+fold)
      val imagesByLevels = List.tabulate(levels+1){i =>
        if(i == 0) {
          image_files.filter(_.getName != testimagebasic)
        } else {
          training_images.filter(file =>
            file.getName.contains("_level_"+i+".png")
          )
        }
      }

      val landmarks: List[(Int, List[DenseVector[Double]])] = training_indices.map{i =>
        (i, List.tabulate(8){j =>
          readLandmarks(dataRoot+"landmarks"+i+"-"+(j+1)+".txt")
        })
      }.toList

      val landmarksByTooth = List.tabulate(8){tooth =>
        landmarks.map{couple => (couple._1, couple._2(tooth))}.toMap
      }


      println("Images for each tooth: "+landmarksByTooth.head.size)
      val models = landmarksByTooth.map(i => ActiveShapeModel(i, imagesByLevels, k))
      val meanshape = models.head.alignShapes
      //Evaluate for fold.
      val imageTest = ActiveShapeModel.getImage(0, test_images(0))._2
      val result = DenseVector.tabulate[Double](8)((tooth) => {
        val toothModel = models(tooth)
        println("\nPerforming Multi-Resolution search for tooth: "+(tooth+1)+"\n")
        val res = toothModel.MultiResolutionSearch(test_images,
          ns, 40, test_landmarks(tooth))
        val toothLandmarks = res._3
        //calculate the edges of the smallest rectangle
        //containing all landmarks
        //[xmin, xmax] x [ymin, ymax]
        val points = ActiveShapeModel.landmarksAsPoints(toothLandmarks)
        val (xmin, xmax) = (points.map(_._1).min, points.map(_._1).max)
        val (ymin, ymax) = (points.map(_._2).min, points.map(_._2).max)
        println("XMin: "+xmin+" XMax "+xmax)
        println("YMin: "+ymin+" YMax "+ymax)
        //Output the segmentation result for each tooth
        //create a file (0)fold-tooth.jpg using map
        //operation on (0)fold.tif
        val toothSegment = imageTest.map((x, y, pixel) => {
          if(x <= xmax && y <= ymax && x >= xmin && y >= ymin) {
            //If the winding number is close to one it must be painted gray
            //else painted black
            val wn = toothModel.windingNumber(DenseVector(x.toDouble, y.toDouble),
              points)
            //println("Winding Number "+wn)
            if(wn >= 0.9*2.0*math.Pi) {
              PixelTools.rgb(112,112,112)
            } else {
              pixel
            }

          } else {
            pixel
          }
        })
        val fileName = if(fold < 10) "0"+fold+"-"+tooth+".png" else fold+"-"+tooth+".png"
        toothSegment.write("data/Results/"+fileName, PNG)
        res._2
      })

      val p: DenseVector[Double] = result * 100.0
      println("Error of fit: "+p+"\n\n")
      net_error :+= result
    }
    net_error :/= (0.01*folds.toDouble)
    println("Final Result ...\n"+"Net Error (vector of percentages): "+net_error+"\n")
    net_error
  }
}
