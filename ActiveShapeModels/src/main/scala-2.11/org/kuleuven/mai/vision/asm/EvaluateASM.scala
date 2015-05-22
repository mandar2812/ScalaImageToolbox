package org.kuleuven.mai.vision.asm

import java.io.File

import breeze.linalg.{inv, norm, det, DenseVector}
import com.github.tototoshi.csv.CSVReader

/**
 * @author mandar2812
 *
 * Program to conduct leave one out validation
 * of Incisor Active Shape Model on the training
 * data set of 14 radiographs.
 */
object EvaluateASM {
  def main(args: Array[String]): Unit = {

    val dataRoot = "data/Landmarks/original/"
    val imageRoot = "data/Radiographs/"
    val levels = args(0).toInt
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
      val testimagestr = if(fold < 10) "0"+fold+".tif" else fold+".tif"
      val testImageStr = List(testimagestr) ::: (1 to levels).map{level => fold+"_level_"+level+".png"}.toList
      val training_images = net_images.filter(file => !testImageStr.contains(file.getName))
      val training_indices = (1 to folds).filter(_ != fold)

      println("Fold: "+fold/*+" Training Images: "+training_images.length*/)
      //println("Training Images"+training_images.map(_.getName))
      //println("Test Images: "+testImageStr)

      val imagesByLevels = List.tabulate(levels+1){i =>
        if(i == 0) {
          image_files.filter(_.getName != testimagestr)
        } else {
          training_images.filter(file => file.getName.contains("_level_"+i+".png"))
        }
      }

      //println("\n"+imagesByLevels)

      val landmarks: List[(Int, List[DenseVector[Double]])] = training_indices.map{i =>
        (i, List.tabulate(8){j =>
          val file = dataRoot+"landmarks"+i+"-"+(j+1)+".txt"
          val points = Array.fill(80)(0.0)
          val reader = CSVReader.open(new File(file))
          val it = reader.iterator

          (1 to 40).foreach{point =>
            points.update(point-1, it.next().head.toDouble)
            points.update(point+39, it.next().head.toDouble)
          }
          reader.close()
          DenseVector(points)
        })
      }.toList

      val landmarksByTooth = List.tabulate(8){tooth =>
        landmarks.map{couple => (couple._1, couple._2(tooth))}.toMap
      }


      println("Images for each tooth: "+landmarksByTooth.head.size)
      val models = landmarksByTooth.map(i => ActiveShapeModel(i, imagesByLevels, 5))
      val meanshape = models.head.alignShapes
      //println("Mean Shape \n"+meanshape)
      println("Pixel Structure: Inv(Variance) = "+
        models.head.pixelStructure(3).view.map(x => inv(x._2)).head)
      println("Pixel Structure norm(mu) = "+models.head.pixelStructure(3).map(x => norm(x._1, 2)).head)

      //Evaluate for fold.

    }

  }
}
