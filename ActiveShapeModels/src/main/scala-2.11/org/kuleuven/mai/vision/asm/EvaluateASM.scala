package org.kuleuven.mai.vision.asm

import java.io.File

import breeze.linalg.DenseVector
import com.github.tototoshi.csv.CSVReader

import scala.collection.immutable.HashMap

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

    /*
     * First copy the original images into the data/processed directory
     * Then for each fold i, calculate the image indexes for training
     * and test.
     *
     * For each fold i, train a List of 8 Active shape models
     * one for each incisor. Use the code from IncisorsAlign.
     */

    val image_files = new java.io.File(imageRoot)
      .listFiles
      .filter(_.getName.endsWith(".tif"))

    val folds = image_files.length

    (1 to 1).foreach{fold =>
      //for this fold calculate the training and test split
      val testimagestr = if(fold < 10) "0"+fold+".tif" else fold+".tif"
      val training_indices = (1 to folds).filter(_ != fold)
      val training_images = image_files.filter(_.getName != testimagestr)
      println("Fold: "+fold+" Training Images: "+training_images.length)

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
      val models = landmarksByTooth.map(i => ActiveShapeModel(i, training_images, 5))
      val meanshape = models.head.alignShapes
      println("Mean Shape \n"+meanshape)

    }

  }
}
