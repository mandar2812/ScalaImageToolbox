package org.kuleuven.mai.vision.asm

import java.io.File
import breeze.linalg.{norm, sum, DenseVector}
import com.github.tototoshi.csv.CSVReader

/**
 * @author mandar2812
 */
object IncisorsAlign {
  def main(args: Array[String]): Unit = {
    val dataRoot = "data/Landmarks/original/"
    val imageRoot = "data/Radiographs/"
    //Load the landmarks for all 14 images
    //for every image, load the landmarks of
    //each incisor in a list
    val landmarks: List[(Int, List[DenseVector[Double]])] = (1 to 14).map{i =>
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

    val image_files = new java.io.File(imageRoot)
      .listFiles
      .filter(_.getName.endsWith(".tif"))

    val models = landmarksByTooth.map(i => ActiveShapeModel(i, image_files, 5))

    println("Number of teeth: "+models.length)
    println("Number of points for each tooth: "+
      models.head.getNormalizedShapes.length)

    println(models.head.getRawShapes(1))
    //println(models.head.getNormalizedShapes(1))
    println("After alignment with the first")
    val meanshape = models.head.alignShapes
    println(models.head.getNormalizedShapes(1))
    //println("Mean Shape: "+meanshape)
    //println("Carrying out PCA: ")
    //println(models.head.getNormalizedShapesAsMatrix * models.head.pca(20))
    //println("Number of points: "+models.head.numPoints+"\n")
    println("Fit for point: \n"+models(1).getRawShapes.head)
    println("Params: \n"+models.head.fit(models(1).getRawShapes.head))
    //val recon = models.head.reconstructData().head
    //println("Reconstructed Data: "+recon)
    //println("Dims: "+recon.length)

  }
}
