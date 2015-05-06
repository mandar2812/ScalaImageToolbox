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
    //Load the landmarks for all 14 images
    //for every image, load the landmarks of
    //each incisor in a list

    val landmarks: List[List[DenseVector[Double]]] = List.tabulate(14){i =>
      List.tabulate(8){j =>
        val file = dataRoot+"landmarks"+(i+1)+"-"+(j+1)+".txt"
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
    }

    val landmarksByTooth = List.tabulate(8){tooth =>
      landmarks.map{_(tooth)}
    }

    val models = landmarksByTooth.map(i => new ActiveShapeModel(i))

    println("Number of teeth: "+models.length)
    println("Number of points for each tooth: "+
      models.head.getNormalizedShapes.length)


    println(models.head.getRawShapes(1))
    println(models.head.getNormalizedShapes(1))
    println("After alignment with the first")
    models.head.align()
    println(models.head.getNormalizedShapes(1))

  }
}
