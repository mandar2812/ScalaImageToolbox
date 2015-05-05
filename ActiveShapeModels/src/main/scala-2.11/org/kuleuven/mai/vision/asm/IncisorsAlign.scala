package org.kuleuven.mai.vision.asm

import java.io.File
import breeze.linalg.DenseVector
import com.github.tototoshi.csv.CSVReader
import scala.collection.mutable.MutableList

/**
 * @author mandar2812
 */
object IncisorsAlign {
  def main(args: Array[String]): Unit = {
    val dataRoot = "data/Landmarks/original/"
    //Load the landmarks for all 14 images
    //for every image, load the landmarks of
    //each incisor in a list

    val landmarks: List[List[List[DenseVector[Double]]]] = List.tabulate(14){i =>
      List.tabulate(8){j =>
        val file = dataRoot+"landmarks"+(i+1)+"-"+(j+1)+".txt"
        val points = new MutableList[DenseVector[Double]]()
        val reader = CSVReader.open(new File(file))
        val it = reader.iterator

        (1 to 40).foreach{point =>
          points += DenseVector(it.next().head.toDouble,
            it.next().head.toDouble)
        }
        reader.close()
        points.toList
      }
    }
    println("The loaded landmarks are: "+landmarks)

  }
}
