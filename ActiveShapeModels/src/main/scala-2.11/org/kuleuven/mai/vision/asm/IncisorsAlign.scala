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

    println("Centering the data points: ")

    val centeredLandmarks = landmarks.map((image) => {
      image.map((vector) => {
        val num: Int = vector.length/2
        val x = vector(0 to num - 1)
        val y = vector(num to vector.length - 1)
        val xmean = sum(x)/num
        val ymean = sum(y)/num
        x :-= xmean
        y :-= ymean
        DenseVector.vertcat(x,y)
      })
    })

    val scaledLandmarks = centeredLandmarks.map{image =>
      image.map{vector =>
        vector :/= norm(vector, 2)
      }
    }

    println("Scaled Landmarks: "+scaledLandmarks)
  }
}
