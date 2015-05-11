package org.kuleuven.mai.vision.image

import java.io.File
import com.sksamuel.scrimage.io.TiffReader
import org.apache.commons.io.FileUtils

import scala.collection.mutable


/**
 * @author mandar & romain
 */
object SmoothImages {
  def main(args: Array[String]): Unit = {

    val dataRoot = "data/Radiographs/01.tif"

    val image = TiffReader.read(FileUtils.openInputStream(new File(dataRoot)))

    println(s"Width: ${image.width} Height: ${image.height} Ratio: ${image.ratio}")

    val im: List[mutable.MutableList[Int]] = List.tabulate(image.width) { i =>
      mutable.MutableList.tabulate(image.height) {j =>
        image.pixel(i, j)
      }
    }


    val obj = new RadioGraph(im,2)
    obj.denoising

    /*var imge :BufferedImage = new BufferedImage(
      3023,1500 , BufferedImage.TYPE_INT_RGB )
     imge  = List.tabulate(imge.getWidth) { i =>
      mutable.MutableList.tabulate(imge.getHeight) { j =>
        imge.....(j,i) = obj.denoising(i)(j)
      }
    }
   val  outputfile : File = new File("data/Radiographs/image.tif")
        ImageIO.write(imge ,"tif", outputfile)*/

  }
}
