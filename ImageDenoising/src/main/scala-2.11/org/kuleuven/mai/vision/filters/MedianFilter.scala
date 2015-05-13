package org.kuleuven.mai.vision.filters

import org.kuleuven.mai.vision.utils

/**
 * @author mandar2812
 */
class MedianFilter extends
Filter[Iterable[(Int, Int, Int, Int)], List[List[Int]]]{
  override def evaluate(window: Iterable[(Int, Int, Int, Int)]): Array[Int] =
    Array(window.map(_._1), window.map(_._2), window.map(_._3), window.map(_._4))
      .map{l => utils.median(l.map{_.toDouble}.toSeq).toInt}
}
