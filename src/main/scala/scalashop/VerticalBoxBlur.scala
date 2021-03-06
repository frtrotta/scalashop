package scalashop

import org.scalameter._
import common._

import scala.concurrent.forkjoin.ForkJoinTask

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 4
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasksA = 2
    val partimeA = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasksA, radius)
    }
    println(s"fork/join $numTasksA time: $partimeA ms")
    println(s"speedup: ${seqtime / partimeA}")

    val numTasksB = 32
    val partimeB = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasksB, radius)
    }
    println(s"fork/join $numTasksB blur time: $partimeB ms")
    println(s"speedup: ${seqtime / partimeB}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    var x = from
    while (x < end) {
      var y = 0
      while (y < src.height) {
        dst(x,y) = boxBlurKernel(src, x, y, radius)
        y += 1
      }
      x += 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    if (numTasks == 0)
      blur(src, dst, 0, src.width, radius)
    else {
      val stripWidth: Int = src.width / numTasks

      val tasks: List[java.util.concurrent.ForkJoinTask[Unit]] = ((0 until numTasks) map { case i => task(blur(src, dst, i * stripWidth, (i + 1) * stripWidth, radius)) }).toList

      if (src.width % numTasks > 0) blur(src, dst, src.width / numTasks * numTasks, src.width, radius)

      tasks map (t => t.join)
    }
  }

}
