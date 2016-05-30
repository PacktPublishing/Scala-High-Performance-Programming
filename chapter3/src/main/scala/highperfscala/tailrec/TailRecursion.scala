package highperfscala.tailrec

import java.io.{BufferedReader, IOException}

import scala.annotation.tailrec

object TailRecursion {

  // This function cannot be marked @tailrec since the call to `sum`
  // is not the last instruction
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }

  def tailrecSum(l: List[Int]): Int = {
    @tailrec
    def loop(list: List[Int], acc: Int): Int = list match {
      case Nil => acc
      case x :: xs => loop(xs, acc + x)
    }
    loop(l, 0)
  }

  def sum2(l: List[Int]): Int = {

    def loop(list: List[Int], acc: Int): Int = list match {
      case Nil => acc
      case x :: xs => info(xs, acc + x)
    }

    def info(list: List[Int], acc: Int): Int = {
      println(s"${list.size} elements to examine. sum so far: $acc")
      loop(list, acc)
    }

    loop(l, 0)
  }

  def tailrecSum2(l: List[Int]): Int = {

    @tailrec
    def loop(list: List[Int], acc: Int): Int = list match {
      case Nil => acc
      case x :: xs =>
        println(s"${list.size} elements to examine. sum so far: $acc")
        loop(list, acc)
    }

    loop(l, 0)
  }

  def sumFromReader(br: BufferedReader): Int = {

    def read(acc: Int, reader: BufferedReader): Int = {
      Option(reader.readLine().toInt)
        .fold(acc)(i => read(acc + i, reader))
    }

    read(0, br)
  }

  def tailrecSumFromReader(br: BufferedReader): Int = {
    @tailrec
    def read(acc: Int, reader: BufferedReader): Int = {
      val opt = Option(reader.readLine().toInt)
      if (opt.isEmpty) acc else read(acc + opt.get, reader)
    }

    read(0, br)
  }

  class Printer(msg: String) {
    def printMessageNTimes(n: Int): Unit = {
      if (n > 0) {
        println(msg)
        printMessageNTimes(n - 1)
      }
    }
  }

  class TailRecPrinter(msg: String) {
    @tailrec
    final def printMessageNTimes(n: Int): Unit = {
      if (n > 0) {
        println(msg)
        printMessageNTimes(n - 1)
      }
    }
  }

  def tryCatchBlock(l: List[Int]): Int = {
    def loop(list: List[Int], acc: Int): Int = list match {
      case Nil => acc
      case x :: xs =>
        try {
          loop(xs, acc + x)
        } catch {
          case e: IOException =>
            println(s"Recursion got interrupted by exception")
            acc
        }
    }


    loop(l, 0)
  }

}
