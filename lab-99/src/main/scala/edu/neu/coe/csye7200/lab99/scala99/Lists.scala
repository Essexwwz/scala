/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.lab99.scala99

import edu.neu.coe.csye7200.lab99.scala99.P01.last

import scala.annotation.tailrec

object P00 {
  def flatten[X](xss: List[List[X]]): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], wss: List[List[X]]): List[X] = wss match {
      case Nil => r
      case h :: t => inner(r ++ h, t)
    }

    inner(Nil, xss)
  }

  def fill[X](n: Int)(x: X): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }
}

object P01 {

  @scala.annotation.tailrec
  def last[X](xs: List[X]): X = xs match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
  }

}

object P02 {

  @scala.annotation.tailrec
  def penultimate[X](xs: List[X]): X = xs match {
    case h ::_:: Nil  => h
    case _ :: tail => penultimate(tail)
    case _         => throw new NoSuchElementException
  }
}

object P03 {

  @scala.annotation.tailrec
  def kth[X](k: Int, xs: List[X]): X =(k, xs) match {
    case (0, h :: _   ) => h
    case (n, _ :: tail) => kth(n - 1, tail)
    case (_, Nil      ) => throw new NoSuchElementException
  }

}

object P04 {

  def length[X](xs: List[X]): Int = {
    @tailrec
    def inner(result: Int, _xs: List[X]): Int = {
      _xs match {
        case Nil => result
        case _ :: t => inner(result + 1, t)
      }
    }

    inner(0, xs)
  }
}

object P05 {

  def reverse[X](xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    def reverseR(result: List[X], curList: List[X]): List[X] = curList match {
      case Nil       => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, xs)
  }
}

object P06 {

  // inefficient solution
  def isPalindrome[X](xs: List[X]): Boolean = (xs==xs.reverse) // TO BE IMPLEMENTED
}

object P07 {

  type ListAny = List[Any]

  def flatten(xs: ListAny): ListAny = xs flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }
}

object P08 {

  def compress[X](xs: List[X]): List[X] = {
    // TO BE IMPLEMENTED
    ???
  }
}

object P09 {

  def pack[X](xs: List[X]): List[List[X]] = {
    // TO BE IMPLEMENTED
    if (xs.isEmpty) List(List())
    else {
      val (packed, next) = xs span { _ == xs.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
}

object P10 {
  import P09.pack
  def encode[X](xs: List[X]): List[(Int, X)] = pack[X](xs) map { e => (e.length, e.head) } // TO BE IMPLEMENTED
}

object P11 {
  import P10.encode
  def encodeModified[X](xs: List[X]): List[Any] = encode(xs) map { t => if (t._1 == 1) t._2 else t } // TO BE IMPLEMENTED
}

object P12 {

  def decode[X](xIs: List[(Int, X)]): List[X] = xIs flatMap { e => List.fill(e._1)(e._2) } // TO BE IMPLEMENTED
}

object P13 {

  def encodeDirect[X](xs: List[X]): List[(Int, X)] = {
    // TO BE IMPLEMENTED
    if (xs.isEmpty) Nil
    else {
      val (packed, next) = xs span {
        _ == xs.head
      }
      (packed.length, packed.head) :: encodeDirect(next)
    }
  }

  object P14 {

    def duplicate[X](xs: List[X]): List[X] = xs flatMap { e => List(e, e) }
  }

  object P15 {

    def duplicateN[X](n: Int, xs: List[X]): List[X] = {
      // TO BE IMPLEMENTED
      xs flatMap {
        List.fill(n)(_)
      }
    }
  }
}
