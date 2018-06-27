package com.haskworks.collections

import scala.collection.mutable

object ops {

  implicit class ListOps[+A](list: List[A]) {

    def stableGroupBy[K](f: A => K): List[(K, List[A])] = {
      val result = mutable.ListMap.empty[K, mutable.Builder[A, List[A]]]
      for (a <- list) {
        val k = f(a)
        if (result.contains(k)) {
          result(k) += a
        } else {
          val newBuilder = List.newBuilder[A]
          newBuilder += a
          result += (k -> newBuilder)
        }
      }
      result.map { case (k, v) => k -> v.result() }.toList
    }

  }

}
