package com.saacole.primefactors

object PrimeFactors {

  def result(int: Int): List[Int] = {
    def inner(number: Int, currentInt: Int = 2, acc: List[Int] = List.empty): List[Int] = if (currentInt <= number) {
      if (isPrime(number)) acc :+ number
      else if (isWholeNumber(number.toFloat / currentInt)) inner(number / currentInt, currentInt, acc :+ currentInt)
      else inner(number, currentInt + 1, acc)
    } else acc

    inner(int)
  }

  private def isWholeNumber(f: Float): Boolean = f % 1 == 0

  private def isPrime(int: Int): Boolean = {
    if (int <= 1) false
    else if (int == 2) true
    else !(2 until int).exists(x => int % x == 0)
  }
}
