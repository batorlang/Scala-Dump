package com.rockthejvm.assignments
//Bator Lang - 002233655
//AI-declaration: I have not used AI in this assignment

object Assignment2 {
  def addition(a:Int, b:Int): Int = {//this function increments on of the integers one by one while decrements the other and then returns the number that is not 0
    if (b == 0) a
    else if (b > 0) {
      addition(a + 1, b - 1)
    }
    else
    {
      addition(a - 1, b + 1)
    }
  }


  def subtraction(a:Int, b:Int): Int = {//this function uses addition logic to subtraction
    if (b==0) a
    else addition(a, -b)
  }

  def multiplication(a:Int, b:Int): Int = {//multiplication works like that it adds the number as many times as the other number decrements and reaches zero.
    if (b==0) 0
    else if (b>0) addition(a, multiplication(a, b - 1))
    else addition(-a,multiplication(a, b + 1))
  }

  def division(a:Int, b:Int): Int = {
    if (b==0) throw new ArithmeticException("Can not divide by zero")
    else if(a==0) 0
    else if (a < 0 && b < 0) division(-a, -b)//both negative = both positive.
    else if (a < 0) -division(-a, b)
    else if (b < 0) -division(a, -b)
    else if (a < b) 0
    else subtraction(division(subtraction(a,b), b), -1)

  }

  def factorial(a:Int): Int = {
    if (a <= 1) 1
    else a * factorial(a-1)
  }

  def isPrime(a: Int): Boolean = {
    if (a <= 1) false //1 is not a prime number
    def isPrimeUntil(y:Int): Boolean= {
      if (y == 1) true
      else if(a % y ==0) false
      else isPrimeUntil(y-1)
    }
    isPrimeUntil(a -1)


  }



  def task7(): Unit = {
    //From the way how loops work, loops reserve a stack of memory and there is no additional memory reserved.
    //By this using loops have O(1) time complexity. Loops are memory efficient, because of no new memory allocations needed.
    //If we take a for loop that as to add a list of numbers, then the program knows the exact length of the list and can allocate memory accordingly, meeting each element exactly once.
    //Standard recursion on the other hand has O(n) time complexity, because throughout the execution the program stops and has to allocate a new stack of memory every time the function has to call itself.
    //For example in this exercise the multiplication reserves a stack of memory, and when it calls itself again, it has to allocate another stack for the next iteration, because the program has to run through the if statements.
    //There is a possibility that too many stacks are allocated and the result is "StackOverFlow".
  }

  def task8(): Unit = {
    //Variables can be defined at class or object level, also in a function level. Global scope means variables (var or val) are defined in a class and every function can access them and some functions can actually modify them if mutable.
    //Example could be for example in this code, integer a and b, which could be accessed by every function and some even modify them inside the function to produce output.
    //Local scope means the variable is defined inside a function, that is not returned, so it is not accessible or modifiable by other functions.
    //An example could be defining pi in a function which calculates area of a circle. No other function can us it, but the one it is defined in, uses it and produces output.
  }


  def main(args: Array[String]): Unit = {
    val a: Int = 9
    val b: Int = 0
    println(addition(a, b))
    println(subtraction(a, b))
    println(multiplication(a, b))
    println(division(a,b))
    println(factorial(a))
    println(isPrime(a))

  }

}

