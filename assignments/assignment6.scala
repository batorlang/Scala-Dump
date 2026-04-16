package com.rockthejvm.assignments
//Bator Lang - 002233655
//AI-dec; No AI has been used in this assignment.
object assignment6 {
  //Task1
  def task1(list: List[Int]): List[Either[String, Int]]= {
    list.map { element =>
      if (lookingForPrimes(element)) {
        Right(element)
      } else {
        Left("Error: This is not a Prime number.")
      }
    }

  }

  def lookingForPrimes(a: Int): Boolean = {
    if (a <= 1) {false} else {
      def isPrime(y: Int): Boolean = {
        if (y == 1) true
        else if (a % y == 0) false
        else isPrime(y - 1)
      }

      isPrime(a - 1)
    }
  }
//Task2
  case class Student(name: String, age: Int, grade: Option[Int])

  def task2(list:List[Student]): Double={
    val flattened:List[Int] = list.flatMap(_.grade)
    flattened.sum / flattened.size

  }
  //task3
  def task3(pairs: List[(Int, Int)]): Int= {
    val res: List[Either[String, Int]] = pairs.map { case (a, b) =>
      if (b == 0) Left("Error: Cant divide by zero!")
      else Right(a / b)
    }
    val (succ, err) = res.partition(_.isRight)
    succ.foldLeft(0) { (accumulator, either) =>
      either match {
        case Right(value) => accumulator + value
        case _ => accumulator
      }
    }
  }

  //task4
  def task4(list:List[String]): Option[String]= {
    if (list.exists(_.toLowerCase.contains("error"))) None
    else Some(list.mkString(""))
  }


  def main(args: Array[String]): Unit= {
    //task1
    val ints = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val list = task1(ints)
    list.foreach(n => println(n))
    //task2
    val students = List(
      Student("S1", 20, Some(80)),
      Student("S2", 21, Some(90)),
      Student("S3", 22, None)
    )
    println(s"AVG of the existing grades: ${task2(students)}")
    //task3
    val nums = List((6,0), (8, 4), (9, 3), (7, 0))
    val sum = task3(nums)
    println(s"Results after partition: $sum")
    //task4
    val stringList1 = List("Hello", "this", "is", "Scala")
    task4(stringList1) match {
      case Some(concatenatedString) => println(concatenatedString)
      case None => println("Error has been found.")
        val stringList1 = List("errors", "apple", "giant")
        val result = task4(stringList1)
        result match {
          case Some(element) => println(s"Found the Error: $element")
          case None => println(result)
        }
    }
  }
}
