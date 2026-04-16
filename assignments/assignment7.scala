package com.rockthejvm.assignments
import scala.annotation.tailrec
//Bator Balazs Lang - 002233655
//AI-declaration: I have not used any AI tools in this assignment
object assignment7 {
  def task1(list1: List[Int], list2: List[Int]): Int = {
    @tailrec
    def sumLists(l1: List[Int], l2: List[Int], accumulator: Int): Int = (l1, l2) match {
      case (head1 :: tail1, _) => sumLists(tail1, l2, accumulator + head1)
      case (Nil, head2 :: tail2) => sumLists(Nil, tail2, accumulator + head2)
      case (Nil, Nil) => accumulator
    }
    sumLists(list1, list2, 0)
  }

  def task2(list: List[String]): Option[String]= {
    if (list.exists(string => string.contains("error"))) None
    else Some(list.foldLeft("")((acc, string) => acc + string))
  }


//Task3
  //Through my journey with Scala and Functional Programming, I had great hardships, to figure out how to perform a
  //simple task, where I can not use loops, but recursion.
  //On the other hand I found functions and their return value type declaration extremely useful, since with this help
  //it makes the output predictable, no explicit need to check for return type, and most errors are avoided. Not to mention
  //I have to spend way less time to spend on finding bugs related to variables due to immutability.


  def main(args: Array[String]): Unit= {
    //Task1
    val list1 = List(1, 2, 3, 4, 5)
    val list2 = List(6, 7, 8, 9, 10)
    println(task1(list1, list2))
    //Task2
    val goodList = List("apple", "banana", "lemon")
    val badList = List("error", "apple", "banana")
    println(task2(goodList))
    println(task2(badList))



  }

}
