package com.rockthejvm.assignments
//Bator Balazs Lang
//002233655

//AI-declaration: I have not used any AI tool in this assignment
object assignment3 {
  //Task1
  def operateOnList (oldList: List[Int], x:Function[Int, Int]): List[Int]= {
    if (oldList.isEmpty) oldList
    else {
      val newList: List[Int] = x(oldList.head) :: operateOnList(oldList.tail, x)
      newList
    }
  }
  def oddToEven(int1: Int): Int = {
    if (int1%2 ==0) int1
    else
      int1 + 1
  }
  //Task2
  def filterAndMap(List1: List[String], x:Function[String, Boolean], y:Function[String, String]): List[String]={
    if (List1.isEmpty) List1
    else if (!x(List1.head)) filterAndMap(List1.tail, x, y)
    else concat(List1.head) :: filterAndMap(List1.tail, x, y)
  }
  def greaterLength(s:String): Boolean= {
    if (s.length >= 6) true
    else false
  }
  def concat(s:String): String= {
    s+s"${s.length}"
  }
  //Task3
  def listTransformation[A, B](list: List[A], transformFunc: A => B): List[B]={
    if (list.isEmpty) Nil //I return Nil, because of the undefined list element types, and in this case the list we return would be empty.
    else
      transformFunc(list.head) :: listTransformation(list.tail, transformFunc)
  }
  val stringToInteger: String => Int = (str: String) => str.length
  val IntegerToDouble: Int => Double = (int: Int) => int*2.toDouble
  //Task4

  def task4(int1: Int)(int2: Int)(int3: Int): Int={
    int1*int2*int3
  }



  def main(args: Array[String]): Unit= {
    val int1: Int = 2
    val int2: Int = 4
    val int3: Int = 6
    val Numbers = List(1, 2, 3, 4, 5)
    val Animals = List("Kutya", "Elefant", "Marci", "Megszentsegtelenihetetlensegeskedeseitekert")
    println(s"The new list is: ${operateOnList(Numbers, oddToEven)}")
    println(s"The New String List: ${filterAndMap(Animals, greaterLength, concat)}")
    println(s"Transformed lift of numbers ${listTransformation(Numbers, IntegerToDouble)}")
    println(s"Transformed lift of strings ${listTransformation(Animals, stringToInteger)}")
    println(s"The three integers product is ${task4(int1)(int2)(int3)}")
    val partial = task4(10) _
    println(s"Partial test 1: ${partial(int1)(int2)}")
    println(s"Partial test 2: ${partial(int2)(int3)}")
  }
}