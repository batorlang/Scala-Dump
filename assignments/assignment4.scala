package com.rockthejvm.assignments
import scala.io.Source
import scala.util.Using
//Bator Balazs Lang - 002233655
//AI-declaration: I have not used any AI tools for this assignment.
object assignment4 {
  def task0(): Unit = {
    val nums: List[Int] = (1 to 10).toList
    val squares: List[Int] = nums.map(n => n*n)
    val evenSquares: List[Int] = squares.filter(n=>n % 2 == 0)
    val evenSet: Set[Int] = nums.filter(n=>n % 2 == 0).toSet
    val sum_evenSet: Int = evenSet.foldLeft(0)((acc, n)=>acc + n)
    val squaresMap: Map[Int, Int] = nums.map(n=>(n,n*n)).toMap
    val sqrEleven: Int = squaresMap.getOrElse(11, 0)

    println(s"nums: $nums")
    println(s"squares: $squares")
    println(s"evenSquares: $evenSquares")
    println(s"evenSet: $evenSet")
    println(s"sum_evenSet: $sum_evenSet")
    println(s"squaresMap: $squaresMap")
    println(s"sqrEleven: $sqrEleven")
  }

  def removeDuplicates(seq: Seq[String]): Seq[String] = {
    seq.foldLeft(Seq.empty[String]) { (newSeq, currentString) =>
      if (newSeq.contains(currentString)) {
        newSeq
      } else {
        newSeq :+ currentString
      }
    }
  }

  def findCommonElements[A](set1: Set[A], set2: Set[A]): Set[A] = {
    set1.intersect(set2)
  }

  def removeKeys[K, V](map: Map[K, V], keyRemove: List[K]): Map[K, V]= {
    map -- keyRemove
  }
  def task4(a: String): Map[String, Int] = {
    Using(Source.fromFile(a)) {source =>
      source
        .getLines()
        .flatMap(_.toLowerCase.split(" "))
        .filter(_ != "")
        .foldLeft(Map.empty[String, Int]) {(current, word) =>
          val count = current.getOrElse(word, 0)
          current + (word -> (count + 1))
        }
    }.getOrElse(Map.empty[String, Int])
  }


  def main(args: Array[String]): Unit={
    task0()
    //task1
    val sequence = Seq("hello", "world", "hello", "scala", "world")
    val outcome = removeDuplicates(sequence)
    println(s"Example Input:  $sequence")
    println(s"Example Output: $outcome")
    //task2
    val setA = Set(1, 2, 3, 4 ,5)
    val setB = Set(4, 5, 6, 7, 8)
    val resulting_set = findCommonElements(setA, setB)
    println(s"Example Input:  $setA & $setB")
    println(s"Example Output: $resulting_set")
    //task3
    val mainMap = Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)
    val toDrop = List("b", "c")
    val removed = removeKeys(mainMap, toDrop)
    println(s"Original Map: $mainMap")
    println(s"Removed keys Map: $removed")
    //task4
    val filename = "input.txt"
    val countOfWords = task4(filename)
    countOfWords.foreach {case (word, count) =>
    println(f"$word%-12s $count")}

  }
}
