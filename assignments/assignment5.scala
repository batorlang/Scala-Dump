package com.rockthejvm.assignments
//Bator Balazs Lang - 002233655
//AI-declaration
object assignment5 {
  def task1[A: Numeric](list: List[A]): A = {

    }


  def makeAllSound(): Unit= {


    abstract class Animal {
      def makeSound(sound: String):String={sound}
    }
    abstract class Bird extends Animal {
      override def makeSound(sound: String): String = {
        sound
      }
    }
    abstract class Mammal extends Animal {
      override def makeSound(sound: String): String = {
        sound
      }
    }
  }



  def main(args: Array[String] ): Unit= {

  }

}
