package it.andrea.xmlparser

import com.codecommit.antixml.Elem
import shapeless.{HList, HNil, _}
import it.andrea.xml.XmlOps._
import scala.util.Random

trait Generator[A] {
  def generate: A
}

object XmlParser {
  def generate[A](implicit gen: Generator[A]) = {
    gen.generate
  }


  implicit def intGenerator = new Generator[Int] {
    override def generate = Random.nextInt
  }

  implicit def doubleGenerator = new Generator[Double] {
    override def generate = Random.nextDouble
  }

  implicit def StringGenerator = new Generator[String] {
    val loremWords = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.".split(" ")

    override def generate = Random.shuffle(loremWords.toList).take(5).mkString(" ")
  }

  implicit def booleanGenerator = new Generator[Boolean] {
    override def generate = Random.nextBoolean
  }

  implicit def elemGenerator = new Generator[Elem] {
    override def generate: Elem = <Ciao A="aaaaa"/>
  }

  implicit def hnilGenerator: Generator[HNil] = new Generator[HNil] {
    override def generate = HNil
  }

  implicit def hconsGenerator[H, T <: HList](implicit headGen: Generator[H], tailGen: Generator[T]): Generator[H :: T] =
    new Generator[H :: T] {
      override def generate = headGen.generate :: tailGen.generate
    }

  implicit def genericToGenerator[T, L <: HList](implicit generic: Generic.Aux[T, L], lGen: Generator[L]): Generator[T] =
    new Generator[T] {
      override def generate = generic.from(lGen.generate)
    }

}
