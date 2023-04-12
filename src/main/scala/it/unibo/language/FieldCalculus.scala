package it.unibo.language
import it.unibo.core.VM
object FieldCalculus:

  def branch[A](condition: => Boolean)(ifTrue: => A)(ifFalse: => A)(using vm: VM): A = ???

  def rep[A](init: => A)(loop: A => A)(using vm: VM): A = ???

  def nbr[A](value: => A)(using vm: VM): Map[Int, A] = ???
