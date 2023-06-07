package it.unibo.language
import it.unibo.core.{Local, Tag, VM}

import java.util.concurrent.ExecutorService

// API -- Language specific
object FieldCalculus:
  val repTag = Tag("rep")
  val nbrTag = Tag("nbr")
  def branchTag(condition: Boolean) = Tag("branch-" + condition)

  def branch[A](condition: => Boolean)(ifTrue: => A)(ifFalse: => A)(using vm: VM): A = {
    val evalCondition = condition
    vm.enter(branchTag(evalCondition))
    val data = if (evalCondition) ifTrue else ifFalse
    vm.exit()
    data
  }

  def rep[A: Writer: Reader](init: => A)(loop: A => A)(using vm: VM): A =
    vm.enter(repTag)
    val local = vm.received.get(vm.context.self)
    val current = local match {
      case None => init
      case Some(data) => summon[Reader[A]](data)
    }
    val updated = loop(current)
    vm.store(summon[Writer[A]](updated))
    updated

  def nbr[A: Reader: Writer](value: => A)(using vm: VM): Map[Int, A] =
    vm.enter(nbrTag)
    val local = summon[Writer[A]](value)
    val toSend = vm.context.neighbours.map(_ -> local).toMap
    vm.send(toSend)
    vm.received.map { case (id, local) => (id, summon[Reader[A]](local)) }

  def mid(using vm: VM): Int = vm.context.self
