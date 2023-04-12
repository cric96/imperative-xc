package it.unibo.language
import it.unibo.core.VM
object FieldCalculus {

  def branch[A](condition: => Boolean)(ifTrue: => A)(ifFalse: => A)(using vm: VM): A = {
    val evalCondition = condition
    val scope = vm.alignEnter(evalCondition)
    val result = if (condition) ifTrue else ifFalse
    vm.exit(scope.completeWith(result)).asInstanceOf[A]
  }

  def rep[A](init: => A)(loop: A => A)(using vm: VM): A = {
    val scope = vm.exchangeEnter().withDefaultValue(init)
    val me = scope.aligned.getOrElse(vm.context.self, scope.default)
    val updated = Map(vm.context.self -> loop(me.asInstanceOf[A]))
    vm.exit(scope.completeWith(updated)).asInstanceOf[A]
  }

  def nbr[A](value: => A)(using vm: VM): Map[Int, A] = {
    val scope = vm.exchangeEnter()
    val local = value
    val updated = scope.aligned
    vm.exit(scope.withDefaultValue(local).completeWith(updated)).asInstanceOf[Map[Int, A]]
  }
}
