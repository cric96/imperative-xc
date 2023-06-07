package it.unibo.language
import it.unibo.core.{VM, Tag, Empty, Local}
object Exchange {
  def exchange[A: Reader: Writer](
      init: => A
  )(logic: (Map[Int, A]) => (Map[Int, A], Map[Int, A]))(using vm: VM): Map[Int, A] = {
    vm.enter(Tag("exchange"))
    val default = init
    val aligned = Map(
      vm.context.self -> default
    ) ++ vm.received
      .map { case (id, data) => (id, summon[Reader[A]](data)) }
    val neighValue = aligned
    val (ret, toSend) = logic(neighValue)
    val notification: Map[Int, Local] = vm.context.neighbours.map { id =>
      id -> summon[Writer[A]](default)
    }.toMap ++ toSend.map { case (id, data) => id -> summon[Writer[A]](data) }
    vm.send(notification)
    vm.exit()
    ret
  }

  def exchangeRetSend[A: Reader: Writer](init: => A)(logic: Map[Int, A] => Map[Int, A])(using vm: VM): Map[Int, A] = {
    exchange(init) { neighValue =>
      val ret = logic(neighValue)
      (ret, ret)
    }
  }

  def rep[A: Reader: Writer](init: => A)(loop: A => A)(using vm: VM): A =
    import FieldCalculus.mid
    val result = exchangeRetSend[A](init) { neighValue =>
      Map(mid -> loop(neighValue(mid)))
    }
    result(mid)

  def nbr[A: Reader: Writer](data: => A)(using vm: VM): Map[Int, A] = exchange(data) { neighValue =>
    (neighValue, neighValue.map(_._1 -> data))
  }
}
