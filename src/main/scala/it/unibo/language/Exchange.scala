package it.unibo.language
import it.unibo.core.{VM, Tag, Empty, Local}
object Exchange:
  private val exchangeTag = Tag("exchange")
  def exchange[A: Reader: Writer](
      init: => A
  )(logic: (Map[Int, A]) => (Map[Int, A], Map[Int, A]))(using vm: VM): Map[Int, A] =
    vm.enter(exchangeTag)
    // eval only once
    val default = init
    // unmarshalling received data
    val receivedUnmarshalled = vm.received
      .map { case (id, data) => (id, summon[Reader[A]](data)) }
    val alignedNeighborhoodData = Map(vm.context.self -> default) ++ receivedUnmarshalled
    val (ret, toSend) = logic(alignedNeighborhoodData)
    val toSendMarshalled = toSend.map { case (id, data) => (id, summon[Writer[A]](data)) }
    val notifications = vm.context.neighbours
      .map(id => id -> summon[Writer[A]](default)) // send default value to all neighbours, to align the neighbourhood
      .toMap ++ toSendMarshalled
    vm.send(notifications)
    vm.exit()
    ret

  def exchangeRetSend[A: Reader: Writer](init: => A)(logic: Map[Int, A] => Map[Int, A])(using vm: VM): Map[Int, A] = {
    exchange(init) { neighValue =>
      val ret = logic(neighValue)
      (ret, ret)
    }
  }

  // just an example of how to use exchange to implement a rep, please use the FP version
  def rep[A: Reader: Writer](init: => A)(loop: A => A)(using vm: VM): A =
    import FieldCalculus.mid
    val result = exchangeRetSend[A](init) { neighValue =>
      Map(mid -> loop(neighValue(mid)))
    }
    result(mid)

  def nbr[A: Reader: Writer](data: => A)(using vm: VM): Map[Int, A] = exchange(data) { neighValue =>
    (neighValue, neighValue.map(_._1 -> data))
  }
