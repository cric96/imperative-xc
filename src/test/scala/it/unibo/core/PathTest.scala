package it.unibo.core
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PathTest extends AnyFlatSpec with Matchers {
  "A Path" should "be empty when created" in {
    val path = Path()
    path.path shouldBe List()
  }

  it should "throw an exception when pull is called on an empty path" in {
    val path = Path()
    a[Exception] should be thrownBy path.pull()
  }

  it can "be updated with a new Slot" in {
    val path = Path()
    val slot = Slot.Exchange(0)
    val newPath = path.push(Slot.Exchange(0))
    newPath.path shouldBe List(slot)
  }

  it must "put new element in head" in {
    val path = Path()
    val firstSlot = Slot.Exchange(0)
    val secondSlot = Slot.Exchange(1)
    val newPath = path.push(firstSlot).push(secondSlot)
    newPath.path.head shouldBe secondSlot
  }

  it must "remove the head when pull is called" in {
    val path = Path()
    val firstSlot = Slot.Exchange(0)
    val secondSlot = Slot.Exchange(1)
    val newPath = path.push(firstSlot).push(secondSlot)
    val newPath2 = newPath.pull()
    newPath2 shouldBe Path(firstSlot :: Nil)
  }

}
