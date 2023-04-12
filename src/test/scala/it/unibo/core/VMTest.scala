package it.unibo.core
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.{*, given}
class VMTest extends AnyFlatSpec with Matchers {
  val emptyContext = Context(List.empty, 0)
  "VMStatus" should "have an empty current path, index = 0" in {
    val status = VMStatus()
    status.path shouldBe Path()
    status.index shouldBe 0
  }

  it should "support index increment" in {
    val status = VMStatus()
    status.incrementIndex().index shouldBe 1
    status.incrementIndex().incrementIndex().index shouldBe 2
  }

  it should "push the current state in the status" in {
    val status = VMStatus()
    val pushed = status.push()
    pushed.index shouldBe 0
    pushed.path shouldBe Path()
  }

  it should "support nesting" in {
    val status = VMStatus()
    val pushed = status.push().nest(Slot(0, status.index))
    pushed.path shouldBe Path(List(Slot(0, 0)))
    pushed.index shouldBe 0
  }

  "VM" should "be empty at the beggining" in {
    val vm = VM(emptyContext)
    vm.status.path shouldBe Path()
    vm.status.index shouldBe 0
  }
}
