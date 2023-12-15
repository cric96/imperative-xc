package it.unibo.core
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import it.unibo.util.{*, given}
import it.unibo.core.{Tag => TreeTag}
class VMTest extends AnyFlatSpec with Matchers:
  val emptyContext = Context(Map.empty, 0)
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
    val pushed = status.push().nest(Slot(TreeTag(0), status.index))
    pushed.path shouldBe Path(List(Slot(TreeTag(0), 0)))
    pushed.index shouldBe 0
  }

  "VM" should "be empty at the beginning" in {
    val vm = VM(emptyContext)
    vm.status.path shouldBe Path()
    vm.status.index shouldBe 0
  }

  "VM" should "support call to enter" in {
    val vm = VM(emptyContext)
    vm.enter(TreeTag(0))
    vm.status.path shouldBe Path(Slot(TreeTag(0), 0))
    vm.status.index shouldBe 0
  }

  "VM" should "support enter and exiting from scopes" in {
    val vm = VM(emptyContext)
    vm.enter(TreeTag(0))
    vm.exit()
    vm.status.index shouldBe 1
    vm.status.path shouldBe Path()
  }

  "VM" should "support nesting" in {
    val vm = VM(emptyContext)
    vm.enter(TreeTag(0))
    vm.enter(TreeTag(1))
    vm.status.path shouldBe Path(Slot(TreeTag(1), 0), Slot(TreeTag(0), 0))
    vm.status.index shouldBe 0
  }

  "VM" should "support several call at the same level" in {
    val vm = VM(emptyContext)
    vm.enter(TreeTag(0))
    vm.status.path shouldBe Path(Slot(TreeTag(0), index = 0))
    vm.exit()
    vm.enter(TreeTag(1))
    vm.status.path shouldBe Path(Slot(TreeTag(1), index = 1))
    vm.exit()
    vm.enter(TreeTag(2))
    vm.status.path shouldBe Path(Slot(TreeTag(2), index = 2))
    vm.exit()
    vm.status.path shouldBe Path()
    vm.status.index shouldBe 3
  }

  "VM" should "align data from neighbors" in {
    val context = Context(
      Map(
        0 -> Export(Map(Path(Slot(TreeTag(0), index = 0)) -> 2)),
        1 -> Export(Map(Path(Slot(TreeTag(0), index = 0)) -> 2)),
        2 -> Export(Map(Path(Slot(TreeTag(0), index = 0)) -> 3))
      ),
      self = 2
    )
    val vm = VM(context)
    vm.enter(TreeTag(0))
    vm.received shouldBe Map[Int, Local](0 -> 2, 1 -> 2, 2 -> 3)
  }

  "VM" should "align only the neighbours with the same path" in {
    val context = Context(
      Map(
        0 -> Export(Map(Path(Slot(TreeTag(0), index = 0)) -> 2)),
        1 -> Export(Map(Path(Slot(TreeTag(1), index = 0)) -> 2)),
        2 -> Export(Map(Path(Slot(TreeTag(0), index = 0)) -> 3))
      ),
      self = 2
    )
    val vm = VM(context)
    vm.enter(TreeTag(0))
    vm.received shouldBe Map[Int, Local](0 -> 2, 2 -> 3)
  }

  "VM" should "create an alignment path after sending a neighbourhood value" in {
    val vm = VM(emptyContext)
    vm.enter(TreeTag(0))
    vm.send(Map(1 -> 0))
    vm.exit()
    vm.outExports shouldBe Map(1 -> Export(Map(Path(Slot(TreeTag(0), 0)) -> 0)))
  }

  "VM" should "support multiple sent in different nesting level" in {
    val vm = VM(emptyContext)
    vm.enter(TreeTag(0))
    vm.send(Map(1 -> 0))
    vm.exit()
    vm.enter(TreeTag(1))
    vm.send(Map(2 -> 0))
    vm.enter(TreeTag(2))
    vm.send(Map(3 -> 0, 1 -> 1))
    vm.exit()
    vm.exit()
    val result = Map(
      1 -> Export(Map(Path(Slot(TreeTag(0), 0)) -> 0, Path(Slot(TreeTag(2), 0), Slot(TreeTag(1), 1)) -> 1)),
      2 -> Export(Map(Path(Slot(TreeTag(1), 1)) -> 0)),
      3 -> Export(Map(Path(Slot(TreeTag(2), 0), Slot(TreeTag(1), 1)) -> 0))
    )
    result shouldBe vm.outExports
  }

  "VM" should "be coherent in different rounds" in {
    val vm = VM(emptyContext)
    vm.enter(TreeTag(2))
    vm.store(10)
    vm.received shouldBe Map.empty
    vm.exit()
    val exports = vm.outExports
    val nextVm = VM(Context(exports, emptyContext.self))
    nextVm.enter(TreeTag(2))
    nextVm.received shouldBe Map[Int, Local](emptyContext.self -> 10)
    nextVm.exit()
    nextVm.received shouldBe Map.empty
  }
