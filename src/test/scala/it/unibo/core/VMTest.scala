package it.unibo.core
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
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
    val pushed = status.push().nest(Slot.Align(0, status.index))
    pushed.path shouldBe Path(List(Slot.Align(0, 0)))
    pushed.index shouldBe 0
  }

  it can "support a standard stack operation (push on the stack, perform a nesting, popping on the stack)" in {
    val status = VMStatus()
    val nest = status.push().nest(Slot.Exchange(status.index))
    nest.index shouldBe 0
    nest.path shouldBe Path(List(Slot.Exchange(status.index)))
    val pop = nest.pop()
    pop.index shouldBe 0
    pop.path shouldBe Path()
  }

  "VM" should "have an empty status at the beginning" in {
    val vm = VM(emptyContext)
    vm.status.path shouldBe Path()
    vm.status.index shouldBe 0
    vm.exportData.paths shouldBe Map.empty
  }

  "VM" should "support align on tag" in {
    val vm = VM(emptyContext)
    val scope = vm.alignEnter(0)
    vm.status.path shouldBe Path(List(Slot.Align(0, 0)))
    val completed = scope.completeWith(0)
    vm.exit(completed) shouldBe 0
    vm.status.path shouldBe Path()
    vm.exportData.paths.keySet shouldBe Set(Path(List(Slot.Align(0, 0))))
  }

  "VM" should "support exchange" in {
    val vm = VM(emptyContext)
    val scope = vm.exchangeEnter()
    vm.status.path shouldBe Path(List(Slot.Exchange(0)))
    val completed = scope.withDefaultValue(0).completeWith(Map(0 -> 0))
    vm.exit(completed).shouldBe(Map(0 -> 0))
    vm.status.path shouldBe Path()
    vm.exportData.paths shouldBe Map(Path(List(Slot.Exchange(0))) -> Map(0 -> 0))
  }

  "VM" should "support several call in the same level" in {
    val vm = VM(emptyContext)
    val scope1 = vm.exit(vm.alignEnter(0).completeWith(0))
    val scope2 = vm.exit(vm.alignEnter(0).completeWith(1))
    vm.exportData.paths shouldBe Map(
      Path(List(Slot.Align(tag = 0, index = 0))) -> 0,
      Path(List(Slot.Align(tag = 0, index = 1))) -> 1
    )
  }

  "VM" should "support nesting" in {
    val vm = VM(emptyContext)
    val outerScope = vm.alignEnter(0)
    val innerScope = vm.alignEnter(1)
    vm.exit(innerScope.completeWith(1))
    vm.exit(outerScope.completeWith(1))
    vm.exportData.paths shouldBe Map(
      Path(List(Slot.Align(tag = 0, index = 0))) -> 1,
      Path(List(Slot.Align(tag = 1, index = 0), Slot.Align(tag = 0, index = 0))) -> 1
    )
  }

  "VM" should "correctly align data from neighborhood" in {
    def simpleExport(data: Int): Export =
      Export(
        Map(
          Path(List(Slot.Align(tag = 0, index = 0))) -> 0,
          Path(List(Slot.Exchange(0), Slot.Align(tag = 0, index = 0))) -> data
        )
      )
    val context = Context(
      Seq(
        0 -> simpleExport(0),
        1 -> simpleExport(1),
        2 -> simpleExport(2)
      ),
      2
    )
    val vm = VM(context)
    val scope = vm.alignEnter(0)
    val exchange = vm.exchangeEnter()
    exchange.aligned shouldBe Map(0 -> 0, 1 -> 1, 2 -> 2)
    vm.exit(scope.completeWith(0))
  }
}
