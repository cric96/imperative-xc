package it.unibo.core
// Test export using scala test
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.{*, given}
class ExportTest extends AnyFlatSpec with Matchers {
  val root = Path()
  val exchangePath = Path(Slot(0, 0) :: Nil)
  "An export" should "be empty" in {
    val exportData = Export()
    exportData.paths.isEmpty shouldBe true
  }

  it should "be updatable with paths" in {
    val exportData = Export()
    exportData.put(Path(), 10)
    exportData.paths.size shouldBe 1
    exportData.paths.head shouldBe (Path(), 10)
  }

  it should "have as root the Path with empty slots" in {
    val exportData = Export()
    exportData.put(Path(), 10)
    exportData.root() shouldBe RawLocal(10)
  }
}
