package it.unibo.core

trait Program {
  final def logic()(using context: Context): (Any, Seq[(Int, Export)]) = {
    given vm: VM = new VM(context)
    val root = run()
    (root, vm.exports)
  }
  def run(): VM ?=> Any
}
