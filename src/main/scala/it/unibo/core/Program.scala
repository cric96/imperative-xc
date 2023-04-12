package it.unibo.core

trait Program {
  final def logic()(using context: Context): Export = {
    given vm: VM = new VM(context)
    val root = run()
    vm.exportData.put(Path(), root)
    vm.exportData
  }
  def run(): VM ?=> Any
}
