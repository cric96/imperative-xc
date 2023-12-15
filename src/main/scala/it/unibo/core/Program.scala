package it.unibo.core

trait Program:
  final def logic()(using context: Context): (Any, Map[Int, Export]) =
    given vm: VM = new VM(context)
    val root = run()
    (root, vm.outExports)
  def run(): VM ?=> Any
