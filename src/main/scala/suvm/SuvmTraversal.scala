package suvm

import suvm.SuvmObjectGlobals._

abstract class SuvmStructureProxy[T <: SuvmComponent] extends SuvmObject {
  def getImmediateChildren(s: T): Seq[T]
}

abstract class SuvmVisitor[T <: SuvmComponent] extends SuvmObject {
  def beginV(): Unit
  def endV(): Unit
  def visit(node: T): Unit
}

abstract class SuvmVisitorAdapter[S <: SuvmComponent, T <: SuvmVisitor[S]] extends SuvmObject {
  def accept(s: S, v: T, p: SuvmStructureProxy[S], invokeBeginEnd: Boolean = true): Unit
}

class SuvmComponentProxy(val name: String) extends SuvmStructureProxy[SuvmComponent] {
  override def getImmediateChildren(s: SuvmComponent): Seq[SuvmComponent] = s.getChildren
}

class SuvmTopDownVisitorAdapter(val name: String) extends
  SuvmVisitorAdapter[SuvmComponent, SuvmVisitor[SuvmComponent]] {
  override def accept(s: SuvmComponent,
                      v: SuvmVisitor[SuvmComponent],
                      p: SuvmStructureProxy[SuvmComponent],
                      invokeBeginEnd: Boolean): Unit = {
    if (invokeBeginEnd) v.beginV()
    v.visit(s)
    val c = p.getImmediateChildren(s)
    c.foreach(accept(_, v, p, invokeBeginEnd = false))
    if (invokeBeginEnd) v.endV()
  }
}

class SuvmBottomUpVisitorAdapter(val name: String) extends
  SuvmVisitorAdapter[SuvmComponent, SuvmVisitor[SuvmComponent]] {
  override def accept(s: SuvmComponent,
                      v: SuvmVisitor[SuvmComponent],
                      p: SuvmStructureProxy[SuvmComponent],
                      invokeBeginEnd: Boolean): Unit = {
    if (invokeBeginEnd) v.beginV()
    val c = p.getImmediateChildren(s)
    c.foreach(accept(_, v, p, invokeBeginEnd = false))
    v.visit(s)
    if (invokeBeginEnd) v.endV()
  }
}

class SuvmComponentNameCheckVisitor(val name: String) extends SuvmVisitor[SuvmComponent] {
  override def beginV(): Unit = {}

  override def endV(): Unit = {}

  def getNameConstraint: String =
    ".+"

  override def visit(node: SuvmComponent): Unit =
    if (node != SuvmRoot.getInst) {
      if (!suvmIsMatch(getNameConstraint, node.getName))
        suvmWarning("UVM/COMP/NAME", "violates constraints")
    }
}