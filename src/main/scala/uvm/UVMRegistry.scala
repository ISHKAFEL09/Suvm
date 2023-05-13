package uvm

trait UVMObjectWrapper[+T] {
  def createObject(name: String): Option[T] = None
  def createComponent(name: String, parent: UVMComponent): Option[T] = None
  def getTypeName: String
  def initialize(): Unit
}

class UVMComponentRegistry[+T <: UVMComponent](typeName: String = "<unknown>",
                                               create: (String, UVMComponent) => T)
  extends UVMObjectWrapper[T] {

  override def createComponent(name: String, parent: UVMComponent): Option[T] =
    Some(create(name, parent))

  override def getTypeName: String = typeName

  override def initialize(): Unit = {
    UVMFactory.get.register(this)
    // TODO:
  }
}

class UVMObjectRegistry[+T <: UVMObject](typeName: String = "<unknown>",
                                         create: String => T)
  extends UVMObjectWrapper[T] {

  override def createObject(name: String): Option[T] =
    Some(create(name))

  override def getTypeName: String = typeName

  override def initialize(): Unit = {
    UVMFactory.get.register(this)
    // TODO:
  }
}
