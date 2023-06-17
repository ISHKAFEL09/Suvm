package uvm

import scala.reflect.ClassTag

abstract class UVMFactory {
  def register[T <: UVMObject](obj: UVMObjectWrapper[T]): Unit

  def createObjectByType[T <: UVMObject](wrapper: UVMObjectWrapper[T],
                                         name: String = "",
                                         parentPath: String = ""): T

  def createComponentByType[T <: UVMComponent](wrapper: UVMObjectWrapper[T],
                                               name: String,
                                               parent: Option[UVMComponent],
                                               parentPath: String = ""): T

  def getWrapperByName[T <: UVMObject](name: String): Option[UVMObjectWrapper[T]]

  def setTypeOverrideByName[T <: UVMObject](orig: String, ovrd: String, replace: Boolean): Unit

  def setTypeOverrideByName[T <: UVMObject : ClassTag](orig: String)(ovrd: String => T): Unit
}

object UVMFactory {
  def get: UVMFactory = UVMCoreService().getFactory

  def set(factory: UVMFactory): Unit = UVMCoreService().setFactory(factory)
}

class UVMDefaultFactory extends UVMFactory {
  private val name2wrapper = collection.mutable.HashMap.empty[String, UVMObjectWrapper[UVMObject]]

  private def className(name: String): String = {
    name.replaceAll("\\$.+?$", "")
  }

  override def register[T <: UVMObject](obj: UVMObjectWrapper[T]): Unit = {
    name2wrapper(className(obj.getTypeName)) = obj
    uvmInfo("Factory", s"Register ${className(obj.getTypeName)}", UVM_DEBUG)
    // TODO: overrides
  }

  override def createObjectByType[T <: UVMObject](wrapper: UVMObjectWrapper[T],
                                                  name: String,
                                                  parentPath: String): T = {
//    println(name2wrapper.map { case (k, v) => (k, v.getTypeName)})
    wrapper.createObject(name).get
  }

  override def getWrapperByName[T <: UVMObject](name: String): Option[UVMObjectWrapper[T]] = {
    name2wrapper.get(className(name)).map(_.asInstanceOf[UVMObjectWrapper[T]])
  }

  override def setTypeOverrideByName[T <: UVMObject](orig: String, ovrd: String, replace: Boolean): Unit = {
    // TODO:
  }

  override def setTypeOverrideByName[T <: UVMObject : ClassTag](orig: String)(ovrd: String => T): Unit = {
    val wrapper = new UVMObjectRegistry(implicitly[ClassTag[T]].runtimeClass.getPureName, ovrd)
    name2wrapper(className(orig)) = wrapper
  }

  override def createComponentByType[T <: UVMComponent](wrapper: UVMObjectWrapper[T],
                                                        name: String,
                                                        parent: Option[UVMComponent],
                                                        parentPath: String): T = {
    wrapper.createComponent(name, parent).get
  }
}
