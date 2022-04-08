package suvm

trait SuvmConfigDb {
  type T

  def get(cntxt: SuvmComponent, instName: String, fieldName: String, value: T): (Boolean, T)
}

object SuvmConfigDbInt extends SuvmConfigDb {
  type T = Int

  override def get(cntxt: SuvmComponent, instName: String, fieldName: String, value: Int): (Boolean, Int) = {
    (false, 0)
  }
}
