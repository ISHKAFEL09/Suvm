package rockets.tilelink

trait DirectoryRepresentation {
  val width: Int
}

trait HasDirectoryRepresentation {
  val dir: DirectoryRepresentation
}