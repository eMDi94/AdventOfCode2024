package day9

enum DiskBlock(length: Int):
  case File(id: Int, length: Int) extends DiskBlock(length)
  case FreeSpace(length: Int) extends DiskBlock(length)