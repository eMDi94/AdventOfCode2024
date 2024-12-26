package day22

extension (self: Long)
  inline def nextSecret: Long = step(_ * 64).step(_ / 32).step(_ * 2048)
  private inline def step(f: Long => Long): Long = mix(f(self)).prune
  private inline def mix(n: Long): Long = self ^ n
  private inline def prune: Long = self % 16777216
  
  def secretsIterator: Iterator[Long] = Iterator.iterate(self)(_.nextSecret)
