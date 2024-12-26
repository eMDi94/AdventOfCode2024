package day22

extension [A](self: Iterator[A])
  def foldMap[B](f: A => B)(using M: Monoid[B]): B =
    self.map(f).foldLeft(M.zero)(M.combine)

  def nth(index: Int): A = self.drop(index).next()
