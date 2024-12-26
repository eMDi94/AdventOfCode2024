package day22

trait Semigroup[A]:
  def combine(a0: A, a1: A): A

trait Monoid[A] extends Semigroup[A]:
  def zero: A

given NumericMonoid[A](using N: Numeric[A]): Monoid[A] with
  override def zero: A = N.zero
  override def combine(a0: A, a1: A): A = N.plus(a0, a1)

given MapMonoid[A, B](using S: Semigroup[B]): Monoid[Map[A, B]] with
  override def zero: Map[A, B] = Map.empty

  override def combine(ab0: Map[A, B], ab1: Map[A, B]): Map[A, B] =
    ab1.foldLeft(ab0) {
      case (ab, (a1, b1)) =>
        ab.updatedWith(a1) {
          case Some(b0) => Some(S.combine(b0, b1))
          case None => Some(b1)
        }
    }

def leftBiasedSemigroup[A]: Semigroup[A] = (a0: A, _: A) => a0