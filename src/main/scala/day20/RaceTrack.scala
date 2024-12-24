package day20

case class Cheat(start: Point, end: Point, saved: Int)

case class RaceTrack(start: Point, end: Point, board: Board):

  private lazy val basePath: Seq[Point] = board.minimumPath(start, end).get

  def cheats(limit: Int): Seq[Cheat] =
    basePath.zipWithIndex.flatMap: (lp, li) =>
      basePath.zipWithIndex.drop(li).flatMap: (rp, ri) =>
        val dist = lp.linearDistance(rp)
        Option.when(dist <= limit && (dist < ri - li))(Cheat(lp, rp, (ri - li) - dist))
