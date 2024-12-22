package day19

import scala.collection.mutable as mut

type Pattern = String
type Patterns = Vector[Pattern]

case class PatternsSolver(availablePatterns: Patterns):
  
  private val cache = mut.HashMap[Pattern, Long]("" -> 1)
  
  def makePattern(pattern: Pattern): Long =
    def updatePattern() =
      availablePatterns.filter(pattern.startsWith)
        .map(currentPattern => makePattern(pattern.drop(currentPattern.length)))
        .sum
    cache.getOrElseUpdate(pattern, updatePattern())
