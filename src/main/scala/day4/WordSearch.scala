package day4


case class WordSearch(input: List[String]):
  
  import WordSearch.Index

  private val map = input.mkString.toList
  val width: Int = input.head.length
  val height: Int = input.size

  def apply(row: Int, column: Int): Char = map(row * width + column)
  def apply(indexes: List[Index]): String = indexes.map(index => this(index.row, index.column)).mkString
  def apply(indexList: List[List[Index]]): List[String] = indexList.map(this(_))

  def getAllIndexesForCharacter(char: Char): List[Index] =
    map.zipWithIndex.filter(_._1 == char)
      .map { res =>
        val (_, index) = res
        val row = index / width
        val column = index - (row * width)
        Index(row, column)
      }

object WordSearch:
  case class Index(row: Int, column: Int)
