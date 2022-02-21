package Models

import scala.util.{Try, Success, Failure}

enum HintType(val hint: Char):
  case Incorrect extends HintType('i')
  case Potential extends HintType('p')
  case Correct extends HintType('c')

case class GuessResult private(word: String, hints: Vector[HintType], invalidChars: Seq[Char], potentialChars: Seq[Char], charCount: Map[Char, Int])

object GuessResult {
  private val hintMap: Map[Char, HintType] = Map(
    'i' -> HintType.Incorrect,
    'p' -> HintType.Potential,
    'c' -> HintType.Correct
  )
  
  def apply(word: String, hints: Vector[HintType]): Try[GuessResult] = {
    if (word.length == hints.length) {
      val zipped: Array[(Char, HintType)] = word.toCharArray.zip(hints)
      val invalidChars: Seq[Char] = zipped.filter(_._2 == HintType.Incorrect).map(_._1)
      val potentialChars: Seq[Char] = zipped.filter(_._2 == HintType.Potential).map(_._1)
      val charCount: Map[Char, Int] = potentialChars.map(_ -> 1).groupMapReduce(_._1)(_._2)(_ + _)

      Success(new GuessResult(word, hints, invalidChars, potentialChars, charCount))
    } else {
      Failure(new Exception("Length of guessed word does not match length of hints"))
    }
  }

  def translateStringToHints(hintsString: String): Vector[HintType] = {
    hintsString.map(hintMap.getOrElse(_, HintType.Incorrect)).toVector
  }
}
