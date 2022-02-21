package Models

import Models.HintType

import org.scalatest.funsuite.AnyFunSuite
import scala.util.{Failure, Success, Try}

class GuessResultTest extends AnyFunSuite {
  test("Uneven arg length returns failure") {
    val hintSeq: Vector[HintType] = Vector(HintType.Correct, HintType.Incorrect)
    val word: String = "cat"

    val guessResult: Try[GuessResult] = GuessResult(word, hintSeq)

    assert(guessResult.isFailure)
  }

  test("Even arg length returns success") {
    val hintSeq: Vector[HintType] = Vector(HintType.Correct, HintType.Incorrect, HintType.Potential)
    val word: String = "cat"

    val guessResult: Try[GuessResult] = GuessResult(word, hintSeq)

    assert(guessResult.isSuccess)
  }

  test("Constructor creates GuestResult correctly") {
    val invalidChars: Seq[Char] = Seq('b', 'y')
    val potentialChars: Seq[Char] = Seq('a', 'a')
    val charCount: Map[Char, Int] = Map('a' -> 2)

    val guessResult: Try[GuessResult] = GuessResult("ababy", Vector(HintType.Potential, HintType.Correct, HintType.Potential, HintType.Incorrect, HintType.Incorrect))

    guessResult match {
      case Failure(g) => assert(false)
      case Success(g) => {
        assert(g.invalidChars == invalidChars)
        assert(g.potentialChars == potentialChars)
        assert(g.charCount == charCount)
      }
    }
  }
}
