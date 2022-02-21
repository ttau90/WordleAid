package Filters

import Models.{GuessResult, HintType}
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class GuessResultFilterTest extends AnyFunSuite with BeforeAndAfter {
  var filter: GuessResultFilter = _

  before {
    val hints: Vector[HintType] = Vector(HintType.Correct, HintType.Incorrect, HintType.Incorrect, HintType.Potential)
    val guessResult = GuessResult("baby", hints).get
    filter = GuessResultFilter(guessResult)
  }

  test("can add a char") {
    val updatedFilter: GuessResultFilter = filter.addLetter('a')

    assert(updatedFilter.word == "a")
  }

  test("can add 2 chars") {
    val updatedFilter: GuessResultFilter = filter.addLetter('a').addLetter('b')

    assert(updatedFilter.word == "ab")
  }

  test("isLetterValid returns true if letter is not in guessed word") {
    val updatedFilter: GuessResultFilter = filter.addLetter('z')

    assert(updatedFilter.isCurrentWordValid)
  }

  test("isLetterValid returns false if invalid char is added to word") {
    val updatedFilter: GuessResultFilter = filter.addLetter('a')

    assert(!updatedFilter.isCurrentWordValid)
  }

  test("isLetterValid returns true if correct char is added to word") {
    val updatedFilter: GuessResultFilter = filter.addLetter('b')

    assert(updatedFilter.isCurrentWordValid)
  }

  test("isLetterValid returns false if potential letter is added in 'potential' position from guess") {
    val updatedFilter: GuessResultFilter = filter.addLetter('b').addLetter('u').addLetter('r').addLetter('y')

    assert(!updatedFilter.isCurrentWordValid)
  }

  test("isLetterValid returns false if hint indicates a letter is used at max 1 time") {
    val updatedFilter: GuessResultFilter = filter.addLetter('b').addLetter('a').addLetter('b')

    assert(!updatedFilter.isCurrentWordValid)
  }

  test("'byde' should return true") {
    var updatedFilter: GuessResultFilter = filter.addLetter('b')
    assert(updatedFilter.isCurrentWordValid)

    updatedFilter = updatedFilter.addLetter('y')
    assert(updatedFilter.isCurrentWordValid)

    updatedFilter = updatedFilter.addLetter('d')
    assert(updatedFilter.isCurrentWordValid)

    updatedFilter = updatedFilter.addLetter('e')
    assert(updatedFilter.isCurrentWordValid)
  }

  test("'brya' should return false") {
    var updatedFilter: GuessResultFilter = filter.addLetter('b')
    assert(updatedFilter.isCurrentWordValid)

    updatedFilter = updatedFilter.addLetter('r')
    assert(updatedFilter.isCurrentWordValid)

    updatedFilter = updatedFilter.addLetter('y')
    assert(updatedFilter.isCurrentWordValid)

    updatedFilter = updatedFilter.addLetter('a')
    assert(!updatedFilter.isCurrentWordValid)
  }

  test("isFinalWordValid returns true if word contains all potential hints, all correct positions, and no invalid letters") {
    val updatedFilter: GuessResultFilter = filter.addLetter('b').addLetter('y').addLetter('e').addLetter('s')

    assert(updatedFilter.isFinalWordValid)
  }

  test("isFinalWordValid returns false if word is missing a potential") {
    val updatedFilter: GuessResultFilter = filter.addLetter('b').addLetter('x').addLetter('e').addLetter('s')

    assert(!updatedFilter.isFinalWordValid)
  }

  test("isFinalWordValid returns false if word is missing a correct") {
    val updatedFilter: GuessResultFilter = filter.addLetter('x').addLetter('y').addLetter('e').addLetter('s')

    assert(!updatedFilter.isFinalWordValid)
  }

  test("isFinalWordValid returns false if word has extra potential") {
    val updatedFilter: GuessResultFilter = filter.addLetter('x').addLetter('y').addLetter('y').addLetter('s')

    assert(!updatedFilter.isFinalWordValid)
  }
}
