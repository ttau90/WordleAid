package Utils

import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite

import java.io.FileNotFoundException
import scala.util.{Failure, Success, Try}

class FileHelperUtilTest extends AnyFunSuite with TryValues {

  test("ReadFile should return words as sequence.") {
    val validationList: Seq[String] = List(
      "a",
      "aa",
      "aaa",
      "aah",
      "aahed",
      "aahing",
      "aahs",
      "aal",
      "aalii",
      "aaliis",
      "aals",
      "aam",
      "aani",
      "aardvark",
      "aardvarks",
      "aardwolf",
      "aardwolves",
      "aargh",
      "aaron",
      "aaronic",
      "aaronical",
      "aaronite",
      "aaronitic",
      "aarrgh",
      "aarrghh",
      "aaru",
      "aas",
      "aasvogel",
      "aasvogels",
      "ab"
    )

    val wordSeq: Try[Seq[String]] = FileHelperUtil.readFileToSeq("words_alpha_sample.txt")

    wordSeq match {
      case Success(s) => assert(s.size == validationList.size)
      case Failure(_) => fail("Actual list size did not match expected list size.")
    }
  }

  test("ReadFile should return Failure if file is not found.") {
    val validationList: Try[Seq[String]] = FileHelperUtil.readFileToSeq("bad_file.txt")

    validationList match {
      case Success(_) => fail("A file was found although file was not expected.")
      case Failure(_) => succeed
    }
  }
}
