package Utils

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class WordProcessorUtilTest extends AnyFunSuite {

  test("Filter N chars returns words of N length") {
    val wordList: Seq[String] = List("abcde", "abc", "qwert", "", "poiuyt")
    val filteredSet: Seq[String] = WordProcessorUtil.filterWordsNLength(wordList, 5)

    assert(filteredSet.size == 2)
    filteredSet.foreach(word => assert(word.length == 5))
  }

  test("CreateReverseIndex returns map of char => words that contains that char") {
    val wordList: Seq[String] = List("cat", "mat", "act")
    val validationMap: Map[Char, Seq[String]] = Map(
      'a' -> List("cat", "mat", "act"),
      'c' -> List("cat", "act"),
      'm' -> List("mat"),
      't' -> List("cat", "mat", "act"),
    )

    val reverseIndex: Map[Char, Seq[String]] = WordProcessorUtil.createReverseIndex(wordList)

    assert(reverseIndex.keySet == validationMap.keySet)
    assert(reverseIndex.get('a') == validationMap.get('a'))
    assert(reverseIndex.get('c') == validationMap.get('c'))
    assert(reverseIndex.get('m') == validationMap.get('m'))
    assert(reverseIndex.get('t') == validationMap.get('t'))
  }

  test("CalculateLetterScores returns map of char => count/(total letters)") {
    val wordList: Seq[String] = List("cat", "mat", "act")
    val validationMap: Map[Char, Double] = Map(
      'a' -> 3.0/9.0,
      'c' -> 2.0/9.0,
      'm' -> 1.0/9.0,
      't' -> 3.0/9.0
    )

    val letterScores: Map[Char, Double] = WordProcessorUtil.calcLetterScores(wordList)

    assert(letterScores.get('a') == validationMap.get('a'))
    assert(letterScores.get('c') == validationMap.get('c'))
    assert(letterScores.get('m') == validationMap.get('m'))
    assert(letterScores.get('t') == validationMap.get('t'))
  }

  test("CalcPositionScores return array of maps of char => score for letter for that position") {
    val wordList: Seq[String] = List("cat", "mat", "act")
    val pos1Map: Map[Char, Double] = Map(
      'a' -> 1.0/3.0,
      'c' -> 1.0/3.0,
      'm' -> 1.0/3.0
    )
    val pos2Map: Map[Char, Double] = Map(
      'a' -> 2.0/3.0,
      'c' -> 1.0/3.0
    )
    val pos3Map: Map[Char, Double] = Map(
      't' -> 3.0/3.0
    )

    val validationData: Array[Map[Char, Double]] = Array(pos1Map, pos2Map, pos3Map)

    val positionScores: Array[Map[Char, Double]] = WordProcessorUtil.calcPositionScores(wordList)

    (0 to 2).foreach(i => {
      val validationMap = validationData(i)
      val testMap = positionScores(i)

      assert(validationMap.keySet == testMap.keySet)
      testMap.keySet.foreach(k => assert(testMap.get(k) == validationMap.get(k)))
    })
  }

  test("scoreWordByLetters returns correct score") {
    val wordList: Seq[String] = List("cat", "mat", "act")

    // Below is reference of calculated scores
//    val validationMap: Map[Char, Double] = Map(
//      'a' -> 3.0/9.0,
//      'c' -> 2.0/9.0,
//      'm' -> 1.0/9.0,
//      't' -> 3.0/9.0
//    )

    val letterScores: Map[Char, Double] = WordProcessorUtil.calcLetterScores(wordList)

    val score: Double = WordProcessorUtil.scoreWordByLetters("cat", letterScores)

    assert(score == 8.0/9.0)
  }

  test("scoreWordByPosition returns correct score") {
    val wordList: Seq[String] = List("cat", "mat", "act")

    //Below is reference of calculated scores
//    val pos1Map: Map[Char, Double] = Map(
//      'a' -> 1.0/3.0,
//      'c' -> 1.0/3.0,
//      'm' -> 1.0/3.0
//    )
//    val pos2Map: Map[Char, Double] = Map(
//      'a' -> 2.0/3.0,
//      'c' -> 1.0/3.0
//    )
//    val pos3Map: Map[Char, Double] = Map(
//      't' -> 3.0/3.0
//    )

    val positionScores: Array[Map[Char, Double]] = WordProcessorUtil.calcPositionScores(wordList)

    val score: Double = WordProcessorUtil.scoreWordByPosition("cat", positionScores)

    assert(score == 2.0)
  }

  test("scoreWordByPosition returns correct score again") {
    val wordList: Seq[String] = List("cat", "mat", "act")

    // Below is reference of calculated scores
//    val pos1Map: Map[Char, Double] = Map(
//      'a' -> 1.0/3.0,
//      'c' -> 1.0/3.0,
//      'm' -> 1.0/3.0
//    )
//    val pos2Map: Map[Char, Double] = Map(
//      'a' -> 2.0/3.0,
//      'c' -> 1.0/3.0
//    )
//    val pos3Map: Map[Char, Double] = Map(
//      't' -> 3.0/3.0
//    )


    val positionScores: Array[Map[Char, Double]] = WordProcessorUtil.calcPositionScores(wordList)

    val score: Double = WordProcessorUtil.scoreWordByPosition("cab", positionScores)

    assert(score == 1.0)
  }

  test("scoreWordByBoth returns correct score") {
    val wordList: Seq[String] = List("cat", "mat", "act")

    val letterScores: Map[Char, Double] = WordProcessorUtil.calcLetterScores(wordList)
    val positionScores: Array[Map[Char, Double]] = WordProcessorUtil.calcPositionScores(wordList)

    val score: Double = WordProcessorUtil.scoreWordByBoth("cat", letterScores, positionScores)

    assert(score == 26.0/9.0)
  }
}
