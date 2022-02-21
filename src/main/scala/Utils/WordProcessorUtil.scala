package Utils

import scala.util.{Failure, Success, Try}

object WordProcessorUtil {
  def filterWordsNLength(wordList: Seq[String], wordLength: Int): Seq[String] = {
    wordList.filter(_.length == wordLength)
  }

  def createReverseIndex(wordList: Seq[String]): Map[Char, Seq[String]] = {
    wordList.flatMap(word =>
      word.toCharArray
        .distinct
        .map((_, word)))
      .groupMap(_._1)(_._2)
  }

  def calcLetterScores(wordList: Seq[String]): Map[Char, Double] = {
    val totalLetters = (wordList.head.length * wordList.size).toDouble

    wordList
      .flatMap(_.toCharArray)
      .groupBy(x => x)
      .map(t => (t._1, t._2.size / totalLetters))
  }

  def calcPositionScores(wordList: Seq[String]): Array[Map[Char, Double]] = {
    wordList
      .flatMap(_.split("").zipWithIndex)
      .groupMap(_._2)(_._1)
      .map((k, v) => calcLetterScores(v))
      .toArray
  }

  def scoreWords(wordList: Seq[String]): Seq[(String, Double)] = {
    val letterScores: Map[Char, Double] = calcLetterScores(wordList)
    val positionScores: Array[Map[Char, Double]] = calcPositionScores(wordList)

    wordList.map(word => word -> scoreWordByBoth(word, letterScores, positionScores) * word.distinct.length / word.length)
  }

  def scoreWordsLettersOnly(wordList: Seq[String]): Seq[(String, Double)] = {
    val letterScores: Map[Char, Double] = calcLetterScores(wordList)

    wordList.map(word => word -> scoreWordByLetters(word, letterScores) * word.distinct.length / word.length)
  }

  def scoreWordsPositionsOnly(wordList: Seq[String]): Seq[(String, Double)] = {
    val positionScores: Array[Map[Char, Double]] = calcPositionScores(wordList)

    wordList.map(word => word -> scoreWordByPosition(word, positionScores) * word.distinct.length / word.length)
  }

  def scoreWordByBoth(word: String, letterScores: Map[Char, Double], positionScores: Array[Map[Char, Double]]): Double = {
    scoreWordByLetters(word, letterScores) + scoreWordByPosition(word, positionScores)
  }

  def scoreWordByLetters(word: String, letterScores: Map[Char, Double]): Double = {
    word.map(c => letterScores.getOrElse(c, 0.0)).sum
  }

  def scoreWordByPosition(word: String, positionScores: Array[Map[Char, Double]]): Double = {
    word.zipWithIndex.map(pair => positionScores(pair._2).getOrElse(pair._1, 0.0) / word.length).sum
  }
}
