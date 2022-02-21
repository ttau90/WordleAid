import Models.*
import Filters.*
import Utils.*

import java.util

object Main extends App {
  val words = FileHelperUtil.readFileToSeq("official_words.txt").get

  val wordScores = WordProcessorUtil.scoreWords(words)
  val wordScoresLetters = WordProcessorUtil.scoreWordsLettersOnly(words)
  val wordScoresPositions = WordProcessorUtil.scoreWordsPositionsOnly(words)

  val sortedWordScores = wordScores.sortBy(_._2)(Ordering[Double].reverse)
  val sortedWordLetters = wordScoresLetters.sortBy(_._2)(Ordering[Double].reverse)
  val sortedWordPositions = wordScoresPositions.sortBy(_._2)(Ordering[Double].reverse)

  println(sortedWordScores.head)
}