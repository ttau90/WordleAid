package Filters

import Models.{GuessResult, HintType}

case class GuessResultFilter private(guessResult: GuessResult, word: String) {

  def addLetter(letter: Char): GuessResultFilter = {
    this.copy(word = word + letter)
  }

  // Word is valid only if each letter is valid and all of the potential and correct letters are used
  def isFinalWordValid: Boolean = {
    if (word.zipWithIndex.forall(tuple => isCurrentWordValid(tuple._1, tuple._2))) {
      val allPotentials = guessResult.charCount.forall(pair => word.count(_ == pair._1) >= pair._2)
      val allCorrects = guessResult.hints.zipWithIndex
        .filter(pair => pair._1 == HintType.Correct)
        .forall(pair => guessResult.word(pair._2) == word(pair._2))
      
      allCorrects && allPotentials
    } else {
      false
    }
  }

  def isCurrentWordValid: Boolean = {
    val addedLetter: Char = word.last
    val addedPosition: Int = word.length - 1

    isCurrentWordValid(addedLetter, addedPosition)
  }

  private def isCurrentWordValid(letter: Char, position: Int): Boolean = {
    if !isLetterInGuessResult(letter) then return true
    if isLetterInPositionValid(letter, position) then return true
    if isLetterInPositionPotential(letter, position) then return false
    if isLetterInInvalidListOnly(letter) then return false
    isLetterPotential(letter)
  }

  // If letter was not in guess, then it's unknown and therefore valid
  private def isLetterInGuessResult(letter: Char): Boolean = {
    guessResult.word.contains(letter)
  }

  // If letter is in the list of invalid chars and not in the list of potential chars, it is invalid
  private def isLetterInInvalidListOnly(letter: Char): Boolean = {
    guessResult.invalidChars.contains(letter) && !guessResult.potentialChars.contains(letter)
  }

  // If letter is in the right position and correct, then is valid
  private def isLetterInPositionValid(letter: Char, position: Int) = {
    guessResult.word.charAt(position) == letter && guessResult.hints(position) == HintType.Correct
  }

  // If letter is a potential letter but is in the same position as the letter in the guess, it is invalid
  private def isLetterInPositionPotential(letter: Char, position: Int): Boolean = {
    guessResult.word.charAt(position) == letter && guessResult.hints(position) == HintType.Potential
  }

  // If the letter is a potential letter, it is valid
  private def isLetterPotential(letter: Char): Boolean = {
    if(guessResult.invalidChars.contains(letter) && guessResult.potentialChars.contains(letter)) {
      word.toCharArray.count(_ == letter) <= guessResult.charCount(letter)
    } else {
      guessResult.potentialChars.contains(letter)
    }
  }
}

object GuessResultFilter {
  def apply(guessResult: GuessResult): GuessResultFilter = {
    new GuessResultFilter(guessResult, "")
  }
}
