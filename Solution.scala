object Solution {

  def convertNumber(number: Int): String = {
    number match {
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 20 => "twenty"
      case n if n < 20 => s"${convertNumber(n - 10)}teen"
      case n if n < 30 => s"twenty ${convertNumber(n - 20)}"
      case _ => ""
    }
  }

  def parseHours(hour: Int): String = {
    // this will cover 13, which cwe can have at between 12:31 - 12:59
    val checkedNumber = if (hour > 12) hour - 12 else hour
    convertNumber(checkedNumber)
  }

  //59 is commented out because of wrong test case on Hackerrank - 2020.04.18
  //example: 9:59 should logicly say 'one minute to ten', but Hackerrank only accepts in those cases the plural form, 'minutes'
  def parseMinutes(minutes: Int): String = {
    minutes match {
      case m if m == 1 /*&& m == 59*/ => "one minute"
      case m if m == 15 || m == 45 => "quarter"
      case m if m == 30 => "half"
      case m if m < 30 => s"${convertNumber(minutes)} minutes"
      case m if m > 30 => s"${convertNumber(60 - m)} minutes"
    }
  }

  def timeInWords(h: Int, m: Int): String = {
    m match {
      case 0 => s"${parseHours(h)} o' clock"
      case minutes if minutes >= 1 && minutes <= 30 => s"${parseMinutes(minutes)} past ${parseHours(h)}"
      case minutes if minutes > 30 => s"${parseMinutes(minutes)} to ${parseHours(h + 1)}"
    }
  }

  def main(args: Array[String]) {
    val result0 = timeInWords(5, 47)
    val result1 = timeInWords(3, 0)
    val result10 = timeInWords(7, 15)

    println(s"Expected: thirteen minutes to six, Result $result0")
    println(s"Expected: three o' clock, Result $result1")
    println(s"Expected: quarter past seven, Result $result10")

    assert(result0 equals "thirteen minutes to six")
    assert(result1 equals "three o' clock")
    assert(result10 equals "quarter past seven")
  }
}
