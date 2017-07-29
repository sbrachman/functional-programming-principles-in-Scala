package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0) 1
      else if (c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def checkFirstAndLast(): Boolean = {
          if (chars.head == ')' || chars.last == '(') false
          else true
        }

        def countParentheses(characters: List[Char], counter: Int): Int = {
          if (characters.isEmpty) counter
          else if (characters.head == '(') countParentheses(characters.tail, counter + 1)
          else if (characters.head == ')') countParentheses(characters.tail, counter - 1)
          else countParentheses(characters.tail, counter)
        }

        if (countParentheses(chars, 0) == 0 && checkFirstAndLast()) true
        else false
    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty || money < 0) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
