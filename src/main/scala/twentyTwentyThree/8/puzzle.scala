package dayEight

import utils.inputReader.{lines, wholeFile}

final case class Node(value: String, left: String, right: String)

object puzzle extends App {
  private val instructions = wholeFile("8_instructions.txt").split("").toList
  private val nodesText = lines("8_nodes.txt")

  private def parseNode(string: String): Node = {
    val nodeRegex = """([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)""".r
    string match {
      case nodeRegex(value, left, right) => Node(value, left, right)
    }
  }

  private val parsedNodes = nodesText.map(parseNode)

  private def findNextNode(
      instruction: String,
      nodes: List[Node],
      current: Node
  ): Node = {
    if (instruction == "L") nodes.filter(_.value == current.left).head
    else nodes.filter(_.value == current.right).head
  }

  private def isAtNodeEndingIn(
      node: Node,
      nodes: List[Node],
      suffix: String
  ): Boolean =
    nodes.filter(_.value.endsWith(suffix)).contains(node)

  private def countStepsRequired(
      instructions: List[String],
      current: Node,
      nodes: List[Node],
      count: Long,
      index: Int,
      suffix: String
  ): Long = {
    val nextIndex = if (index == instructions.length - 1) 0 else index + 1
    if (isAtNodeEndingIn(current, nodes, suffix)) {
      count
    } else {
      countStepsRequired(
        instructions,
        findNextNode(instructions(index), nodes, current),
        nodes,
        count + 1,
        nextIndex,
        suffix
      )
    }
  }

  private def findPrimeFactors(num: Long): List[Long] = {
    val maxPrime = num / 2
    def findPrimes(n: Long): List[Long] = {
      def sieve(numbers: List[Int], primes: List[Long]): List[Long] =
        numbers match {
          case Nil => primes.reverse
          case head :: tail =>
            val filtered = tail.filter(_ % head != 0)
            sieve(filtered, head :: primes)
        }
      if (n < 2) {
        List()
      } else {
        val numbers = (2 to n.toInt).toList
        sieve(numbers, List())
      }
    }
    findPrimes(maxPrime).filter(prime => num % prime == 0)
  }

  private def greatestCommonDivisor(
      nums: (Long, Long),
      primes: List[Long]
  ): Long = {
    primes.filter(prime => nums._1 % prime == 0 && nums._2 % prime == 0).last
  }

  private def lowestCommonMultiple(
      nums: (Long, Long),
      primes: List[Long]
  ): Long = {
    (nums._1 * nums._2) / greatestCommonDivisor(nums, primes)
  }

  private val partOneSolution = {
    countStepsRequired(
      instructions,
      parsedNodes.filter(_.value == "AAA").head,
      parsedNodes,
      0,
      0,
      "ZZZ"
    )
  }

  println(partOneSolution)

  private val partTwoSolution = {
    val nodesEndingInA = parsedNodes.filter(_.value.endsWith("A"))
    val stepCounts = nodesEndingInA
      .map(node =>
        countStepsRequired(instructions, node, parsedNodes, 0, 0, "Z")
      )
    val primeFactors = findPrimeFactors(stepCounts.min)
    stepCounts.reduceLeft((a, b) => lowestCommonMultiple((a, b), primeFactors))
  }

  println(partTwoSolution)

}
