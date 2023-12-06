package dayFive

import utils.inputReader.wholeFile

final case class Row(
    seedRange: SeedRange,
    transformation: BigInt => BigInt
)

final case class SeedRange(seedStart: BigInt, seedEnd: BigInt) {
  def split(splitPoint: BigInt): List[SeedRange] = {
    if (splitPoint > this.seedEnd) List(this)
    else
      List(
        SeedRange(this.seedStart, splitPoint),
        SeedRange(splitPoint + 1, this.seedEnd)
      )
  }

  def contains(seed: BigInt): Boolean = {
    seed >= this.seedStart && seed <= this.seedEnd
  }

  def map(transformation: BigInt => BigInt): SeedRange =
    SeedRange(transformation(seedStart), transformation(seedEnd))
}

object puzzle extends App {
  private val seedString =
    "1044452533 40389941 3710737290 407166728 1552449232 639689359 3327654041 26912583 3440484265 219136668 1126550158 296212400 2332393052 229950158 200575068 532702401 4163696272 44707860 3067657312 45353528"
  private val seedNumbers =
    seedString
      .split(" ")
      .map(BigInt(_))
      .toList
  private val maps = wholeFile("5.txt")

  private def parseRow(row: String): Row = {
    val rowRegex = """(\d+) (\d+) (\d+)""".r
    row match {
      case rowRegex(destinationStart, sourceStart, sourceRange) =>
        val transformation = (x: BigInt) =>
          (x - BigInt(sourceStart)) + BigInt(destinationStart)
        Row(
          SeedRange(
            BigInt(sourceStart),
            BigInt(sourceStart) + BigInt(sourceRange) - 1
          ),
          transformation
        )
    }
  }
  private def parseSeedRange(seeds: String): List[SeedRange] = {
    val seedRegex = """(\d+) (\d+)""".r
    (seedRegex findAllIn seeds).matchData.map {
      case seedRegex(seedStart, seedRange) =>
        SeedRange(
          BigInt(seedStart),
          BigInt(seedStart) + BigInt(seedRange) - 1
        )
    }.toList
  }
  private def parseMaps(maps: String): List[List[Row]] = {
    maps
      .split("\n\n")
      .toList
      .map(map => map.split(":").last)
      .map(row =>
        row.trim
          .split("\n")
          .map(parseRow)
          .sortBy(_.seedRange.seedStart)
          .toList
      )
  }

  // Part One
  private def transformSeed(seed: BigInt, map: List[Row]): BigInt = {
    map
      .find(row => row.seedRange.contains(seed))
      .fold(seed)(_.transformation(seed))
  }

  private def transformSeedAcrossMaps(
      seed: BigInt,
      map: List[List[Row]]
  ): BigInt = {
    map match {
      case ::(head, next) =>
        transformSeedAcrossMaps(transformSeed(seed, head), next)
      case Nil => seed
    }
  }

  private val parsedMaps = parseMaps(maps)

  private val partOneSolution = seedNumbers
    .map(seed => transformSeedAcrossMaps(seed, parsedMaps))
    .min

  println(partOneSolution)

  //Part Two
  def transformSeedRange(
      seedRange: SeedRange,
      map: List[Row],
      transformedSeedRanges: List[SeedRange] = List.empty
  ): List[SeedRange] = {
    map.find(row => row.seedRange.contains(seedRange.seedStart)) match {
      case Some(row) =>
        val splitSeedRange = seedRange.split(row.seedRange.seedEnd)
        if (splitSeedRange.length == 1)
          seedRange.map(row.transformation) :: transformedSeedRanges
        else
          transformSeedRange(
            splitSeedRange.last,
            map,
            splitSeedRange.head.map(row.transformation) :: transformedSeedRanges
          )
      case None => seedRange :: transformedSeedRanges
    }
  }

  def transformSeedRanges(
      seedRanges: List[SeedRange],
      map: List[Row]
  ): List[SeedRange] = {
    seedRanges match {
      case ::(head, next) =>
        transformSeedRange(head, map) ++ transformSeedRanges(next, map)
      case Nil => seedRanges
    }
  }

  def transformSeedRangesAcrossMaps(
      seedRanges: List[SeedRange],
      maps: List[List[Row]]
  ): List[SeedRange] = {
    maps match {
      case ::(head, next) =>
        transformSeedRangesAcrossMaps(
          transformSeedRanges(seedRanges, head),
          next
        )
      case Nil => seedRanges
    }
  }

  val parsedSeedStrings = parseSeedRange(seedString)

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  time {
    transformSeedRangesAcrossMaps(parsedSeedStrings, parsedMaps)
      .sortBy(
        _.seedStart
      )
      .head
      .seedStart
  }

  private val partTwoSolution =
    transformSeedRangesAcrossMaps(parsedSeedStrings, parsedMaps)
      .sortBy(
        _.seedStart
      )
      .head
      .seedStart

  println(partTwoSolution)

}
