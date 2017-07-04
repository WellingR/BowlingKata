import scala.util.Random

object Bowling {
  sealed trait FrameScore {
    def totaalScore(volgendeBallen: List[Int]): Int = {
      this match {
        case Strike => 10 + volgendeBallen.take(2).sum
        case Spare(_) => 10 + volgendeBallen.take(1).sum
        case Worp(a, b) => a + b
        case LaatsteSpare(_, extra) => 10 + extra
        case LaatsteStrike(extra1, extra2) => 10 + extra1 + extra2
      }
    }

    def ballen: List[Int] = this match {
      case Strike => List(10)
      case Spare(eerste) => List(eerste, 10 - eerste)
      case Worp(first, second) => List(first, second)
      case LaatsteSpare(eerste, extra) => List(eerste, 10 - eerste, extra)
      case LaatsteStrike(extra1, extra2) => List(10, extra1, extra2)
    }

    def presenteer: String = this match {
      case Strike => "X     "
      case Spare(eerste) => s"$eerste /  "
      case Worp(eerste, tweede) => s"$eerste $tweede  "
      case s @ LaatsteSpare(eerste, extra) => s"$eerste / $extra"
      case LaatsteStrike(extra1, extra2) => s"X $extra1 $extra2"
    }
  }
  sealed trait LaatsteFrameScore extends FrameScore
  sealed trait NormaalFrameScore extends FrameScore

  case class Worp(first: Int, second: Int) extends NormaalFrameScore with LaatsteFrameScore
  case object Strike extends NormaalFrameScore
  case class Spare(eerste: Int) extends NormaalFrameScore
  case class LaatsteStrike(extra1: Int, extra2: Int) extends LaatsteFrameScore
  case class LaatsteSpare(eerste: Int, extraBal: Int) extends LaatsteFrameScore

  def worp(pillonneOver: Int = 10)(implicit random: Random): Int =
    random.nextInt(pillonneOver+1)

  def beurt()(implicit random: Random): NormaalFrameScore = {
    val worp1 = worp()
    if (worp1 == 10) Strike
    else {
      val worp2 = worp(10 - worp1)
      if (worp2 + worp1 == 10) Spare(worp1)
      else Worp(worp1, worp2)
    }
  }

  def laatsteBeurt()(implicit random: Random): LaatsteFrameScore = {
    beurt() match {
      case gewoon: Worp => gewoon
      case Spare(eerste) => LaatsteSpare(eerste, worp())
      case Strike =>
        val extra1 = worp()
        val extra2 = worp(pillonneOver = 10 - extra1)
        LaatsteStrike(extra1, extra2)
    }
  }

  def main(args: Array[String]): Unit = {
    // seed 17 eindigt met strike
    val seed = Random.nextLong()
    println(s"random seed: $seed")
    implicit val random = new Random(seed)
    val beurten = (for (i <- 1 to 9) yield beurt()) :+ laatsteBeurt()

    val slidingWindow = beurten.sliding(3).toList ++ Seq(beurten.takeRight(2), beurten.takeRight(1))

    val scoresPerFrame = slidingWindow.map {
      case Seq(current, next, afterNext) =>
        val volgendeBallen = next.ballen ::: afterNext.ballen
        (current, current.totaalScore(volgendeBallen))
      case Seq(current, next) =>
        val volgendeBallen = next.ballen
        (current, current.totaalScore(volgendeBallen))
      case Seq(current) =>
        (current, current.totaalScore(Nil))
    }

    val totaalScores = scoresPerFrame.scanLeft(0){
      case (prevScore, (frame, score)) => prevScore + score
    }.tail
    val scoresPerBeurt = beurten zip totaalScores

    println(scoresPerBeurt.map{case (frame, score) => s"${frame.presenteer}  => $score"}.mkString("\n"))
  }

}
