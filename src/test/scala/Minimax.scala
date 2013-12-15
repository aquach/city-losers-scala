import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import com.citylosers.Minimax

object MinimaxTest extends Properties("Minimax") {
  case class TestCase(tree: Seq[Any], depth: Int, resultScore: Int)

  val testCases = Array(
    TestCase(
      Seq(
        Seq(
          Seq(
            Seq(10, 10000),
            Seq(5)
          ),
          Seq(
            Seq(-10)
          )
        ),
        Seq(
          Seq(
            Seq(7, 5),
            Seq(-10000)
          ),
          Seq(
            Seq(-7, -5)
          )
        )
      ),
      4,
      -7
    ),

    TestCase(
      Seq(
        Seq(
          Seq(
            Seq(3, 17),
            Seq(2, 12)
          ),
          Seq(
            Seq(15),
            Seq(25, 0)
          )
        ),
        Seq(
          Seq(
            Seq(2, 5),
            Seq(3)
          ),
          Seq(
            Seq(2, 14)
          )
        )
      ),
      4,
      3
    ),

    TestCase(
      Seq(
        Seq(
          Seq(
            Seq(5, 6),
            Seq(7, 4, 5)
          ),
          Seq(
            Seq(3)
          )
        ),
        Seq(
          Seq(
            Seq(6),
            Seq(6, 9)
          ),
          Seq(
            Seq(7)
          )
        ),
        Seq(
          Seq(
            Seq(5)
          ),
          Seq(
            Seq(9, 8),
            Seq(6)
          )
        )
      ),
      4,
      6
    )
  )
    
  def positionHeuristic(state: Any) = state.asInstanceOf[Int]
  def computeNextStates(state: Any): Seq[Any] = state.asInstanceOf[Seq[Any]]

  property("aminimaxPassesTestCases") = forAll(Gen.oneOf(testCases)) { t =>
    val m = new Minimax(positionHeuristic, computeNextStates, maxDepth = t.depth)
    m.aminimax(t.tree, forPositivePlayer = true)._2 == t.resultScore
  }

  property("minimaxPassesTestCases") = forAll(Gen.oneOf(testCases)) { t =>
    val m = new Minimax(positionHeuristic, computeNextStates, maxDepth = t.depth)
    m.minimax(t.tree, forPositivePlayer = true)._2 == t.resultScore
  }
}
