
object WaterPouring {
  def main(args: Array[String]) {
    val waterPouring = new WaterPouring(Vector(4, 5, 7))
    val paths = waterPouring.calculatePaths(6)

    println(paths take 10 mkString "\n")
  }
}

class WaterPouring(val capacities: Vector[Int]) {

  type State = Vector[Int]

  sealed trait Action {
    def perform(state: State): State
  }
  case class Empty(val jug: Int) extends Action {
    override def perform(state: State): State = state.updated(jug, 0)
  }
  case class Fill(val jug: Int) extends Action {
    override def perform(state: State): State = state.updated(jug, capacities(jug))
  }
  case class Transfer(val fromJug: Int, val toJug: Int) extends Action {
    override def perform(state: State): State = {
      val amntTransferred = state(fromJug) min (capacities(toJug) - state(toJug))
      state updated (fromJug, state(fromJug) - amntTransferred) updated (toJug, state(toJug) + amntTransferred)
    }
  }

  val moves = (for (i <- 0 until capacities.length) yield Empty(i)) ++
    (for (i <- 0 until capacities.length) yield Empty(i)) ++
    (for (i <- 0 until capacities.length) yield Fill(i)) ++
    (for (i <- 0 until capacities.length; j <- 0 until capacities.length; if i != j) yield Transfer(i, j))



  class Path(history: List[Action], val endState: State) {
    def extend(action: Action): Path = new Path(action :: history, action.perform(endState))

    override def toString: String = {
      (history.reverse mkString " ") + " --> " + endState
    }
  }


  def calculatePaths(requiredCapacity: Int): Stream[Path] = {

    val initialState = capacities.map{e => 0}
    val initialPath = new Path(Nil, initialState)

    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
        if (paths.isEmpty) Stream.empty
        else {
          val nextPaths = for {
            path <- paths
            nextPath <- moves map path.extend
            if !(explored contains(nextPath.endState))
          } yield nextPath
          paths #:: from(nextPaths, explored ++ (nextPaths map {_.endState}))
        }
    }

    def solutions: Stream[Path] = {
      for {
        paths <- from(Set(initialPath), Set(initialState))
        path <- paths
        if path.endState contains requiredCapacity
      } yield path
    }


    solutions
  }

}
