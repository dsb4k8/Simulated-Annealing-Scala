import scala.util.Random
object Main {
  //init globals
  val numLabs = 5
  val labCapacity = 20
  val numStudents = 100
  val studentPrefs = Seq.fill(numStudents)(Random.shuffle((1 to numLabs).toList))

//  define init state and temp params
  val initState = studentPrefs.map(prefs => Random.shuffle((1 to numLabs).toList).find(prefs.contains).get)
  val t0 = 10.0
  val alpha = 0.9

//  define the loss function
  def computeLoss(state: Seq[Int]) = {
    val labCounts = state.groupBy(identity).view.mapValues(_.size)
    val overCapacity = labCounts.values.map(count => Math.max(count - labCapacity, 0)).sum
    val studentLosses = state.zip(studentPrefs).map{
      case (lab,prefs) => val idx = prefs.indexOf(lab)
        if(idx >= 0) idx else numLabs
    }
    studentLosses.sum + overCapacity
  }
//  define neighbor function to generate next canadates
  def generateNeighbor(state: Seq[Int]): Seq[Int] = {
    val i = Random.nextInt(numStudents)
    state.updated(i, Random.shuffle((1 to numLabs).toList).find(_!=state(i)).get)
  }
//  simulated annealing loop => optimal state search
  var state = initState
  var loss = computeLoss(state)
//  todo => complete looping logic
  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }
}