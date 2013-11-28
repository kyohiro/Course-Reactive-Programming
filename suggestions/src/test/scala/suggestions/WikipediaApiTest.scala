package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import rx.lang.scala.concurrency.Schedulers


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
  
  test("WikipediaApi recovered") {
    //Here we have to use throw new Exception
    //new Exception only returns an object and capture by type inference, wrapped into Try[Any]
    //throws new Exception would cause onError on the Observable stream
    val stream = Observable(1,2,3,4,5).map(x => if(x != 3) x else throw new Exception)
    val res = stream.recovered.foldLeft(0)((acc, tn) => tn match{
      case Success(n) => acc + 1
      case Failure(t) => acc
    })
    var v = 0
    res.observeOn(Schedulers.immediate).subscribe(e => v = e)
    assert(v === 2)
}
  
  test("concatRecovered behaves as promised") {
    val req = Observable(1,2,3,4,5)
    
    val response = req.concatRecovered(num => if (num != 4) Observable(num) else Observable(new Exception))
    val res = response.foldLeft((0,0)) { (acc, tn) =>
      tn match {
        case Success(n) => (acc._1 + n, acc._2)
        case Failure(_) => (acc._1, acc._2 + 1)
      }
    }
    
    var pair = (0, 0)
    res.observeOn(Schedulers.immediate).subscribe(e => pair = e)
    val (sum, fc) = pair
    assert(sum == (1 + 2 + 3 + 5), "Wrong sum: " + sum)
    assert(fc == 1, "Wrong failurecount: " + fc)
  }
}