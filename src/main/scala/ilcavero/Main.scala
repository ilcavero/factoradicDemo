package ilcavero

import org.scalajs.dom
import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.{*, given}

@main
def Main(): Unit = {
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    FactoradicDemo()
  )
}

case class Model(array: IndexedSeq[String]) {

  val lehmer: List[Int] = {
    val intValues = array.map(_.toInt).reverse.toList

    def lehmerRec(l: List[Int]): List[Int] = l match {
      case Nil => l
      case head :: tail =>
        (head - tail.count(_ < head)) :: lehmerRec(tail)
    }

    lehmerRec(intValues).reverse
  }

  private def factorial(n: Int): Long = {
    if (n < 2) 1
    else n * factorial(n - 1)
  }

  val maxIndex: Int = array.length - 1
  val factoradicMax: Long = factorial(array.length) - 1
  val factoradicResult: Long = {
    lehmer.zipWithIndex.foldLeft(0L) {
      case (acum, (x, i)) =>
        acum + x * factorial(maxIndex - i)
    }
  }

}

object Model {

  def apply(size: Int): Model = Model((0 until size).map(_.toString))

  def resize(oldModel: Model, newMax: Int): Model = {
    if(newMax > oldModel.array.size) {
      Model(oldModel.array.appendedAll(( oldModel.array.size until newMax).map(_.toString)))
    } else {
      var newArray = oldModel.array.take(newMax).map(_.toInt)
      for(i <- newMax until oldModel.array.size) {
        val outValue = oldModel.array(i).toInt
        if(outValue < newMax) {
          val (_, j) = newArray.zipWithIndex.find(x => x._1 >= newMax).get
          newArray = newArray.updated(j, outValue)
        }
      }
      Model(newArray.map(_.toString))
    }
  }

  def updateDecimalFactoradic(oldModel: Model, newFactoradicResult: Long): Model = {
    var factoradic = newFactoradicResult
    val lehmerReverse = for (i <- 1 to oldModel.array.length) yield {
      val factoradicNextDiv = factoradic / i
      val factoradicNextMod = factoradic % i
      factoradic = factoradicNextDiv
      factoradicNextMod.toInt
    }
    val setIndices = oldModel.array.indices.toBuffer
    val newArray = lehmerReverse.reverse.map { lehmerCodePoint =>
      setIndices.remove(lehmerCodePoint).toString
    }
    Model(newArray)
  }

  def updateArray(m: Model, i: Int, nextValue: String): Model = {
    val oldValue: String = m.array(i)
    val j = m.array.indexOf(nextValue)
    Model(m.array.updated(i, nextValue).updated(j, oldValue))
  }

  def updateLehmer(oldModel: Model, i: Int, l: String): Model = {
    val setIndices = oldModel.array.indices.toBuffer
    val newLehmer = oldModel.lehmer.toIndexedSeq.updated(i, l.toInt)
    dom.console.debug(s"updating $i $l  $newLehmer")
    val newArray = newLehmer.map { lehmerCodePoint =>
      setIndices.remove(lehmerCodePoint).toString
    }
    Model(newArray)
  }
}

object FactoradicDemo {

  def apply(): HtmlElement = {
    val InitialSize = 5
    val model = Var(Model(InitialSize))
    val resizer = model.updater[Int]((m, i) => Model.resize(m, i))

    def arrayUpdater(i: Int) = model.updater[String]((m, n) => Model.updateArray(m, i, n))

    def factoradicDecimalUpdater = model.updater[String]{ (m,d) =>
      d.toLongOption match
        case Some(v) if v >= 0 && v <= m.factoradicMax => Model.updateDecimalFactoradic(m, v)
        case _ => m
    }

    def lehmerUpdater(i: Int) = model.updater[String]( (m, l) => Model.updateLehmer(m,i,l))

    div(
      className := "m-3",
      children <-- model.signal.map { (m: Model) =>
        List(
          div(
            className := "mb-3",
            h1("A set of elements..."),
            p(
              "The elements of an ordered set (for example letters) can be mapped to the first ",
              input(
                typ := "number",
                controlled(
                  value <-- Var(m.array.length.toString),
                  onInput.mapToValue.filter(_.toIntOption.exists(x => x > 0 && x < 12)).map(_.toInt) --> resizer
                )
              ),
              " numbers starting with 0."
            ),
            div(
              className := "container  d-inline-block",
              p(
                className := "row ",
                ('A' to 'Z').take(m.array.length).zipWithIndex.map { (letter, i) =>
                  span(className:= "col-1", s"$letter → $i")
                }
              )
            )
          ),
          div(
            className := "mb-3",
            h1("Permutation of mapped elements"),
            p("These numbers ordered without duplicates represent a permutation. Try changing the values to create a new permutation, this page will not let you input duplicates."),
            div(
              className := "container d-inline-block",
              div(
                className := "row",
                for (i <- m.array.indices) yield {
                  div(
                    className := "col-1 p-1",
                    input(
                      className := "w-100",
                      typ := "number",
                      controlled(
                        value <-- Var(m.array(i)),
                        onInput.mapToValue.filter(_.toIntOption.exists(x => x >= 0 && x < m.array.length)) --> arrayUpdater(i)
                      )
                    )
                  )
                }
              )
            )
          ),
          div(
            className := "mb-3",
            h1("Permutation as a Lehmer Code"),
            p("The permutation can also be represented by a Lehmer code if after each position you remap the set by removing selected elements to account for the reduced alternatives available. Try changing the code to create a new permutation."),
            div(
              className := "container d-inline-block",
              div(
                className := "row align-items-end",
                {
                  val letters = ('A' to 'Z').take(m.array.length).toBuffer
                  for (i <- m.lehmer.indices) yield {
                    val lehmerCodePoint = m.lehmer(i)
                    val explanation = letters.zipWithIndex.map { (letter, i) =>
                      val text = s"$letter → $i"
                      p(className := "mb-1 text-monospace", if(i == lehmerCodePoint) mark(cls := "p-0", text) else text)
                    }
                    letters.remove(lehmerCodePoint)
                    div(
                      className := "col-1 p-1",
                      explanation,
                      input(
                        className := "w-100",
                        typ := "number",
                        controlled(
                          value <-- Var(lehmerCodePoint.toString),
                          onInput.mapToValue.filter(_.toIntOption.exists(x => x >= 0 && x <= (m.maxIndex - i))) --> lehmerUpdater(i)
                        )
                      )
                    )
                  }
                }
              )
            )
          ),
          div(
            className := "mb-3",
            h1("Factoradic to decimal"),
            p("Interpreting the Lehmer code in the factoradic number system uniquely maps the permutation to a single number. This number can be converted to a decimal representation if we multiply each position in by the factorial of the position index."),
            div(
                className := "container d-inline-block",
                div(
                  className := "row align-items-center",
                  m.lehmer.zipWithIndex.map {
                    case (x, i) =>
                      div(
                        className := "col-1 p-1 text-center",
                        span(className := "text-monospace", s"$x × ${m.maxIndex - i}!"),
                        span(className := "float-right", if(i == m.array.length -1) "=" else "+")
                      )
                  },
                  div(
                    className := "col-1 p-1 text-nowrap",
                    input(
                      typ := "number",
                      value := m.factoradicResult.toString,
                      onChange.mapToValue --> factoradicDecimalUpdater
                    ),
                    span(f" out of ${m.factoradicMax}%,d (Try changing this number to generate a new permutation)")
                  )
                )
            ),
          ),
          div(
            className := "mb-3",
            h1("Decimal to Factoradic"),
            p("The decimal representation can be converted back to factoradic by repeatedly performing integer division (performed from right to left in here) and keeping the remainders."),
            div(
              className := "container d-inline-block",
              div(
                cls := "row align-items-end",
                {
                  var factoradic = m.factoradicResult
                  for (i <- 1 to m.array.length) yield {
                    val factoradicNextDiv = factoradic / i
                    val factoradicNextMod = factoradic % i
                    val oldFactoradic = factoradic
                    factoradic = factoradicNextDiv
                    div(cls := "col-1 p-1",
                      p(cls := "text-monospace", s"$oldFactoradic / $i = $factoradicNextDiv"),
                      p(cls := "text-monospace", s"$oldFactoradic % $i = ", mark(s"$factoradicNextMod")),
                      input(
                        className := "w-100",
                        value := m.lehmer(m.array.length - i).toString,
                        disabled := true
                      )
                    )
                  }
                }.reverse
              )
            )
          ),
          div(
            className := "mb-3",
            h1("Lehmer code to Permutation"),
            p("Finally the permutation emerges back again by mapping the Lehmer code to the set elements and removing selected elements left to right."),
            div(
              className := "container d-inline-block",
              div(
                className := "row align-items-end",
                {
                  val letters = ('A' to 'Z').take(m.array.length).toBuffer
                  for (lehmerCodePoint <- m.lehmer) yield {
                    val explanation = letters.zipWithIndex.map { (letter, i) =>
                      val text = s"$i → $letter"
                      p(className := "mb-1 text-monospace", if(i == lehmerCodePoint) mark(cls := "p-0",text) else text)
                    }
                    val letter = letters.remove(lehmerCodePoint)
                    div(
                      className := "col-1 p-1",
                      explanation,
                      input(
                        className := "w-100",
                        value := letter.toString,
                        disabled := true
                      )
                    )
                  }
                }
              )
            )
          )
        )
      }
    )
  }
}