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

  val maxIndex = array.length - 1
  val factoradicMax = Model.factorial(array.length) - 1
  val factoradicResult: Long = {
    lehmer.zipWithIndex.foldLeft(0L) {
      case (acum, (x, i)) =>
        acum + x * Model.factorial(maxIndex - i)
    }
  }

  def updateArray(i: Int, nextValue: String): Model = {
    val oldValue: String = array(i)
    val j = array.indexOf(nextValue)
    Model(array.updated(i, nextValue).updated(j, oldValue))
  }
}

object Model {

  def apply(size: Int): Model = Model((0 until size).map(_.toString))

  def resize(oldModel: Model, newMax: Int): Model = Model(newMax)

  def factorial(n: Int): Long = {
    if (n < 2) 1
    else n * factorial(n - 1)
  }

  def apply(oldModel: Model, newFactoradicResult: Long): Model = {
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
}

object FactoradicDemo {

  def apply(): HtmlElement = {
    val InitialSize = 5
    val model = Var(Model(InitialSize))
    val resizer = model.updater[Int]((m, i) => Model.resize(m, i))

    def arrayUpdates(i: Int) = model.updater[String]((m, n) => m.updateArray(i, n))
    def factoradicDecimalUpdater = model.updater[Long]((m,d) => Model(m, d))

    div(
      className := "m-3",
      children <-- model.signal.map { (m: Model) =>
        List(
          div(
            className := "mb-3",
            h1("The set of elements"),
            p(
              "The elements of an ordered set (for example letters) are mapped to the first ",
              input(
                typ := "number",
                controlled(
                  value <-- Var(m.array.length.toString),
                  onInput.mapToValue.filter(_.toIntOption.exists(x => x > 0 && x < 12)).map(_.toInt) --> resizer
                )
              ),
              " numbers."
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
            h1("Permutation"),
            p("You can arrange the elements of the set in any order."),
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
                        onInput.mapToValue.filter(_.toIntOption.exists(x => x >= 0 && x < m.array.length)) --> arrayUpdates(i)
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
            p("The elements of the set are remapped after each position to account for the reduced alternatives available."),
            div(
              className := "container d-inline-block",
              div(
                className := "row align-items-end",
                {
                  val letters = ('A' to 'Z').take(m.array.length).toBuffer
                  for (lehmerCodePoint <- m.lehmer) yield {
                    val explanation = letters.zipWithIndex.map { (letter, i) =>
                      p(className := "mb-1 text-monospace", s"$letter → $i")
                    }
                    letters.remove(lehmerCodePoint)
                    div(
                      className := "col-1 p-1",
                      explanation,
                      input(
                        className := "w-100",
                        value := lehmerCodePoint.toString,
                        disabled := true
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
            p("Interpreting the Lehmer code in the fatoradic number system maps uniquely the permutation to a number. This number can be converted to a decimal representation if we multiply each position in by the factorial of the position index."),
            div(
                className := "container d-inline-block",
                div(
                  className := "row",
                  m.lehmer.zipWithIndex.map {
                    case (x, i) =>
                      div(
                        className := "col-1 p-1 text-center align-bottom",
                        span(className := "text-monospace", s"$x × ${m.maxIndex - i}!"),
                        span(className := "float-right", if(i == m.array.length -1) "=" else "+")
                      )
                  },
                  div(
                    className := "col-1 p-1 text-nowrap",
                    input(
                      typ := "number",
                      value := m.factoradicResult.toString,
                      onChange.mapToValue.map(_.toLongOption).collect { case Some(v) if v >= 0 && v <= m.factoradicMax => v } --> factoradicDecimalUpdater
                    ),
                    span(f" out of ${m.factoradicMax}%,d")
                  )
                )
            ),
          ),
          div(
            className := "mb-3",
            h1("Decimal to Factoradic"),
            p("The decimal representation can be converted back to factoradic by repeatedly performing integer division (displayed from right to left in here) and keeping the remainders."),
            div(
              className := "container d-inline-block",
              div(
                className := "row",
                {
                  var factoradic = m.factoradicResult
                  for (i <- 1 to m.array.length) yield {
                    val factoradicNextDiv = factoradic / i
                    val factoradicNextMod = factoradic % i
                    val oldFactoradic = factoradic
                    factoradic = factoradicNextDiv
                    div(
                      className := "col-1 p-1 text-monospace",
                      mathTag(
                        htmlTag("mfrac")(
                          htmlTag("mrow")(
                            htmlTag("mn")(oldFactoradic)
                          ),
                          htmlTag("mrow")(
                            htmlTag("mn")(i)
                          )
                        ),
                        htmlTag("mo")("="),
                        htmlTag("mn")(factoradicNextDiv),
                        htmlTag("mo")("+"),
                        htmlTag("mn")(factoradicNextMod),
                      ),
                      p(s"$oldFactoradic/$i=$factoradicNextDiv"),
                      p(
                        s"$factoradic%$i=",
                        mark(s"$factoradicNextMod"))
                    )
                  }
                }.reverse
              ),
              div(
                className := "row",
                for (x <- m.lehmer) yield {
                  div(
                    className := "col-1 p-1",
                    input(
                      className := "w-100",
                      value := x.toString,
                      disabled := true
                    )
                  )
                }
              )
            )
          ),
          div(
            className := "mb-3",
            h1("Lehmer code to Permutation"),
            p("Finally the permutation emerges back again by remapping the Lehmer code to the ascending numbers left to right."),
            m.array.map { n =>
              input(
                value := n,
                disabled := true
              )
            }
          )
        )
      }
    )
  }
}
