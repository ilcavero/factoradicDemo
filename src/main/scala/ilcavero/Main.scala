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
    if (newMax > oldModel.array.size) {
      Model(oldModel.array.appendedAll((oldModel.array.size until newMax).map(_.toString)))
    } else {
      var newArray = oldModel.array.take(newMax).map(_.toInt)
      for (i <- newMax until oldModel.array.size) {
        val outValue = oldModel.array(i).toInt
        if (outValue < newMax) {
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

  def updateLehmer(oldModel: Model, i: Int, l: Int): Model = {
    val setIndices = oldModel.array.indices.toBuffer
    val newLehmer = oldModel.lehmer.toIndexedSeq.updated(i, l)
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
    val resizer = model.updater[String]((m, s) => s.toIntOption.filter(x => x > 0 && x < 12).map(i => Model.resize(m, i)).getOrElse(m))

    def arrayUpdater(i: Int) = model.updater[String]((m, n) => if n.toIntOption.exists(x => x >= 0 && x < m.array.length) then Model.updateArray(m, i, n) else m)

    def factoradicDecimalUpdater = model.updater[String] { (m, d) =>
      d.toLongOption match
        case Some(v) if v >= 0 && v <= m.factoradicMax => Model.updateDecimalFactoradic(m, v)
        case _ => m
    }

    def lehmerUpdater(i: Int) = model.updater[String]((m, l) => l.toIntOption.filter(x => x >= 0 && x <= (m.maxIndex - i)).map(li => Model.updateLehmer(m, i, li)).getOrElse(m))

    div(
      className := "m-3",
      children <-- model.signal.map { (m: Model) =>
        val cols = if (m.array.size > 5) "col-1" else "col-2"
        List(
          div(
            className := "mb-3",
            h1("A set of elements..."),
            p(
              "The elements of an ordered set (for example letters) can be mapped to the first ",
              input(
                typ := "number",
                minAttr := "1",
                maxAttr := "11",
                value := m.array.length.toString,
                onChange.mapToValue --> resizer
              ),
              " numbers starting with 0."
            ),
            div(
              className := "container  d-inline-block",
              p(
                className := "row ",
                ('A' to 'Z').take(m.array.length).zipWithIndex.map { (letter, i) =>
                  span(className := s"$cols", s"$letter → $i")
                }
              )
            )
          ),
          div(
            className := "mb-3",
            h1("Permutation of mapped elements"),
            p("A permutation of the set (no duplicates) can be represented as a sequence of these mapped numbers. Try changing the numbers to create a new permutation, this page will not let you input duplicates."),
            div(
              className := "container d-inline-block",
              div(
                className := "row",
                for (i <- m.array.indices) yield {
                  div(
                    className := s"$cols p-1",
                    p(className := "text-center", ('A' + m.array(i).toInt).toChar),
                    input(
                      className := "w-100",
                      typ := "number",
                      minAttr := "0",
                      maxAttr := s"${m.maxIndex}",
                      value := m.array(i),
                      onChange.mapToValue --> arrayUpdater(i)
                    )
                  )
                }
              )
            )
          ),
          div(
            className := "mb-3",
            h1("Permutation as a Lehmer Code"),
            p("The permutation can also be represented by a ", a("Lehmer code", target := "_blank", href := "https://en.wikipedia.org/wiki/Lehmer_code"), " if after each position you remap the set to numbers by removing selected elements to account for the reduced alternatives available. Try changing the code to create a new permutation."),
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
                      p(className := "mb-1 text-monospace", if (i == lehmerCodePoint) mark(cls := "p-0", text) else text)
                    }
                    letters.remove(lehmerCodePoint)
                    div(
                      className := s"$cols p-1",
                      explanation,
                      input(
                        className := "w-100",
                        typ := "number",
                        minAttr := "0",
                        maxAttr := s"${m.maxIndex - i}",
                        value := lehmerCodePoint.toString,
                        onChange.mapToValue --> lehmerUpdater(i)
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
            p("Interpreting the Lehmer code in the ", a("factoradic number system", target := "_blank", href := "https://en.wikipedia.org/wiki/Factorial_number_system"), f" uniquely maps the permutation to a number between 0 and ${m.factoradicMax}%,d. This conversion is done by multiplying each position in the code by the factorial of the position index. Try changing this number to generate a new permutation."),
            div(
              className := "container d-inline-block",
              div(
                className := "row align-items-center",
                m.lehmer.zipWithIndex.map {
                  case (x, i) =>
                    div(
                      className := s"$cols p-1 text-center",
                      span(className := "text-monospace", s"$x × ${m.maxIndex - i}!"),
                      span(className := "float-right", if (i == m.array.length - 1) "=" else "+")
                    )
                },
                div(
                  className := "col-1 p-1",
                  input(
                    typ := "number",
                    minAttr := "0",
                    maxAttr := s"${m.factoradicMax}",
                    value := m.factoradicResult.toString,
                    onChange.mapToValue --> factoradicDecimalUpdater
                  )
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
                cls := "row align-items-end", {
                  var factoradic = m.factoradicResult
                  for (i <- 1 to m.array.length) yield {
                    val factoradicNextDiv = factoradic / i
                    val factoradicNextMod = factoradic % i
                    val oldFactoradic = factoradic
                    factoradic = factoradicNextDiv
                    div(cls := s"$cols p-1",
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
                      p(className := "mb-1 text-monospace", if (i == lehmerCodePoint) mark(cls := "p-0", text) else text)
                    }
                    val letter = letters.remove(lehmerCodePoint)
                    div(
                      className := s"$cols p-1",
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