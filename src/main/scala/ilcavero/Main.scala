package ilcavero

import com.raquo.airstream.state.{DerivedVar, SourceVar}

import scala.scalajs.js
import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}


@main
def Main(): Unit = {
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    FactoradicDemo()
  )
}

object FactoradicDemo {

  def apply(): HtmlElement = {
    val InitialSize = 10
    val arraySize = Var(InitialSize)
    val factoradicDescription = Var("")

    div(
      h1("Size of the sequence"),
      input(
        typ := "number",
        controlled(
          value <-- arraySize.signal.map(_.toString),
          onInput.mapToValue.filter(_.toIntOption.exists(x => x > 0 && x <= 20)).map(_.toInt) --> arraySize
        )
      ),
      h1("Sequence"),
      div(
        child <-- arraySize.signal.map { (max: Int) =>
          dom.console.debug(s"resetting array")
          val array = (0 until max).map(i => Var(i.toString))
          val arrayUpdates = array.map { varr =>
            varr.updater[String] { (old, next) =>
              array.find(_.now() == next).foreach(_.set(old))
              next
            }
          }

          def sequenceReverse[T](vars: Seq[Var[T]]): Signal[List[T]] = {
            vars.foldLeft[Signal[List[T]]](Var(List.empty[T]).signal) {
              case (acumSignal, xVar) =>
                acumSignal.combineWith(xVar.signal).map {
                  case (acum, x) => x :: acum
                }
            }
          }
          val lehmer = sequenceReverse(array).map { values =>
            val intValues = values.map(_.toInt)
            def lehmer(l: List[Int]): List[Int] = l match {
              case Nil => l
              case head :: tail =>
                (head - tail.count(_ < head)) :: lehmer(tail)
            }
            lehmer(intValues).reverse
          }

          div(
            for(i <- 0 until max) yield {
              input(
                typ := "number",
                controlled(
                  value <-- array(i),
                  onInput.mapToValue.filter(_.toIntOption.exists(x => x >= 0 && x < max)) --> arrayUpdates(i))
              )
            },
            h1("Sequence as lehmer Code"),
            div(
              children <-- lehmer.map { values =>
                for(x <- values) yield {
                  input(
                    value := x.toString,
                    disabled := true
                  )
                }
              }
            ),
            h1("Factoradic to decimal"),
            div(
              children <-- lehmer.map { values =>
                def factorial(n: Int): Long = {
                  if(n < 2) 1
                  else n * factorial(n - 1)
                }
                val factorialMax = values.size - 1
                val steps = values.zipWithIndex.map {
                  case (x, i) =>
                    s"${x} × ${factorialMax - i}!"
                }.mkString(" + ")
                val result = values.zipWithIndex.foldLeft(0L) {
                  case (acum, (x, i)) =>
                    acum + x * factorial(factorialMax - i)
                }
                List(
                  label(s"$steps = "),
                  input(value := s"$result", typ := "number"),
                  label(f" out of ${factorial(values.size) - 1}%,d")
                )
              }
            )

          )
        }
      )
    )
  }
}
