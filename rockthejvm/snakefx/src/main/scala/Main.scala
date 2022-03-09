import scalafx.application.JFXApp3
import scalafx.beans.property.IntegerProperty
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
import scalafx.application.Platform

case class State(snake: List[(Double, Double)], food: (Double, Double)):
  def newState(dir: Int): State =
    val (x, y) = snake.head

    val (newx, newy) = dir match
      case 1 => (x, y - 25)
      case 2 => (x, y + 25)
      case 3 => (x - 25, y)
      case 4 => (x + 25, y)
      case _ => (x, y)

    val newSnake =
      if newx < 0 || newx >= 600 || newy < 0 || newy >= 600 || snake.tail.contains((newx, newy)) then
        initialSnake
      else if food == (newx, newy) then food :: snake
      else (newx, newy) :: snake.init

    val newFood =
      if food == (newx, newy) then randomFood else food

    State(newSnake, newFood)

  def rectangles: List[Rectangle] =
    rectangle(food._1, food._2, Color.Red) :: snake.map { case (x, y) =>
      rectangle(x, y, Color.Green)
    }

  def rectangle(xr: Double, yr: Double, color: Color): Rectangle =
    new:
      x = xr
      y = yr
      width = 25
      height = 25
      fill = color

end State

val initialSnake = List(
  (250.0, 200.0),
  (225.0, 200.0),
  (200.0, 200.0)
)

def randomFood: (Double, Double) = (Random.nextInt(24) * 25.0, Random.nextInt(24) * 25.0)

object Main extends JFXApp3:

  def gameLoop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1000 / 25 * 2)
    }.flatMap(_ => Future(gameLoop(update)))

  override def start(): Unit =
    val state     = ObjectProperty(State(initialSnake, randomFood))
    val frame     = IntegerProperty(0)
    val direction = IntegerProperty(4)

    frame.onChange {
      state.update(state.value.newState(direction.value))
    }

    stage = new JFXApp3.PrimaryStage:
      width = 600
      height = 600
      scene = new Scene:
        fill = Color.White
        content = state.value.rectangles
        onKeyPressed = key =>
          key.getText match
            case "w" => direction.value = 1
            case "s" => direction.value = 2
            case "a" => direction.value = 3
            case "d" => direction.value = 4
            case _   => direction.value = 0

        frame.onChange {
          Platform.runLater {
            content = state.value.rectangles
          }
        }

    gameLoop(() => frame.update(frame.value + 1))
