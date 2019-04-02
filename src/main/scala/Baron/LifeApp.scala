package Baron

import Life.{ConwayState, Position}
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.stage.WindowEvent
import scalafx.Includes._
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.util.Random

object LifeApp extends JFXApp {

  /**
    * A whole bunch of setup crap.
    */ //SETUP CRAP
  /**
    * Actor system setup for updating.
    */ //ACTOR SETUP
  case object Tick
  val system = ActorSystem("SimpleSystem")
  val actor: ActorRef = system.actorOf(Props[ScheduleActor])
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  /**
    * Random state setup.
    */ //RANDOM STATE SETUP
  val r: Random.type = scala.util.Random
  val rand: ConwayState = newRandomState(0, 120, 0, 100, 5000)

  /**
    * Sets up the graphics system. canvas and gc must be kept publicly accessible.
    */ //GRAPHICS SETUP
  val canvas: Canvas = new Canvas {
    layoutY = 0
    layoutX = 0
    width = 1200
    height = 1000
  }
  val gc: GraphicsContext = canvas.graphicsContext2D
  stage = new JFXApp.PrimaryStage {
    title.value = "Life"
    width = 1200
    height = 1000
    onCloseRequest = (we: WindowEvent) => system.terminate()

    actor ! Tick

    scene = new Scene(1200, 1000) {
      fill = LightGrey
      content = canvas
    }
  }

  /**
    *This is where you define your initial state.
    */ //INITIAL STATE SETUP
  var state:ConwayState = Map()
  setCurrentState(rand)


  /**
    * Returns a randomised position between the coordinates provided, presumably to be used as a mapped value in a ConwayState
    * @param minx Minimum X mapping coordinate
    * @param maxx Maximum X
    * @param miny Minimum Y
    * @param maxy Maximum Y
    * @return Returns a Position
    */ //RANDOM POSITION
  def randPosition(minx:Int, maxx:Int, miny:Int, maxy:Int):Position = (r.nextInt(maxx-minx+1)+minx,r.nextInt(maxy-miny+1)+miny)

  /**
    * Creates a new full random state, for testing purposes.
    * @param x Minimum X mapping of your state.
    * @param y Minimum Y
    * @param xx Maximum X
    * @param yy Maximum Y
    * @param pop How populated do you want the state to be? (Note that this is an upper bound on the cells, as
    *            it's likely the randomiser will map some cells multiple times.)
    * @return Returns a ConwayState.
    */ //GENERATES RANDOM STATE
  def newRandomState(x:Int,y:Int,xx:Int,yy:Int,pop:Int): ConwayState = {
    var tempMap:ConwayState = Map()
    for (a <- 1 to pop) {
      val newCell: Position = randPosition(x, xx, y, yy)
      tempMap += (newCell -> true)
    }
    tempMap
  }

  /**
    * Sets the current state to the input state. Would like to overload this to assume the current state if
    * no parameters are provided.
    * @param newState The state to update to.
    */ //SETS THE CURRENT STATE
  def setCurrentState(newState: ConwayState):Unit ={
    state=newState
    updateCanvas(state)
  }

  /**
    * Updates the graphics canvas with the given state, presumably the current one. Would like to overload this to
    * default to updating to the current state.
    * @param newState The state to update to.
    */ //UPDATES THE CANVAS
  def updateCanvas(newState: ConwayState){
    clearCanvas()
    for ((pos, bool) <- newState) {
      gc.fillRect(10*pos._1,10*pos._2,10,10)
    }
  }

  /**
    * Clears the canvas for redrawing. I feel like there should be a faster way than drawing a new rect every frame?
    * But there's no canvas.clear(), so I don't know...
    */ //CLEARS THE CANVAS
  def clearCanvas(): Unit ={
    gc.fill=LightGrey
    gc.fillRect(canvas.getLayoutX,canvas.getLayoutY,canvas.getWidth,canvas.getHeight)
    gc.fill=Black
  }

  /**
    * Processes into the next ConwayState, updates the canvas, and schedules the next tick. "ticker" is intentional,
    * as calling ticker.cancel() will essentially pause the ConwayState until tick is called again manually. This is
    * for future implementation of a pause feature. Note that the scheduler delay can be used to control the rate at
    * which the ConwaySystem progresses.
    */ //PROCESSES CONWAY SYSTEM ONCE
  def tick() {
    state= Life.nextConwayState(state)
    updateCanvas(state)
    var ticker = system.scheduler.scheduleOnce(50 milliseconds, actor, Tick)
  }

  /**
    * This class listens for the word "Tick" being sent its way, and then causes the system to process once.
    */ //TIMER LISTENER
  class ScheduleActor extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case Tick => tick()
    }
  }
}