import java.io.{ BufferedWriter, FileWriter }

import scala.collection.mutable.{ ArrayBuffer, BitSet => MBitSet, Map => MMap }

import org.nlogo.api.MersenneTwisterFast


enum GlobalVar {
  case Population, DiffusionRate, EvaporationRate
}

enum PatchVar {
  case Chemical, Food, IsNest, NestScent, FoodSourceNum
}

import GlobalVar.*
import PatchVar.*

val RNG: MersenneTwisterFast = new MersenneTwisterFast()

object Json {

  def str(s: Any): String =
    s""""${s.asInstanceOf[String]}""""

  def bool(b: Any): String =
    if (b.asInstanceOf[Boolean])
      "true"
    else
      "false"

  def num(x: Any): String =
    x match {
      case d: Double if d == d.toLong => d.toLong.toString
      case d: Double                  => d.toString
      case i: Int                     => i.toString
    }

  def obj(pairs: (String, String)*): String =
    pairs.map { case (k, v) => s""""$k": $v""" }.mkString("{", ", ", "}")

}

trait UpdateKey

enum TurtleKey extends UpdateKey {
  case Breed, Color, Heading, Who, LabelColor, Hidden, Label, PenSize, PenMode, Shape, Size, Xcor, Ycor
}

enum PatchKey extends UpdateKey {
  case PatchID, PColor, PLabel, PLabelColor, Pxcor, Pycor
}

enum WorldKey extends UpdateKey {
  case Height, PatchesAllBlack, PatchesWithLabels, MaxPxcor, MaxPycor, MinPxcor, MinPycor, PatchSize,
       Ticks, UnbreededLinksAreDirected, Width, WorldID, WrappingAllowedInX, WrappingAllowedInY
}

type MiniUpdate[T <: UpdateKey] = MMap[T, Any]

def worldToJson(update: MiniUpdate[WorldKey]): String = {

  import WorldKey.*

  val mappings =
    Seq( "height"                    -> Json.num  -> Height
       , "id"                        -> Json.num  -> WorldID
       , "patchesAllBlack"           -> Json.bool -> PatchesAllBlack
       , "patchesWithLabels"         -> Json.bool -> PatchesWithLabels
       , "maxPxcor"                  -> Json.num  -> MaxPxcor
       , "maxPycor"                  -> Json.num  -> MaxPycor
       , "minPxcor"                  -> Json.num  -> MinPxcor
       , "minPycor"                  -> Json.num  -> MinPycor
       , "patchSize"                 -> Json.num  -> PatchSize
       , "ticks"                     -> Json.num  -> Ticks
       , "unbreededLinksAreDirected" -> Json.bool -> UnbreededLinksAreDirected
       , "width"                     -> Json.num  -> Width
       , "wrappingAllowedInX"        -> Json.bool -> WrappingAllowedInX
       , "wrappingAllowedInY"        -> Json.bool -> WrappingAllowedInY
       )

  val updatePairs =
    mappings.flatMap {
      case ((k, conv), uk) => update.get(uk).map(conv andThen (v => k -> v))
    }

  Json.obj(updatePairs*)

}

def turtleToJson(update: MiniUpdate[TurtleKey]): String = {

  import TurtleKey.*

  val mappings =
    Seq( "breed"       -> Json.str  -> Breed
       , "color"       -> Json.num  -> Color
       , "heading"     -> Json.num  -> Heading
       , "who"         -> Json.num  -> Who
       , "label-color" -> Json.num  -> LabelColor
       , "hidden?"     -> Json.bool -> Hidden
       , "label"       -> Json.str  -> Label
       , "pen-size"    -> Json.num  -> PenSize
       , "pen-mode"    -> Json.str  -> PenMode
       , "shape"       -> Json.str  -> Shape
       , "size"        -> Json.num  -> Size
       , "xcor"        -> Json.num  -> Xcor
       , "ycor"        -> Json.num  -> Ycor
       )

  val updatePairs =
    mappings.flatMap {
      case ((k, conv), uk) => update.get(uk).map(conv andThen (v => k -> v))
    }

  Json.obj(updatePairs*)

}

def patchToJson(update: MiniUpdate[PatchKey]): String = {

  import PatchKey.*

  val mappings =
    Seq( "id"           -> Json.num -> PatchID
       , "pcolor"       -> Json.num -> PColor
       , "plabel"       -> Json.str -> PLabel
       , "plabel-color" -> Json.num -> PLabelColor
       , "pxcor"        -> Json.num -> Pxcor
       , "pycor"        -> Json.num -> Pycor
       )

  val updatePairs =
    mappings.flatMap {
      case ((k, conv), uk) => update.get(uk).map(conv andThen (v => k -> v))
    }

  Json.obj(updatePairs*)

}

object Updater {

  val turtles: MMap[Int, MiniUpdate[TurtleKey]] = MMap.empty
  val patches: MMap[Int, MiniUpdate[PatchKey]]  = MMap.empty
  val world:   MMap[Int, MiniUpdate[WorldKey]]  = MMap.empty

  def drain(): String = {

    def mapJson[V](m: MMap[Int, V], f: V => String): String =
      m.map { case (k, v) => s""""$k": ${f(v)}""" }.mkString("{", ", ", "}")

    val out =
      Json.obj( "turtles" -> mapJson(turtles, turtleToJson)
              , "patches" -> mapJson(patches, patchToJson)
              , "world"   -> mapJson(  world, worldToJson)
              )

    turtles.clear()
    patches.clear()
    world  .clear()

    out

  }

}

object Box {

  def distancexy(patch: Any, x: Any, y: Any): Double = {

    val p =
      patch match {
        case p: Patch => p
        case _        => throw new Exception("Boop!")
      }

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    val shortX = math.abs(p.pxcor - i)
    val shortY = math.abs(p.pycor - j)
    math.sqrt(shortX * shortX + shortY * shortY)

  }

}

object ColorModel {

  val BaseColors: Array[Double] = Array.tabulate(14)(i => i * 10.0 + 5.0)

  def scaleColor(color: Double, value: Double, min: Double, max: Double): Double = {

    val (realMin, realMax) =
      if (min <= max)
        (min, max)
      else
        (max, min)

    val percent =
      if (value > realMax)
        1.0
      else if (value < realMin)
        0.0
      else
        (value - realMin) / (realMax - realMin)

    val percent10 = percent * 10.0

    val finalPercent =
      if (percent10 >= 9.9999)
        9.9999
      else if (percent10 < 0)
        0.0
      else
        percent10

    val modColor     = color % 140.0
    val wrappedColor = if (modColor >= 0) modColor else 140.0 + modColor
    val finalColor   = math.floor(wrappedColor / 10.0)

    finalColor * 10.0 + finalPercent

  }
}

object Random {
  def oneOf[T](choices: Array[T]): T =
    choices(RNG.nextInt(choices.length))
}

trait Agent

class Turtle(val who: Int, shapeName: String, workspace: Workspace) extends Agent {

  var dx:      Double = -1.0
  var dy:      Double = -1.0
  var color:   Double = Random.oneOf(ColorModel.BaseColors)
  var heading: Double = RNG.nextInt(360).toDouble
  var shape:   String = shapeName
  var size:    Double = 1.0
  var xcor:    Double = 0.0
  var ycor:    Double = 0.0

  val myLinks = ArrayBuffer[Unit]()

  recomputeDXY()
  workspace.patchAtCor(xcor, ycor).get.trackTurtle(who)

  Updater.turtles(who) = {
    import TurtleKey.*
    MMap( Breed      -> "turtles"
        , Color      -> color
        , Heading    -> heading
        , Who        -> who
        , LabelColor -> 0
        , Hidden     -> false
        , Label      -> ""
        , PenSize    -> 1
        , PenMode    -> "up"
        , Shape      -> shapeName
        , Size       -> 1
        , Xcor       -> 0
        , Ycor       -> 0
        )
  }

  def ask(f: Turtle => Unit): Unit =
    f(this)

  def rotate(degrees: Any): Unit = {

    val d =
      degrees match {
        case d: Double => d
        case _         => throw new Exception("Boop!")
      }

    val newHeading = (heading + d + 360.0) % 360.0

    if (heading != newHeading) {

      heading = newHeading
      recomputeDXY()

      val mapping = TurtleKey.Heading -> heading
      Updater.turtles.getOrElseUpdate(who, MMap.empty) += mapping

      var i = 0
      while (i < myLinks.size) {
        // Would move any rigid link neighbors
        i = i + 1
      }

    }

  }

  def setColor(value: Double): Unit = {
    if (color != value) {
      color = value
      val mapping = TurtleKey.Color -> color
      Updater.turtles.getOrElseUpdate(who, MMap.empty) += mapping
    }
  }

  def setSize(value: Double): Unit = {
    size = value
    val mapping = TurtleKey.Size -> size
    Updater.turtles.getOrElseUpdate(who, MMap.empty) += mapping
  }

  private def recomputeDXY(): Unit = {

    val rads  = heading * math.Pi / 180.0
    val rawDx = math.sin(rads)
    val rawDy = math.cos(rads)

    dx = if (math.abs(rawDx) < 3.2e-15) 0.0 else rawDx
    dy = if (math.abs(rawDy) < 3.2e-15) 0.0 else rawDy

  }

}

class Patch(idNum: Int, val pxcor: Int, val pycor: Int) extends Agent {

  private val variables: Array[Any] = new Array(PatchVar.values.length)

  var pcolor: Double = 0.0

  private var turtlesHere = MBitSet()

  Updater.patches(idNum) = {
    import PatchKey.*
    MMap( PatchID     -> idNum
        , PColor      -> pcolor
        , PLabel      -> ""
        , PLabelColor -> 0
        , Pxcor       -> pxcor
        , Pycor       -> pycor
        )
  }

  def clear(): Unit = {

    import PatchKey.*

    pcolor = 0.0

    for (n <- 0 until variables.length) {
      variables(n) = 0.0
    }

    Updater.patches(idNum) =
      MMap( PatchID     -> idNum
          , PColor      -> 0.0
          , PLabel      -> ""
          , PLabelColor -> 0
          , Pxcor       -> pxcor
          , Pycor       -> pycor
          )

  }

  def getVar(name: PatchVar): Any =
    variables(name.ordinal)

  def setColor(value: Double): Unit = {
    if (pcolor != value) {
      pcolor = value
      val mapping = PatchKey.PColor -> pcolor
      Updater.patches.getOrElseUpdate(idNum, MMap.empty) += mapping
    }
  }

  def setVar(name: PatchVar, value: Any): Unit = {
    variables(name.ordinal) = value
  }

  def trackTurtle(who: Int): Unit = {
    turtlesHere += who
  }

  def untrackTurtle(who: Int): Unit = {
    turtlesHere -= who
  }

}

class AgentSet[T <: Agent](xs: Array[T]) {

  private val agents: Array[T] = xs.clone()

  def ask(f: T => Unit): Unit = {

    var i = agents.length - 1
    while (i > 0) {
      val j     = RNG.nextInt(i + 1)
      val tmp   = agents(i)
      agents(i) = agents(j)
      agents(j) = tmp
      i -= 1
    }

    var k = 0
    while (k < agents.length) {
      f(agents(k))
      k += 1
    }

  }

  def size: Int =
    agents.length

}

type TurtleSet = AgentSet[Turtle]
type PatchSet  = AgentSet[Patch]

class Workspace(val minPxcor: Int, val maxPxcor: Int, val minPycor: Int, val maxPycor: Int) {

  private var dtsName: String = "turtle"

  private val globals: Array[Any] = new Array(GlobalVar.values.length)

  val worldWidth:  Int = 1 + maxPxcor - minPxcor
  val worldHeight: Int = 1 + maxPycor - minPycor

  private val patches: Array[Patch] = {
    val buffer = ArrayBuffer.empty[Patch]
    for {
      y <- maxPycor to minPycor by -1
      x <- minPxcor to maxPxcor
    } {
      buffer += new Patch(buffer.length, x, y)
    }
    buffer.toArray
  }

  private var turtles: ArrayBuffer[Turtle] = ArrayBuffer.empty

  var ticks: Int = -1

  Updater.world(0) = {
    import WorldKey.*
    MMap( Height                    -> worldHeight
        , WorldID                   -> 0
        , PatchesAllBlack           -> false
        , PatchesWithLabels         -> false
        , MaxPxcor                  -> maxPxcor
        , MaxPycor                  -> maxPycor
        , MinPxcor                  -> minPxcor
        , MinPycor                  -> minPycor
        , PatchSize                 -> 7.0
        , Ticks                     -> -1
        , UnbreededLinksAreDirected -> true
        , Width                     -> worldWidth
        , WrappingAllowedInX        -> false
        , WrappingAllowedInY        -> false
        )
  }

  def allPatches(): PatchSet  = new AgentSet(patches)
  def allTurtles(): TurtleSet = new AgentSet(turtles.toArray)

  def ask(askee: Any, block: Any): Unit = {
    askee match {
      case as: AgentSet[?] => as.ask(block.asInstanceOf[Agent => Unit])
    }
  }

  def canMove(turtle: Any, distance: Any): Boolean = {

    val t =
      turtle match {
        case t: Turtle => t
        case _         => throw new Exception("Boop!")
      }

    val d =
      distance match {
        case d: Double => d
        case _         => throw new Exception("Boop!")
      }

    patchRightAndAhead(t, 0.0, d).isDefined

  }

  def clearAll(): Unit = {
    turtles.clear()
    for (p <- patches) {
      p.clear()
    }
  }

  def createTurtles(num: Any, init: Any): Unit = {

    val n =
      num match {
        case n: Int => n
        case _      => throw new Exception("Boop!")
      }

    val f =
      init match {
        case f: Any => f.asInstanceOf[Turtle => Unit]
      }

    for (_ <- 0 until n) {
      val t = new Turtle(turtles.length, dtsName, this)
      t.ask(f)
      turtles += t
    }

  }

  private val scratch: Array[Double] = new Array(patches.length)

  def diffuse(varName: Any, value: Any): Unit = {

    val vn =
      varName match {
        case vn: PatchVar => vn
        case _            => throw new Exception("Boop!")
      }

    val v =
      value match {
        case v: Double => v
        case _         => throw new Exception("Boop!")
      }

    val xx = worldWidth

    val numPatches = scratch.length

    for (i <- 0 until numPatches) {
      scratch(i) = patches(i).getVar(vn).asInstanceOf[Double]
    }

    for (i <- 0 until numPatches) {

      val bitMask: Short =
        if      (i == 0)                 { 0b0000_1011 }  // Top-left
        else if (i == (xx - 1))          { 0b0001_0110 }  // Top-right
        else if (i == (numPatches - xx)) { 0b0110_1000 }  // Bottom-left
        else if (i == (numPatches - 1))  { 0b1101_0000 }  // Bottom-right
        else if (i < xx)                 { 0b0001_1111 }  // Top row
        else if (i >= (numPatches - xx)) { 0b1111_1000 }  // Bottom row
        else if ((i % xx) == 0)          { 0b0110_1011 }  // Left column
        else if (((i + 1) % xx) == 0)    { 0b1101_0110 }  // Right column
        else                             { 0b1111_1111 }

      var numBits = 0
      var sum     = 0.0

      if ((bitMask & 0b1000_0000) != 0) { sum += scratch(i - xx - 1); numBits += 1 }  // Top-left
      if ((bitMask & 0b0100_0000) != 0) { sum += scratch(i - xx    ); numBits += 1 }  // Top
      if ((bitMask & 0b0010_0000) != 0) { sum += scratch(i - xx + 1); numBits += 1 }  // Top-right
      if ((bitMask & 0b0001_0000) != 0) { sum += scratch(i      - 1); numBits += 1 }  // Left
      if ((bitMask & 0b0000_1000) != 0) { sum += scratch(i      + 1); numBits += 1 }  // Right
      if ((bitMask & 0b0000_0100) != 0) { sum += scratch(i + xx - 1); numBits += 1 }  // Bottom-left
      if ((bitMask & 0b0000_0010) != 0) { sum += scratch(i + xx    ); numBits += 1 }  // Bottom
      if ((bitMask & 0b0000_0001) != 0) { sum += scratch(i + xx + 1); numBits += 1 }  // Bottom-right

      val newValue = scratch(i) + v * (sum / numBits - scratch(i))
      patches(i).setVar(vn, newValue)

    }

  }

  def forward(turtle: Any, units: Any): Unit = {

    val t =
      turtle match {
        case t: Turtle => t
        case _         => throw new Exception("Boop!")
      }

    val us =
      units match {
        case us: Double => us
        case _         => throw new Exception("Boop!")
      }

    val startingPatch = patchAtCor(t.xcor, t.ycor).get

    val isNeg     = us < 0
    var remaining = Math.abs(us)

    while (remaining > 0) {
      val amount      = Math.min(1, remaining)
      val finalAmount = if (isNeg) -amount else amount
      if (canMove(t, finalAmount)) {
        t.xcor += finalAmount * t.dx
        t.ycor += finalAmount * t.dy
      }
      remaining = if (remaining < 1) 0 else remaining - 1
    }

    if (us != 0) {

      var i = 0
      while (i < t.myLinks.size) {
        // Would move any rigid link neighbors
        i = i + 1
      }

      val mappingX = TurtleKey.Xcor -> t.xcor
      val mappingY = TurtleKey.Ycor -> t.ycor

      Updater.turtles.get(t.who).fold {
        Updater.turtles += t.who -> MMap(mappingX, mappingY)
      } {
        _ ++= Seq(mappingX, mappingY)
      }

    }

    val endingPatch = patchAtCor(t.xcor, t.ycor).get

    if (startingPatch != endingPatch) {
      startingPatch.untrackTurtle(t.who)
        endingPatch.  trackTurtle(t.who)
    }

  }

  def getGlobal(name: GlobalVar): Any =
    globals(name.ordinal)

  def patchAt(turtle: Turtle): Option[Patch] =
    patchAtCor(turtle.xcor, turtle.ycor)

  def patchAtCor(x: Double, y: Double): Option[Patch] = {

    val rx =
      if (x > 0) {
        (x + 0.5).toInt
      } else {
        val integral   = x.toInt
        val fractional = integral - x
        if (fractional > 0.5) integral - 1 else integral
      }

    val ry =
      if (y > 0) {
        (y + 0.5).toInt
      } else {
        val integral   = y.toInt
        val fractional = integral - y
        if (fractional > 0.5) integral - 1 else integral
      }

    if (rx >= (maxPxcor + 0.5).toInt ||
        rx <  (minPxcor - 0.5).toInt ||
        ry >= (maxPycor + 0.5).toInt ||
        ry <  (minPycor - 0.5).toInt) {
      None
    } else {
      val index = ((maxPycor - ry) * worldWidth) + rx - minPxcor
      Some(patches(index))
    }

  }

  def patchRightAndAhead(turtle: Any, angle: Any, dist: Any): Option[Patch] = {

    val t =
      turtle match {
        case t: Turtle => t
        case _         => throw new Exception("Boop!")
      }

    val a =
      angle match {
        case a: Double => a
        case _         => throw new Exception("Boop!")
      }

    val d =
      dist match {
        case d: Double => d
        case _         => throw new Exception("Boop!")
      }

    val trueAngle = (t.heading + a) % 360.0
    val rads      = trueAngle * math.Pi / 180.0
    val dx        = math.sin(rads)
    val dy        = math.cos(rads)

    val x = t.xcor + d * (if (math.abs(dx) < 3.2e-15) 0.0 else dx)
    val y = t.ycor + d * (if (math.abs(dy) < 3.2e-15) 0.0 else dy)

    patchAtCor(x, y)

  }

  def resetTicks(): Unit = {
    ticks = 0
    val mapping = WorldKey.Ticks -> ticks
    Updater.world.getOrElseUpdate(0, MMap.empty) += mapping
  }

  def setDefaultTurtleShape(shapeName: Any): Unit = {
    val sn =
      shapeName match {
        case sn: String => sn
        case _          => throw new Exception("Boop!")
      }
    dtsName = sn
  }

  def setGlobal(name: GlobalVar, value: Any): Unit = {
    globals(name.ordinal) = value
  }

  def tick(): Unit = {
    ticks += 1
    val mapping = WorldKey.Ticks -> ticks
    Updater.world.getOrElseUpdate(0, MMap.empty) += mapping
  }

}

object AntsModel {

  val workspace = new Workspace(-35, 35, -35, 35)

  workspace.setGlobal(  DiffusionRate,  50.0)
  workspace.setGlobal(EvaporationRate,  10.0)
  workspace.setGlobal(     Population, 125.0)

  def setup(): Unit = {
    workspace.clearAll()
    workspace.setDefaultTurtleShape("bug")
    workspace.createTurtles(
      workspace.getGlobal(Population).asInstanceOf[Double].toInt,
      (self: Turtle) => {
        self.setSize(  2.0)
        self.setColor(15.0)
      }
    )
    setupPatches()
    workspace.resetTicks()
  }

  def setupPatches(): Unit = {
    workspace.ask(
      workspace.allPatches()
    , (self: Patch) =>
        setupNest(self)
        setupFood(self)
        recolorPatch(self)
    )
  }

  def setupNest(self: Patch): Unit = {
    self.setVar(IsNest,    lessThan(    Box.distancexy(self, 0.0, 0.0), 5.0))
    self.setVar(NestScent, minus(200.0, Box.distancexy(self, 0.0, 0.0)))
  }

  def setupFood(self: Patch): Unit = {
    badIf (lessThan(Box.distancexy(self, times(0.6, workspace.maxPxcor.toDouble), 0.0), 5.0), {
      self.setVar(FoodSourceNum, 1.0)
    })
    badIf (lessThan(Box.distancexy(self, times(-0.6, workspace.maxPxcor.toDouble), times(-0.6, workspace.maxPycor.toDouble)), 5.0), {
      self.setVar(FoodSourceNum, 2.0)
    })
    badIf (lessThan(Box.distancexy(self, times(-0.8, workspace.maxPxcor.toDouble), times(0.8, workspace.maxPycor.toDouble)), 5.0), {
      self.setVar(FoodSourceNum, 3.0)
    })
    badIf (greaterThan(self.getVar(FoodSourceNum).asInstanceOf[Double], 0.0), {
      self.setVar(Food, Random.oneOf(Array(1.0, 2.0)))
    })
  }

  def recolorPatch(self: Patch): Unit = {
    badIfElse (self.getVar(IsNest) == true, {
      self.setColor(115.0)
    }, { badIfElse (greaterThan(self.getVar(Food).asInstanceOf[Double], 0.0), {
        val fsn = self.getVar(FoodSourceNum).asInstanceOf[Double]
        badIf (nlEquals(fsn, 1.0), { self.setColor( 85.0) })
        badIf (nlEquals(fsn, 2.0), { self.setColor( 95.0) })
        badIf (nlEquals(fsn, 3.0), { self.setColor(105.0) })
      }, {
        val color = ColorModel.scaleColor(55.0, self.getVar(Chemical).asInstanceOf[Double], 0.1, 5.0)
        self.setColor(color)
      })
    })
  }

  def go(): Unit = {

    workspace.ask(
      workspace.allTurtles()
    , (self: Turtle) =>
        badIf (lessThan(self.who.toDouble, workspace.ticks.toDouble), {
          badIfElse (nlEquals(self.color, 15.0), {
            lookForFood(self)
          }, {
            returnToNest(self)
          })
          wiggle(self)
          workspace.forward(self, 1.0)
        })
    )

    workspace.diffuse(Chemical, div(workspace.getGlobal(DiffusionRate).asInstanceOf[Double], 100.0))

    val evapRate = workspace.getGlobal(EvaporationRate).asInstanceOf[Double]
    workspace.ask(
      workspace.allPatches()
    , (self: Patch) =>
        self.setVar(Chemical, div(times(self.getVar(Chemical).asInstanceOf[Double], (100.0 - evapRate)), 100.0))
        recolorPatch(self)
    )

    workspace.tick()

  }

  def returnToNest(self: Turtle): Unit = {
    val patchHere = workspace.patchAt(self).get
    badIfElse (patchHere.getVar(IsNest) == true, {
      self.setColor(15.0)
      self.rotate(180.0)
    }, {
      patchHere.setVar(Chemical, plus(patchHere.getVar(Chemical).asInstanceOf[Double], 60.0))
      uphillNestScent(self)
    })
  }

  def lookForFood(self: Turtle): Unit = {
    val patchHere = workspace.patchAt(self).get
    val food      = patchHere.getVar(Food).asInstanceOf[Double]
    badIfElse (greaterThan(food, 0.0), {
      self.setColor(26.0)
      patchHere.setVar(Food, minus(food, 1.0))
      self.rotate(180.0)
    }, {
      val chem = patchHere.getVar(Chemical).asInstanceOf[Double]
      badIf (and(greaterThanOrEqual(chem, 0.05), lessThan(chem, 2.0)), {
        uphillChemical(self)
      })
    })
  }

  def uphillChemical(self: Turtle): Unit = {
    val scentAhead = chemicalAtAngle(self,   0.0)
    val scentRight = chemicalAtAngle(self,  45.0)
    val scentLeft  = chemicalAtAngle(self, -45.0)
    badIf (or(greaterThan(scentRight, scentAhead), greaterThan(scentLeft, scentAhead)), {
      var deg: Double = 0
      badIfElse (greaterThan(scentRight, scentLeft), { deg = 45.0 }, { deg = -45.0 })
      self.rotate(deg)
    })
  }

  def uphillNestScent(self: Turtle): Unit = {
    val scentAhead = nestScentAtAngle(self,   0.0)
    val scentRight = nestScentAtAngle(self,  45.0)
    val scentLeft  = nestScentAtAngle(self, -45.0)
    badIf (or(greaterThan(scentRight, scentAhead), greaterThan(scentLeft, scentAhead)), {
      var deg: Double = 0
      badIfElse (greaterThan(scentRight, scentLeft), { deg = 45.0 }, { deg = -45.0 })
      self.rotate(deg)
    })
  }

  def wiggle(self: Turtle): Unit = {
    self.rotate( RNG.nextInt(40.0.toInt).toDouble)
    self.rotate(-RNG.nextInt(40.0.toInt).toDouble)
    val canInFactMove = workspace.canMove(self, 1.0).asInstanceOf[Any]
    val yesCanMove =
      canInFactMove match {
        case cm: Boolean => cm
        case _           => throw new Exception("Boop!")
      }
    badIf (not(yesCanMove), {
      self.rotate(180.0)
    })
  }

  def nestScentAtAngle(turtle: Turtle, angle: Double): Double =
    workspace.patchRightAndAhead(turtle, angle, 1.0).fold(0.0) {
      patch =>
        val x = patch.asInstanceOf[Any]
        x match {
          case p: Patch => p.getVar(NestScent).asInstanceOf[Double]
          case _        => throw new Exception("Boop!")
        }
    }

  def chemicalAtAngle(turtle: Turtle, angle: Double): Double =
    workspace.patchRightAndAhead(turtle, angle, 1.0).fold(0.0) {
      patch =>
        val x = patch.asInstanceOf[Any]
        x match {
          case p: Patch => p.getVar(Chemical).asInstanceOf[Double]
          case _        => throw new Exception("Boop!")
        }
    }

  def and(x: Any, y: => Any): Boolean = {

    val i =
      x match {
        case i: Boolean => i
        case _          => throw new Exception("Boop!")
      }

    i && {
      val j =
        y match {
          case j: Boolean => j
          case _          => throw new Exception("Boop!")
        }
      j
    }

  }

  def badIf(pred: Any, block: => Any): Unit = {

    val cond =
      pred match {
        case c: Boolean => c
        case _          => throw new Exception("Boop!")
      }

    if (cond) {
      block
    }

  }

  def badIfElse(pred: Any, block1: => Any, block2: => Any): Unit = {

    val cond =
      pred match {
        case c: Boolean => c
        case _          => throw new Exception("Boop!")
      }

    if (cond) {
      block1
    } else {
      block2
    }

  }

  def div(x: Any, y: Any): Double = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    i / j

  }

  def greaterThan(x: Any, y: Any): Boolean = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    i > j

  }

  def greaterThanOrEqual(x: Any, y: Any): Boolean = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    i >= j

  }

  def lessThan(x: Any, y: Any): Boolean = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    i < j

  }

  def minus(x: Any, y: Any): Double = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    i - j

  }

  def nlEquals(x: Any, y: Any): Boolean =
    (x, y) match {
      case (x: Double, y: Double) => x == y
      case _                      => throw new Exception("Boop!")
    }

  def negate(x: Any): Double = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    -i

  }

  def not(x: Any): Boolean = {

    val y =
      x match {
        case y: Boolean => y
        case _          => throw new Exception("Boop!")
      }

    !y

  }

  def or(x: Any, y: => Any): Boolean = {

    val i =
      x match {
        case i: Boolean => i
        case _          => throw new Exception("Boop!")
      }

    i || {
      val j =
        y match {
          case j: Boolean => j
          case _          => throw new Exception("Boop!")
        }
      j
    }

  }

  def plus(x: Any, y: Any): Double = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    i + j

  }

  def times(x: Any, y: Any): Double = {

    val i =
      x match {
        case i: Double => i
        case _         => throw new Exception("Boop!")
      }

    val j =
      y match {
        case j: Double => j
        case _         => throw new Exception("Boop!")
      }

    i * j

  }

}

object Runner {

  @main def run(mode: String): Unit = {
    mode match {

      case "bench" =>
        val start = System.nanoTime()
        for (_ <- 0 until 20) {
          RNG.setSeed(1234)
          AntsModel.setup()
          for (_ <- 0 until 1000) {
            AntsModel.go()
          }
        }
        val end = System.nanoTime()
        println((end - start) / 1e9)

      case "json" =>

        val writer = new BufferedWriter(new FileWriter("../microtortoise-data-scala.json"))
        writer.write("[")
        writer.write(Updater.drain())

        AntsModel.setup()
        writer.write(",")
        writer.write(Updater.drain())

        for (_ <- 0 until 1000) {
          AntsModel.go()
          writer.write(",")
          writer.write(Updater.drain())
        }

        writer.write("]")
        writer.close()

      case _ =>
        throw new Exception("Run mode must be one of: bench | json")

    }
  }

}
