import org.nlogo.api.MersenneTwisterFast

import java.io.{BufferedWriter, FileWriter}

import scala.collection.mutable.{ ArrayBuffer, Map => MMap }

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
  def distancexy(patch: Patch, x: Double, y: Double): Double = {
    val shortX = math.abs(patch.pxcor - x)
    val shortY = math.abs(patch.pycor - y)
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

class Turtle(val who: Int, shapeName: String) extends Agent {

  private val variables: MMap[String, Any] = MMap.empty

  var dx:      Double = -1.0
  var dy:      Double = -1.0
  var color:   Double = Random.oneOf(ColorModel.BaseColors)
  var heading: Double = RNG.nextInt(360).toDouble
  var shape:   String = shapeName
  var size:    Double = 1.0
  var xcor:    Double = 0.0
  var ycor:    Double = 0.0

  recomputeDXY()

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

  def getVar(name: String): Any =
    variables(name)

  def rotate(degrees: Double): Unit = {

    heading = (heading + degrees + 360.0) % 360.0
    recomputeDXY()

    val mapping = TurtleKey.Heading -> heading
    Updater.turtles.get(who).fold {
      Updater.turtles += who -> MMap(mapping)
    } {
      _ += mapping
    }

  }

  def setColor(value: Double): Unit = {
    color = value
    val mapping = TurtleKey.Color -> color
    Updater.turtles.get(who).fold {
      Updater.turtles += who -> MMap(mapping)
    } {
      _ += mapping
    }
  }

  def setSize(value: Double): Unit = {
    size = value
    val mapping = TurtleKey.Size -> size
    Updater.turtles.get(who).fold {
      Updater.turtles += who -> MMap(mapping)
    } {
      _ += mapping
    }
  }

  def setVar(name: String, value: Any): Unit = {
    variables(name) = value
  }

  private def recomputeDXY(): Unit = {

    val rads  = heading * math.Pi / 180.0
    val rawDx = math.sin(rads)
    val rawDy = math.cos(rads)

    dx = if (math.abs(rawDx) < 3.2e-15) 0.0 else rawDx
    dy = if (math.abs(rawDy) < 3.2e-15) 0.0 else rawDy

  }

}

class Patch(idNum: Int, vars: Array[String], val pxcor: Int, val pycor: Int) extends Agent {

  private val variables: MMap[String, Any] = MMap.empty

  var pcolor: Double = 0.0

  for (v <- vars) {
    variables(v) = 0.0
  }

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

    for (k <- variables.keys) {
      variables(k) = 0.0
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

  def getVar(name: String): Any =
    variables(name)

  def setColor(value: Double): Unit = {
    pcolor = value
    val mapping = PatchKey.PColor -> pcolor
    Updater.patches.get(idNum).fold {
      Updater.patches += idNum -> MMap(mapping)
    } {
      _ += mapping
    }
  }

  def setVar(name: String, value: Any): Unit = {
    variables(name) = value
  }

}

class AgentSet[T <: Agent](xs: Array[T]) {

  private val agents: Array[T] = xs.clone()

  def ask(f: T => Unit): Unit = {
    var i = 0
    while (i < agents.length) {
      if (i < agents.length - 1) {
        val randNum = i + RNG.nextInt(agents.length - i)
        f(agents(randNum))
        agents(randNum) = agents(i)
      } else {
        f(agents(i))
      }
      i += 1
    }
  }

  def size: Int =
    agents.length

}

type TurtleSet = AgentSet[Turtle]
type PatchSet  = AgentSet[Patch]

class Workspace( globalVars: Array[String], patchVars: Array[String], val minPxcor: Int
               , val maxPxcor: Int, val minPycor: Int, val maxPycor: Int) {

  private var dtsName: String = "turtle"

  private val globals: MMap[String, Any] = MMap.empty

  for (v <- globalVars) {
    globals(v) = 0.0
  }

  val worldWidth:  Int = 1 + maxPxcor - minPxcor
  val worldHeight: Int = 1 + maxPycor - minPycor

  private val patches: Array[Patch] = {
    val buffer = ArrayBuffer.empty[Patch]
    for {
      y <- maxPycor to minPycor by -1
      x <- minPxcor to maxPxcor
    } {
      buffer += new Patch(buffer.length, patchVars, x, y)
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

  def canMove(turtle: Turtle, distance: Double): Boolean =
    patchRightAndAhead(turtle, 0, distance).isDefined

  def clearAll(): Unit = {
    turtles.clear()
    for (p <- patches) {
      p.clear()
    }
  }

  def createTurtles(num: Int, init: Turtle => Unit): Unit = {
    for (_ <- 0 until num) {
      val t = new Turtle(turtles.length, dtsName)
      t.ask(init)
      turtles += t
    }
  }

  def diffuse(varName: String, value: Double): Unit = {

    val xx = worldWidth

    val scratch: Array[Double] = patches.map(p => p.getVar(varName).asInstanceOf[Double])
    val numPatches = scratch.length

    for (i <- 0 until numPatches) {

      val bitMask =
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

      val newValue = scratch(i) + value * (sum / numBits - scratch(i))
      patches(i).setVar(varName, newValue)
    }
  }

  def forward(turtle: Turtle, units: Double): Unit = {

    if (canMove(turtle, units)) {

      turtle.xcor += units * turtle.dx
      turtle.ycor += units * turtle.dy

      val mappingX = TurtleKey.Xcor -> turtle.xcor
      val mappingY = TurtleKey.Ycor -> turtle.ycor

      Updater.turtles.get(turtle.who).fold {
        Updater.turtles += turtle.who -> MMap(mappingX, mappingY)
      } {
        _ ++= Seq(mappingX, mappingY)
      }

    }

  }

  def getGlobal(name: String): Any =
    globals(name)

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

  def patchRightAndAhead(turtle: Turtle, angle: Double, dist: Double): Option[Patch] = {

    val trueAngle = (turtle.heading + angle) % 360.0
    val rads      = trueAngle * math.Pi / 180.0
    val dx        = math.sin(rads)
    val dy        = math.cos(rads)

    val x = turtle.xcor + dist * (if (math.abs(dx) < 3.2e-15) 0.0 else dx)
    val y = turtle.ycor + dist * (if (math.abs(dy) < 3.2e-15) 0.0 else dy)

    patchAtCor(x, y)

  }

  def resetTicks(): Unit = {
    ticks = 0
    val mapping = WorldKey.Ticks -> ticks
    Updater.world.get(0).fold {
      Updater.world += 0 -> MMap(mapping)
    } {
      _ += mapping
    }
  }

  def setDefaultTurtleShape(shapeName: String): Unit = {
    dtsName = shapeName
  }

  def setGlobal(name: String, value: Any): Unit = {
    globals(name) = value
  }

  def tick(): Unit = {
    ticks += 1
    val mapping = WorldKey.Ticks -> ticks
    Updater.world.get(0).fold {
      Updater.world += 0 -> MMap(mapping)
    } {
      _ += mapping
    }
  }

}

object AntsModel {

  val globalVarNames = Array("population", "diffusion-rate", "evaporation-rate")
  val patchVarNames  = Array("chemical", "food", "nest?", "nest-scent", "food-source-number")
  val workspace      = new Workspace(globalVarNames, patchVarNames, -35, 35, -35, 35)

  workspace.setGlobal(  "diffusion-rate",  50.0)
  workspace.setGlobal("evaporation-rate",  10.0)
  workspace.setGlobal(      "population", 125.0)

  def setup(): Unit = {
    workspace.clearAll()
    workspace.setDefaultTurtleShape("bug")
    workspace.createTurtles(
      workspace.getGlobal("population").asInstanceOf[Double].toInt,
      (self: Turtle) => {
        self.setSize(  2.0)
        self.setColor(15.0)
      }
    )
    setupPatches()
    workspace.resetTicks()
  }

  def setupPatches(): Unit = {
    workspace.allPatches().ask {
      (self: Patch) =>
        setupNest(self)
        setupFood(self)
        recolorPatch(self)
    }
  }

  def setupNest(self: Patch): Unit = {
    self.setVar("nest?",              Box.distancexy(self, 0, 0) < 5)
    self.setVar("nest-scent", 200.0 - Box.distancexy(self, 0, 0)    )
  }

  def setupFood(self: Patch): Unit = {
    if (Box.distancexy(self, 0.6 * workspace.maxPxcor, 0) < 5) {
      self.setVar("food-source-number", 1.0)
    }
    if (Box.distancexy(self, -0.6 * workspace.maxPxcor, -0.6 * workspace.maxPycor) < 5) {
      self.setVar("food-source-number", 2.0)
    }
    if (Box.distancexy(self, -0.8 * workspace.maxPxcor, 0.8 * workspace.maxPycor) < 5) {
      self.setVar("food-source-number", 3.0)
    }
    if (self.getVar("food-source-number").asInstanceOf[Double] > 0) {
      self.setVar("food", Random.oneOf(Array(1.0, 2.0)))
    }
  }

  def recolorPatch(self: Patch): Unit = {
    if (self.getVar("nest?") == true) {
      self.setColor(115.0)
    } else if (self.getVar("food").asInstanceOf[Double] > 0) {
      self.getVar("food-source-number").asInstanceOf[Double].toInt match {
        case 1 => self.setColor(85.0)
        case 2 => self.setColor(95.0)
        case 3 => self.setColor(105.0)
        case n => System.err.println(s"Impossible food source number: $n")
      }
    } else {
      val color = ColorModel.scaleColor(55.0, self.getVar("chemical").asInstanceOf[Double], 0.1, 5.0)
      self.setColor(color)
    }
  }

  def go(): Unit = {

    workspace.allTurtles().ask {
      (self: Turtle) =>
        if (self.who < workspace.ticks) {
          if (self.color == 15.0)
            lookForFood(self)
          else
            returnToNest(self)
          wiggle(self)
          workspace.forward(self, 1.0)
        }
    }

    workspace.diffuse("chemical", workspace.getGlobal("diffusion-rate").asInstanceOf[Double] / 100.0)

    val evapRate = workspace.getGlobal("evaporation-rate").asInstanceOf[Double]
    workspace.allPatches().ask {
      (self: Patch) =>
        self.setVar("chemical", self.getVar("chemical").asInstanceOf[Double] * (100.0 - evapRate) / 100.0)
        recolorPatch(self)
    }

    workspace.tick()

  }

  def returnToNest(self: Turtle): Unit = {
    val patchHere = workspace.patchAt(self).get
    if (patchHere.getVar("nest?") == true) {
      self.setColor(15.0)
      self.rotate(180.0)
    } else {
      patchHere.setVar("chemical", patchHere.getVar("chemical").asInstanceOf[Double] + 60.0)
      uphillNestScent(self)
    }
  }

  def lookForFood(self: Turtle): Unit = {
    val patchHere = workspace.patchAt(self).get
    val food      = patchHere.getVar("food").asInstanceOf[Double]
    if (food > 0) {
      self.setColor(26.0)
      patchHere.setVar("food", food - 1.0)
      self.rotate(180.0)
    } else {
      val chem = patchHere.getVar("chemical").asInstanceOf[Double]
      if (chem >= 0.05 && chem < 2.0) {
        uphillChemical(self)
      }
    }
  }

  def uphillChemical(self: Turtle): Unit = {
    val scentAhead = chemicalAtAngle(self,   0)
    val scentRight = chemicalAtAngle(self,  45)
    val scentLeft  = chemicalAtAngle(self, -45)
    if ((scentRight > scentAhead) || (scentLeft > scentAhead)) {
      val deg = if (scentRight > scentLeft) 45.0 else -45.0
      self.rotate(deg)
    }
  }

  def uphillNestScent(self: Turtle): Unit = {
    val scentAhead = nestScentAtAngle(self,   0)
    val scentRight = nestScentAtAngle(self,  45)
    val scentLeft  = nestScentAtAngle(self, -45)
    if ((scentRight > scentAhead) || (scentLeft > scentAhead)) {
      val deg = if (scentRight > scentLeft) 45.0 else -45.0
      self.rotate(deg)
    }
  }

  def wiggle(self: Turtle): Unit = {
    self.rotate( RNG.nextInt(40).toDouble)
    self.rotate(-RNG.nextInt(40).toDouble)
    if (!workspace.canMove(self, 1.0)) {
      self.rotate(180.0)
    }
  }

  def nestScentAtAngle(turtle: Turtle, angle: Double): Double =
    workspace.patchRightAndAhead(turtle, angle, 1.0)
      .fold(0.0)(_.getVar("nest-scent").asInstanceOf[Double])

  def chemicalAtAngle(turtle: Turtle, angle: Double): Double =
    workspace.patchRightAndAhead(turtle, angle, 1.0)
      .fold(0.0)(_.getVar("chemical").asInstanceOf[Double])

}

object Runner {

  @main def run(mode: String): Unit = {
    mode match {

      case "bench" =>
        val start = System.nanoTime()
        for (_ <- 0 until 20) {
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
