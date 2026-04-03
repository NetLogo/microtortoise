declare global {
  interface Window { // eslint-disable-line @typescript-eslint/consistent-type-definitions
    RNG: { nextInt: (x: number) => number };
  }
}

type NetLogoValue =
  boolean | number | string | Turtle | Patch | TurtleSet | PatchSet | Array<NetLogoValue>;

type WorldUpdate = {
  height:                    number
, id:                        number
, patchesAllBlack:           boolean
, patchesWithLabels:         boolean
, maxPxcor:                  number
, maxPycor:                  number
, minPxcor:                  number
, minPycor:                  number
, patchSize:                 number
, ticks:                     number
, unbreededLinksAreDirected: boolean
, width:                     number
, wrappingAllowedInX:        boolean
, wrappingAllowedInY:        boolean
};

type TurtleUpdate = {
  breed:         string
, color:         number
, heading:       number
, who:           number
, "label-color": number
, "hidden?":     boolean
, label:         string
, "pen-size":    number
, "pen-mode":    string
, shape:         string
, size:          number
, xcor:          number
, ycor:          number
};

type PatchUpdate = {
  id:             number
, pcolor:         number
, plabel:         string
, "plabel-color": number
, pxcor:          number
, pycor:          number
};

const Updater = {

  turtles: {} as Record<number, TurtleUpdate | undefined>
, patches: {} as Record<number,  PatchUpdate | undefined>
, world:   {} as Record<number,  WorldUpdate | undefined>

, drain(): object {

    const out =
      { turtles: this.turtles
      , patches: this.patches
      ,   world: this.world
      };

    this.turtles = {} as Record<number, TurtleUpdate | undefined>;
    this.patches = {} as Record<number,  PatchUpdate | undefined>;
    this.world   = {} as Record<number,  WorldUpdate | undefined>;

    return out;

  }

};

/* eslint-disable @typescript-eslint/no-explicit-any */
const Box = {

  distancexy: (patch: any, x: any, y: any): number => {

    const p = (patch instanceof Patch) ? patch : boop();
    const i = (typeof x === "number")  ?     x : boop();
    const j = (typeof y === "number")  ?     y : boop();

    const shortX = Math.abs(p.pxcor - i);
    const shortY = Math.abs(p.pycor - j);

    return Math.sqrt((shortX ** 2) + (shortY ** 2));

  }

};

const ColorModel = {

  BaseColors: Array.from({ length: 14 }, (_, i) => i * 10 + 5)

, scaleColor: (color: number, value: number, min: number, max: number): number => {

    let percent = -1;

    const [realMin, realMax] = (min <= max) ? [min, max] : [max, min];

    if (value > realMax) {
      percent = 1;
    } else if (value < realMin) {
      percent = 0;
    } else {
      percent = (value - realMin) / (realMax - realMin);
    }

    const percent10    = percent * 10;
    const finalPercent = (percent10 >= 9.9999) ? 9.9999 : (percent10 < 0) ? 0 : percent10;

    const modColor     = color % 140;
    const wrappedColor = (modColor >= 0) ? modColor : (140 + modColor);
    const finalColor   = Math.floor(wrappedColor / 10);

    return finalColor * 10 + finalPercent;

  }

};

const Random = {
  oneOf: <T>(choices: Array<T>): T => {
    return choices[window.RNG.nextInt(choices.length)]!;
  }
};

const AgentBrand = Symbol("Agent");
class Agent {
  private readonly [AgentBrand]!: never;
}

class Turtle extends Agent {

  private readonly workspace: Workspace;

  private variables: Record<string, NetLogoValue> = {};

  public dx:      number      = -1;
  public dy:      number      = -1;
  public myLinks: Array<void> = [];

  public color:   number =  0;
  public heading: number = -1;
  public shape:   string = "turtle";
  public size:    number =  1;
  public who:     number = -1;
  public xcor:    number =  0;
  public ycor:    number =  0;

  public constructor(w: number, shapeName: string, ws: Workspace) {

    super();

    this.workspace = ws;

    this.color   = Random.oneOf(ColorModel.BaseColors);
    this.heading = window.RNG.nextInt(360);
    this.shape   = shapeName;
    this.who     = w;

    this.recomputeDXY();
    ws.patchAtCor(this.xcor, this.ycor)!.trackTurtle(w);

    Updater.turtles[w] = {
      breed:         "turtles"
    , color:         this.color
    , heading:       this.heading
    , who:           w
    , "label-color": 0
    , "hidden?":     false
    , label:         ""
    , "pen-size":    1
    , "pen-mode":    "up"
    , shape:         shapeName
    , size:          1
    , xcor:          0
    , ycor:          0
    };

  }

  public ask(f: (self: Turtle) => void): void {
    f(this);
  }

  public getVar(name: string): NetLogoValue {
    return this.variables[name]!;
  }

  public rotate(degrees: any): void {
    const d          = (typeof degrees === "number") ? degrees : boop();
    const newHeading = (this.heading + (d + 360)) % 360;
    if (this.heading !== newHeading) {
      this.heading = newHeading;
      this.recomputeDXY();
      (Updater.turtles[this.who] ??= {} as TurtleUpdate).heading = this.heading;
      let i = 0;
      while (i++ < this.myLinks.length) { /* Would move any rigid link neighbors */ }
    }
  }

  public setColor(value: number): void {
    if (this.color !== value) {
      this.color = value;
      (Updater.turtles[this.who] ??= {} as TurtleUpdate).color = value;
    }
  }

  public setSize(value: number): void {
    this.size = value;
    (Updater.turtles[this.who] ??= {} as TurtleUpdate).size = value;
  }

  public setVar(name: string, value: NetLogoValue): void {
    this.variables[name] = value;
  }

  private recomputeDXY(): void {

    const rads = this.heading * Math.PI / 180;
    const dx   = Math.sin(rads);
    const dy   = Math.cos(rads);

    this.dx = (Math.abs(dx) < 3.2e-15) ? 0 : dx;
    this.dy = (Math.abs(dy) < 3.2e-15) ? 0 : dy;

  }

}

class Patch extends Agent {

  private readonly turtlesHere = new Set<number>();

  private variables: Record<string, NetLogoValue> = {};

  private id: number = -1;

  public pcolor: number =  0;
  public pxcor:  number = -1;
  public pycor:  number = -1;

  public constructor(idNum: number, vars: Array<string>, x: number, y: number) {

    super();

    this.id = idNum;

    this.pxcor = x;
    this.pycor = y;

    for (const v of vars) {
      this.variables[v] = 0;
    }

    Updater.patches[idNum] = {
      id:             idNum
    , pcolor:         this.pcolor
    , plabel:         ""
    , "plabel-color": 0
    , pxcor:          x
    , pycor:          y
    };

  }

  public clear(): void {

    this.pcolor = 0;

    for (const k of Object.keys(this.variables)) {
      this.variables[k] = 0;
    }

    Updater.patches[this.id] = {
      id:             this.id
    , pcolor:         this.pcolor
    , plabel:         ""
    , "plabel-color": 0
    , pxcor:          this.pxcor
    , pycor:          this.pycor
    };

  }

  public getVar(name: string): NetLogoValue {
    return this.variables[name]!;
  }

  public setColor(value: number): void {
    if (this.pcolor !== value) {
      this.pcolor = value;
      (Updater.patches[this.id] ??= {} as PatchUpdate).pcolor = value;
    }
  }

  public setVar(name: string, value: NetLogoValue): void {
    this.variables[name] = value;
  }

  public trackTurtle(who: number): void {
    this.turtlesHere.add(who);
  }

  public untrackTurtle(who: number): void {
    this.turtlesHere.delete(who);
  }


}

class AgentSet<T extends Agent> {

  private agents: Array<T> = [];

  public constructor(xs: Array<T>) {
    this.agents = [...xs];
  }

  public ask(f: (self: T) => void): void {

    for (let i = this.agents.length - 1; i > 0; i--) {
      const j = window.RNG.nextInt(i + 1);
      const tmp = this.agents[i]!;
      this.agents[i] = this.agents[j]!;
      this.agents[j] = tmp;
    }

    for (const agent of this.agents) {
      f(agent);
    }

  }

  public size(): number {
    return this.agents.length;
  }

}

type TurtleSet = AgentSet<Turtle>;
type  PatchSet = AgentSet< Patch>;

class Workspace {

  private          dtsName: string                       = "turtle";
  private readonly globals: Record<string, NetLogoValue> = {};
  private readonly patches: Array<Patch>                 = [];
  private          turtles: Array<Turtle>                = [];

  public maxPxcor: number = -1;
  public maxPycor: number = -1;
  public minPxcor: number = -1;
  public minPycor: number = -1;

  public ticks: number = -1;

  public worldHeight: number = -1;
  public worldWidth:  number = -1;

  public constructor( globalVars: Array<string>, patchVars: Array<string>
                    , mnx: number, mxx: number, mny: number, mxy: number) {

    this.maxPxcor = mxx;
    this.maxPycor = mxy;
    this.minPxcor = mnx;
    this.minPycor = mny;

    for (const varName of globalVars) {
      this.globals[varName] = 0;
    }

    for (let y = mxy; y >= mny; y--) {
      for (let x = mnx; x <= mxx; x++) {
        const patch = new Patch(this.patches.length, patchVars, x, y);
        this.patches.push(patch);
      }
    }

    this.worldHeight = 1 + mxy - mny;
    this.worldWidth  = 1 + mxx - mnx;

    Updater.world[0] = {
      height:                    this.worldHeight
    , id:                        0
    , patchesAllBlack:           false
    , patchesWithLabels:         false
    , maxPxcor:                  mxx
    , maxPycor:                  mxy
    , minPxcor:                  mnx
    , minPycor:                  mny
    , patchSize:                 7
    , ticks:                     -1
    , unbreededLinksAreDirected: true
    , width:                     this.worldWidth
    , wrappingAllowedInX:        false
    , wrappingAllowedInY:        false
    };

  }

  public allPatches(): PatchSet {
    return new AgentSet(this.patches);
  }

  public allTurtles(): TurtleSet {
    return new AgentSet(this.turtles);
  }

  public static ask(askee: any, block: any): void {
    const asky = (askee instanceof AgentSet) ? askee : boop();
    asky.ask(block); // eslint-disable-line @typescript-eslint/no-unsafe-argument
  }

  public canMove(turtle: any, distance: any): boolean {
    const t = (turtle instanceof Turtle)     ?   turtle : boop();
    const d = (typeof distance === "number") ? distance : boop();
    return this.patchRightAndAhead(t, 0.0, d) !== undefined;
  }

  public clearAll(): void {

    this.turtles = [];

    for (const patch of this.patches) {
      patch.clear();
    }

  }

  public createTurtles(num: any, init: any): void {
    const n = (typeof num === "number") ? num : boop();
    for (let i = 0; i < n; i++) {
      const turtle = new Turtle(this.turtles.length, this.dtsName, this);
      turtle.ask(init as (self: Turtle) => void);
      this.turtles.push(turtle);
    }
  }

  public diffuse(varName: any, value: any): void {

    const vn = (typeof varName === "string") ? varName : boop();
    const  v = (typeof   value === "number") ?   value : boop();

    const xx = this.worldWidth;

    const scratch: Array<number> = [];

    for (const patch of this.patches) {
      scratch.push(patch.getVar(vn) as number);
    }

    const numPatches = scratch.length;

    /* eslint-disable no-bitwise */
    for (let i = 0; i < numPatches; i++) {

      let bitMask = 0b00000000;

      if (i === 0) { // Top-left
        bitMask = 0b00001011;
      } else if (i === (xx - 1)) { // Top-right
        bitMask = 0b00010110;
      } else if (i === (numPatches - xx)) { // Bottom-left
        bitMask = 0b01101000;
      } else if (i === (numPatches - 1)) { // Bottom-right
        bitMask = 0b11010000;
      } else if (i < xx) { // Top row
        bitMask = 0b00011111;
      } else if (i >= (numPatches - xx)) { // Bottom row
        bitMask = 0b11111000;
      } else if ((i % xx) === 0) { // Left column
        bitMask = 0b01101011;
      } else if (((i + 1) % xx) === 0) { // Right column
        bitMask = 0b11010110;
      } else {
        bitMask = 0b11111111;
      }

      let numBits = 0;
      let sum     = 0;

      if (bitMask & 0b10000000) { // Top-left
        sum     += scratch[(i - xx) - 1]!;
        numBits += 1;
      }

      if (bitMask & 0b01000000) { // Top
        sum     += scratch[(i - xx)]!;
        numBits += 1;
      }

      if (bitMask & 0b00100000) { // Top-right
        sum     += scratch[(i - xx) + 1]!;
        numBits += 1;
      }

      if (bitMask & 0b00010000) { // Left
        sum     += scratch[i - 1]!;
        numBits += 1;
      }

      if (bitMask & 0b00001000) { // Right
        sum     += scratch[i + 1]!;
        numBits += 1;
      }

      if (bitMask & 0b00000100) { // Bottom-left
        sum     += scratch[(i + xx) - 1]!;
        numBits += 1;
      }

      if (bitMask & 0b00000010) { // Bottom
        sum     += scratch[i + xx]!;
        numBits += 1;
      }

      if (bitMask & 0b00000001) { // Bottom-right
        sum     += scratch[i + xx + 1]!;
        numBits += 1;
      }

      const newValue = scratch[i]! + v * (sum / numBits - scratch[i]!);
      this.patches[i]!.setVar(vn, newValue);

    }
    /* eslint-enable no-bitwise */

  }

  public forward(turtle: any, units: any): void {

    const t  = (turtle instanceof Turtle)  ? turtle : boop();
    const us = (typeof units === "number") ?  units : boop();

    const startingPatch = this.patchAtCor(t.xcor, t.ycor)!;

    const isNeg = us < 0;

    let remaining = Math.abs(us);

    while (remaining > 0) {
      const amount      = Math.min(1, remaining);
      const finalAmount = isNeg ? -amount : amount;
      if (this.canMove(t, finalAmount)) {
        t.xcor += finalAmount * t.dx;
        t.ycor += finalAmount * t.dy;
      }
      remaining = (remaining < 1) ? 0 : (remaining - 1);
    }

    if (us !== 0) {
      Updater.turtles[t.who] ??= {} as TurtleUpdate;
      Updater.turtles[t.who]!.xcor = t.xcor;
      Updater.turtles[t.who]!.ycor = t.ycor;
      let i = 0;
      while (i++ < t.myLinks.length) { /* Would move any rigid link neighbors */ }
    }

    const endingPatch = this.patchAtCor(t.xcor, t.ycor)!;

    if (startingPatch !== endingPatch) {
      startingPatch.untrackTurtle(t.who);
        endingPatch.  trackTurtle(t.who);
    }

  }

  public getGlobal(name: string): NetLogoValue {
    return this.globals[name]!;
  }

  public patchAt(turtle: Turtle): Patch | undefined {
    return this.patchAtCor(turtle.xcor, turtle.ycor);
  }

  public patchAtCor(x: number, y: number): Patch | undefined {

    let roundedX = -1;

    if (x > 0) {
      roundedX = (x + 0.5) | 0; // eslint-disable-line no-bitwise
    } else {
      const integral   = x | 0; // eslint-disable-line no-bitwise
      const fractional = integral - x;
      roundedX = (fractional > 0.5) ? (integral - 1) : integral;
    }

    let roundedY = -1;

    if (y > 0) {
      roundedY = (y + 0.5) | 0; // eslint-disable-line no-bitwise
    } else {
      const integral   = y | 0; // eslint-disable-line no-bitwise
      const fractional = integral - y;
      roundedY = (fractional > 0.5) ? (integral - 1) : integral;
    }

    if (roundedX >= (this.maxPxcor + 0.5) ||
        roundedX <  (this.minPxcor - 0.5) ||
        roundedY >= (this.maxPycor + 0.5) ||
        roundedY <  (this.minPycor - 0.5)) {
      return undefined;
    } else {
      const index = ((this.maxPycor - roundedY) * this.worldWidth) + roundedX - this.minPxcor;
      return this.patches[index]!;
    }

  }

  public patchRightAndAhead(turtle: any, angle: any, dist: any): Patch | undefined {

    const t = (turtle instanceof Turtle)  ? turtle : boop();
    const a = (typeof angle === "number") ?  angle : boop();
    const d = (typeof  dist === "number") ?   dist : boop();

    const trueAngle = (t.heading + a) % 360;

    const rads = trueAngle * Math.PI / 180;
    const dx   = Math.sin(rads);
    const dy   = Math.cos(rads);

    const x = t.xcor + d * ((Math.abs(dx) < 3.2e-15) ? 0 : dx);
    const y = t.ycor + d * ((Math.abs(dy) < 3.2e-15) ? 0 : dy);

    return this.patchAtCor(x, y);

  }

  public resetTicks(): void {
    this.ticks = 0;
    (Updater.world[0] ??= {} as WorldUpdate).ticks = this.ticks;
  }

  public setDefaultTurtleShape(shapeName: any): void {
    const sn = (typeof shapeName === "string") ? shapeName : boop();
    this.dtsName = sn;
  }

  public setGlobal(name: string, value: NetLogoValue): void {
    this.globals[name] = value;
  }

  public tick(): void {
    this.ticks += 1;
    (Updater.world[0] ??= {} as WorldUpdate).ticks = this.ticks;
  }

}

// Model starts here

const globalVarNames = ["population", "diffusion-rate", "evaporation-rate"];
const patchVarNames  = ["chemical", "food", "nest?", "nest-scent", "food-source-number"];
const workspace      = new Workspace(globalVarNames, patchVarNames, -35, 35, -35, 35);

workspace.setGlobal(  "diffusion-rate",  50);
workspace.setGlobal("evaporation-rate",  10);
workspace.setGlobal(      "population", 125);

const setup = (): void => {
  workspace.clearAll();
  workspace.setDefaultTurtleShape("bug");
  workspace.createTurtles(workspace.getGlobal("population"), (self: Turtle) => {
    self.setSize(2);
    self.setColor(15);
  });
  setupPatches();
  workspace.resetTicks();
};

const setupPatches = (): void => {
  Workspace.ask(
    workspace.allPatches()
  , (self: Patch) => {
      setupNest(self);
      setupFood(self);
      recolorPatch(self);
    }
  );
};

const setupNest = (self: Patch): void => {
  self.setVar("nest?"     , lessThan(     Box.distancexy(self, 0, 0), 5));
  self.setVar("nest-scent",    minus(200, Box.distancexy(self, 0, 0)   ));
};

const setupFood = (self: Patch): void => {

  badIf (
    lessThan(Box.distancexy(self, times(0.6, workspace.maxPxcor), 0), 5)
  , () => {
      self.setVar("food-source-number", 1);
    }
  );

  badIf (
    lessThan(Box.distancexy(self, -times(0.6, workspace.maxPxcor), times(-0.6, workspace.maxPycor)), 5)
  , () => {
      self.setVar("food-source-number", 2);
    }
  );

  badIf (
    lessThan(Box.distancexy(self, -times(0.8, workspace.maxPxcor), times(0.8, workspace.maxPycor)), 5)
  , () => {
      self.setVar("food-source-number", 3);
    }
  );

  badIf (greaterThan(self.getVar("food-source-number"), 0), () => {
      self.setVar("food", Random.oneOf([1, 2]));
    }
  );

};

const recolorPatch = (self: Patch): void => {
  badIfElse (
    self.getVar("nest?") === true
  , () => {
      self.setColor(115);
    }
  , () => {
      badIfElse (greaterThan(self.getVar("food"), 0)
      , () => {
          const fsn = self.getVar("food-source-number");
          badIf (nlEquals(fsn, 1.0), () => { self.setColor( 85.0); });
          badIf (nlEquals(fsn, 2.0), () => { self.setColor( 95.0); });
          badIf (nlEquals(fsn, 3.0), () => { self.setColor(105.0); });
        }
      , () => {
          const color = ColorModel.scaleColor(55, self.getVar("chemical") as number, 0.1, 5);
          self.setColor(color);
        }
      );
    }
  );
};

const go = (): void => {

  Workspace.ask(
    workspace.allTurtles()
  , (self: Turtle) => {

      badIf (
        lessThan(self.who, workspace.ticks)
      , () => {
          badIfElse (nlEquals(self.color, 15)
          , () => {
              lookForFood(self);
            }
          , () => {
              returnToNest(self);
            }
          );
          wiggle(self);
          workspace.forward(self, 1);
        }
      );

    }
  );

  workspace.diffuse("chemical", div(workspace.getGlobal("diffusion-rate"), 100));

  const evapRate = workspace.getGlobal("evaporation-rate");
  Workspace.ask(
    workspace.allPatches()
  , (self: Patch) => {
      self.setVar("chemical", times(self.getVar("chemical"), div(minus(100, evapRate), 100)));
      recolorPatch(self);
    }
  );

  workspace.tick();

};

const returnToNest = (self: Turtle): void => {
  const patchHere = workspace.patchAt(self)!;
  badIfElse (
    patchHere.getVar("nest?") === true
  , () => {
      self.setColor(15);
      self.rotate(180);
    }
  , () => {
      patchHere.setVar("chemical", plus(patchHere.getVar("chemical"), 60));
      uphillNestScent(self);
    }
  );
};

const lookForFood = (self: Turtle): void => {
  const patchHere = workspace.patchAt(self)!;
  const food      = patchHere.getVar("food");
  badIfElse (
    greaterThan(food, 0)
  , () => {
      self.setColor(26);
      patchHere.setVar("food", minus(food, 1));
      self.rotate(180);
    }
  , () => {
      badIf (
        and( greaterThanOrEqual(patchHere.getVar("chemical"), 0.05)
           , lessThan(patchHere.getVar("chemical"), 2)
           )
      , () => {
          uphillChemical(self);
        }
      );
    }
  );
};

const uphillChemical = (self: Turtle): void => {

  const scentAhead = chemicalAtAngle(self,   0);
  const scentRight = chemicalAtAngle(self,  45);
  const scentLeft  = chemicalAtAngle(self, -45);

  let deg = 0;

  badIf (
    or(greaterThan(scentRight, scentAhead), greaterThan(scentLeft, scentAhead))
  , () => {
      badIfElse ( greaterThan(scentRight, scentLeft)
                , () => { deg =  45; }
                , () => { deg = -45; }
                );
    }
  );

  self.rotate(deg);

};

const uphillNestScent = (self: Turtle): void => {

  const scentAhead = nestScentAtAngle(self,   0);
  const scentRight = nestScentAtAngle(self,  45);
  const scentLeft  = nestScentAtAngle(self, -45);

  let deg = 0;

  badIf (
    or(greaterThan(scentRight, scentAhead), greaterThan(scentLeft, scentAhead))
  , () => {
      badIfElse ( greaterThan(scentRight, scentLeft)
                , () => { deg =  45; }
                , () => { deg = -45; }
                );
    }
  );

  self.rotate(deg);

};

const wiggle = (self: Turtle): void => {
  self.rotate( window.RNG.nextInt(40));
  self.rotate(-window.RNG.nextInt(40));
  const canInFactMove = workspace.canMove(self, 1) as any; // eslint-disable-line @typescript-eslint/no-unsafe-assignment
  const yesCanMove    = (typeof canInFactMove === "boolean") ? canInFactMove : boop();
  badIf(
    not(yesCanMove)
  , () => {
      self.rotate(180);
    }
  );
};

const nestScentAtAngle = (turtle: Turtle, angle: number): number => {
  const p = workspace.patchRightAndAhead(turtle, angle, 1);
  if (p instanceof Patch) {
    return p.getVar("nest-scent") as number;
  } else {
    return 0;
  };
};

const chemicalAtAngle = (turtle: Turtle, angle: number): number => {
  const p = workspace.patchRightAndAhead(turtle, angle, 1);
  if (p instanceof Patch) {
    return p.getVar("chemical") as number;
  } else {
    return 0;
  };
};

const boop = (): never => {
  throw new Error("Boop!");
};

const and = (x: any, y: any): boolean => {
  const i = (typeof x === "boolean") ? x : boop();
  const j = (typeof y === "boolean") ? y : boop();
  return i && j;
};

const badIf = (pred: any, block: () => any): void => {
  const cond = (typeof pred === "boolean") ? pred : boop();
  if (cond) {
    block();
  }
};

const badIfElse = (pred: any, block1: () => any, block2: () => any): void => {
  const cond = (typeof pred === "boolean") ? pred : boop();
  if (cond) {
    block1();
  } else {
    block2();
  }
};

const div = (x: any, y: any): number => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i / j;
};

const greaterThan = (x: any, y: any): boolean => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i > j;
};

const greaterThanOrEqual = (x: any, y: any): boolean => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i >= j;
};

const lessThan = (x: any, y: any): boolean => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i < j;
};

const minus = (x: any, y: any): number => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i - j;
};

const nlEquals = (x: any, y: any): boolean => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i === j;
};

const not = (x: any): boolean => {
  const y = (typeof x === "boolean") ? x : boop();
  return !y;
};

const or = (x: any, y: any): boolean => {
  const i = (typeof x === "boolean") ? x : boop();
  const j = (typeof y === "boolean") ? y : boop();
  return i || j;
};

const plus = (x: any, y: any): number => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i + j;
};

const times = (x: any, y: any): number => {
  const i = (typeof x === "number") ? x : boop();
  const j = (typeof y === "number") ? y : boop();
  return i * j;
};
/* eslint-enable @typescript-eslint/no-explicit-any */

export { setup, go, Updater };
