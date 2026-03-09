declare global {
  interface Window { // eslint-disable-line @typescript-eslint/consistent-type-definitions
    RNG: { nextInt: (x: number) => number };
  }
}

type NetLogoValue =
  boolean | number | string | Turtle | Patch | TurtleSet | PatchSet | Array<NetLogoValue>;


const Box = {
  distancexy: (patch: Patch, x: number, y: number): number => {
    const shortX = Math.abs(patch.pxcor - x);
    const shortY = Math.abs(patch.pycor - y);
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

  private variables: Record<string, NetLogoValue> = {};

  public dx: number = -1;
  public dy: number = -1;

  public color:   number =  0;
  public heading: number = -1;
  public shape:   string = "turtle";
  public size:    number =  1;
  public who:     number = -1;
  public xcor:    number =  0;
  public ycor:    number =  0;

  public constructor(w: number, shapeName: string) {

    super();

    this.color   = Random.oneOf(ColorModel.BaseColors);
    this.heading = window.RNG.nextInt(360);
    this.shape   = shapeName;
    this.who     = w;

    this.recomputeDXY();

  }

  public ask(f: (self: Turtle) => void): void {
    f(this);
  }

  public getVar(name: string): NetLogoValue {
    return this.variables[name]!;
  }

  public rotate(degrees: number): void {
    this.heading = (this.heading + (degrees + 360)) % 360;
    this.recomputeDXY();
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

  }

  public clear(): void {

    this.pcolor = 0;

    for (const k of Object.keys(this.variables)) {
      this.variables[k] = 0;
    }

  }

  public getVar(name: string): NetLogoValue {
    return this.variables[name]!;
  }

  public setVar(name: string, value: NetLogoValue): void {
    this.variables[name] = value;
  }

}


class AgentSet<T extends Agent> {

  private agents: Array<T> = [];

  public constructor(xs: Array<T>) {
    this.agents = [...xs];
  }

  public ask(f: (self: T) => void): void {
    let i = 0;
    while (i < this.agents.length) {
      if (i < this.agents.length - 1) {
        const randNum = i + window.RNG.nextInt(this.agents.length - i);
        f(this.agents[randNum]!);
        this.agents[randNum] = this.agents[i]!;
      } else {
        f(this.agents[i]!);
      }
      i++;
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

  }

  public allPatches(): PatchSet {
    return new AgentSet(this.patches);
  }

  public allTurtles(): TurtleSet {
    return new AgentSet(this.turtles);
  }

  public canMove(turtle: Turtle, distance: number): boolean {
    return this.patchRightAndAhead(turtle, 0, distance) !== undefined;
  }

  public clearAll(): void {

    this.turtles = [];

    for (const patch of this.patches) {
      patch.clear();
    }

  }

  public createTurtles(num: number, init: (self: Turtle) => void): void {
    for (let i = 0; i < num; i++) {
      const turtle = new Turtle(this.turtles.length, this.dtsName);
      turtle.ask(init);
      this.turtles.push(turtle);
    }
  }

  public diffuse(varName: string, value: number): void {

    const yy = this.worldHeight;
    const xx = this.worldWidth;

    const scratch: Array<number> = [];

    for (const patch of this.patches) {
      scratch.push(patch.getVar(varName) as number);
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
      } else if ((i % yy) === 0) { // Left column
        bitMask = 0b01101011;
      } else if (((i + 1) % yy) === 0) { // Right column
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

      const newValue = scratch[i]! + value * (sum / numBits - scratch[i]!);
      this.patches[i]!.setVar(varName, newValue);

    }
    /* eslint-enable no-bitwise */

  }

  public forward(turtle: Turtle, units: number): void {
    if (this.canMove(turtle, units)) {
      turtle.xcor += units * turtle.dx;
      turtle.ycor += units * turtle.dy;
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

  public patchRightAndAhead(turtle: Turtle, angle: number, dist: number): Patch | undefined {

    const trueAngle = (turtle.heading + angle) % 360;

    const rads = trueAngle * Math.PI / 180;
    const dx   = Math.sin(rads);
    const dy   = Math.cos(rads);

    const x = turtle.xcor + dist * ((Math.abs(dx) < 3.2e-15) ? 0 : dx);
    const y = turtle.ycor + dist * ((Math.abs(dy) < 3.2e-15) ? 0 : dy);

    return this.patchAtCor(x, y);

  }

  public resetTicks(): void {
    this.ticks = 0;
  }

  public setDefaultTurtleShape(shapeName: string): void {
    this.dtsName = shapeName;
  }

  public setGlobal(name: string, value: NetLogoValue): void {
    this.globals[name] = value;
  }

  public tick(): void {
    this.ticks += 1;
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
  workspace.createTurtles(workspace.getGlobal("population") as number, (self: Turtle) => {
    self.size  = 2;
    self.color = 15;
  });
  setupPatches();
  workspace.resetTicks();
};

const setupPatches = (): void => {
  workspace.allPatches().ask(
    (self: Patch) => {
      setupNest(self);
      setupFood(self);
      recolorPatch(self);
    }
  );
};

const setupNest = (self: Patch): void => {
  self.setVar("nest?"     ,       Box.distancexy(self, 0, 0) < 5);
  self.setVar("nest-scent", 200 - Box.distancexy(self, 0, 0));
};

const setupFood = (self: Patch): void => {

  if (Box.distancexy(self, 0.6 * workspace.maxPxcor, 0) < 5) {
    self.setVar("food-source-number", 1);
  }

  if (Box.distancexy(self, -0.6 * workspace.maxPxcor, -0.6 * workspace.maxPycor) < 5) {
    self.setVar("food-source-number", 2);
  }

  if (Box.distancexy(self, -0.8 * workspace.maxPxcor, 0.8 * workspace.maxPycor) < 5) {
    self.setVar("food-source-number", 3);
  }

  if ((self.getVar("food-source-number") as number) > 0) {
    self.setVar("food", Random.oneOf([1, 2]));
  }

};

const recolorPatch = (self: Patch): void => {

  if (self.getVar("nest?") === true) {
    self.pcolor = 115;
  } else if ((self.getVar("food") as number) > 0) {
    switch (self.getVar("food-source-number") as number) {
      case 1: {
        self.pcolor = 85;
        break;
      }
      case 2: {
        self.pcolor = 95;
        break;
      }
      case 3: {
        self.pcolor = 105;
        break;
      }
      default:
        console.error("Impossible food source number");
    }
  } else {
    self.pcolor = ColorModel.scaleColor(55, self.getVar("chemical") as number, 0.1, 5);
  }

  self.setVar("nest?"     ,       Box.distancexy(self, 0, 0) < 5);
  self.setVar("nest-scent", 200 - Box.distancexy(self, 0, 0));

};

const go = (): void => {

  workspace.allTurtles().ask(
    (self: Turtle) => {

      if (self.who < workspace.ticks) {

        if (self.color === 15) {
          lookForFood(self);
        } else {
          returnToNest(self);
        }

        wiggle(self);
        workspace.forward(self, 1);

      }

    }
  );

  workspace.diffuse("chemical", (workspace.getGlobal("diffusion-rate") as number) / 100);

  const evapRate = workspace.getGlobal("evaporation-rate") as number;
  workspace.allPatches().ask(
    (self: Patch) => {
      self.setVar("chemical", (self.getVar("chemical") as number) * (100 - evapRate) / 100);
      recolorPatch(self);
    }
  );

  workspace.tick();

};

const returnToNest = (self: Turtle): void => {
  const patchHere = workspace.patchAt(self)!;
  if (patchHere.getVar("nest?") === true) {
    self.color = 15;
    self.rotate(180);
  } else {
    patchHere.setVar("chemical", (patchHere.getVar("chemical") as number) + 60);
    uphillNestScent(self);
  }
};

const lookForFood = (self: Turtle): void => {
  const patchHere = workspace.patchAt(self)!;
  const food      = patchHere.getVar("food") as number;
  if (food > 0) {
    self.color = 26;
    patchHere.setVar("food", food - 1);
    self.rotate(180);
  } else if ((patchHere.getVar("chemical") as number) >= 0.05 && (patchHere.getVar("chemical") as number) < 2) {
    uphillChemical(self);
  }
};

const uphillChemical = (self: Turtle): void => {

  const scentAhead = chemicalAtAngle(self,   0);
  const scentRight = chemicalAtAngle(self,  45);
  const scentLeft  = chemicalAtAngle(self, -45);

  if ((scentRight > scentAhead) || (scentLeft > scentAhead)) {
    self.rotate(scentRight > scentLeft ? 45 : -45);
  }

};

const uphillNestScent = (self: Turtle): void => {

  const scentAhead = nestScentAtAngle(self,   0);
  const scentRight = nestScentAtAngle(self,  45);
  const scentLeft  = nestScentAtAngle(self, -45);

  if ((scentRight > scentAhead) || (scentLeft > scentAhead)) {
    self.rotate(scentRight > scentLeft ? 45 : -45);
  }

};

const wiggle = (self: Turtle): void => {
  self.rotate( window.RNG.nextInt(40));
  self.rotate(-window.RNG.nextInt(40));
  if (!workspace.canMove(self, 1)) {
    self.rotate(180);
  }
};

const nestScentAtAngle = (turtle: Turtle, angle: number): number => {
  const p = workspace.patchRightAndAhead(turtle, angle, 1);
  return (p !== undefined) ? p.getVar("nest-scent") as number : 0;
};

const chemicalAtAngle = (turtle: Turtle, angle: number): number => {
  const p = workspace.patchRightAndAhead(turtle, angle, 1);
  return (p !== undefined) ? p.getVar("chemical") as number : 0;
};

export { setup, go };
