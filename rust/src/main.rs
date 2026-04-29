//! NetLogo "Ants" model — Rust port of the Scala implementation.
//!
//! # Required module: `mersenne_twister`
//! Must expose a `MersenneTwisterFast` struct
//!
//! # Run modes (first CLI argument):
//!   `bench`  — 20 repetitions × 1 000 ticks; prints total wall-clock seconds.
//!   `json`   — writes `microtortoise-data-rust.json` next to the binary.
//!
//! # JSON compatibility notes
//! - Field order within each per-agent object matches the Scala `mappings` sequence.
//! - Per-tick maps are keyed and sorted by numeric agent ID (BTreeMap) rather than
//!   Scala's non-deterministic HashMap order; use a JSON diff tool for comparison.
//! - `json_num` mirrors Scala `Json.num`: whole-number f64 values are emitted as
//!   integers ("15" not "15.0").
//! - Rust's f64 Display uses shortest-representation (Grisu3/Ryu), matching Java's
//!   `Double.toString` in the vast majority of cases.

pub trait Rng {
    fn next_int(&mut self, max: i64) -> i64;
}

mod mersenne_twister;
use mersenne_twister::MersenneTwisterFast;

use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::time::Instant;

// ════════════════════════════════════════════════════════════
//  Enum indices
// ════════════════════════════════════════════════════════════

#[derive(Clone, Copy)]
#[repr(usize)]
pub enum GlobalVar {
    Population = 0,
    DiffusionRate = 1,
    EvaporationRate = 2,
}

#[derive(Clone, Copy)]
#[repr(usize)]
pub enum PatchVar {
    Chemical = 0,
    Food = 1,
    IsNest = 2,
    NestScent = 3,
    FoodSourceNum = 4,
}

const GLOBAL_COUNT: usize = 3;
const PATCH_VAR_COUNT: usize = 5;

// ════════════════════════════════════════════════════════════
//  Color helpers
// ════════════════════════════════════════════════════════════

const BASE_COLORS: [i64; 14] = [5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135];

fn scale_color(color: f64, value: f64, min: f64, max: f64) -> f64 {
    let (real_min, real_max) = if min <= max { (min, max) } else { (max, min) };
    let percent = if value > real_max {
        1.0
    } else if value < real_min {
        0.0
    } else {
        (value - real_min) / (real_max - real_min)
    };
    let p10 = percent * 10.0;
    let final_percent = if p10 >= 9.9999 {
        9.9999
    } else if p10 < 0.0 {
        0.0
    } else {
        p10
    };
    let mod_color = color % 140.0;
    let wrapped = if mod_color >= 0.0 {
        mod_color
    } else {
        140.0 + mod_color
    };
    (wrapped / 10.0).floor() * 10.0 + final_percent
}

// ════════════════════════════════════════════════════════════
//  Heading ↔ (dx, dy)
// ════════════════════════════════════════════════════════════

#[inline]
fn heading_to_dxdy(heading: f64) -> (f64, f64) {
    let rads = heading * std::f64::consts::PI / 180.0;
    let raw_dx = rads.sin();
    let raw_dy = rads.cos();
    let dx = if raw_dx.abs() < 3.2e-15 { 0.0 } else { raw_dx };
    let dy = if raw_dy.abs() < 3.2e-15 { 0.0 } else { raw_dy };
    (dx, dy)
}

// ════════════════════════════════════════════════════════════
//  JSON helpers
// ════════════════════════════════════════════════════════════

/// Mirrors Scala `Json.num`: if an f64 equals its i64 truncation, emit an
/// integer literal (e.g. 15.0 → "15"); otherwise emit the decimal form.
#[inline]
fn json_num(v: f64) -> String {
    let t = v as i64;
    if v == t as f64 {
        t.to_string()
    } else {
        format!("{v}")
    }
}

fn json_obj(pairs: &[(&str, String)]) -> String {
    if pairs.is_empty() {
        return "{}".to_string();
    }
    let inner = pairs
        .iter()
        .map(|(k, v)| format!("\"{k}\": {v}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{{{inner}}}")
}

// ════════════════════════════════════════════════════════════
//  Updater — delta-state accumulator
// ════════════════════════════════════════════════════════════

/// Accumulates only the fields that changed since the last `drain()`.
/// `None` means the field was not modified in this frame.
/// Field order in `to_json()` mirrors the Scala `mappings` sequences.
#[derive(Default)]
struct TurtleUpdate {
    breed: Option<&'static str>,
    color: Option<i64>,
    heading: Option<f64>,
    who: Option<usize>,
    label_color: Option<f64>,
    hidden: Option<bool>,
    label: Option<&'static str>,
    pen_size: Option<f64>,
    pen_mode: Option<&'static str>,
    shape: Option<String>,
    size: Option<i64>,
    xcor: Option<f64>,
    ycor: Option<f64>,
}

impl TurtleUpdate {
    fn to_json(&self) -> String {
        let mut p = Vec::<(&str, String)>::new();
        if let Some(v) = self.breed {
            p.push(("breed", format!("\"{v}\"")));
        }
        if let Some(v) = self.color {
            p.push(("color", json_num(v as f64)));
        }
        if let Some(v) = self.heading {
            p.push(("heading", json_num(v)));
        }
        if let Some(v) = self.who {
            p.push(("who", v.to_string()));
        }
        if let Some(v) = self.label_color {
            p.push(("label-color", json_num(v)));
        }
        if let Some(v) = self.hidden {
            p.push(("hidden?", v.to_string()));
        }
        if let Some(v) = self.label {
            p.push(("label", format!("\"{v}\"")));
        }
        if let Some(v) = self.pen_size {
            p.push(("pen-size", json_num(v)));
        }
        if let Some(v) = self.pen_mode {
            p.push(("pen-mode", format!("\"{v}\"")));
        }
        if let Some(ref v) = self.shape {
            p.push(("shape", format!("\"{v}\"")));
        }
        if let Some(v) = self.size {
            p.push(("size", json_num(v as f64)));
        }
        if let Some(v) = self.xcor {
            p.push(("xcor", json_num(v)));
        }
        if let Some(v) = self.ycor {
            p.push(("ycor", json_num(v)));
        }
        json_obj(&p)
    }
}

#[derive(Default)]
struct PatchUpdate {
    id: Option<usize>,
    pcolor: Option<f64>,
    plabel: Option<&'static str>,
    plabel_color: Option<f64>,
    pxcor: Option<i32>,
    pycor: Option<i32>,
}

impl PatchUpdate {
    fn to_json(&self) -> String {
        let mut p = Vec::<(&str, String)>::new();
        if let Some(v) = self.id {
            p.push(("id", v.to_string()));
        }
        if let Some(v) = self.pcolor {
            p.push(("pcolor", json_num(v)));
        }
        if let Some(v) = self.plabel {
            p.push(("plabel", format!("\"{v}\"")));
        }
        if let Some(v) = self.plabel_color {
            p.push(("plabel-color", json_num(v)));
        }
        if let Some(v) = self.pxcor {
            p.push(("pxcor", v.to_string()));
        }
        if let Some(v) = self.pycor {
            p.push(("pycor", v.to_string()));
        }
        json_obj(&p)
    }
}

#[derive(Default)]
struct WorldUpdate {
    height: Option<i64>,
    id: Option<i64>,
    patches_all_black: Option<bool>,
    patches_with_labels: Option<bool>,
    max_pxcor: Option<i64>,
    max_pycor: Option<i64>,
    min_pxcor: Option<i64>,
    min_pycor: Option<i64>,
    patch_size: Option<f64>,
    ticks: Option<i64>,
    unbreeded_links_are_directed: Option<bool>,
    width: Option<i64>,
    wrapping_allowed_in_x: Option<bool>,
    wrapping_allowed_in_y: Option<bool>,
}

impl WorldUpdate {
    fn to_json(&self) -> String {
        let mut p = Vec::<(&str, String)>::new();
        if let Some(v) = self.height {
            p.push(("height", v.to_string()));
        }
        if let Some(v) = self.id {
            p.push(("id", v.to_string()));
        }
        if let Some(v) = self.patches_all_black {
            p.push(("patchesAllBlack", v.to_string()));
        }
        if let Some(v) = self.patches_with_labels {
            p.push(("patchesWithLabels", v.to_string()));
        }
        if let Some(v) = self.max_pxcor {
            p.push(("maxPxcor", v.to_string()));
        }
        if let Some(v) = self.max_pycor {
            p.push(("maxPycor", v.to_string()));
        }
        if let Some(v) = self.min_pxcor {
            p.push(("minPxcor", v.to_string()));
        }
        if let Some(v) = self.min_pycor {
            p.push(("minPycor", v.to_string()));
        }
        if let Some(v) = self.patch_size {
            p.push(("patchSize", json_num(v)));
        }
        if let Some(v) = self.ticks {
            p.push(("ticks", v.to_string()));
        }
        if let Some(v) = self.unbreeded_links_are_directed {
            p.push(("unbreededLinksAreDirected", v.to_string()));
        }
        if let Some(v) = self.width {
            p.push(("width", v.to_string()));
        }
        if let Some(v) = self.wrapping_allowed_in_x {
            p.push(("wrappingAllowedInX", v.to_string()));
        }
        if let Some(v) = self.wrapping_allowed_in_y {
            p.push(("wrappingAllowedInY", v.to_string()));
        }
        json_obj(&p)
    }
}

struct Updater {
    /// Slot per turtle ID; `None` means no pending update for that turtle.
    turtles: Vec<Option<TurtleUpdate>>,
    /// IDs of turtles that have at least one pending field change.
    turtle_dirty: Vec<usize>,
    /// Slot per patch ID; `None` means no pending update for that patch.
    patches: Vec<Option<PatchUpdate>>,
    /// IDs of patches that have at least one pending field change.
    patch_dirty: Vec<usize>,
    /// World has only one logical entry (id = 0).
    world: Option<WorldUpdate>,
}

impl Updater {
    fn new(num_turtles: usize, num_patches: usize) -> Self {
        Updater {
            turtles: (0..num_turtles).map(|_| None).collect(),
            turtle_dirty: Vec::with_capacity(num_turtles),
            patches: (0..num_patches).map(|_| None).collect(),
            patch_dirty: Vec::with_capacity(num_patches),
            world: None,
        }
    }

    // ── Entry helpers (mirror BTreeMap::entry().or_default()) ─

    #[inline]
    fn turtle_entry(&mut self, id: usize) -> &mut TurtleUpdate {
        if self.turtles[id].is_none() {
            self.turtles[id] = Some(TurtleUpdate::default());
            self.turtle_dirty.push(id);
        }
        self.turtles[id].as_mut().unwrap()
    }

    #[inline]
    fn patch_entry(&mut self, id: usize) -> &mut PatchUpdate {
        if self.patches[id].is_none() {
            self.patches[id] = Some(PatchUpdate::default());
            self.patch_dirty.push(id);
        }
        self.patches[id].as_mut().unwrap()
    }

    #[inline]
    fn world_entry(&mut self) -> &mut WorldUpdate {
        if self.world.is_none() {
            self.world = Some(WorldUpdate::default());
        }
        self.world.as_mut().unwrap()
    }

    /// Serializes all accumulated deltas into one JSON object, then clears
    /// the dirty state. Mirrors Scala's `Updater.drain()`.
    ///
    /// Dirty lists are sorted before serialization so that output key order
    /// is ascending-numeric, matching the original BTreeMap behaviour and
    /// keeping JSON diffs against the Scala output stable.
    fn drain(&mut self) -> String {
        self.turtle_dirty.sort_unstable();
        self.patch_dirty.sort_unstable();

        let turtles_str = {
            let inner = self
                .turtle_dirty
                .iter()
                .map(|&k| format!("\"{k}\": {}", self.turtles[k].as_ref().unwrap().to_json()))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{inner}}}")
        };
        let patches_str = {
            let inner = self
                .patch_dirty
                .iter()
                .map(|&k| format!("\"{k}\": {}", self.patches[k].as_ref().unwrap().to_json()))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{inner}}}")
        };
        let world_str = match &self.world {
            Some(w) => format!("{{\"0\": {}}}", w.to_json()),
            None => "{}".to_string(),
        };

        for &k in &self.turtle_dirty {
            self.turtles[k] = None;
        }
        for &k in &self.patch_dirty {
            self.patches[k] = None;
        }
        self.turtle_dirty.clear();
        self.patch_dirty.clear();
        self.world = None;

        format!(
            "{{\"turtles\": {turtles_str}, \"patches\": {patches_str}, \"world\": {world_str}}}"
        )
    }
}

// ════════════════════════════════════════════════════════════
//  Turtle  (plain data — all mutations go through Workspace)
// ════════════════════════════════════════════════════════════

struct Turtle {
    who: usize,
    color: i64,
    heading: f64,
    size: i64,
    xcor: f64,
    ycor: f64,
    dx: f64,
    dy: f64,
}

// ════════════════════════════════════════════════════════════
//  Patch  (plain data — all mutations go through Workspace)
// ════════════════════════════════════════════════════════════

struct Patch {
    id: usize,
    pxcor: i32,
    pycor: i32,
    pcolor: f64,
    vars: [f64; PATCH_VAR_COUNT],
}

impl Patch {
    fn new(id: usize, pxcor: i32, pycor: i32) -> Self {
        Patch {
            id,
            pxcor,
            pycor,
            pcolor: 0.0,
            vars: [0.0; PATCH_VAR_COUNT],
        }
    }

    #[inline]
    fn get_var(&self, v: PatchVar) -> f64 {
        self.vars[v as usize]
    }
    #[inline]
    fn set_var(&mut self, v: PatchVar, x: f64) {
        self.vars[v as usize] = x;
    }

    // IsNest is stored as 0.0 (false) / 1.0 (true).
    #[inline]
    fn is_nest(&self) -> bool {
        self.vars[PatchVar::IsNest as usize] != 0.0
    }
}

// ════════════════════════════════════════════════════════════
//  Workspace
// ════════════════════════════════════════════════════════════

struct Workspace {
    // World dimensions
    min_pxcor: i32,
    max_pxcor: i32,
    min_pycor: i32,
    max_pycor: i32,
    world_width: usize,
    world_height: usize,

    // Pre-computed boundary thresholds that mirror Scala's truncate-toward-zero
    // cast. For max_pxcor=35: (35.5_f64) as i32 = 35  →  out-of-bounds if rx >= 35.
    // For min_pxcor=-35: (-35.5_f64) as i32 = -35  →  out-of-bounds if rx < -35.
    // This is an asymmetric boundary matching NetLogo's no-wrapping behaviour.
    bound_max_x: i32,
    bound_min_x: i32,
    bound_max_y: i32,
    bound_min_y: i32,

    globals: [f64; GLOBAL_COUNT],
    patches: Vec<Patch>,
    turtles: Vec<Turtle>,
    ticks: i64,
    dts_name: String, // default turtle shape name

    scratch: Vec<f64>, // reused by diffuse(); avoids per-call allocation
    updater: Updater,
}

impl Workspace {
    fn new(min_pxcor: i32, max_pxcor: i32, min_pycor: i32, max_pycor: i32) -> Self {
        let world_width = (1 + max_pxcor - min_pxcor) as usize;
        let world_height = (1 + max_pycor - min_pycor) as usize;
        let n_patches = world_width * world_height;

        // Build patches in NetLogo row-major order:
        //   top-to-bottom (maxPycor → minPycor), left-to-right (minPxcor → maxPxcor).
        let mut patches = Vec::with_capacity(n_patches);
        for y in (min_pycor..=max_pycor).rev() {
            for x in min_pxcor..=max_pxcor {
                patches.push(Patch::new(patches.len(), x, y));
            }
        }

        let scratch = vec![0.0f64; n_patches];

        // Truncate-toward-zero boundary thresholds (matching Scala's `.toInt`).
        let bound_max_x = (max_pxcor as f64 + 0.5) as i32;
        let bound_min_x = (min_pxcor as f64 - 0.5) as i32;
        let bound_max_y = (max_pycor as f64 + 0.5) as i32;
        let bound_min_y = (min_pycor as f64 - 0.5) as i32;

        // No turtles yet; turtle vec will be grown in create_turtles().
        let mut updater = Updater::new(0, n_patches);

        // Record initial patch state — mirrors each Patch constructor's
        // `Updater.patches(idNum) = MMap(...)` call in the Scala.
        for p in &patches {
            // Direct assignment: slot is already None so no dirty-list entry
            // exists yet; push one now.
            updater.patches[p.id] = Some(PatchUpdate {
                id: Some(p.id),
                pcolor: Some(0.0),
                plabel: Some(""),
                plabel_color: Some(0.0),
                pxcor: Some(p.pxcor),
                pycor: Some(p.pycor),
            });
            updater.patch_dirty.push(p.id);
        }

        // Record initial world state — mirrors `Updater.world(0) = MMap(...)`.
        updater.world = Some(WorldUpdate {
            height: Some(world_height as i64),
            id: Some(0),
            patches_all_black: Some(false),
            patches_with_labels: Some(false),
            max_pxcor: Some(max_pxcor as i64),
            max_pycor: Some(max_pycor as i64),
            min_pxcor: Some(min_pxcor as i64),
            min_pycor: Some(min_pycor as i64),
            patch_size: Some(7.0),
            ticks: Some(-1),
            unbreeded_links_are_directed: Some(true),
            width: Some(world_width as i64),
            wrapping_allowed_in_x: Some(false),
            wrapping_allowed_in_y: Some(false),
        });

        Workspace {
            min_pxcor,
            max_pxcor,
            min_pycor,
            max_pycor,
            world_width,
            world_height,
            bound_max_x,
            bound_min_x,
            bound_max_y,
            bound_min_y,
            globals: [0.0; GLOBAL_COUNT],
            patches,
            turtles: Vec::new(),
            ticks: -1,
            dts_name: "turtle".to_string(),
            scratch,
            updater,
        }
    }

    // ── Globals ───────────────────────────────────────────────

    fn get_global(&self, v: GlobalVar) -> f64 {
        self.globals[v as usize]
    }
    fn set_global(&mut self, v: GlobalVar, x: f64) {
        self.globals[v as usize] = x;
    }

    // ── Lifecycle ─────────────────────────────────────────────

    /// Mirrors `Workspace.clearAll()`.
    /// Clears the turtle list and resets every patch (recording a fresh full
    /// update entry for each, replacing any previous entry).
    fn clear_all(&mut self) {
        // Discard all pending turtle updates and the turtle vec itself.
        for &id in &self.updater.turtle_dirty {
            self.updater.turtles[id] = None;
        }
        self.updater.turtle_dirty.clear();
        self.turtles.clear();

        // Discard world update; setup will re-record ticks via reset_ticks().
        self.updater.world = None;

        // Reset every patch and record a fresh full update, overwriting any
        // partial entry that may have accumulated before clear_all was called.
        // Clear the dirty list first so we rebuild it cleanly below.
        for &id in &self.updater.patch_dirty {
            self.updater.patches[id] = None;
        }
        self.updater.patch_dirty.clear();

        for i in 0..self.patches.len() {
            self.patches[i].pcolor = 0.0;
            self.patches[i].vars = [0.0; PATCH_VAR_COUNT];
            let id = self.patches[i].id;
            let pxcor = self.patches[i].pxcor;
            let pycor = self.patches[i].pycor;
            // Direct assignment rather than patch_entry() so that we emit a
            // complete PatchUpdate (all fields set), not a partial delta.
            self.updater.patches[id] = Some(PatchUpdate {
                id: Some(id),
                pcolor: Some(0.0),
                plabel: Some(""),
                plabel_color: Some(0.0),
                pxcor: Some(pxcor),
                pycor: Some(pycor),
            });
            self.updater.patch_dirty.push(id);
        }
    }

    fn set_default_turtle_shape(&mut self, name: &str) {
        self.dts_name = name.to_string();
    }

    /// Creates `num` turtles using `rng` for initial heading and color, then
    /// calls `init` on each.  Records a full TurtleUpdate for every new
    /// turtle, mirroring the Scala `new Turtle(who, shapeName)` constructor.
    fn create_turtles(
        &mut self,
        num: usize,
        rng: &mut MersenneTwisterFast,
        init: impl Fn(&mut Workspace, usize),
    ) {
        // Extend the updater's turtle vec to accommodate all new turtles at once.
        let new_total = self.turtles.len() + num;
        self.updater.turtles.resize_with(new_total, || None);
        self.updater.turtle_dirty.reserve(num);

        for _ in 0..num {
            let who = self.turtles.len();
            let heading = rng.next_int(360) as f64;
            let color = BASE_COLORS[rng.next_int(BASE_COLORS.len() as i64) as usize];
            let (dx, dy) = heading_to_dxdy(heading);
            let shape = self.dts_name.clone();

            self.turtles.push(Turtle {
                who,
                color,
                heading,
                size: 1,
                xcor: 0.0,
                ycor: 0.0,
                dx,
                dy,
            });

            // Full initial entry — mirrors Scala Turtle constructor.
            // Slot was just extended with None, so push to dirty list.
            self.updater.turtles[who] = Some(TurtleUpdate {
                breed: Some("turtles"),
                color: Some(color),
                heading: Some(heading),
                who: Some(who),
                label_color: Some(0.0),
                hidden: Some(false),
                label: Some(""),
                pen_size: Some(1.0),
                pen_mode: Some("up"),
                shape: Some(shape),
                size: Some(1),
                xcor: Some(0.0),
                ycor: Some(0.0),
            });
            self.updater.turtle_dirty.push(who);

            init(self, who);
        }
    }

    fn reset_ticks(&mut self) {
        self.ticks = 0;
        self.updater.world_entry().ticks = Some(0);
    }

    fn tick(&mut self) {
        self.ticks += 1;
        self.updater.world_entry().ticks = Some(self.ticks);
    }

    fn drain(&mut self) -> String {
        self.updater.drain()
    }

    // ── Shuffle helpers (mirror AgentSet.ask) ─────────────────

    /// Returns a Fisher-Yates-shuffled vector of all turtle indices.
    fn shuffled_turtle_indices(&self, rng: &mut MersenneTwisterFast) -> Vec<usize> {
        let mut v: Vec<usize> = (0..self.turtles.len()).collect();
        for i in (1..v.len()).rev() {
            let j = rng.next_int((i + 1) as i64) as usize;
            v.swap(i, j);
        }
        v
    }

    /// Returns a Fisher-Yates-shuffled vector of all patch indices.
    fn shuffled_patch_indices(&self, rng: &mut MersenneTwisterFast) -> Vec<usize> {
        let mut v: Vec<usize> = (0..self.patches.len()).collect();
        for i in (1..v.len()).rev() {
            let j = rng.next_int((i + 1) as i64) as usize;
            v.swap(i, j);
        }
        v
    }

    // ── Patch lookup ──────────────────────────────────────────

    /// Rounds a world coordinate to the nearest patch coordinate.
    ///
    /// Equivalent to `(c + 0.5).floor() as i32`, which matches the Scala
    /// `patchAtCor` rounding (rounds half toward +∞).
    #[inline]
    fn round_cor(c: f64) -> i32 {
        (c + 0.5).floor() as i32
    }

    /// Returns the patch index for world coordinates `(x, y)`, or `None` if
    /// the point falls outside the world boundary.
    ///
    /// The boundary thresholds use truncate-toward-zero (matching Scala's
    /// `.toInt`), producing the same asymmetric effective boundary as
    /// NetLogo with wrapping disabled.
    fn patch_at_cor(&self, x: f64, y: f64) -> Option<usize> {
        let rx = Self::round_cor(x);
        let ry = Self::round_cor(y);
        if rx >= self.bound_max_x
            || rx < self.bound_min_x
            || ry >= self.bound_max_y
            || ry < self.bound_min_y
        {
            return None;
        }
        let col = (rx - self.min_pxcor) as usize;
        let row = (self.max_pycor - ry) as usize;
        Some(row * self.world_width + col)
    }

    fn patch_at_turtle(&self, who: usize) -> Option<usize> {
        let (xcor, ycor) = (self.turtles[who].xcor, self.turtles[who].ycor);
        self.patch_at_cor(xcor, ycor)
    }

    fn patch_right_and_ahead(&self, who: usize, angle: f64, dist: f64) -> Option<usize> {
        let (heading, xcor, ycor) = {
            let t = &self.turtles[who];
            (t.heading, t.xcor, t.ycor)
        };
        let true_angle = (heading + angle) % 360.0;
        let rads = true_angle * std::f64::consts::PI / 180.0;
        let dx = {
            let v = rads.sin();
            if v.abs() < 3.2e-15 {
                0.0
            } else {
                v
            }
        };
        let dy = {
            let v = rads.cos();
            if v.abs() < 3.2e-15 {
                0.0
            } else {
                v
            }
        };
        self.patch_at_cor(xcor + dist * dx, ycor + dist * dy)
    }

    fn can_move(&self, who: usize, distance: f64) -> bool {
        self.patch_right_and_ahead(who, 0.0, distance).is_some()
    }

    // ── Turtle mutation (each records to Updater) ─────────────

    fn turtle_set_color(&mut self, who: usize, value: i64) {
        if self.turtles[who].color != value {
            self.turtles[who].color = value;
            self.updater.turtle_entry(who).color = Some(value);
        }
    }

    fn turtle_set_size(&mut self, who: usize, value: i64) {
        self.turtles[who].size = value;
        self.updater.turtle_entry(who).size = Some(value);
    }

    fn turtle_rotate(&mut self, who: usize, degrees: f64) {
        let old_heading = self.turtles[who].heading;
        let new_heading = (old_heading + degrees + 360.0) % 360.0;
        if old_heading != new_heading {
            let (dx, dy) = heading_to_dxdy(new_heading);
            self.turtles[who].heading = new_heading;
            self.turtles[who].dx = dx;
            self.turtles[who].dy = dy;
            self.updater.turtle_entry(who).heading = Some(new_heading);
        }
    }

    /// Moves turtle `who` forward by `units` if the destination is in-bounds.
    fn forward(&mut self, who: usize, units: f64) {
        if self.can_move(who, units) {
            let new_xcor = self.turtles[who].xcor + units * self.turtles[who].dx;
            let new_ycor = self.turtles[who].ycor + units * self.turtles[who].dy;
            self.turtles[who].xcor = new_xcor;
            self.turtles[who].ycor = new_ycor;
            let entry = self.updater.turtle_entry(who);
            entry.xcor = Some(new_xcor);
            entry.ycor = Some(new_ycor);
        }
    }

    // ── Patch mutation ────────────────────────────────────────

    /// Records a pcolor change, merging into an existing per-patch entry or
    /// creating a minimal new one — mirrors Scala's
    /// `getOrElseUpdate(idNum, MMap.empty) += PColor -> pcolor`.
    fn patch_set_color(&mut self, idx: usize, value: f64) {
        if self.patches[idx].pcolor != value {
            self.patches[idx].pcolor = value;
            self.updater.patch_entry(idx).pcolor = Some(value);
        }
    }

    /// Sets a patch variable.  Patch vars (Chemical, Food, etc.) are NOT
    /// tracked by the Updater; only `pcolor` changes appear in the JSON.
    #[inline]
    fn patch_set_var(&mut self, idx: usize, v: PatchVar, value: f64) {
        self.patches[idx].set_var(v, value);
    }

    #[inline]
    fn patch_get_var(&self, idx: usize, v: PatchVar) -> f64 {
        self.patches[idx].get_var(v)
    }

    // ── Diffusion ─────────────────────────────────────────────

    /// Diffuses `var_name` at fractional rate `value` (0.0–1.0), without
    /// world-wrapping.  Replicates the Scala bitmask boundary logic exactly,
    /// including the non-power-of-2 neighbour averages at edges and corners.
    fn diffuse(&mut self, var_name: PatchVar, value: f64) {
        let xx = self.world_width;
        let num_patches = self.patches.len();

        // Copy current values into the reusable scratch buffer.
        for i in 0..num_patches {
            self.scratch[i] = self.patches[i].get_var(var_name);
        }

        for i in 0..num_patches {
            // Each bit indicates whether the corresponding neighbour exists.
            // Bit positions (MSB→LSB):
            //   7: top-left    (i - xx - 1)
            //   6: top         (i - xx    )
            //   5: top-right   (i - xx + 1)
            //   4: left        (i      - 1)
            //   3: right       (i      + 1)
            //   2: bottom-left (i + xx - 1)
            //   1: bottom      (i + xx    )
            //   0: bottom-right(i + xx + 1)
            let bit_mask: u8 = if i == 0 {
                0b0000_1011 // top-left corner:    right, bottom, bottom-right
            } else if i == xx - 1 {
                0b0001_0110 // top-right corner:   left, bottom-left, bottom
            } else if i == num_patches - xx {
                0b0110_1000 // bottom-left corner: top, top-right, right
            } else if i == num_patches - 1 {
                0b1101_0000 // bottom-right corner:top-left, top, left
            } else if i < xx {
                0b0001_1111 // top row
            } else if i >= num_patches - xx {
                0b1111_1000 // bottom row
            } else if i % xx == 0 {
                0b0110_1011 // left column
            } else if (i + 1) % xx == 0 {
                0b1101_0110 // right column
            } else {
                0b1111_1111 // interior
            };

            let mut num_bits = 0u32;
            let mut sum = 0.0f64;

            if bit_mask & 0b1000_0000 != 0 {
                sum += self.scratch[i - xx - 1];
                num_bits += 1;
            }
            if bit_mask & 0b0100_0000 != 0 {
                sum += self.scratch[i - xx];
                num_bits += 1;
            }
            if bit_mask & 0b0010_0000 != 0 {
                sum += self.scratch[i - xx + 1];
                num_bits += 1;
            }
            if bit_mask & 0b0001_0000 != 0 {
                sum += self.scratch[i - 1];
                num_bits += 1;
            }
            if bit_mask & 0b0000_1000 != 0 {
                sum += self.scratch[i + 1];
                num_bits += 1;
            }
            if bit_mask & 0b0000_0100 != 0 {
                sum += self.scratch[i + xx - 1];
                num_bits += 1;
            }
            if bit_mask & 0b0000_0010 != 0 {
                sum += self.scratch[i + xx];
                num_bits += 1;
            }
            if bit_mask & 0b0000_0001 != 0 {
                sum += self.scratch[i + xx + 1];
                num_bits += 1;
            }

            let new_value = self.scratch[i] + value * (sum / num_bits as f64 - self.scratch[i]);
            self.patches[i].set_var(var_name, new_value);
        }
    }

    // ── Scent helpers ─────────────────────────────────────────

    fn chemical_at_angle(&self, who: usize, angle: f64) -> f64 {
        self.patch_right_and_ahead(who, angle, 1.0)
            .map_or(0.0, |idx| self.patches[idx].get_var(PatchVar::Chemical))
    }

    fn nest_scent_at_angle(&self, who: usize, angle: f64) -> f64 {
        self.patch_right_and_ahead(who, angle, 1.0)
            .map_or(0.0, |idx| self.patches[idx].get_var(PatchVar::NestScent))
    }
}

// ════════════════════════════════════════════════════════════
//  Geometry helper
// ════════════════════════════════════════════════════════════

fn distance_xy(pxcor: i32, pycor: i32, x: f64, y: f64) -> f64 {
    let dx = (pxcor as f64 - x).abs();
    let dy = (pycor as f64 - y).abs();
    (dx * dx + dy * dy).sqrt()
}

// ════════════════════════════════════════════════════════════
//  AntsModel
// ════════════════════════════════════════════════════════════

struct AntsModel {
    workspace: Workspace,
    rng: MersenneTwisterFast,
}

impl AntsModel {
    fn new() -> Self {
        let mut workspace = Workspace::new(-35, 35, -35, 35);
        workspace.set_global(GlobalVar::Population, 125.0);
        workspace.set_global(GlobalVar::DiffusionRate, 50.0);
        workspace.set_global(GlobalVar::EvaporationRate, 10.0);
        AntsModel {
            workspace,
            rng: MersenneTwisterFast::new(None),
        }
    }

    fn drain(&mut self) -> String {
        self.workspace.drain()
    }

    // ── Setup ──────────────────────────────────────────────────

    fn setup(&mut self) {
        self.workspace.clear_all();
        self.workspace.set_default_turtle_shape("bug");

        let population = self.workspace.get_global(GlobalVar::Population) as usize;
        self.workspace
            .create_turtles(population, &mut self.rng, |ws, who| {
                ws.turtle_set_size(who, 2);
                ws.turtle_set_color(who, 15);
            });

        self.setup_patches();
        self.workspace.reset_ticks();
    }

    fn setup_patches(&mut self) {
        let order = self.workspace.shuffled_patch_indices(&mut self.rng);
        for idx in order {
            self.setup_nest(idx);
            self.setup_food(idx);
            self.recolor_patch(idx);
        }
    }

    fn setup_nest(&mut self, idx: usize) {
        let px = self.workspace.patches[idx].pxcor;
        let py = self.workspace.patches[idx].pycor;
        let dist = distance_xy(px, py, 0.0, 0.0);
        self.workspace
            .patch_set_var(idx, PatchVar::IsNest, if dist < 5.0 { 1.0 } else { 0.0 });
        self.workspace
            .patch_set_var(idx, PatchVar::NestScent, 200.0 - dist);
    }

    fn setup_food(&mut self, idx: usize) {
        let px = self.workspace.patches[idx].pxcor;
        let py = self.workspace.patches[idx].pycor;
        let max_px = self.workspace.max_pxcor as f64;
        let max_py = self.workspace.max_pycor as f64;

        if distance_xy(px, py, 0.6 * max_px, 0.0) < 5.0 {
            self.workspace
                .patch_set_var(idx, PatchVar::FoodSourceNum, 1.0);
        }
        if distance_xy(px, py, -0.6 * max_px, -0.6 * max_py) < 5.0 {
            self.workspace
                .patch_set_var(idx, PatchVar::FoodSourceNum, 2.0);
        }
        if distance_xy(px, py, -0.8 * max_px, 0.8 * max_py) < 5.0 {
            self.workspace
                .patch_set_var(idx, PatchVar::FoodSourceNum, 3.0);
        }
        if self.workspace.patch_get_var(idx, PatchVar::FoodSourceNum) > 0.0 {
            // Mirrors `Random.oneOf(Array(1.0, 2.0))`.
            let food = if self.rng.next_int(2) == 0 { 1.0 } else { 2.0 };
            self.workspace.patch_set_var(idx, PatchVar::Food, food);
        }
    }

    fn recolor_patch(&mut self, idx: usize) {
        if self.workspace.patches[idx].is_nest() {
            self.workspace.patch_set_color(idx, 115.0);
        } else if self.workspace.patch_get_var(idx, PatchVar::Food) > 0.0 {
            let src_num = self.workspace.patch_get_var(idx, PatchVar::FoodSourceNum) as i32;
            let color = match src_num {
                1 => 85.0,
                2 => 95.0,
                3 => 105.0,
                n => {
                    eprintln!("Impossible food source number: {n}");
                    return;
                }
            };
            self.workspace.patch_set_color(idx, color);
        } else {
            let chem = self.workspace.patch_get_var(idx, PatchVar::Chemical);
            let color = scale_color(55.0, chem, 0.1, 5.0);
            self.workspace.patch_set_color(idx, color);
        }
    }

    // ── Go ─────────────────────────────────────────────────────

    fn go(&mut self) {
        // Read ticks before shuffling; tick() is called at the end, matching
        // the Scala `if (self.who < workspace.ticks)` guard semantics.
        let current_ticks = self.workspace.ticks;
        let turtle_order = self.workspace.shuffled_turtle_indices(&mut self.rng);

        for who in turtle_order {
            if (who as i64) < current_ticks {
                if self.workspace.turtles[who].color == 15 {
                    self.look_for_food(who);
                } else {
                    self.return_to_nest(who);
                }
                self.wiggle(who);
                self.workspace.forward(who, 1.0);
            }
        }

        let diff_rate = self.workspace.get_global(GlobalVar::DiffusionRate) / 100.0;
        self.workspace.diffuse(PatchVar::Chemical, diff_rate);

        let evap_rate = self.workspace.get_global(GlobalVar::EvaporationRate);
        let patch_order = self.workspace.shuffled_patch_indices(&mut self.rng);
        for idx in patch_order {
            let chem = self.workspace.patch_get_var(idx, PatchVar::Chemical);
            self.workspace.patch_set_var(
                idx,
                PatchVar::Chemical,
                chem * (100.0 - evap_rate) / 100.0,
            );
            self.recolor_patch(idx);
        }

        self.workspace.tick();
    }

    // ── Turtle behaviours ─────────────────────────────────────

    fn return_to_nest(&mut self, who: usize) {
        let patch_idx = self.workspace.patch_at_turtle(who).unwrap();
        if self.workspace.patches[patch_idx].is_nest() {
            self.workspace.turtle_set_color(who, 15);
            self.workspace.turtle_rotate(who, 180.0);
        } else {
            let chem = self.workspace.patch_get_var(patch_idx, PatchVar::Chemical);
            self.workspace
                .patch_set_var(patch_idx, PatchVar::Chemical, chem + 60.0);
            self.uphill_nest_scent(who);
        }
    }

    fn look_for_food(&mut self, who: usize) {
        let patch_idx = self.workspace.patch_at_turtle(who).unwrap();
        let food = self.workspace.patch_get_var(patch_idx, PatchVar::Food);
        if food > 0.0 {
            self.workspace.turtle_set_color(who, 26);
            self.workspace
                .patch_set_var(patch_idx, PatchVar::Food, food - 1.0);
            self.workspace.turtle_rotate(who, 180.0);
        } else {
            let chem = self.workspace.patch_get_var(patch_idx, PatchVar::Chemical);
            if chem >= 0.05 && chem < 2.0 {
                self.uphill_chemical(who);
            }
        }
    }

    fn uphill_chemical(&mut self, who: usize) {
        let ahead = self.workspace.chemical_at_angle(who, 0.0);
        let right = self.workspace.chemical_at_angle(who, 45.0);
        let left = self.workspace.chemical_at_angle(who, -45.0);
        if right > ahead || left > ahead {
            let deg = if right > left { 45.0 } else { -45.0 };
            self.workspace.turtle_rotate(who, deg);
        }
    }

    fn uphill_nest_scent(&mut self, who: usize) {
        let ahead = self.workspace.nest_scent_at_angle(who, 0.0);
        let right = self.workspace.nest_scent_at_angle(who, 45.0);
        let left = self.workspace.nest_scent_at_angle(who, -45.0);
        if right > ahead || left > ahead {
            let deg = if right > left { 45.0 } else { -45.0 };
            self.workspace.turtle_rotate(who, deg);
        }
    }

    fn wiggle(&mut self, who: usize) {
        let r1 = self.rng.next_int(40) as f64;
        let r2 = -(self.rng.next_int(40) as f64);
        self.workspace.turtle_rotate(who, r1);
        self.workspace.turtle_rotate(who, r2);
        if !self.workspace.can_move(who, 1.0) {
            self.workspace.turtle_rotate(who, 180.0);
        }
    }
}

// ════════════════════════════════════════════════════════════
//  Runner
// ════════════════════════════════════════════════════════════

fn main() {
    let mode = env::args().nth(1).unwrap_or_default();
    match mode.as_str() {
        "bench" => {
            let start = Instant::now();
            for _ in 0..20 {
                let mut model = AntsModel::new();
                model.rng.set_seed(1234);
                model.setup();
                for _ in 0..1000 {
                    model.go();
                }
            }
            println!("{:.6}", start.elapsed().as_secs_f64());
        }

        "json" => {
            let file =
                File::create("../microtortoise-data-rust.json").expect("cannot create output file");
            let mut w = BufWriter::new(file);

            let mut model = AntsModel::new();

            // Frame 0: initial world + patch state (before setup).
            w.write_all(b"[").unwrap();
            w.write_all(model.drain().as_bytes()).unwrap();

            // Frame 1: post-setup state (turtles created, patches coloured).
            model.setup();
            w.write_all(b",").unwrap();
            w.write_all(model.drain().as_bytes()).unwrap();

            // Frames 2–1001: one frame per tick.
            for _ in 0..1000 {
                model.go();
                w.write_all(b",").unwrap();
                w.write_all(model.drain().as_bytes()).unwrap();
            }

            w.write_all(b"]").unwrap();
        }

        _ => panic!("Run mode must be one of: bench | json"),
    }
}
