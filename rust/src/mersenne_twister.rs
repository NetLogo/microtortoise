use std::num::Wrapping as Wr;
use std::time::Instant;

use super::Rng;

// Constants for the Mersenne Twister algorithm
const STATE_VECTOR_SIZE: usize = 624;
const TEMPERING_SHIFT: usize = 397;
const TEMPERING_MATRIX_A: Wr<u32> = Wr(0x9908b0df); // Applied in the tempering step
const UPPER_MASK: Wr<u32> = Wr(0x80000000); // Mask for the higher bits
const LOWER_MASK: Wr<u32> = Wr(0x7fffffff); // Mask for the lower bits
const TEMPERING_MASK_B: Wr<u32> = Wr(0x9d2c5680); // Used in tempering
const TEMPERING_MASK_C: Wr<u32> = Wr(0xefc60000); // Used in tempering

pub struct MersenneTwisterFast {
    state_vector: [Wr<u32>; STATE_VECTOR_SIZE],
    /// Index into `state_vector`.
    state_index: usize,
    /// We use the Box-Muller transform to generate two Gaussian random numbers
    /// at a time. Since we can only return a single one per call to
    /// `next_gaussian`, the second one is stored here to be returned on the
    /// next call to `next_gaussian`.
    precomputed_gaussian: Option<f64>,
}

impl MersenneTwisterFast {
    /// Creates a new RNG with the given seed.
    pub fn new(seed_opt: Option<u32>) -> Self {
        // initial values don't matter because we immediately set_seed.
        let mut rng = MersenneTwisterFast {
            state_vector: [Wr(0); STATE_VECTOR_SIZE],
            state_index: STATE_VECTOR_SIZE,
            precomputed_gaussian: None,
        };
        let seed = seed_opt.unwrap_or(Instant::now().elapsed().as_nanos().try_into().unwrap());
        rng.set_seed(seed);
        rng
    }

    /// Sets the seed and initializes the state vector.
    pub fn set_seed(&mut self, seed: u32) {
        self.state_vector[0] = Wr(seed);
        for i in 1..STATE_VECTOR_SIZE {
            self.state_vector[i] = Wr(1812433253)
                * (self.state_vector[i - 1] ^ (self.state_vector[i - 1] >> 30))
                + Wr(i as u32);
        }
        self.state_index = STATE_VECTOR_SIZE; // set index to trigger generation
    }

    /// Generates a random `u32`, uniformly drawn from all possible `u32` values.
    pub fn next_u32(&mut self) -> u32 {
        let mut y;

        // if the state vector is exhausted, generate the next batch
        if self.state_index >= STATE_VECTOR_SIZE {
            for i in 0..(STATE_VECTOR_SIZE - TEMPERING_SHIFT) {
                y = (self.state_vector[i] & UPPER_MASK) | (self.state_vector[i + 1] & LOWER_MASK);
                self.state_vector[i] =
                    self.state_vector[i + TEMPERING_SHIFT] ^ (y >> 1) ^ apply_tempering_matrix(y);
            }
            for i in (STATE_VECTOR_SIZE - TEMPERING_SHIFT)..(STATE_VECTOR_SIZE - 1) {
                y = (self.state_vector[i] & UPPER_MASK) | (self.state_vector[i + 1] & LOWER_MASK);
                self.state_vector[i] = self.state_vector[i + TEMPERING_SHIFT - STATE_VECTOR_SIZE]
                    ^ (y >> 1)
                    ^ apply_tempering_matrix(y);
            }
            y = (self.state_vector[STATE_VECTOR_SIZE - 1] & UPPER_MASK)
                | (self.state_vector[0] & LOWER_MASK);
            self.state_vector[STATE_VECTOR_SIZE - 1] =
                self.state_vector[TEMPERING_SHIFT - 1] ^ (y >> 1) ^ apply_tempering_matrix(y);
            self.state_index = 0;
        }

        // extract the next value
        y = self.state_vector[self.state_index];
        self.state_index += 1;

        // temper the value
        y ^= y >> 11;
        y ^= (y << 7) & TEMPERING_MASK_B;
        y ^= (y << 15) & TEMPERING_MASK_C;
        y ^= y >> 18;

        y.0
    }

    /// Generates a random `u32` in the range `[0, upper_bound)`. `upper_bound`
    /// must be greater than 0 and less than or equal to `i32::MAX`.
    pub fn next_u32_in(&mut self, upper_bound: u32) -> u32 {
        if (upper_bound as i32) <= 1 {
            panic!("invalid upper bound");
        }

        // check if the bound is a power of 2
        let n = upper_bound as i32;
        if (n & -n) == n {
            let raw_val = self.next_u32() >> 1;
            // transform to be within the bound
            ((upper_bound as u64 * raw_val as u64) >> 31) as u32
        } else {
            loop {
                let raw_val = self.next_u32() >> 1;
                // transform to be within the desired range
                let value = raw_val % upper_bound;
                // reject if the value is in the region that would cause bias
                if ((raw_val - value) + (upper_bound - 1)) as i32 >= 0 {
                    break value;
                }
            }
        }
    }

    /// Generates a random `u64` in the range `[0, upper_bound)`. `upper_bound`
    /// must be greater than 0 and less than or equal to `i64::MAX`.
    pub fn next_u64_in(&mut self, upper_bound: u64) -> u64 {
        if (upper_bound as i64) <= 0 {
            panic!("invalid upper bound");
        }
        loop {
            let y = self.next_u32() as u64;
            // the correct behavior would have been to do `let z =
            // self.next_u32() as u64;`, but this is not what the existing
            // engine does with the MersenneTwisterFast.scala file. that code
            // has a bug which unintentionally sign-extends the integer.
            // therefore we replicate the behavior here and hope that people are
            // okay with changing up the RNG. when this gets fixed, also remove
            // the wrapping_add below.
            // https://github.com/NetLogo/Tortoise/blob/master/engine/src/main/scala/MersenneTwisterFast.scala#L540
            // FIXME this has been fixed in Tortoise, so fix it here too
            let z = self.next_u32() as i32 as i64 as u64;

            // concatenate two 32-bit values into a 64-bit value. the wrapping
            // add is only necessary because of the aforementioned bug,
            // otherwise both y and z should have cleanly fit into the u64
            let raw_val = ((y << 32).wrapping_add(z)) >> 1;

            // transform to be within the desired range
            let value = raw_val % upper_bound;

            // reject if the value is in the region that would cause bias
            if ((raw_val - value) + (upper_bound - 1)) as i64 >= 0 {
                break value;
            }
        }
    }

    /// Generates a random `f64` in the range `[0, 1)`.
    pub fn next_f64(&mut self) -> f64 {
        let y = self.next_u32() as u64;
        let z = self.next_u32() as u64;

        const DENOMINATOR: f64 = (1u64 << 53) as f64;
        (((y >> 6) << 27) + (z >> 5)) as f64 / DENOMINATOR
    }

    pub fn next_gaussian(&mut self) -> f64 {
        // return a precomputed gaussian if we have one
        if let Some(next) = self.precomputed_gaussian.take() {
            return next;
        }

        // otherwise, generate two new gaussians using the Box-Muller transform
        let (g0, g1) = loop {
            // sample uniformly from the unit square [-1, 1]
            let x = 2.0 * self.next_f64() - 1.0;
            let y = 2.0 * self.next_f64() - 1.0;

            let s = x * x + y * y;

            if s < 1.0 && s != 0.0 {
                let multiplier = f64::sqrt(-2.0 * f64::ln(s) / s);
                break (x * multiplier, y * multiplier);
            }
        };

        // return one now; store the other one to be returned later
        self.precomputed_gaussian = Some(g1);
        g0
    }
}

impl Rng for MersenneTwisterFast {
    fn next_int(&mut self, max: i64) -> i64 {
        if max <= 0 {
            panic!("invalid upper bound");
        }
        self.next_u64_in(max as u64) as i64
    }
}

/// Helper function to apply the tempering matrix (MAG01 transformation)
#[inline]
fn apply_tempering_matrix(y: Wr<u32>) -> Wr<u32> {
    if y.0 & 0x1 == 0 {
        Wr(0)
    } else {
        TEMPERING_MATRIX_A
    }
}
