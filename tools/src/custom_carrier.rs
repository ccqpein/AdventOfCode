use std::fmt::Debug;

//:= rename to CarryableUnit?

pub trait Unit {
    type UnitResult: PosResult;
    fn next(&self) -> Self::UnitResult;
}

pub trait UnitMut: Unit {
    type UnitMutResult: PosResult;
    fn next_mut(&mut self) -> Self::UnitMutResult;
}

trait PosResult: Debug {
    fn carried(&self) -> bool;
}

impl PosResult for bool {
    fn carried(&self) -> bool {
        *self
    }
}

impl<T: Unit + Debug> PosResult for (T, bool) {
    fn carried(&self) -> bool {
        self.1
    }
}

///////////////////////////////////////////
// demo implement

// impl Unit for char {
//     type UnitResult = (char, bool);
//     fn next(&self) -> Self::UnitResult {
//         match self {
//             'a' => ('b', false),
//             'b' => ('c', false),
//             'c' => ('a', true),
//             _ => (*self, false),
//         }
//     }
// }

// impl UnitMut for char {
//     type UnitMutResult = bool;
//     fn next_mut(&mut self) -> Self::UnitMutResult {
//         let a = self.next();
//         *self = a.0;
//         a.carried()
//     }
// }

////////////////

#[derive(Debug)]
pub struct AlphaBetLowCase(u8);

impl AlphaBetLowCase {
    pub fn byte(&self) -> &u8 {
        &self.0
    }
}

impl TryFrom<u8> for AlphaBetLowCase {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < 97 || value > 122 {
            Err("not in alphabet".to_string())
        } else {
            Ok(AlphaBetLowCase(value))
        }
    }
}

impl Unit for AlphaBetLowCase {
    type UnitResult = (AlphaBetLowCase, bool);

    fn next(&self) -> Self::UnitResult {
        if self.0 == 122 {
            (Self(97), true)
        } else {
            (Self(self.0 + 1), false)
        }
    }
}

impl UnitMut for AlphaBetLowCase {
    type UnitMutResult = bool;

    fn next_mut(&mut self) -> Self::UnitMutResult {
        if self.0 == 122 {
            self.0 = 97;
            true
        } else {
            self.0 += 1;
            false
        }
    }
}

////////////////////////////

#[derive(Clone, Copy, Debug)]
pub enum Orientation {
    ToLeft,
    ToRight,
    FromCenter,
}

pub trait Align<T: UnitMut> {
    fn index(&self) -> usize;

    fn get_mut(&mut self, index: usize) -> Option<&mut T>;

    fn orientation(&self) -> Orientation;

    fn next_on(&mut self, index: usize, ori: &Orientation) {
        let a = match self.get_mut(index) {
            Some(aa) => aa.next_mut(),
            None => return,
        };

        if a.carried() {
            match ori {
                Orientation::ToLeft if index != 0 => self.next_on(index - 1, ori),
                Orientation::ToLeft => (),

                Orientation::ToRight => self.next_on(index + 1, ori),
                Orientation::FromCenter => {
                    if index != 0 {
                        self.next_on(index - 1, ori)
                    }
                    self.next_on(index + 1, ori)
                }
            }
        }
    }

    fn next(&mut self) {
        self.next_on(self.index(), &self.orientation())
    }
}

pub trait IntoAlign<T: UnitMut> {
    type Alignor: Align<T>;
    type Error;
    fn into_align(self, org: Orientation) -> Result<Self::Alignor, Self::Error>;
}

////////////////////
////////////////////

#[derive(Debug)]
pub struct VecAlign<T: UnitMut> {
    inner: Vec<T>,
    this: usize,
    org: Orientation,
}

impl<T: UnitMut> VecAlign<T> {
    pub fn inner(&self) -> &Vec<T> {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut Vec<T> {
        &mut self.inner
    }
}

impl<T: UnitMut> Align<T> for VecAlign<T> {
    fn index(&self) -> usize {
        self.this
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.inner.get_mut(index)
    }

    fn orientation(&self) -> Orientation {
        self.org
    }
}

impl<T: UnitMut> IntoAlign<T> for Vec<T> {
    type Alignor = VecAlign<T>;
    type Error = String;

    fn into_align(self, org: Orientation) -> Result<Self::Alignor, Self::Error> {
        let len = self.len();
        if len != 0 {
            Ok(VecAlign {
                inner: self,
                this: match org {
                    Orientation::ToLeft => len - 1,
                    Orientation::ToRight => 0,
                    Orientation::FromCenter => (len - 1) / 2,
                },
                org,
            })
        } else {
            Err("vector length is 0".into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_align() {
        let mut input = "zzz"
            .bytes()
            .map(|b| b.try_into().unwrap())
            .collect::<Vec<AlphaBetLowCase>>()
            .into_align(Orientation::ToLeft)
            .unwrap();
        input.next();
        dbg!(&input); // should be aaa

        input.next();
        dbg!(&input); // should be aab
    }
}
