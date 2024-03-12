use core::ops::Add;
use std::fmt::Debug;

trait Unit {
    type UnitResult: PosResult;
    //:= bountry ?
    fn next(&self) -> Self::UnitResult;
    //:= after bountry, next_times
}

trait UnitMut: Unit {
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

impl Unit for char {
    type UnitResult = (char, bool);
    fn next(&self) -> Self::UnitResult {
        match self {
            'a' => ('b', false),
            'b' => ('c', false),
            'c' => ('a', true),
            _ => (*self, false),
        }
    }
}

impl UnitMut for char {
    type UnitMutResult = bool;
    fn next_mut(&mut self) -> Self::UnitMutResult {
        let a = self.next();
        *self = a.0;
        a.carried()
    }
}

////////////////

enum Orientation {
    ToLeft,
    ToRight,
    FromCenter,
}

trait Align<T: Unit> {
    fn index(&self) -> usize;

    fn get_mut(&mut self, index: usize) -> Option<&mut T>;

    fn orientation(&self) -> Orientation;

    fn next_on(&mut self, index: usize, ori: &Orientation) {
        let a = match self.get_mut(index) {
            Some(aa) => aa.next(),
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

trait IntoAlign<T: Unit> {
    type Alignor: Align<T>;
    fn into_align(self, org: Orientation) -> Self::Alignor;
}

////////////////////
////////////////////

struct VecAlign<T: Unit> {
    inner: Vec<T>,
    this: usize,
    org: Orientation,
}

// impl<T: Unit> Align<T> for VecAlign<T> {
//     fn index(&self) -> usize {
//         todo!()
//     }

//     fn next_on(&mut self, ind: usize, ori: Orientation) {
//         todo!()
//     }

//     fn orientation(&self) -> Orientation {
//         todo!()
//     }

//     fn get_mut(&mut self, index: usize) -> &mut T {
//         todo!()
//     }
// }

// impl<T: Unit> IntoAlign<T> for Vec<T> {
//     type Alignor = VecAlign<T>;

//     fn into_align(self, org: Orientation) -> Self::Alignor {
//         let len = self.len();
//         VecAlign {
//             inner: self,
//             this: match org {
//                 Orientation::ToLeft => len,
//                 Orientation::ToRight => 0,
//                 Orientation::FromCenter => len / 2,
//             },
//             org,
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_align() {
        let mut c = 'a';
        dbg!(<char as UnitMut>::next_mut(&mut c));
        dbg!(c);
        dbg!(c.next_mut());
        dbg!(c);
        dbg!(c.next_mut());
        dbg!(c);
    }
}
