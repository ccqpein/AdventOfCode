use std::fmt::{Debug, Display};

#[derive(Debug)]
pub struct Map<T> {
    /// total length of row and col
    r_len: usize,
    c_len: usize,

    //:= need to [[T]]? Or just one Vec<>
    inner: Vec<T>,
}

impl<T: Clone> Map<T> {
    pub fn new(r: usize, c: usize, init_v: T) -> Self {
        Self {
            r_len: r,
            c_len: c,
            inner: vec![init_v; r * c],
        }
    }

    pub fn row_len(&self) -> usize {
        self.r_len
    }

    pub fn col_len(&self) -> usize {
        self.c_len
    }

    fn coop_cal(&self, r: usize, c: usize) -> usize {
        // c and r start from 0
        self.c_len * r + c
    }

    pub fn get(&self, (x, y): (usize, usize)) -> Option<&T> {
        self.inner.get(self.coop_cal(x, y))
    }

    pub fn iter(&self) -> MapIter<'_, T> {
        MapIter::new(self)
    }

    pub fn get_mut(&mut self, r: usize, c: usize) -> Option<&mut T> {
        let x = self.coop_cal(r, c);
        self.inner.get_mut(x)
    }

    pub fn set(&mut self, r: usize, c: usize, v: T) -> Result<(), String> {
        *self.get_mut(r, c).ok_or("not found".to_string())? = v;
        Ok(())
    }

    pub fn iter_mut(&mut self) -> MapIterMut<'_, T> {
        MapIterMut::new(self)
    }

    pub fn get_around(&self, (r, c): (usize, usize)) -> impl Iterator<Item = ((usize, usize), &T)> {
        let (r, c) = (r as isize, c as isize);
        [
            (r - 1, c - 1),
            (r - 1, c),
            (r - 1, c + 1),
            (r, c - 1),
            (r, c + 1),
            (r + 1, c - 1),
            (r + 1, c),
            (r + 1, c + 1),
        ]
        .into_iter()
        .filter_map(|(r, c)| {
            if r < 0 || c < 0 || r as usize >= self.r_len || c as usize >= self.c_len {
                None
            } else {
                Some((
                    (r as usize, c as usize),
                    self.get((r as usize, c as usize)).unwrap(),
                ))
            }
        })
    }

    pub fn get_around_horiz(
        &self,
        (r, c): (usize, usize),
    ) -> impl Iterator<Item = ((usize, usize), &T)> {
        let (r, c) = (r as isize, c as isize);
        [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]
            .into_iter()
            .filter_map(|(r, c)| {
                if r >= 0 && c >= 0 && (r as usize) < self.r_len && (c as usize) < self.c_len {
                    Some((
                        (r as usize, c as usize),
                        self.get((r as usize, c as usize)).unwrap(),
                    ))
                } else {
                    None
                }
            })
    }

    /// get the line from coordinate to upper edge.
    /// order is from coord to edge. including the start element self.
    pub fn go_through_up(
        &self,
        (r, c): (usize, usize),
    ) -> impl Iterator<Item = ((usize, usize), &T)> {
        let coops = (0..=r).rev().into_iter().map(move |rr| (rr, c));
        coops.filter_map(|(rr, cc)| match self.get((rr, cc)) {
            Some(v) => Some(((rr, cc), v)),
            None => None,
        })
    }

    /// get the line from coordinate to bottom edge.
    /// order is from coord to edge. including the start element self.
    pub fn go_through_down(
        &self,
        (r, c): (usize, usize),
    ) -> impl Iterator<Item = ((usize, usize), &T)> {
        let coops = (r..self.r_len).into_iter().map(move |rr| (rr, c));
        coops.filter_map(|(rr, cc)| match self.get((rr, cc)) {
            Some(v) => Some(((rr, cc), v)),
            None => None,
        })
    }

    /// get the line from coordinate to left edge.
    /// order is from coord to edge. including the start element self.
    pub fn go_through_left(
        &self,
        (r, c): (usize, usize),
    ) -> impl Iterator<Item = ((usize, usize), &T)> {
        let coops = (0..=c).rev().into_iter().map(move |cc| (r, cc));
        coops.filter_map(|(rr, cc)| match self.get((rr, cc)) {
            Some(v) => Some(((rr, cc), v)),
            None => None,
        })
    }

    /// get the line from coordinate to right edge.
    /// order is from coord to edge. including the start element self.
    pub fn go_through_right(
        &self,
        (r, c): (usize, usize),
    ) -> impl Iterator<Item = ((usize, usize), &T)> {
        let coops = (c..self.c_len).into_iter().map(move |cc| (r, cc));
        coops.filter_map(|(rr, cc)| match self.get((rr, cc)) {
            Some(v) => Some(((rr, cc), v)),
            None => None,
        })
    }

    /// give map four corners, clockwised. they are indexable coop
    /// if map just one row, still return four corners but two of them are duplication
    pub fn four_corners(&self) -> [(usize, usize); 4] {
        let r = if self.row_len() == 0 {
            0
        } else {
            self.row_len() - 1
        };

        let c = if self.col_len() == 0 {
            0
        } else {
            self.col_len() - 1
        };

        [(0, 0), (0, c), (r, c), (r, 0)]
    }
}

impl<T: Display + Debug> Display for Map<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in &self.inner {
            write!(f, "{:?}\n", line)?
        }
        Ok(())
    }
}

impl<'a, T: Clone> IntoIterator for &'a Map<T> {
    type Item = ((usize, usize), &'a T);
    type IntoIter = MapIter<'a, T>;

    fn into_iter(self) -> MapIter<'a, T> {
        self.iter()
    }
}

impl<'a, T: Clone + Debug> IntoIterator for &'a mut Map<T> {
    type Item = ((usize, usize), &'a mut T);
    type IntoIter = MapIterMut<'a, T>;

    fn into_iter(self) -> MapIterMut<'a, T> {
        self.iter_mut()
    }
}

impl<T> From<Vec<Vec<T>>> for Map<T> {
    fn from(v: Vec<Vec<T>>) -> Self {
        let r = v.len();
        let c = v[0].len();
        let vv = v.into_iter().map(|l| l.into_iter()).flatten().collect();

        Map {
            r_len: r,
            c_len: c,
            inner: vv,
        }
    }
}

///
/// MapIter
pub struct MapIter<'a, T: 'a> {
    /// offsets are where is this map during iter
    r_offset: usize,
    c_offset: usize,

    map: &'a Map<T>,
}

impl<'a, T> MapIter<'a, T> {
    fn new(m: &'a Map<T>) -> Self {
        Self {
            r_offset: 0,
            c_offset: 0,
            map: m,
        }
    }
}

impl<'a, T: Clone> Iterator for MapIter<'a, T> {
    type Item = ((usize, usize), &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.c_offset == self.map.c_len {
            self.c_offset = 0;
            self.r_offset += 1;
        }

        if self.r_offset == self.map.r_len {
            return None;
        }

        let a = Some((
            (self.r_offset, self.c_offset),
            self.map.get((self.r_offset, self.c_offset)).unwrap(),
        ));

        self.c_offset += 1;
        a
    }
}

///
/// MapIterMut
pub struct MapIterMut<'a, T>
where
    T: 'a,
{
    /// offsets are where is this map during iter
    r_offset: usize,
    c_offset: usize,

    map: &'a mut Map<T>,
}

impl<'a, T> MapIterMut<'a, T> {
    fn new(m: &'a mut Map<T>) -> Self {
        Self {
            r_offset: 0,
            c_offset: 0,
            map: m,
        }
    }
}

impl<'a, T: Clone + Debug> Iterator for MapIterMut<'a, T> {
    type Item = ((usize, usize), &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.c_offset == self.map.c_len {
            self.c_offset = 0;
            self.r_offset += 1;
        }

        if self.r_offset == self.map.r_len {
            return None;
        }

        // unsafe here
        let result = match unsafe {
            let a = self.map as *mut Map<T>;
            let a = a.as_mut()?; // convert a's lifetime
            a.get_mut(self.r_offset, self.c_offset)
        } {
            Some(a) => Some(((self.r_offset, self.c_offset), a)),
            None => {
                panic!()
            }
        };

        self.c_offset += 1;
        //println!("result: {:?}", &result);
        result
    }
}

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn map_test() {
        let mut m = Map::from(vec![vec![1, 2, 3], vec![1, 2, 3]]);
        for (i, v) in &m {
            println!("{:?} {:?}", i, v);
        }

        for (i, v) in &mut m {
            *v += 1;
            //print_type_of(i);
        }

        for (i, v) in &m {
            //print_type_of(i);
            println!("{:?}", v);
        }

        *m.get_mut(0, 0).unwrap() += 1;
        assert_eq!(m.get((0, 0)), Some(&3));

        let mut v = vec![1, 2, 3];
        for i in &v {
            // if delete &, i is T of print_type_of, now it is &T
            print_type_of(i);
        }

        for i in &mut v {
            print_type_of(i);
        }
    }

    #[test]
    fn map_go_through_test() {
        let m = Map::from(vec![
            vec![3, 0, 3, 7, 3],
            vec![2, 5, 5, 1, 2],
            vec![6, 5, 3, 3, 2],
            vec![3, 3, 5, 4, 9],
            vec![3, 5, 3, 9, 0],
        ]);

        assert_eq!(
            m.go_through_up((1, 2))
                .map(|((x, y), a)| ((x, y), *a))
                .collect::<Vec<_>>(),
            vec![((1, 2), 5), ((0, 2), 3)]
        );

        assert_eq!(
            m.go_through_down((1, 2))
                .map(|((x, y), a)| ((x, y), *a))
                .collect::<Vec<_>>(),
            vec![((1, 2), 5), ((2, 2), 3), ((3, 2), 5), ((4, 2), 3)]
        );

        assert_eq!(
            m.go_through_left((1, 2))
                .map(|((x, y), a)| ((x, y), *a))
                .collect::<Vec<_>>(),
            vec![((1, 2), 5), ((1, 1), 5), ((1, 0), 2)]
        );

        assert_eq!(
            m.go_through_right((1, 2))
                .map(|((x, y), a)| ((x, y), *a))
                .collect::<Vec<_>>(),
            vec![((1, 2), 5), ((1, 3), 1), ((1, 4), 2)]
        );
    }
}
