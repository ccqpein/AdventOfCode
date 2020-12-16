//:= TODO: Map iter around
pub struct Map<T> {
    /// total length of row and col
    r_len: usize,
    c_len: usize,

    inner: Vec<Vec<T>>,
}

impl<T> Map<T> {
    fn new() -> Self {
        Self {
            r_len: 0,
            c_len: 0,
            inner: Vec::new(),
        }
    }

    fn iter(&self) -> MapIter<'_, T> {
        MapIter::new(self)
    }

    fn get_mut(&mut self, r: usize, c: usize) -> Option<&mut T> {
        self.inner.get_mut(r)?.get_mut(c)
    }

    fn iter_mut(&mut self) -> MapIterMut<'_, T> {
        MapIterMut::new(self)
    }
}

impl<'a, T> IntoIterator for &'a Map<T> {
    type Item = &'a T;
    type IntoIter = MapIter<'a, T>;

    fn into_iter(self) -> MapIter<'a, T> {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Map<T> {
    type Item = &'a mut T;
    type IntoIter = MapIterMut<'a, T>;

    fn into_iter(self) -> MapIterMut<'a, T> {
        self.iter_mut()
    }
}

impl<T> From<Vec<Vec<T>>> for Map<T> {
    fn from(v: Vec<Vec<T>>) -> Self {
        Map {
            r_len: v.len(),
            c_len: v[0].len(),
            inner: v,
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

impl<'a, T> Iterator for MapIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.c_offset == self.map.c_len {
            self.c_offset = 0;
            self.r_offset += 1;
        }

        if self.r_offset == self.map.r_len {
            return None;
        }

        let a = Some(&self.map.inner[self.r_offset][self.c_offset]);
        self.c_offset += 1;
        a
    }
}

///
/// MapIterMut
pub struct MapIterMut<'a, T: 'a> {
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

impl<'a, T> Iterator for MapIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.c_offset == self.map.c_len {
            self.c_offset = 0;
            self.r_offset += 1;
        }

        if self.r_offset == self.map.r_len {
            return None;
        }

        // unsafe here
        let a = unsafe {
            let a = &mut self.map.inner;
            let r = &mut *a.as_mut_ptr().add(self.r_offset) as &mut Vec<T>;
            r.as_mut_ptr().add(self.c_offset).as_mut()
        };

        self.c_offset += 1;
        a
    }
}

struct MapIterScope<'a, T: 'a> {
    /// offsets are where is this map during iter
    r_offset: usize,
    c_offset: usize,

    scope: isize,

    map: &'a Map<T>,
}

impl<'a, T> MapIterScope<'a, T> {
    fn new(m: &'a Map<T>) -> Self {
        Self {
            r_offset: 0,
            c_offset: 0,
            scope: 0,
            map: m,
        }
    }
}

//:= TODO: stop here
// impl<'a, T> Iterator for MapIterScope<'a, T> {
//     type Item = &'a T;

//     fn next(&mut self) -> Option<Self::Item> {

//     }
// }

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn map_test() {
        let mut m = Map::from(vec![vec![1, 2, 3], vec![1, 2, 3]]);
        for i in &m {
            println!("{:?}", i);
        }

        for i in &mut m {
            *i += 1;
            print_type_of(i);
        }

        for i in &m {
            print_type_of(i);
            println!("{:?}", i);
        }

        *m.get_mut(0, 0).unwrap() += 1;
        assert_eq!(m.inner[0][0], 3);

        let mut v = vec![1, 2, 3];
        for i in &v {
            // if delete &, i is T of print_type_of, now it is &T
            print_type_of(i);
        }

        for i in &mut v {
            print_type_of(i);
        }
    }
}
