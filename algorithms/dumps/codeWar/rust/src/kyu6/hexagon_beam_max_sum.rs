pub fn max_hexagon_beam(n: u8, seq: &[i32]) -> i32 {
    let hex = Hex { n: n as usize, seq };
    println!("seq: {:?}, n: {:?}", seq, n);
    let result = *[
        hex.maxsum(Box::new(|r: usize, c: usize| (r, c))),
        hex.maxsum(hex.rotate_left()),
        hex.maxsum(hex.rotate_right()),
    ]
    .iter()
    .max()
    .unwrap();
    println!("result {}", result);
    result
}

struct Hex<'a> {
    n: usize,
    seq: &'a [i32],
}

type CoordTransform = Box<dyn Fn(usize, usize) -> (usize, usize)>;

trait HexCoordTransform {
    fn rotate_left(&self) -> CoordTransform;
    fn rotate_right(&self) -> CoordTransform;
}

impl HexCoordTransform for Hex<'_> {
    // row_size = 7, n = 4
    //      (r, r)
    //         2p  4   6   8  /  +----- c == 4
    //         \             /  /
    //       2  \4p  6   8  / 2
    //           \         /
    //     4   6  \8p  2  / 4   6
    //             \     /
    //    8   2   4 \ 6p/  8   2   4.
    //  -------------\-/----------------
    //      6   8   2 x 4p  6   8. ----- r == n (r == 4)
    //               / \
    //        2   4 / 6 \ 8p  2
    //             /     \
    //          4 / 6   8 \ 2p
    //                        \__(r, n -1) == (r, 3)

    fn rotate_left(&self) -> CoordTransform {
        let n = self.n; // 4 n-1 = 3
        let row_sz = self.row_sz();
        Box::new(move |r: usize, c: usize| {
            let (_, pc) = if r < n { (r, r) } else { (r, n - 1) };
            let gap = ((pc as i32) - (c as i32)).abs() as usize;
            if r < n {
                if c < n {
                    (c, r)
                } else {
                    (c, n - 1 - gap)
                }
            } else {
                let rev_idx = r - n;
                if c < n - 1 {
                    let rev_gap = (n as i32 - gap as i32).abs() as usize;
                    (
                        rev_gap + rev_idx,
                        if r + c >= row_sz {
                            r - c + (row_sz - r - 1)
                        } else {
                            r
                        },
                    )
                } else if c == n - 1 {
                    (r, c)
                } else {
                    let col_sz = Hex::col_sz_static(n, r);
                    (r + gap, col_sz - c + rev_idx)
                }
            }
        })
    }

    // symetric about horizontal diagonal.
    fn rotate_right(&self) -> CoordTransform {
        let n = self.n;
        let row_sz = Hex::row_sz_static(n);
        let upper_bound = row_sz - 1;
        let rotate_left = self.rotate_left();
        Box::new(move |r: usize, c: usize| {
            let (ra, ca) = rotate_left(r, c);
            assert!(upper_bound >= ra);
            (upper_bound - ra, ca)
        })
    }
}

impl Hex<'_> {
    // generic max
    fn maxsum(&self, f: CoordTransform) -> i32 {
        let mut buf: Vec<i32> = {
            let mut x = Vec::new();
            x.resize(self.row_sz(), 0);
            x
        };
        let mut subbuf = buf.clone();
        for r in 0..self.row_sz() {
            for c in 0..self.col_sz(r) {
                let coord = f(r, c);
                subbuf.push(self.val(coord));
            }
            buf[r] = subbuf.iter().sum();
            subbuf.resize(self.row_sz(), 0);
        }
        *buf.iter().max().unwrap()
    }

    fn val(&self, pos: (usize, usize)) -> i32 {
        self.seq[self.idx(pos)]
    }

    fn idx(&self, pos: (usize, usize)) -> usize {
        let (row, col) = pos;
        let pre = (0..row) // til last row
            .map(|r| self.col_sz(r) as usize)
            .sum::<usize>();
        (pre + col as usize) % self.seq.len() as usize
    }

    fn row_sz(&self) -> usize {
        Hex::row_sz_static(self.n)
    }

    fn col_sz(&self, row: usize) -> usize {
        Hex::col_sz_static(self.n, row)
    }

    fn row_sz_static(n: usize) -> usize {
        n * 2 - 1
    }

    fn col_sz_static(n: usize, row: usize) -> usize {
        let row_sz = Hex::row_sz_static(n);
        if row < n {
            n + row
        } else {
            row_sz - (row + 1 - n)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_examples() {
        let tests: Vec<(u8, Vec<i32>, i32)> = vec![
            (2, vec![5, 8, 3, 8], 24),
            (2, vec![-995], -1990),
            (2, vec![8, 12, 33, 24, 92, 5, 47, 16, 0, 13], 149),
            (3, vec![1, 3, 5, 7], 23),
            (4, vec![2, 4, 6, 8], 34),
            (5, vec![1, 0, 4, -6], 9),
            (5, vec![2], 18),
            (6, vec![11, 12, -13, 14, 15], 110),
            (8, vec![-100, 100, 50, -50, 75, -75], 800),
            (7, vec![9, 8, 7, 6, 5, 6, 7, 8, -6], 83),
            (10, vec![1, 2, 3, 4, 5], 64),
            (20, vec![18, -24, 65, -1, 99, 33, -42], 1489),
            (92, vec![324, -90, 28, -331, 24, 55, 94, -101], 1031),
        ];
        for (n, seq, expected) in tests.iter() {
            let result = max_hexagon_beam(*n, seq);
            assert_eq!(result, *expected, "n = {}, seq = {:?}", n, seq);
        }
    }
}
