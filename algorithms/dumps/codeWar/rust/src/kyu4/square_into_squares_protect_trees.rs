fn decompose(n: i64) -> Option<Vec<i64>> {
    let res = decomp1(n, n * n);
    res.map(|xs| {
        let len = xs.len();
        xs.into_iter().take(len - 1).collect::<Vec<_>>()
    })
}

fn decomp1(n: i64, remain: i64) -> Option<Vec<i64>> {
    println!("n {}, remain {}", n, remain);
    if remain == 0 {
        return Some(vec![n]);
    }

    for i in (1..n).rev() {
        if remain - i ^ 2 >= 0 {
            let r = decomp1(i, remain - i ^ 2);
            match r {
                Some(mut v) => {
                    println!("HEHEEHE {:?}", v);
                    v.push(n);
                    return Some(v);
                }
                _ => (),
            }
        }
    }
    None
}

fn testing(n: i64, exp: Option<Vec<i64>>) -> () {
    assert_eq!(decompose(n), exp)
}

#[test]
fn tests_decompose() {
    testing(50, Some(vec![1, 3, 5, 8, 49]));
    testing(44, Some(vec![2, 3, 5, 7, 43]));
    testing(625, Some(vec![2, 5, 8, 34, 624]));
    testing(5, Some(vec![3, 4]));
}
