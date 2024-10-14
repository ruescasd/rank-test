use rank::tree::*;
use std::time::Instant;

fn main() {

    /*let mut tree = Tree::new(10000);
    tree.attach("root", 0..5000, "child1", 20000);
    tree.attach("root", 5000..10000, "child2", 15000);
    println!("{}", tree);*/

    let size: u64 = 500;
    let part1 = (0..size).filter(|v| *v <= 2000);
    let part2 = (0..size).filter(|v| *v > 2000);
    // let part1 = (0..size).filter(|v| v % 2 == 0);
    // let part2 = (0..size).filter(|v| v % 2 == 1);
    let mut tree = Tree::new(size + 00);
    // tree.attach_with_iterator("root", part1, "child1", 4442);
    // tree.attach_with_iterator("root", part2, "child2",4442);
    tree.predicate_attach("root", |v| v <= 250, "child1", 40);
    tree.predicate_attach("root", |v| v > 250, "child2",44);
    println!("{}", tree);

    let root = tree.get("root").unwrap();
    let max = root.borrow().range().unwrap().1;
    println!("root contiguous: {}", root.borrow().self_children_contiguous());

    /* let test = 11106000;
    let now = Instant::now();
    let mut v = tree.unrank(test);
    println!("rank in {}ms", now.elapsed().as_millis());
    v.reverse();
    let now = Instant::now();
    let r = tree.rank(&v);
    println!("unrank in {}ms", now.elapsed().as_millis());
    println!("{}->{:?}->{}", test, v, r);*/
    let mut now = Instant::now();
    for i in 0..=max {
        let mut v = tree.unrank(i);
        /*if i % 100 == 0 {
            // println!("{}", now.elapsed().as_millis());
            now = Instant::now();
            // std::io::stdout().flush().unwrap();
        }*/
        v.reverse();
        let r = tree.rank(&v);
        /*if i % 100 == 0 {
            // println!("{}", now.elapsed().as_millis());
            now = Instant::now();
            // std::io::stdout().flush().unwrap();
        }*/
        if r != i {
            println!("{}->{:?}->{}", i, v, r);
            println!("{}", tree);
        }   
    }
}