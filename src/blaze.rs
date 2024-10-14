// use roaring::RoaringBitmap as Rb;
// use roaring::RoaringTreemap as Rb;
use range_set_blaze::RangeSetBlaze as Rs;
use ptree::TreeItem;
use ptree::Style;
use ptree::output::write_tree;

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::io;
use std::ops::Range;
use core::ops::BitOrAssign;
use std::rc::Rc;
use std::time::Instant;

fn main() {

    /*let mut tree = Tree::new(10000);
    tree.attach("root", 0..5000, "child1", 20000);
    tree.attach("root", 5000..10000, "child2", 15000);
    println!("{}", tree);*/

    let size: u128 = 500;
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
        if i % 100 == 0 {
            println!("{}", now.elapsed().as_millis());
            now = Instant::now();
            // std::io::stdout().flush().unwrap();
        }
        
        v.reverse();
        let now = Instant::now();
        let r = tree.rank(&v);
        /*if i % 100 == 0 {
            println!("{}", now.elapsed().as_millis());
            // std::io::stdout().flush().unwrap();
        }*/
        if r != i {
            println!("{}->{:?}->{}", i, v, r);
            println!("{}", tree);
        }   
    }
}

struct Tree {
    nodes: HashMap<String, Rc<RefCell<Node>>>
}
impl Tree {
    fn new(cardinality: u128) -> Self {
        let node = Node::new("root", cardinality);
        let node = Rc::new(RefCell::new(node));
        let mut nodes = HashMap::new();
        let key = node.borrow().name.clone();
        nodes.insert(key, node);
        
        Tree { 
            nodes
        }
    }
    fn iterator_attach<I: Iterator<Item = u128>>(&mut self, parent: &str, iter: I, name: &str, cardinality: u128) -> Rc<RefCell<Node>> {
        let interval = Rs::from_iter(iter);
        self.attach_bm(parent, interval, name, cardinality)
    }

    fn predicate_attach(&mut self, parent: &str, pred: fn(u128) -> bool, name: &str, cardinality: u128) -> Rc<RefCell<Node>> {
        let p = self.nodes.get(parent).unwrap();
        let max = p.borrow().cardinality - 1;
        let iter = (0..=max).filter(|v| pred(*v));
        let interval = Rs::from_iter(iter);
        self.attach_bm(parent, interval, name, cardinality)
    }

    fn range_attach(&mut self, parent: &str, interval: Range<u128>, name: &str, cardinality: u128) -> Rc<RefCell<Node>> {
        let i= Rs::from_iter(interval);
        self.attach_bm(parent, i, name, cardinality)
    }

    fn attach(&mut self, parent: &str, name: &str, cardinality: u128) -> Rc<RefCell<Node>> {
        let i = Rs::from_iter([0..=cardinality-1]);
        self.attach_bm(parent, i, name, cardinality)
    }

    fn attach_bm(&mut self, parent: &str, interval: Rs<u128>, name: &str, cardinality: u128) -> Rc<RefCell<Node>> {
        if self.nodes.contains_key(name) {
            panic!("Already had {}", name);
        }
        if interval.len() == 0 {
            panic!("Zero sized interval {:?}", interval);
        }
        let parent = self.nodes.get(parent).unwrap();
        if interval.last().unwrap() >= parent.borrow().cardinality {
            panic!("Interval {:?} greater than cardinality {}", interval, parent.borrow().cardinality);
        }
        
        let node = Node::new(name, cardinality);
        let child = Rc::new(RefCell::new(node));

        let c = parent.borrow().children.clone();
        let collision = c.iter().find(|l| {
            !l.interval.is_disjoint(&interval)
         });
         if let Some(collision) = collision {
             panic!("Interval {:?} collides with {}", interval, collision);
         }
        let link = Link::new(interval, parent.clone(), child.clone());
        
        child.borrow_mut().parent = Some(link.clone());
        parent.borrow_mut().children.push(link);

        self.nodes.insert(name.to_string(), child.clone());

        child
    }


    fn get(&self, name: &str) -> Option<Rc<RefCell<Node>>> {
        self.nodes.get(name).map(|n| n.clone())
    }
    fn unrank(&self, value: u128) -> Vec<String> {
        let n = self.nodes.values().find(|n| {
            if let Some((l, r)) = n.borrow().leaf_range() {
                value >= l && value <= r
            }
            else {
                false
            }
        });

        if let Some(n) = n {
            n.borrow().unrank_start(value)
        }
        else {
            vec!["No match found".to_string()]
        }
    }
    fn rank(&self, value: &[String]) -> u128 {
        let root = self.nodes.get("root").unwrap();

        root.borrow().rank(value, 0)
    }

}

impl Display for Tree {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error>  {        
        let root = self.nodes.get("root");
        if let Some(root) = root {
            write!(f, "{}", root.borrow())
        }
        else {
            write!(f, "Empty")
        }
    }
}

#[derive(Clone)]
struct Node {    
    pub name: String,
    pub cardinality: u128,
    pub parent: Option<Link>,
    pub children: Vec<Link>,
}
impl Node {
    fn new(name: &str, cardinality: u128) -> Self {
        Node {
            name: name.to_string(),
            cardinality,
            parent: None.into(),
            children: vec![],
        }
    }

    /// This will be moved to the source Ranked Set
    fn unrank_self(&self, value: u128) -> String {
        if value >= self.cardinality {
            panic!("out of range: {}", value);
        }

        format!("{}({})", self.name, value)
    }

    /// This will be moved to the source Ranked Set
    fn rank_self(&self, value: &String) -> u128 {
        for i in 0..self.cardinality {
            if self.unrank_self(i) == *value {
                return i
            }
        }
        panic!("Value not present in node {}", value);
    }

    /// Returns the rank for the given sequence of values
    ///
    /// This is called on the root node in Tree::rank
    fn rank(&self, values: &[String], rank: u128) -> u128 {
        let v = self.rank_self(&values[0]);
        // println!("value: {} -> {}", value[0], v);
        let mut index = self.offset();


        ////////////////////////////////////
        let all = Rs::from_iter([0..=self.cardinality - 1]);
        let mut children_union = Rs::new();
        ////////////////////////////////////
        
        for l in self.children.iter() {
            if l.interval.contains(v) {
                // use ranges method to get an iterator over ranges,
                // then use position method on each range until getting the position for v
                // https://docs.rs/range-set-blaze/latest/range_set_blaze/struct.RangeSetBlaze.html#method.ranges
                // let self_rank = l.interval.rank(v) - 1;
                let self_rank = rs_rank(&l.interval, v) - 1;
                
                // println!("----- self_rank: {}, {:?}, {}", self_rank, l.interval, &value[1]);
                let self_product = l.interval.len();
                let self_rank = (rank * self_product) + self_rank;
                // println!("-----> self_rank: {}, {}", self_rank, self_product);
                return l.child.borrow().rank(&values[1..], self_rank);
            }
            else {
                index += l.child.borrow().size();
                /////////////////////////////////
                children_union.bitor_assign(&l.interval);
                /////////////////////////////////
            }
        }

        /////////////////////////////////
        let self_size = all - children_union;
        let self_size_len = self_size.len();
        // use ranges method to get an iterator over ranges,
        // then use position method on each range until getting the position for v
        // https://docs.rs/range-set-blaze/latest/range_set_blaze/struct.RangeSetBlaze.html#method.ranges
        // let self_rank = self_size.rank(v) - 1;
        let self_rank = rs_rank(&self_size, v) - 1;
        
        /////////////////////////////////
        let self_rank = (rank * self_size_len) + self_rank;

        // println!("<---- ret: {}, index: {}, self_rank: {}", self_rank, index, self_rank);
        index + self_rank
    }

    /// Returns the values for the given rank.
    /// 
    /// This is called on the leaf matching node in
    /// Tree::unrank
    fn unrank_start(&self, value: u128) -> Vec<String> {
        let (l, r) = self.range().unwrap();
        if value < l || value > r {
            panic!("out of range: {} ({}-{})", value, l, r);
        }
        let (below, self_size, _) = self.sizes();
        let value = value - self.offset() - below;
        
        // unraking starts with a match on a leaf range
        let v = value % self_size;

        // println!("V {} {} {} {}", value, v, self_size, self.cardinality);
        
        let mut suffix = if let Some(p) = self.parent.as_ref() {
            p.unrank(value / self_size)
        }
        else {
            vec![]
        };

        ///////////
        let leaves = self.leaves_bm();
        // FIXME handle this unwrap
        // use ranges method to get an iterator over ranges,
        // then use nth method on each range until getting the nth value
        // https://docs.rs/range-set-blaze/latest/range_set_blaze/struct.RangeSetBlaze.html#method.ranges
        
        
        // let value = self.unrank_self(leaves.select(v).unwrap());
        let value = self.unrank_self(select(&leaves, v).unwrap());
        ///////////
        // println!("V {} {} {} {}", value, v, self_size, self.cardinality);
        
        suffix.insert(0, value);
        
        suffix
    }
    
    /// Returns the node's offset, the number 
    /// at which this node's values will start
    fn offset(&self) -> u128 {
        if let Some(p) = self.parent.as_ref() {
            p.parent.borrow().child_offset(&self)
        }
        else {
            0
        }
    }

    /// Returns the given node's offset from its parent, by summing over its
    /// left siblings
    fn child_offset(&self, child: &Node) -> u128 {
        let left= self.offset();
        
        let mut sum = 0;
        for c in self.children.iter() {
            let c = c.child.borrow();
            
            // FIXME do proper node equality
            if c.name == child.name {
                break;
            }
            else {
                sum += c.size();
            }
        }

        left + sum
    }
    
    /// Returns the product cardinality for this
    /// node's lineage (upward)
    fn product(&self) -> u128 {
        if let Some(p) = self.parent.as_ref() {
            let parent_product = p.parent.borrow().product();
            let self_product = p.interval.len();

            parent_product * self_product
        }
        else {
            1
        }
    }

    /// Returns the inclusive range for this node
    fn range(&self) -> Option<(u128, u128)> {
        let index = self.offset();
        let size = self.size();

        // range inclusive
        Some((index, index + size - 1))
    }

    /// Returns the bitmap for this node's leaves
    fn leaves_bm(&self) -> Rs<u128> {
        let mut all = Rs::from_iter([0..=self.cardinality - 1]);
        let mut children_union = Rs::new();
        
        for c in self.children.iter() {
            children_union.bitor_assign(&c.interval);
        }

        all - children_union
    }

    /// Range of local leaves: values with no children
    fn leaf_range(&self) -> Option<(u128, u128)> {
        let (b, _, s_product) = self.sizes();
        let index = self.offset();
        let l = index + b;
        let r = l + s_product - 1;
        if r >= l {
            Some((index + b, index + b + s_product - 1))
        }
        else {
            None
        }
    }

    fn self_size(&self) -> u128 {
        self.sizes().1
    }

    /// Returns a tuple with
    ///
    /// the size of the children nodes
    /// 
    /// the size of the local leaves
    /// 
    /// the size of the local leaves multiplied by 
    /// the relevant lineage product
    fn sizes(&self) -> (u128, u128, u128) {
        let product = self.product();
        
        let mut all = Rs::from_iter([0..=self.cardinality - 1]);
        let mut children_union = Rs::new();
        let mut below: u128 = 0;
        
        for c in self.children.iter() {
            children_union.bitor_assign(&c.interval);
            below += c.child.borrow().size();
        }

        let self_size = all - children_union;
        
        (below, self_size.len(), self_size.len() * product)
    }

    /// Returns the size of the node, it is the sum of children 
    /// sizes plus local leaves
    fn size(&self) -> u128 {
        let (below, _, s_product) = self.sizes();
        below + s_product
    }

    /// Returns whether this node's direct children
    /// form a continuous interval
    fn self_children_contiguous(&self) -> bool {
        let mut previous = 0;
        for c in self.children.iter() {
            let min = c.interval.first();
            if min.is_none() { return false; }
            println!("{} {}", min.unwrap(), previous);
            if c.interval.first().unwrap() != previous {
                return false;
            }
            let max = c.interval.last();
            if max.is_none() { return false; }
            let max = max.unwrap();
            println!("{} {}", c.interval.len(), max - previous + 1);
            let ok = c.interval.len() == max - previous + 1;
            if !ok {
                return false;
            }
            previous = max + 1;
        }

        true
        
    }
}
impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error>  {        
        let mut buffer = vec![];
        write_tree(self, &mut buffer).unwrap();
        let s = String::from_utf8(buffer).expect("Invalid UTF-8 sequence");

        write!(f, "{}", s)
    }
}

#[derive(Clone)]
struct Link {
    interval: Rs<u128>,
    parent: Rc<RefCell<Node>>,
    child: Rc<RefCell<Node>>,
}
impl Link {
    fn new(interval: Rs<u128>, parent: Rc<RefCell<Node>>, child: Rc<RefCell<Node>>) -> Self {
        Link {
            interval,
            parent,
            child,
        }
    }
}
impl Link {
    fn unrank(&self, value: u128) -> Vec<String> {
        let len = self.interval.len();
        let v = value % len;
        let parent = self.parent.borrow();
        let mut suffix = if let Some(p) = parent.parent.as_ref() {
            let next = value / len;
            p.unrank(next)
        }
        else {
            vec![]
        };

        // let selected = self.interval.select(v).unwrap();
        let selected = select(&self.interval, v).unwrap();
        suffix.insert(0, parent.unrank_self(selected));

        suffix
    }
}
impl Display for Link {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error>  {
        write!(f, "{} {:?} {}", self.parent.borrow().name, self.interval, self.child.borrow().name)
    }
}

impl TreeItem for Node {
    type Child = Node;
    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        // let (l, r) = self.range();
        let range = self.leaf_range().map(|(a, b)| format!{"{}-{}", a, b})
            .unwrap_or("None".to_string());
        if let Some(parent) = self.parent.as_ref() {
            let (min, max) = (parent.interval.first().unwrap_or(0), parent.interval.last().unwrap_or(0));
            write!(f, "{}", style.paint(format!("{}-{}: {} (|{}|, self_size: {}, range: {})", 
                min, max, self.name.clone(), self.cardinality, self.self_size(), range))
        )
        }
        else {
            write!(f, "{}", style.paint(format!("{} (|{}|, self_size: {}, range: {})", 
                self.name.clone(), self.cardinality, self.self_size(), range))
            )
        }
    }
    fn children(&self) -> Cow<[Self::Child]> {
        let children: Vec<Node> = self.children.iter().map(|l| l.child.borrow().clone()).collect();
        Cow::from(children)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn bijection() {
        let tree = random_tree();
        let root = tree.get("root").unwrap();
        let max = root.borrow().range().unwrap().1;
        for i in 0..=max {
            let mut v = tree.unrank(i);
            v.reverse();
            let r = tree.rank(&v);
            if r != i {
                println!("{}->{:?}->{}", i, v, r);
                println!("{}", tree);
            }   
            assert_eq!(r, i);
        }
        println!("Checked bijection for {} rankings", max);
        println!("{}", tree);
    }

    fn random_tree() -> Tree {
        let mut tree = Tree::new(15);
        tree.range_attach("root", 0..9, "child1", 5);
        tree.range_attach("child1", 0..3, "child1_1", 8);
        tree.range_attach("root", 10..11, "child2", 5);
        tree.range_attach("child2", 0..4, "child2_1", 7);
        
        tree
    }
}

fn select(rb: &Rs<u128>, value: u128) -> Option<u128> {
    panic!("Not implemented")
}
fn rs_rank(rb: &Rs<u128>, value: u128) -> u128 {
    panic!("Not implemented")
}