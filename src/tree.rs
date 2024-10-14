// use roaring::RoaringBitmap as Rb;
use roaring::RoaringTreemap as Rb;
// use range_set_blaze::RangeSetBlaze as Rb;
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

/// A ranked tree, loosely following and And/Or structure.
/// 
/// Parent-child relations are rank products
/// Sibling-sibling relations are rank sums (unions)
pub struct Tree {
    nodes: HashMap<String, Rc<RefCell<Node>>>
}
impl Tree {
    /// Creates a new tree with a root with given cardinality.
    pub fn new(cardinality: u64) -> Self {
        let node = Node::new("root", cardinality);
        let node = Rc::new(RefCell::new(node));
        let mut nodes = HashMap::new();
        let key = node.borrow().name.clone();
        nodes.insert(key, node);
        
        Tree { 
            nodes
        }
    }
    /// Attach a child node using the given iterator as the selector.
    pub fn iterator_attach<I: Iterator<Item = u64>>(&mut self, parent: &str, iter: I, name: &str, cardinality: u64) -> Rc<RefCell<Node>> {
        let interval = Rb::from_iter(iter);
        self.attach_with_bitmap(parent, interval, name, cardinality)
    }

    /// Attach a child node using the given predicate as the selector.
    pub fn predicate_attach(&mut self, parent: &str, pred: fn(u64) -> bool, name: &str, cardinality: u64) -> Rc<RefCell<Node>> {
        let p = self.nodes.get(parent).unwrap();
        let max = p.borrow().cardinality - 1;
        let iter = (0..=max).filter(|v| pred(*v));
        let interval = Rb::from_iter(iter);
        self.attach_with_bitmap(parent, interval, name, cardinality)
    }

    /// Attach a child node using the given range as the selector.
    pub fn range_attach(&mut self, parent: &str, interval: Range<u64>, name: &str, cardinality: u64) -> Rc<RefCell<Node>> {
        let mut i = Rb::new();
        i.insert_range(interval);
        self.attach_with_bitmap(parent, i, name, cardinality)
    }

    /// Attach a child node as the only child.
    pub fn attach(&mut self, parent: &str, name: &str, cardinality: u64) -> Rc<RefCell<Node>> {
        let i = Rb::full();
        self.attach_with_bitmap(parent, i, name, cardinality)
    }

    pub fn get(&self, name: &str) -> Option<Rc<RefCell<Node>>> {
        self.nodes.get(name).map(|n| n.clone())
    }
    
    /// Unranks the given value according to this tree
    pub fn unrank(&self, value: u64) -> Vec<String> {
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
    
    /// Ranks the given value according to this tree
    pub fn rank(&self, value: &[String]) -> u64 {
        let root = self.nodes.get("root").unwrap();

        root.borrow().rank(value, 0)
    }

    fn attach_with_bitmap(&mut self, parent: &str, interval: Rb, name: &str, cardinality: u64) -> Rc<RefCell<Node>> {
        if self.nodes.contains_key(name) {
            panic!("Already had {}", name);
        }
        if interval.len() == 0 {
            panic!("Zero sized interval {:?}", interval);
        }
        let parent = self.nodes.get(parent).unwrap();
        if interval.max().unwrap() >= parent.borrow().cardinality {
            panic!("Interval {:?} greater than cardinality {}", interval, parent.borrow().cardinality);
        }
        
        let node = Node::new(name, cardinality);
        let child = Rc::new(RefCell::new(node));

        let c = parent.borrow().children.clone();
        let collision = c.iter().find(|l| {
            l.interval.intersection_len(&interval) > 0
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

}

#[derive(Clone)]
pub struct Node {    
    pub name: String,
    pub cardinality: u64,
    pub parent: Option<Link>,
    pub children: Vec<Link>,
}
impl Node {
    fn new(name: &str, cardinality: u64) -> Self {
        Node {
            name: name.to_string(),
            cardinality,
            parent: None.into(),
            children: vec![],
        }
    }

    /// This will be moved to the source Ranked Set
    fn unrank_self(&self, value: u64) -> String {
        if value >= self.cardinality {
            panic!("out of range: {}", value);
        }

        format!("{}({})", self.name, value)
    }

    /// This will be moved to the source Ranked Set
    fn rank_self(&self, value: &String) -> u64 {
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
    fn rank(&self, values: &[String], rank: u64) -> u64 {
        let v = self.rank_self(&values[0]);
        // println!("value: {} -> {}", value[0], v);
        let mut index = self.offset();


        ////////////////////////////////////
        let mut all = Rb::new();
        all.insert_range(0..self.cardinality);
        let mut children_union = Rb::new();
        ////////////////////////////////////
        
        for l in self.children.iter() {
            if l.interval.contains(v) {
                let self_rank = l.interval.rank(v) - 1;
                
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
        let self_rank = self_size.rank(v) - 1;
        /////////////////////////////////
        let self_rank = (rank * self_size_len) + self_rank;

        // println!("<---- ret: {}, index: {}, self_rank: {}", self_rank, index, self_rank);
        index + self_rank
    }

    /// Returns the values for the given rank.
    /// 
    /// This is called on the leaf matching node in
    /// Tree::unrank
    fn unrank_start(&self, value: u64) -> Vec<String> {
        let (l, r) = self.range().unwrap();
        if value < l || value > r {
            panic!("out of range: {} ({}-{})", value, l, r);
        }
        let (below, self_size, _) = self.sizes();
        // this is the "local" rank for this node, as opposed to the passed 
        // in global rank for the tree
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
        let value = self.unrank_self(leaves.select(v).unwrap());
        ///////////
        // println!("V {} {} {} {}", value, v, self_size, self.cardinality);
        
        suffix.insert(0, value);
        
        suffix
    }
    
    /// Returns the node's offset, the number 
    /// at which this node's values will start
    fn offset(&self) -> u64 {
        if let Some(p) = self.parent.as_ref() {
            p.parent.borrow().child_offset(&self)
        }
        else {
            0
        }
    }

    /// Returns the given node's offset from its parent, by summing over its
    /// left siblings
    fn child_offset(&self, child: &Node) -> u64 {
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
    fn product(&self) -> u64 {
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
    pub fn range(&self) -> Option<(u64, u64)> {
        let index = self.offset();
        let size = self.size();

        // range inclusive
        Some((index, index + size - 1))
    }

    /// Returns the bitmap for this node's leaves
    fn leaves_bm(&self) -> Rb {
        let mut all = Rb::new();
        all.insert_range(0..self.cardinality);
        let mut children_union = Rb::new();
        
        for c in self.children.iter() {
            children_union.bitor_assign(&c.interval);
        }

        all - children_union
    }

    /// Range of local leaves: values with no children
    fn leaf_range(&self) -> Option<(u64, u64)> {
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

    /// The number of leaves in this node
    fn self_size(&self) -> u64 {
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
    fn sizes(&self) -> (u64, u64, u64) {
        let product = self.product();
        
        let mut all = Rb::new();
        all.insert_range(0..self.cardinality);
        let mut children_union = Rb::new();
        let mut below: u64 = 0;
        
        for c in self.children.iter() {
            children_union.bitor_assign(&c.interval);
            below += c.child.borrow().size();
        }

        let self_size = all - children_union;
        
        (below, self_size.len(), self_size.len() * product)
    }

    /// Returns the size of the node, it is the sum of children 
    /// sizes plus local leaves
    fn size(&self) -> u64 {
        let (below, _, s_product) = self.sizes();
        below + s_product
    }

    /// Returns whether this node's direct children
    /// form a continuous interval
    pub fn self_children_contiguous(&self) -> bool {
        let mut previous = 0;
        for c in self.children.iter() {
            let min = c.interval.min();
            if min.is_none() { return false; }
            if c.interval.min().unwrap() != previous {
                return false;
            }
            
            let max = c.interval.max();
            if max.is_none() { return false; }
            let max = max.unwrap();
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
/// A link between nodes.
/// 
/// Parent-child links are rank products
/// Sibling-sibling relations are rank sums (unions)
pub struct Link {
    interval: Rb,
    parent: Rc<RefCell<Node>>,
    child: Rc<RefCell<Node>>,
}
impl Link {
    fn new(interval: Rb, parent: Rc<RefCell<Node>>, child: Rc<RefCell<Node>>) -> Self {
        Link {
            interval,
            parent,
            child,
        }
    }
    fn unrank(&self, value: u64) -> Vec<String> {
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

        let selected = self.interval.select(v).unwrap();
        suffix.insert(0, parent.unrank_self(selected));

        suffix
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

impl Display for Link {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error>  {
        write!(f, "{} {:?} {}", self.parent.borrow().name, self.interval, self.child.borrow().name)
    }
}

/// Ascii tree visualization
impl TreeItem for Node {
    type Child = Node;
    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        // let (l, r) = self.range();
        let range = self.leaf_range().map(|(a, b)| format!{"{}-{}", a, b})
            .unwrap_or("None".to_string());
        if let Some(parent) = self.parent.as_ref() {
            let (min, max) = (parent.interval.min().unwrap_or(0), parent.interval.max().unwrap_or(0));
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
    use rand::Rng;

    #[test]
    fn bijection() {
        let mut rng = rand::thread_rng();
        let tree = random_tree(4);
        let root = tree.get("root").unwrap();
        let max = root.borrow().range().unwrap().1;
        let checks = 1000;
        
        println!("Checking bijection for {} rankings", checks);
        for _ in 0..checks {
            let i = rng.gen_range(0..=max);
            let mut v = tree.unrank(i);
            v.reverse();
            let r = tree.rank(&v);
            if r != i {
                println!("{}->{:?}->{}", i, v, r);
                println!("{}", tree);
            }   
            assert_eq!(r, i);
        }
        println!("{} done", checks);
        println!("{}", tree);
    }

    fn random_tree(levels: i32) -> Tree {
        
        let mut tree = Tree::new(cardinality());
        let mut children = vec!["root".to_string()];
        let mut level = 0;

        loop {
            let mut next = vec![];
            for child in children {
                let c = add_children(&mut tree, &child);
                next.extend(c);
            }
            
            children = next;
            level += 1;

            if level >= levels { break; }
        }
        
        tree
    }

    fn name(parent: &str, sibling: u64) -> String {
        format!("{}/node-{}", parent, sibling)
    }
    
    fn cardinality() -> u64 {
        let mut rng = rand::thread_rng();
        
        rng.gen_range(10..100)
    }

    fn add_children(tree: &mut Tree, parent: &str) -> Vec<String> {
        let mut ret = vec![];
        let mut rng = rand::thread_rng();
        let branching = rng.gen_range(1..5);
        let node = tree.get(parent).unwrap();
        let max = node.borrow().cardinality / branching;

        let mut offset = 0;
        for i in 0..branching {
            let range = rng.gen_range(1..max);
            let name = name(parent, i);
            tree.range_attach(parent, offset..(offset + range), &name, cardinality());
            offset += range + 1;
            ret.push(name);
        }

        ret
    }
}