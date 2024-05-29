use std::ops::RangeInclusive;
use std::borrow::Cow;
use std::io;

use range_set::RangeSet;
use ptree::TreeItem;
use ptree::Style;
use ptree::output::write_tree;

type Range = RangeSet<[RangeInclusive<u32>; 4]>;

#[derive(Clone, Copy)]
struct Rank(u32);

#[derive(Clone)]
struct Set {
    name: String,
    size: u32,
}

#[derive(Clone)]
struct Node {
    set: Set,
    leaves: Range,
    children: Vec<Link>,
}

#[derive(Clone)]
struct Link {
    condition: Range,
    target: Node,
}


struct Path {
    name: String,
    head: PathElement,
    tail: Vec<PathElement>,
}

#[derive(Clone)]
struct PathElement {
    node: Node,
    prefix: u32,
    base: u32,
    selection: Rank,
    local_rank: u32,
}

impl Path {
    pub fn new(name: &str, head: &PathElement) -> Path {
        Path {
            name: name.to_string(),
            head: head.clone(),
            tail: vec![],
        }
    }
    pub fn select(mut self, s: &str) -> Path {
        
        let link = self.head.node.children.iter().find(|&s| {
            s.condition.contains(self.head.selection.0)
        });
        
        if let Some(link) = link {
            let selected_node = link.target.clone();
            let rank = selected_node.set.rank(s);
            let local_rank = selected_node.local_rank(&rank);
            let prefix = selected_node.size_left(rank);
            let base = selected_node.base(rank);
            let head = PathElement {
                prefix,
                base,
                node: selected_node,
                selection: rank,
                local_rank,
            };

            self.tail.push(self.head);
            self.head = head;

            self
        }
        else {
            panic!("Cannot select further");
        }
    }

    pub fn path_vector(&self) -> Vec<PathElement> {
        let mut ret = self.tail.clone();
        ret.push(self.head.clone());

        ret
    }

    pub fn debug(&self) {
        let path = self.path_vector();
        let mut multiplier = 1;
        let mut prefix = 0;
        let mut middle_rank = 0;
        println!("==== {} ====", self.name);
        for p in path {
            // middle_rank = (p.local_rank * multiplier);
            middle_rank = middle_rank + (p.local_rank * multiplier);
            prefix = prefix + (p.prefix * multiplier);
            println!("From '{}' selected '{}' ({}), rank = {} (prefix = {}, local = {})", p.node.set.name, p.selection.0, p.local_rank, (middle_rank + prefix), prefix, p.prefix);
            // println!("rank = {}, rank2 = {}, prefix = {}, multiplier = {}", middle_rank + prefix, p.local_rank, prefix, multiplier);
            multiplier *= p.base;
        }
        println!("==============");
    }
}



impl Node {
    
    pub fn new(set: Set) -> Node {
        let leaves = RangeSet::from(0..=(set.size() - 1));
        Node {
            leaves,
            set,
            children: vec![],
        }
    }

    // the size of a node = links + leaves
    pub fn size_all(&self) -> u32 {
        let children = self.children.iter().fold(0u32, |sum, val| sum + val.size_all());
    
        children + range_size(&self.leaves)
    }
    
    // ranks less than the passed selection
    pub fn size_left(&self, selection: Rank) -> u32 {
        let link = self.children.iter().find(|&s| {
            s.condition.contains(selection.0)
        });

        
        let left = if let Some(link) = link {
            // if the selection is on a link, the value used to compute left ranks is the left edge of the link
            link.condition.min().unwrap()
        }
        else {
            // if the selection is on a leaf, the value used to compute left ranks is the left edge of the leaves
            self.leaves.min().unwrap()
        };

        let children: Vec<&Link> = self.children
            .iter()
            .filter(|s| s.condition.max().unwrap() < left)
            .collect();
    
        // ranks on the left children
        let children_left = children.iter().fold(0u32, |sum, val| sum + val.size_all());
        // (ranks on the left children) + (ranks on current node smaller than selection)
        children_left + range_size_less_than(&self.leaves, left)
    }
    
    pub fn base(&self, selection: Rank) -> u32 {
        let link = self.children.iter().find(|&s| {
            s.condition.contains(selection.0)
        });

        if let Some(link) = link {
            range_size(&link.condition)
        }
        else {
            range_size(&self.leaves)
        }
    }

    pub fn local_rank(&self, selection: &Rank) -> u32 {
        let link = self.children.iter().find(|&s| {
            s.condition.contains(selection.0)
        });

        let ret = if let Some(link) = link {
            // we do not care about scanning, only happens on the selection path
            link.condition.iter().position(|s| s == selection.0)
        }
        else {
            // we do not care about scanning, only happens on the selection path
            self.leaves.iter().position(|s| s == selection.0)
        };

        ret.unwrap() as u32
    }

    pub fn select(&self, name: &str, selection: &str) -> Path {
        let rank = self.set.rank(selection);
        let local_rank = self.local_rank(&rank);
        let node = self.clone();
        let prefix = self.size_left(rank);
        let base = self.base(rank);
        let head = PathElement {
            prefix,
            base,
            node,
            selection: rank,
            local_rank,
        };

        Path::new(name, &head)
    }

    pub fn attach(&mut self, min_choice: &str, max_choice: &str, target: Node) {
        let min_rank = self.set.rank(min_choice);
        let max_rank = self.set.rank(max_choice);
        let condition = (min_rank.0)..=(max_rank.0);
        
        if condition.is_empty() {
            panic!("Bad interval")
        }        
        let result = self.leaves.remove_range(condition.clone());
        
        let condition: Range = RangeSet::from(condition);
        if result.unwrap() != condition {
            panic!("Condition overlaps with previous attachment")
        }    
        
        let link = Link {
            condition,
            target,
        };
        
        self.children.push(link);
    }

    pub fn to_string(&self) -> String {
        let mut buffer = vec![];
        
        write_tree(self, &mut buffer).unwrap();

        String::from_utf8(buffer).expect("Invalid UTF-8 sequence")
    }
}

pub fn range_size(r: &Range) -> u32 {
    r.clone().into_smallvec().iter()
        .fold(0u32, |sum, val| sum + 1 + (val.clone().max().unwrap() - val.clone().min().unwrap()))
}
pub fn range_size_less_than(r: &Range, value: u32) -> u32 {
    if value == 0 {
        return 0
    }

    let remove = 0..=(value - 1);
    let mut r = r.clone();
    let removed = r.remove_range(remove);
    if let Some(removed) = removed {
        range_size(&removed)
    }
    else {
        0
    }
}

pub fn main() {
    
    let set10 = Set::new("11", 10);
    let mut node10 = Node::new(set10);
    
    let set20 = Set::new("20", 20);
    let node20 = Node::new(set20);
    
    let set5 = Set::new("5", 5);
    let node5 = Node::new(set5);

    let set30 = Set::new("30", 30);
    let mut node30 = Node::new(set30);    

    node30.attach("0", "9", node5);
    node10.attach("0", "4", node20);
    node10.attach("5", "9", node30);

    /* println!("{}", node10.to_string());
    
    let path = node10.select("Path 1", "5");
    let path = path.select("10");

    path.debug();*/

    let path = node10.select("Path 2", "9");
    let path = path.select("9");
    let path = path.select("4");

    path.debug();

/*
    let path = node10.select("Path 2", "5");
    let path = path.select("9");
    let path = path.select("4");

    path.debug();*/

    // let path = node10.select("Path 3", "10");

    // path.debug();
}


impl Set {
    pub fn new(name: &str, size: u32) -> Set {
        Set {
            name: name.to_string(),
            size,
        }
    }
    pub fn size(&self) -> u32 {
        self.size
    }
    pub fn rank(&self, s: &str) -> Rank {
        let choice = s.parse().unwrap();
        
        // < 0 impossible for u32
        if choice >= self.size {
            panic!("Invalid choice '{}'", choice);
        }
        
        Rank(choice)
    }
}



impl Link {
    // the size of a link = width of link * size of target node
    pub fn size_all(&self) -> u32 {
        let width = self.condition.max().unwrap() - self.condition.min().unwrap() + 1;
        
        width * self.target.size_all()
    }
}

impl TreeItem for Node {
    type Child = Self;
    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        write!(f, "{}", style.paint(self.set.name.clone()))
    }
    fn children(&self) -> Cow<[Self::Child]> {
        let children: Vec<Node> = self.children.iter().map(|l| l.target.clone()).collect();
        Cow::from(children)
    }
}

/*impl PathElement {
    pub fn select(self, s: &str) -> PathElement {
        
        let link = self.node.children.iter().find(|&s| {
            s.condition.contains(self.selection.0)
        });
        
        if let Some(link) = link {
            let selected_node = link.target.clone();
            let rank = selected_node.set.rank(s);
            let local_rank = selected_node.local_rank(&rank);
            let prefix = selected_node.size_left(rank);
            let base = selected_node.get_base(rank);
            PathElement {
                parent: Some(Box::new(self)),
                prefix,
                base,
                node: selected_node,
                selection: rank,
                local_rank,
            }            
        }
        else {
            panic!("Cannot select further");
        }
    }

    pub fn path_vector(&self) -> Vec<PathElement> {
        let mut next = self;
        let mut ret = vec![next.clone()];

        while next.parent.is_some() {
            next = next.parent.as_ref().unwrap();
            ret.push(next.clone());
        }

        ret.reverse();

        ret
    }

    pub fn debug(&self) {
        let path = self.path_vector();
        let mut multiplier = 1;
        let mut prefix = 0;
        let mut middle_rank = 0;
            for p in path {
            middle_rank = middle_rank + (p.local_rank * multiplier);
            prefix = prefix + (p.prefix * multiplier);
            // println!("rank = {}, set size = {}, selection = {}, base = {}, prefix = {}", (middle_rank + prefix), p.node.set.size(), p.selection.0, p.base, p.prefix * multiplier);
            println!("rank = {}, rank2 = {}, prefix = {}, multiplier = {}", middle_rank + prefix, p.local_rank, prefix, multiplier);
            multiplier *= p.base;
        }
    }
}*/