use std::cmp::Ord;
use std::cmp::Ordering::{Equal, Greater, Less};
use std::error::Error;
use std::fmt::{format, Debug, Display, Formatter};
use std::marker::Copy;
use std::string::String;
use std::vec::Vec;

type NodeLink<K, V> = Option<Box<Node<K, V>>>;

trait LinkRemover<K: Ord + Copy, V: Copy> {
    fn remove(&mut self, key: K) -> bool;
}

impl<K: Ord + Copy, V: Copy> LinkRemover<K, V> for NodeLink<K, V> {
    fn remove(&mut self, key: K) -> bool {
        if let Some(tmp_node) = self {
            match tmp_node.key.cmp(&key) {
                Greater => {
                    return tmp_node.left.remove(key);
                }
                Less => {
                    return tmp_node.right.remove(key);
                }
                Equal => {
                    match (tmp_node.left.take(), tmp_node.right.take()) {
                        (None, None) => *self = None,
                        (Some(left_node), None) => {
                            *self = Some(left_node);
                        }
                        (None, Some(right_node)) => {
                            *self = Some(right_node);
                        }
                        (Some(left_node), Some(right_node)) => {
                            let new_node = &mut *tmp_node;
                            new_node.left = Some(left_node);
                            new_node.right = Some(right_node);
                            let (new_k, new_v) = new_node.right.as_mut().unwrap().leftmost_leaf();
                            new_node.key = new_k;
                            new_node.value = new_v;
                            new_node.right.remove(new_node.key);
                        }
                    }
                    return true;
                }
            }
        } else {
            return false;
        }
    }
}

struct Node<K: Ord + Copy, V: Copy> {
    key: K,
    value: V,
    left: NodeLink<K, V>,
    right: NodeLink<K, V>,
}

impl<K: Ord + Copy, V: Copy> Node<K, V> {
    fn insert(&mut self, new_node: Box<Node<K, V>>) {
        match self.key.cmp(&new_node.key) {
            Equal => self.value = new_node.value,
            Greater => match self.left.as_mut() {
                None => {
                    self.left = Some(new_node);
                }
                Some(left_node) => {
                    left_node.insert(new_node);
                }
            },
            Less => match self.right.as_mut() {
                None => {
                    self.right = Some(new_node);
                }
                Some(right_node) => {
                    right_node.insert(new_node);
                }
            },
        }
    }

    // inorder traversal
    fn traverse_and_fn(
        &self,
        f: &mut dyn FnMut(K, V) -> Result<(), Box<dyn Error>>,
    ) -> Result<(), Box<dyn Error>> {
        match self.left.as_ref() {
            Some(node) => {
                if let Err(err) = node.traverse_and_fn(f) {
                    return Err(err);
                }
            }
            None => (),
        };
        if let Err(err) = f(self.key, self.value) {
            return Err(err);
        }
        match self.right.as_ref() {
            Some(node) => {
                if let Err(err) = node.traverse_and_fn(f) {
                    return Err(err);
                }
            }
            None => (),
        };
        Ok(())
    }

    fn traverse_preorder_and_fn(
        &self,
        f: &mut dyn FnMut(K, V) -> Result<(), Box<dyn Error>>,
    ) -> Result<(), Box<dyn Error>> {
        if let Err(err) = f(self.key, self.value) {
            return Err(err);
        }
        match self.left.as_ref() {
            Some(node) => {
                if let Err(err) = node.traverse_preorder_and_fn(f) {
                    return Err(err);
                }
            }
            None => (),
        };
        match self.right.as_ref() {
            Some(node) => {
                if let Err(err) = node.traverse_preorder_and_fn(f) {
                    return Err(err);
                }
            }
            None => (),
        };
        Ok(())
    }

    fn leftmost_leaf(&mut self) -> (K, V) {
        if let Some(left_node) = self.left.as_mut() {
            return left_node.leftmost_leaf();
        }
        return (self.key, self.value);
    }

    fn find_val(&self, key: K) -> Option<V> {
        match self.key.cmp(&key) {
            Equal => return Some(self.value),
            Greater => match self.left.as_ref() {
                None => return None,
                Some(left_node) => {
                    return left_node.find_val(key);
                }
            },
            Less => match self.right.as_ref() {
                None => return None,
                Some(right_node) => {
                    return right_node.find_val(key);
                }
            },
        }
    }
}

impl<K: Display + Ord + Copy, V: Copy> Debug for Node<K, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut l_str = String::from("None");
        let mut r_str = String::from("None");
        match self.left.as_ref() {
            None => (),
            Some(left_node) => l_str = format(format_args!("{}", left_node.key)),
        }
        match self.right.as_ref() {
            None => (),
            Some(right_node) => r_str = format(format_args!("{}", right_node.key)),
        }
        write!(
            f,
            "Node{{value: {}, left: {}, right: {}}}",
            self.key, l_str, r_str
        )
    }
}

pub struct Tree<K: Ord + Copy, V: Copy> {
    root: NodeLink<K, V>,
    num_elems: usize,
}

impl<K: Ord + Copy, V: Copy> Tree<K, V> {
    pub fn new() -> Self {
        return Tree {
            root: None,
            num_elems: 0,
        };
    }

    pub fn insert(&mut self, key: K, value: V) {
        let new_node = Box::new(Node {
            key: key,
            value: value,
            left: None,
            right: None,
        });
        match self.root.as_mut() {
            None => {
                self.root = Some(new_node);
            }
            Some(node) => {
                (&mut **node).insert(new_node);
            }
        }
        self.num_elems += 1;
    }

    pub fn get(&self, key: K) -> Option<V> {
        match self.root.as_ref() {
            None => return None,
            Some(node) => return node.find_val(key),
        }
    }

    pub fn num_elems(&self) -> usize {
        return self.num_elems;
    }

    pub fn get_contents(&self) -> Vec<(K, V)> {
        // TODO: might be more efficient to use pre-allocated array
        let mut dst = Vec::with_capacity(self.num_elems);
        let mut writer = |key: K, val: V| -> Result<(), Box<dyn Error>> {
            dst.push((key, val));
            Ok(())
        };

        match self.root.as_ref() {
            None => {
                return dst;
            }
            Some(node) => match node.traverse_and_fn(&mut writer) {
                Ok(()) => return dst,
                Err(e) => panic!("unknown err traversing and writing: {:?}", e),
            },
        }
    }

    pub fn get_preorder_contents(&self) -> Vec<(K, V)> {
        // TODO: might be more efficient to use pre-allocated array
        let mut dst = Vec::with_capacity(self.num_elems);
        let mut writer = |key: K, val: V| -> Result<(), Box<dyn Error>> {
            dst.push((key, val));
            Ok(())
        };

        match self.root.as_ref() {
            None => {
                return dst;
            }
            Some(node) => match node.traverse_preorder_and_fn(&mut writer) {
                Ok(()) => return dst,
                Err(e) => panic!("unknown err traversing and writing: {:?}", e),
            },
        }
    }

    pub fn remove(&mut self, key: K) -> bool {
        let did_remove = self.root.remove(key);
        if did_remove {
            self.num_elems -= 1;
        }
        return did_remove;
    }
}

#[cfg(test)]
mod test {
    use super::Tree;
    #[test]
    fn test_num_elems() {
        let mut tree = Tree::new();

        // verify no initial elements
        assert_eq!(tree.num_elems(), 0);

        // add some values
        tree.insert("c", 2);
        tree.insert("e", 4);
        tree.insert("d", 3);
        tree.insert("b", 1);
        tree.insert("f", 5);

        assert_eq!(tree.num_elems(), 5);
    }

    #[test]
    fn test_get() {
        let mut tree = Tree::new();
        // add some values
        tree.insert("c", 2);
        tree.insert("e", 4);
        tree.insert("d", 3);
        tree.insert("b", 1);
        tree.insert("f", 5);

        // valid key
        let v0 = tree.get("d");
        assert_eq!(3, v0.unwrap());

        // non-present key
        let v1 = tree.get("s");
        assert_eq!(None, v1);
    }

    #[test]
    fn test_get_contents() {
        let mut tree = Tree::new();

        // add some values
        tree.insert("c", 2);
        tree.insert("e", 4);
        tree.insert("d", 3);
        tree.insert("b", 1);
        tree.insert("f", 5);

        let v1 = tree.get_contents();
        let expected_contents = vec![("b", 1), ("c", 2), ("d", 3), ("e", 4), ("f", 5)];
        assert_eq!(expected_contents, v1);

        let v2 = tree.get_preorder_contents();
        let expected_preorder_contents = vec![("c", 2), ("b", 1), ("e", 4), ("d", 3), ("f", 5)];
        assert_eq!(expected_preorder_contents, v2);
    }

    #[test]
    fn test_insert() {
        let mut tree = Tree::new();
        // add some values
        tree.insert("c", 2);
        tree.insert("e", 4);
        tree.insert("d", 3);
        tree.insert("b", 1);
        tree.insert("f", 5);
        // verify initial contents
        let v0 = tree.get_contents();
        let expected_contents = vec![("b", 1), ("c", 2), ("d", 3), ("e", 4), ("f", 5)];
        assert_eq!(expected_contents, v0);

        // add duplicate k,v + verify nothing changed
        tree.insert("b", 1);
        let v1 = tree.get_contents();
        assert_eq!(expected_contents, v1);

        // add duplicate k w/ new v + verify update
        tree.insert("d", 100);
        let v1 = tree.get_contents();
        let expected_contents1 = vec![("b", 1), ("c", 2), ("d", 100), ("e", 4), ("f", 5)];
        assert_eq!(expected_contents1, v1);
    }

    #[test]
    fn test_delete_node_no_children() {
        let mut tree = Tree::new();

        // add some values
        tree.insert("c", 2);
        tree.insert("e", 4);
        tree.insert("b", 1);
        tree.insert("d", 3);
        tree.insert("f", 5);
        tree.insert("g", 6);

        // delete left-most leaf - no children
        let did_remove0 = tree.remove("b");
        let v0 = tree.get_contents();
        let expected_contents0 = vec![("c", 2), ("d", 3), ("e", 4), ("f", 5), ("g", 6)];
        assert_eq!(did_remove0, true);
        assert_eq!(expected_contents0, v0);

        // delete right-most leaf - no children
        let did_remove1 = tree.remove("g");
        let v1 = tree.get_contents();
        let expected_contents1 = vec![("c", 2), ("d", 3), ("e", 4), ("f", 5)];
        assert_eq!(did_remove1, true);
        assert_eq!(expected_contents1, v1);

        // re-delete left-most leaf
        let did_remove2 = tree.remove("a");
        let v2 = tree.get_contents();
        let expected_contents2 = vec![("c", 2), ("d", 3), ("e", 4), ("f", 5)];
        assert_eq!(did_remove2, false);
        assert_eq!(expected_contents2, v2);

        // re-delete right-most leaf
        let did_remove3 = tree.remove("g");
        let v3 = tree.get_contents();
        let expected_contents3 = vec![("c", 2), ("d", 3), ("e", 4), ("f", 5)];
        assert_eq!(did_remove3, false);
        assert_eq!(expected_contents3, v3);
    }

    #[test]
    fn test_delete_node_1_child() {
        let mut tree = Tree::new();

        // add some values
        tree.insert("d", 3);
        tree.insert("c", 2);
        tree.insert("b", 1);
        tree.insert("e", 4);
        tree.insert("f", 5);

        // delete node with 1 left child
        let did_remove0 = tree.remove("c");
        let v0 = tree.get_contents();
        let expected_contents0 = vec![("b", 1), ("d", 3), ("e", 4), ("f", 5)];
        assert_eq!(did_remove0, true);
        assert_eq!(expected_contents0, v0);

        // delete node with 1 right child
        let did_remove1 = tree.remove("e");
        let v1 = tree.get_contents();
        let expected_contents1 = vec![("b", 1), ("d", 3), ("f", 5)];
        assert_eq!(did_remove1, true);
        assert_eq!(expected_contents1, v1);

        // re-delete first node
        let did_remove2 = tree.remove("c");
        let v2 = tree.get_contents();
        let expected_contents2 = vec![("b", 1), ("d", 3), ("f", 5)];
        assert_eq!(did_remove2, false);
        assert_eq!(expected_contents2, v2);

        // re-delete second node
        let did_remove3 = tree.remove("e");
        let v3 = tree.get_contents();
        let expected_contents3 = vec![("b", 1), ("d", 3), ("f", 5)];
        assert_eq!(did_remove3, false);
        assert_eq!(expected_contents3, v3);
    }

    #[test]
    fn test_delete_node_2_child() {
        let mut tree = Tree::new();

        // add some values
        tree.insert("c", 2);
        tree.insert("d", 3);
        tree.insert("b", 1);
        tree.insert("e", 4);
        tree.insert("f", 5);

        // delete node with 2 children
        let did_remove0 = tree.remove("c");
        let v0 = tree.get_contents();
        let expected_contents0 = vec![("b", 1), ("d", 3), ("e", 4), ("f", 5)];
        assert_eq!(did_remove0, true);
        assert_eq!(expected_contents0, v0);

        // delete another node with 2 children
        let did_remove1 = tree.remove("d");
        let v1 = tree.get_contents();
        let expected_contents1 = vec![("b", 1), ("e", 4), ("f", 5)];
        assert_eq!(did_remove1, true);
        assert_eq!(expected_contents1, v1);
    }

    #[test]
    fn test_delete_node_2_child_2() {
        // example from textbook
        let mut tree = Tree::new();
        let init_contents = vec![
            ("p", 15),
            ("g", 6),
            ("e", 4),
            ("k", 10),
            ("i", 8),
            ("j", 9),
            ("l", 11),
            ("n", 13),
            ("s", 18),
            ("r", 17),
            ("u", 20),
        ];
        for x in init_contents.iter() {
            tree.insert(x.0, x.1)
        }
        // verify initial contents
        let expected_preorder = vec![
            ("p", 15),
            ("g", 6),
            ("e", 4),
            ("k", 10),
            ("i", 8),
            ("j", 9),
            ("l", 11),
            ("n", 13),
            ("s", 18),
            ("r", 17),
            ("u", 20),
        ];
        assert_eq!(expected_preorder, tree.get_preorder_contents());

        // remove node w/ 2 children + verify contents
        let did_remove = tree.remove("g");
        assert_eq!(did_remove, true);
        let new_expected_preorder_contents = vec![
            ("p", 15),
            ("i", 8),
            ("e", 4),
            ("k", 10),
            ("j", 9),
            ("l", 11),
            ("n", 13),
            ("s", 18),
            ("r", 17),
            ("u", 20),
        ];
        assert_eq!(new_expected_preorder_contents, tree.get_preorder_contents());
    }

    #[test]
    fn test_delete_all() {
        let mut tree = Tree::new();
        let init_contents = vec![("c", 2), ("d", 3), ("b", 1), ("e", 4), ("f", 5), ("g", 6)];
        for x in init_contents.iter() {
            tree.insert(x.0, x.1)
        }
        assert_eq!(init_contents.len(), tree.num_elems());

        for x in init_contents.iter() {
            let did_remove = tree.remove(x.0);
            assert_eq!(did_remove, true);
        }

        assert_eq!(0, tree.get_contents().len());
        assert_eq!(0, tree.num_elems());
    }
}
