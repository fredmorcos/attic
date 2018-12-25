#![warn(clippy::all)]

//
// # Exercise Expectations
//
// The goal here to is to establish a basis for our technical
// interview, and be able to discuss the choices you made and
// why. We're far more interested in the discussion part than
// receiving a perfect implementation of the following exercise. The
// exercise is not designed to take too long of your personal time,
// and doesn't have to be completed fully (although, bonus point if it
// is). We estimate it should be achieveable to complete between 1 to
// 2 hours of dedicated time.
//
// We don't expect rust expertise, but you *need* to have at least the
// basic fluency with the rust syntax, and we expect that the
// following module compiles.
//
// Please keep this exercise private, and don't make your result
// available publically.
//
// # Recap about Blockchain:
//
// A blockchain is organised as sequence of blocks.
//
//      Block 0 (Genesis) <- Block 1 <- Block 2 <- ...
//
// Block i is a parent of Block i+1
// Block i+1 is a child of Block i
//
// Each block has a specific hash, that are considered unique, and
// each blocks contains a reference to its parent block's hash.
//
// The first block is called genesis, and doesn't have a parent; this
// is the oldest block in the chain.  The latest block is often called
// the tip of the chain and is the yougest block added to the chain.
//
// The blockchain has a process that allows different entities to add
// blocks to the chain, to create a "distributed database". Each
// distributed entities maintains one version of the tip, that
// combines make a tree of blocks, where the common part are the
// previously agreed block history
//
// # Let's get started:
//
// The goal of this module is to define a in-memory blockchain in rust
// where we enforce the usual invariants that make a valid
// blockchain. we omitted any blockchain contents, and stripped the
// block to the minimal: its parent block hash, and a monotonically
// increasing numbers serving as timestamp.
//
// The invariant we have are standard:
//
// * the parent hash of a block. should match the hash of the parent
//
// * all children blocks of a given block should
//
// We define the basis of our exercise with the following definitions:
// *******************************************************************

// Dummy main() for clippy to work
fn main() {
}

/// Block Hash
type BlockHash = u64;

/// Block number (monotonically increasing)
type BlockNumber = u32;

#[derive(Debug, Clone)]
pub struct BlockChain {
  block: Block,

  // Should this really be Box<> instead of Rc<> or Arc<>? I thought
  // we're potentially making trees.
  parent: Option<Box<BlockChain>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockContent { /* block contents omitted */ }

/// A typical block
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
  number: BlockNumber,

  // Shouldn't this be an Option<> as well in case this block belongs
  // to genesis?
  parent_hash: BlockHash,

  content: BlockContent,
}

/// calculate the BlockHash associated with a Block
pub fn hash_of_block(block: &Block) -> BlockHash {
  use std::collections::hash_map::DefaultHasher;
  use std::hash::{Hash, Hasher};
  let mut s = DefaultHasher::new();
  block.number.hash(&mut s);
  block.parent_hash.hash(&mut s);
  s.finish()
}

//****************************************************************
// Part 1.1: define a function to append a block to the blockchain
// make sure that only valid blockchain are created
//****************************************************************

fn block_append(blockchain: BlockChain, block: Block) -> Option<BlockChain> {
  // `blockchain` is the tip and we add a new tip `block`,
  // `blockchain` will be the parent of `block`. I assume that since
  // "block: Block" isn't mutable and the existence of "Option<...>"
  // return in the signature that I only need to check for the
  // validity of block (number and parent-hash) and return None if
  // it's invalid. And that there's no need to actually fill-out the
  // the fields in the `block` struct.

  let tip: &Block = &blockchain.block;

  if block.number <= tip.number || block.parent_hash != hash_of_block(tip) {
    None
  } else {
    Some(BlockChain { block, parent: Some(Box::new(blockchain)) })
  }
}

//***************************************************************
// Part 1.2: Given a block hash, search in the blockchain for the
// block associated
//***************************************************************

fn block_lookup(blockchain: &BlockChain, hash: BlockHash) -> Option<Block> {
  // This would be the recursive implementation. But since that would
  // overflow the stack on a large blockchain, the iterative
  // implementation is below.

  // if hash_of_block(&blockchain.block) == hash {
  //   Some(blockchain.block.clone())
  // } else if let Some(parent) = &blockchain.parent {
  //   block_lookup(parent, hash)
  // } else {
  //   None
  // }

  // If the hash of the current block is not what we are looking for,
  // then we keep climbing back until genesis.

  let mut cursor: &BlockChain = blockchain;

  'outer: loop {
    if hash_of_block(&cursor.block) == hash {
      return Some(cursor.block.clone());
    } else if let Some(parent) = &cursor.parent {
      cursor = parent;
      continue 'outer;
    } else {
      return None;
    }
  }
}

//********************************************************************
// Part 1.3: Given a list of blockchains, returns the block that is
// the common ancestor if it exists
//
// The common ancestor is the youngest block in the blockchain that is
// common to each blockchains
//********************************************************************

// I changed the parameter name from `blockchain` to `blockchains`
// here because I think this is what it's supposed to be: a list of
// blockchains.
fn block_common_ancestor(blockchains: &[BlockChain]) -> Option<Block> {
  if blockchains.len() <= 1 {
    None
  } else {
    // So what happens here is based on the idea that if at least one
    // of the blockchains doesn't have any common elements with any
    // other blockchain, then the whole set does not have a common
    // ancestor. So basically we pick the first blockchain as the
    // "main" one and we iterate over it - from tip to genesis - with
    // a cursor, and for each value of that cursor, we search each of
    // the other blockchains. If _one_ of the other blockchains
    // doesn't have an element equal to the cursor, then it's safe to
    // assume that this value of the cursor is not a common
    // ancestor. So we move the cursor and try again. If we cannot
    // move the cursor anymore (we reached genesis) then we return
    // None. And if each of the other blockchains have an element
    // equal to the cursor the we found the common ancestor and we
    // return it.

    let mut cursor: &BlockChain = &blockchains[0];

    'outer: loop {
      let cursor_hash = hash_of_block(&cursor.block);

      for bc in &blockchains[1 ..] {
        if block_lookup(bc, cursor_hash).is_none() {
          if let Some(parent) = &cursor.parent {
            cursor = parent;
            continue 'outer;
          } else {
            return None;
          }
        }
      }

      return Some(cursor.block.clone());
    }
  }
}

fn common_ancestor<'a>(mut b1: &'a BlockChain, mut b2: &'a BlockChain) -> Option<&'a BlockChain> {
  if b2.block.number > b1.block.number {
    // skip b2 (b2 = parent(b2))
    if let Some(b2) = &b2.parent {
      common_ancestor(b1, b2)
    } else {
      None
    }
  } else if b2.block.number < b1.block.number {
    // skip b1 (b1 = parent(b1))
    if let Some(b1) = &b1.parent {
      common_ancestor(b1, b2)
    } else {
      None
    }
  } else {
    // block numbers are equal
    if hash_of_block(&b1.block) == hash_of_block(&b2.block) {
      Some(&b1)
    } else if let Some(b1) = &b1.parent {
      common_ancestor(b1, b2)
    } else {
      None
    }
  }
}

fn block_common_ancestor_faster(blockchains: &[BlockChain]) -> Option<Block> {
  if blockchains.len() <= 1 {
    None
  } else {
    let mut com = if let Some(com) = common_ancestor(&blockchains[0], &blockchains[1]) {
      com
    } else {
      return None;
    };

    for bc in blockchains.iter().skip(2) {
      com = if let Some(com) = common_ancestor(com, bc) {
        com
      } else {
        return None;
      }
    }

    Some(com.block.clone())
  }
}

//********************************************************************
// Part 1.4: Similar to part 1.2, but now we want to optimise the
// search to be as efficient as possible.
//
// Define an index structure that allow to optimise looking up by
// block hash and check for the existance of a block.
//
// Interesting point to consider is how to keep this index up-to-date
// and how it influences your choices of structure. Just like a
// blockchain that is updating, it would be beneficial to be able to
// create cheap fork of the index to represent fork of the blockchain.
// *******************************************************************

use std::collections::HashMap as Map;
use std::rc::Rc;

struct Index<'a> {
  // This really assumes that forks are rare. But if the blockchain
  // really is a tree of blocks rather than a tree of chains, then a
  // BTreeMap<BlockHash, &'a BlockChain> without a reference to a
  // parent would be the way to go (making lookups O(log B) where B is
  // the number of blocks, while this structure would have lookups of
  // O(log C) where C is the number of chains.

  // Forking the index would simply be pointing to the parent index.

  index: Map<BlockHash, &'a BlockChain>,
  parent: Option<Rc<Index<'a>>>,
}

impl<'a> Index<'a> {
  /// Given a blockchain, create the associated Index structure
  pub fn generate(blockchain: &'a BlockChain) -> Self {
    // This constructor would need a parameter for `parent`.

    let mut index: Map<BlockHash, &BlockChain> = Map::new();
    let mut cursor: &BlockChain = blockchain;

    loop {
      index.insert(hash_of_block(&cursor.block), cursor);

      if let Some(parent) = &cursor.parent {
        cursor = parent;
        continue;
      } else {
        break;
      }
    }

    Index { index, parent: None }
  }

  // I'm assuming it was a typo to pass `self` here rather than
  // `&self`.

  /// Lookup a block in the Blockchain, efficiently
  pub fn lookup(&self, hash: BlockHash) -> Option<Block> {
    if let Some(blockchain) = self.index.get(&hash) {
      Some(blockchain.block.clone())
    } else if let Some(parent) = &self.parent {
      parent.lookup(hash)
    } else {
      None
    }
  }

  // I'm assuming it was a typo to pass `self` here rather than
  // `&self`.

  /// Check if a specific block hash in the blockchain exists
  pub fn exists(&self, hash: BlockHash) -> bool {
    if self.index.get(&hash).is_some() {
      true
    } else if let Some(parent) = &self.parent {
      parent.exists(hash)
    } else {
      false
    }
  }
}

//*******************************************************************
// Part 2: Tests
//
// * Describe your process to tests some of the functions and
// properties.
//
// * Either in the form of valid rust code, or commented pseudo code.
//*******************************************************************
#[cfg(test)]
mod tests {
  use super::*;

  // Apologies for these functions being ugly, it's just easier to
  // view when each Block and BlockChain is on a single line

  fn gen_linear_blockchain() -> BlockChain {
    let bc0 = BlockChain { block: Block { number: 0, parent_hash: 0, content: BlockContent {} }, parent: None };
    let bc1 = BlockChain { block: Block { number: 1, parent_hash: hash_of_block(&bc0.block), content: BlockContent {} }, parent: Some(Box::new(bc0)) };
              BlockChain { block: Block { number: 2, parent_hash: hash_of_block(&bc1.block), content: BlockContent {} }, parent: Some(Box::new(bc1)) }
  }

  fn gen_tree_blockchain() -> ([BlockChain; 3], BlockChain) {
    let bc0 = BlockChain { block: Block { number: 4,  parent_hash: 0, content: BlockContent {} }, parent: None };
    let bc1 = BlockChain { block: Block { number: 5,  parent_hash: hash_of_block(&bc0.block), content: BlockContent {} }, parent: Some(Box::new(bc0)) };
    let bc2 = BlockChain { block: Block { number: 6,  parent_hash: hash_of_block(&bc1.block), content: BlockContent {} }, parent: Some(Box::new(bc1)) };

    let xx0 = BlockChain { block: Block { number: 7,  parent_hash: hash_of_block(&bc2.block), content: BlockContent {} }, parent: Some(Box::new(bc2.clone())) };
    let xx1 = BlockChain { block: Block { number: 8,  parent_hash: hash_of_block(&xx0.block), content: BlockContent {} }, parent: Some(Box::new(xx0)) };

    let yy0 = BlockChain { block: Block { number: 9,  parent_hash: hash_of_block(&bc2.block), content: BlockContent {} }, parent: Some(Box::new(bc2.clone())) };
    let yy1 = BlockChain { block: Block { number: 10, parent_hash: hash_of_block(&yy0.block), content: BlockContent {} }, parent: Some(Box::new(yy0)) };
    let yy2 = BlockChain { block: Block { number: 11, parent_hash: hash_of_block(&yy1.block), content: BlockContent {} }, parent: Some(Box::new(yy1)) };

    let zz0 = BlockChain { block: Block { number: 12, parent_hash: hash_of_block(&bc2.block), content: BlockContent {} }, parent: Some(Box::new(bc2.clone())) };

    ([xx1, yy2, zz0], bc2)      // return the common ancestor as well
  }

  #[test]
  fn test_append() {
    let bc = gen_linear_blockchain();
    let b = Block { number: 3, parent_hash: hash_of_block(&bc.block), content: BlockContent {} };

    assert!(block_append(bc, b).is_some());

    let bc = gen_linear_blockchain();
    let b = Block { number: 2, parent_hash: hash_of_block(&bc.block), content: BlockContent {} };

    assert!(block_append(bc, b).is_none());
  }

  #[test]
  fn test_lookup() {
    let bc = gen_linear_blockchain();
    assert!(block_lookup(&bc, bc.block.parent_hash).is_some());
    assert!(block_lookup(&bc, hash_of_block(&bc.block)).is_some());

    let b = Block { number: 13, parent_hash: hash_of_block(&bc.block), content: BlockContent {} };
    assert!(block_lookup(&bc, hash_of_block(&b)).is_none());

    let ([xx1, yy2, zz0], bc2) = gen_tree_blockchain();
    assert!(block_lookup(&xx1, hash_of_block(&bc2.block)).is_some());
    assert!(block_lookup(&yy2, hash_of_block(&bc2.block)).is_some());
    assert!(block_lookup(&zz0, hash_of_block(&bc2.block)).is_some());

    assert!(block_lookup(&xx1, hash_of_block(&b)).is_none());
    assert!(block_lookup(&yy2, hash_of_block(&b)).is_none());
    assert!(block_lookup(&zz0, hash_of_block(&b)).is_none());
  }

  #[test]
  fn test_common_ancestor() {
    let ([xx1, yy2, zz0], bc2) = gen_tree_blockchain();
    assert!(block_common_ancestor(&[xx1, yy2, zz0]).unwrap() == bc2.block);

    let ([xx1, yy2, zz0], _) = gen_tree_blockchain();
    let bc = gen_linear_blockchain();
    assert!(block_common_ancestor(&[xx1, yy2, zz0, bc]).is_none());
  }

  #[test]
  fn test_index() {
    let ([_, yy2, _], bc2) = gen_tree_blockchain();
    let index = Index::generate(&yy2);
    assert!(index.lookup(hash_of_block(&bc2.block)).is_some());
    assert!(index.lookup(hash_of_block(&yy2.block)).is_some());
    assert!(index.exists(hash_of_block(&bc2.block)));
    assert!(index.exists(hash_of_block(&yy2.block)));
  }

  fn gen_linear_blockchain_faster() -> BlockChain {
    let bc0 = BlockChain { block: Block { number: 0, parent_hash: 0, content: BlockContent {} }, parent: None };
    let bc1 = BlockChain { block: Block { number: 1, parent_hash: hash_of_block(&bc0.block), content: BlockContent {} }, parent: Some(Box::new(bc0)) };
              BlockChain { block: Block { number: 2, parent_hash: hash_of_block(&bc1.block), content: BlockContent {} }, parent: Some(Box::new(bc1)) }
  }

  fn gen_tree_blockchain_faster() -> ([BlockChain; 3], BlockChain) {
    let bc0 = BlockChain { block: Block { number: 4,  parent_hash: 0, content: BlockContent {} }, parent: None };
    let bc1 = BlockChain { block: Block { number: 5,  parent_hash: hash_of_block(&bc0.block), content: BlockContent {} }, parent: Some(Box::new(bc0)) };
    let bc2 = BlockChain { block: Block { number: 6,  parent_hash: hash_of_block(&bc1.block), content: BlockContent {} }, parent: Some(Box::new(bc1)) };

    let xx0 = BlockChain { block: Block { number: 7,  parent_hash: hash_of_block(&bc2.block), content: BlockContent {} }, parent: Some(Box::new(bc2.clone())) };
    let xx1 = BlockChain { block: Block { number: 8,  parent_hash: hash_of_block(&xx0.block), content: BlockContent {} }, parent: Some(Box::new(xx0)) };

    let yy0 = BlockChain { block: Block { number: 7,  parent_hash: hash_of_block(&bc2.block), content: BlockContent {} }, parent: Some(Box::new(bc2.clone())) };
    let yy1 = BlockChain { block: Block { number: 8, parent_hash: hash_of_block(&yy0.block), content: BlockContent {} }, parent: Some(Box::new(yy0)) };
    let yy2 = BlockChain { block: Block { number: 9, parent_hash: hash_of_block(&yy1.block), content: BlockContent {} }, parent: Some(Box::new(yy1)) };

    let zz0 = BlockChain { block: Block { number: 7, parent_hash: hash_of_block(&bc2.block), content: BlockContent {} }, parent: Some(Box::new(bc2.clone())) };

    ([xx1, yy2, zz0], bc2)      // return the common ancestor as well
  }

  #[test]
  fn test_common_ancestor_faster() {
    let ([xx1, yy2, zz0], bc2) = gen_tree_blockchain();
    assert!(block_common_ancestor(&[xx1, yy2, zz0]).unwrap() == bc2.block);
    let ([xx1, yy2, zz0], bc2) = gen_tree_blockchain();
    assert!(block_common_ancestor_faster(&[xx1, yy2, zz0]).unwrap() == bc2.block);
    let ([xx1, yy2, zz0], bc2) = gen_tree_blockchain();
    let ([xx1_2, yy2_2, zz0_2], bc2_2) = gen_tree_blockchain();
    let res = block_common_ancestor(&[xx1, yy2, zz0]);
    let res_2 = block_common_ancestor_faster(&[xx1_2, yy2_2, zz0_2]);
    assert_eq!(res, res_2);
    assert_eq!(res.unwrap(), bc2.block);
    assert_eq!(res_2.unwrap(), bc2_2.block);

    let ([xx1, yy2, zz0], _) = gen_tree_blockchain();
    let bc = gen_linear_blockchain();
    assert!(block_common_ancestor_faster(&[xx1, yy2, zz0, bc]).is_none());
  }
}
