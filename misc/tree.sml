signature TREE = sig

  type key = string
  datatype tree = LEAF | TREE of tree * key * tree

  val empty : tree
  val insert : key * tree -> tree
  val member : key * tree -> bool

end

structure Tree :> TREE = struct

  type key = string
  datatype tree = LEAF | TREE of tree * key * tree

  val empty = LEAF

  fun insert (key, LEAF) = TREE(LEAF, key, LEAF)
    | insert (key, t as TREE(l, k, r)) =
      (case String.compare (key, k)
         of LESS => TREE(insert(key, l), k, r)
          | GREATER => TREE(l, k, insert(key, r))
          | EQUAL => t)
  
  fun member (key, LEAF) = false
    | member (key, TREE(l, k, r)) =
        (case String.compare (key, k)
           of EQUAL => true
            | LESS => member (key, l)
            | GREATER => member (key, r))

end

signature SEARCHTREE = sig

  type key = string
  type 'a node = key * 'a
  type 'a tree

  val empty : 'a tree
  val insert : 'a node * 'a tree -> 'a tree
  val lookup : key * 'a tree -> 'a

end

structure SearchTree :> SEARCHTREE = struct
  
  type key = string
  type 'a node = key * 'a
  datatype 'a tree = LEAF | TREE of 'a tree * 'a node * 'a tree

  exception Lookup of key

  val empty = LEAF

  fun insert (node, LEAF) = TREE (LEAF, node, LEAF)
    | insert (node as (key, _), TREE(l, n as (k, _), r)) =
      (case String.compare (key, k)
         of LESS => TREE(insert(node, l), n, r)
          | GREATER => TREE(l, n, insert(node, r))
          | EQUAL => TREE(l, node, r))
  
  fun lookup (key, LEAF) = raise (Lookup key)
    | lookup (key, TREE(l, (k, v), r)) =
      (case String.compare (key, k)
         of EQUAL => v
          | LESS => lookup (key, l)
          | GREATER => lookup (key, r))

end

structure RedBlackTree :> SEARCHTREE = struct

  type key = string
  type 'a node = key * 'a
  datatype 'a tree =
    LEAF
  | Red of 'a tree * 'a node * 'a tree
  | Black of 'a tree * 'a node * 'a tree

  val empty = LEAF

  exception Lookup of key

  fun lookup (key, tree) =
    let
      fun lk (LEAF) = raise (Lookup key)
        | lk (Red t) = lk' t
        | lk (Black t) = lk' t
      and lk' (l, (k, v), r) =
            (case String.compare (key, k)
               of EQUAL => v
                | LESS => lk l
                | GREATER => lk r)
    in
      lk tree
    end
    
  fun restoreLeft (Black (Red (Red (n1, x, n2), y, n3), z, n4)) =
          Red (Black (n1, x, n2), y, Black (n3, z, n4))
      | restoreLeft (Black (Red (n1, x, Red (n2, y, n3)), z, n4)) =
          Red (Black (n1, x, n2), y, Black (n3, z, n4))
      | restoreLeft tree = tree
  
  fun restoreRight (Black (n1, x, Red (n2, y, Red (n3, z, n4)))) =
          Red (Black (n1, x, n2), y, Black (n3, z, n4))
      | restoreRight (Black (n1, x, Red (Red (n2, y, n3), z, n4))) =
          Red (Black (n1, x, n2), y, Black (n3, z, n4))
      | restoreRight tree = tree

  fun insert (node as (key, value), tree) =
    let
      fun ins (LEAF) = Red (LEAF, node, LEAF)
        | ins (Red (l, node1 as (k, v), r)) =
          (case String.compare (key, k)
             of EQUAL => Red (l, node, r)
              | LESS => Red (ins l, node1, r)
              | GREATER => Red (l, node1, ins r))
        | ins (Black (l, node1 as (k, v), r)) =
          (case String.compare (key, k)
             of EQUAL => Black (l, node, r)
              | LESS => restoreLeft (Black (ins l, node1, r))
              | GREATER => restoreRight (Black (l, node1, ins r)))
    in
      case ins tree
        of Red (t as (Red _, _, _)) => Black t
         | Red (t as (_, _, Red _)) => Black t
         | t => t
    end
end