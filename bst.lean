import Lean

inductive Bst (β : Type) where
  | leaf
  | node (left : Bst β) (value : β) (right : Bst β)
  deriving Repr

def insert [Ord β] (v : β) (t : Bst β) : Bst β :=
  match t with
  | Bst.leaf => Bst.node (Bst.leaf) v (Bst.leaf)
  | Bst.node left value right =>
    match (compare v value) with
    | Ordering.eq => Bst.node left value right
    | Ordering.gt => Bst.node left value (insert v right)
    | Ordering.lt => Bst.node (insert v left) value right

def search [Ord β] (v : β) (t : Bst β) : Bst β :=
  match t with
  | Bst.leaf => Bst.leaf
  | Bst.node left value right => 
    match (compare v value) with
    | Ordering.eq => Bst.node left value right
    | Ordering.gt => search v right
    | Ordering.lt => search v left

#eval insert 1 Bst.leaf |> insert 2

#eval search 2 (insert 2 Bst.leaf |> insert 3 |> insert 1)