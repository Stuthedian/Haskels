data Tree a = Empty | Branch a (Tree a) (Tree a)
data LinkList a = EmptyL | Node a (LinkList a) (LinkList a)

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Branch _ l r) = abs(height l - height r) <= 1


height :: Tree a -> Int
height Empty = 0
height (Branch _ l r) = 1 + (max (height l) (height r))

fromList :: [Int] -> Tree Int
fromList [] = Empty
fromList x = let (l, a, r) = divide x in Branch a (fromList l) (fromList r)

divide :: [a] -> ([a], a, [a])
divide [x] = ([], x, [])
divide x = let l = (length x) `div` 2; t = take l x; (h:d) = drop l x in  (t, h, d)

insert :: a -> LinkList a -> LinkList a
insert x ll = insert' x EmptyL ll
 where 
 insert' :: a -> LinkList a -> LinkList a -> LinkList a
 insert' x prev EmptyL = Node x prev EmptyL
 insert' x _ p@(Node a prev next) = Node a prev $ insert' x p next

instance Show a => Show (LinkList a) where
  show EmptyL = "null"
  show (Node x prev next) = show x ++ " -> " ++ show next

collect :: Int -> Tree a -> LinkList a
collect _ Empty = EmptyL
collect 0 (Branch x _ _) = insert x EmptyL
collect n (Branch _ l r) = collect (n-1) l +++ collect (n-1) r

(+++) :: LinkList a -> LinkList a -> LinkList a
(+++) EmptyL ll = ll
(+++) ll EmptyL = ll
(+++) (Node x prev EmptyL) ll = Node x prev ll
(+++) (Node x prev next) ll = Node x prev (next +++ ll)

treeToList :: Tree a -> [LinkList a]
treeToList t = map (flip collect t) [0..height t]


