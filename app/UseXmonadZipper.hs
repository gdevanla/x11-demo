{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DuplicateRecordFields #-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveDataTypeable#-}


module UseXmonadZipper where

-- Here Stack is a flat zipper --
data Stack a = Empty
  | Node { focus:: a, left:: [a], right::[a]}
  deriving (Show)

data Workspace a = Workspace {
  tag:: Int,
  windowStack:: Stack a
  } deriving (Show)

type Workspaces a = Stack (Workspace a)

-- Constructing a new window manager with 'n' virtual workspaces
new    :: Int -> Workspaces a
new n =  case [(Workspace i Empty) | i<-[0..n-1]] of
           (t:ts) -> Node {focus=t, left=[], right=ts}
           [] -> Empty

with:: b -> (Stack a -> b) -> Workspaces a -> b
with def f ws = case windowStack. focus $ ws of
  Empty -> def
  n -> f n

-- Extract the currently visible window, or nothing if the workspace is empty
peek :: Workspaces a -> Maybe a
peek = with Nothing (return . focus)

-- Index. Extract the windows on the current workspace as a list, for tiling
index  :: Workspaces a -> [a]
index = with [] f where
  f (Node i l r) = reverse l ++ i:r
  f Empty = []


modify:: Stack a -> (Stack a -> Stack a) -> Workspaces a -> Workspaces a
modify def f n = case n of
  Node _ _ _ -> n {focus = (focus n){windowStack = with def f n}}
  Empty -> Empty


focusLeft' :: Stack a -> Stack a
focusLeft' n@(Node _ [] []) = n
focusLeft' (Node t (l:ls) rs) = Node l ls (t:rs)
focusLeft' (Node t [] rs) = Node x xs [t]  where (x:xs) = reverse rs
focusLeft' Empty = Empty

focusRight' :: Stack a -> Stack a
focusRight' n@(Node _ [] []) = n
focusRight' (Node t l (r:rs)) = Node r (t:l) rs
focusRight' (Node t (l:ls) []) = Node l [t] ls
focusRight' Empty = Empty


--  Move focus to the left or right window on the current workspace
focusLeft:: Workspaces a -> Workspaces a
focusLeft  =  modify Empty focusLeft'

focusRight:: Workspaces a -> Workspaces a
focusRight = modify Empty focusRight'

-- -- Bring a new window under management
insert' :: a -> Stack a -> Stack a
insert' a (Node t l r)  = Node {focus=a, left=t:l, right=r}
insert' a Empty = Node {focus=a, left=[], right=[]}

insert   :: a -> Workspaces a -> Workspaces a
insert a ws = let
  def = Node {focus=a, left=[], right=[]}
  in
    modify def (insert' a) ws

-- -- Delete the currently focused window
delete :: Workspaces a  -> Workspaces a
delete = modify Empty $ \c -> case c of
  Node _ ls (r:rs) -> Node r ls rs
  Node _ (l:ls) [] -> Node l ls []
  Node _ [] [] -> Empty
  Empty -> Empty

-- There are some additional operations we’d like to support, which operate on the workspace level:

-- -- View the virtual workspace to the left or right.
viewLeft:: Workspaces a -> Workspaces a
viewLeft (Node t (l:ls) rs) = Node l ls (t:rs)
viewLeft t = t

viewRight:: Workspaces a -> Workspaces a
viewRight (Node t ls (r:rs)) = Node r (t:ls) rs
viewRight t = t

view:: Int -> Workspaces a -> Workspaces a
view i ws@(Node focus _ _)
  | i >= 0 && i < maxWorkspaces = foldr traverse' ws [1..abs(i-n)]
  | otherwise = ws
  where
    n = tag focus
    traverse' _ = if signum (i-n) >=0 then viewRight else viewLeft
view _ Empty = Empty

maxWorkspaces :: Int
maxWorkspaces = 2

shift :: Int -> Workspaces a -> Workspaces a
shift dest_ws ws
  | dest_ws >= 0 && dest_ws < maxWorkspaces && (dest_ws /= old) = maybe ws go (peek ws)
  | otherwise = ws
  where
    old = tag . focus $ ws
    go w = foldl (flip ($)) ws
           [
             delete,
             view dest_ws,
             insert w,
             view old]

-- -- Move the currently focused window to workspace with tag 'n'
-- shift  :: Int -> StackSet a -> StackSet a

test :: Workspaces Integer
test = foldl (flip ($)) (new 3)
    [
      view 0,
      insert 1,
      insert 2,
      insert 3,
      insert 4,
      viewRight,
      insert 5,
      insert 6,
      shift 0
    ]

sample_workspaces :: Workspaces a
sample_workspaces = new maxWorkspaces

main :: IO ()
main = do
  putStrLn "test"
