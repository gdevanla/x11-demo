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

module UseZipper where

data Node a = DeadEnd a
  | Passage a (Node a)
  | Fork a (Node a) (Node a)


data Branch a = KeepStraight a
  | TurnLeft a (Node a)
  | TurnRight a (Node a)

type Thread a = [Branch a]

type Zipper a = (Node a, Thread a)

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (Fork x l r, t) = Just (r, TurnRight x l:t)
turnRight _ = Nothing

keepStraightOn :: Zipper a -> Maybe (Zipper a)
keepStraightOn (Passage x n, t) = Just (n, KeepStraight x:t)
keepStraightOn _ = Nothing

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (Fork x l r, t) = Just (l, TurnLeft x r:t)
turnLeft _ = Nothing

back :: Zipper a -> Maybe (Zipper a)
back ( _, []) = Nothing
back (n, KeepStraight x:ts) = Just ((Passage x n), ts)
back (l, (TurnLeft a r):ts) = Just ((Fork a l r), ts)
back (r, (TurnRight a l): ts) = Just ((Fork a l r), ts)

get :: Zipper a -> a
get (Passage x _, _) = x
get (DeadEnd x, _) = x
get (Fork x _ _, _ ) = x

put :: a -> Zipper a -> Zipper a
put a (Passage _ n, t) = (Passage a n, t)
put a (DeadEnd _, t) = (DeadEnd a, t)
put a (Fork _ l r, t) = (Fork a l r, t)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

data Tree a = Item a | Node [Tree a] deriving (Eq, Show)

data Path a = Top
  | Point {left:: [Tree a], right:: [Tree a], up:: Path a} deriving (Eq, Show)

data Cursor a = Cursor {it:: Tree a, context:: Path a} deriving (Eq, Show)

expr :: Tree String
expr = Node [ Node [ Item "a", Item "*", Item "b"],
              Item "+",
              Node [Item "c", Item "*", Item "d"]
            ]

subexpr :: Cursor String
subexpr = Cursor {
  it = Item "*",
  context = Point {left=[Item "c"],
             right=[Item "d"],
             up=Point{up=Top,
                      right=[],
                      left=[Item "+", Node [Item "a", Item "*", Item "b"]]
                     }
            }
  }

initCursor :: Cursor String
initCursor = Cursor expr (Point [] [] Top)

focusDown :: Cursor a -> Cursor a
focusDown c@(Cursor t p) = case t of
  Node (l:ls) -> Cursor l (Point [] ls p)
  _ -> c


focusLeft :: Cursor a -> Cursor a
focusLeft c@(Cursor t p) = case p of
  Point (l:ls) r u -> Cursor {it=l,  context=Point {left=ls, right=t:r, up=u}}
  _ -> c

focusRight :: Cursor a -> Cursor a
focusRight c@(Cursor t p) = case p of
  Point ls (r:rs) u -> Cursor {it=r,  context=Point {left=t:ls, right=rs, up=u}}
  _ -> c

focusUp:: Cursor a -> Cursor a
focusUp c@(Cursor t p) = case p of
  Top -> c
  Point ls rs up -> Cursor (Node (reverse ls ++ t:rs)) up

insertLeft :: Tree a -> Cursor a -> Cursor a
insertLeft a (Cursor t p) = case p of
  Top -> undefined
  Point ls rs up -> Cursor a (Point ls (t:rs) up)

insertRight :: Tree a -> Cursor a -> Cursor a
insertRight a (Cursor t p) = case p of
  Top -> undefined
  Point ls rs up -> Cursor a (Point (t:ls) rs up)


delete :: Cursor a -> Cursor a
delete (Cursor _ p) = case p of
  Top -> undefined
  Point ls (r:rs) up -> Cursor r (Point ls rs up)
  Point (l:ls) [] up -> Cursor l (Point ls [] up)
  Point [] [] up -> Cursor (Node []) up


--- Zipper for file system

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show, Eq)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show, Eq)

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

type FSZipper = (FSItem, [FSCrumb])


-- XMonad zipper implementation

data WorkspaceStack a =
  WorkspaceStack { current:: Workspace a,
             prev:: [Workspace a],
             next:: [Workspace a]
          }

data Workspace a = Workspace {tag:: Int, stack:: WindowStack a}

data WindowStack a = Empty
  | Window {focus:: a,
             left:: [a],
             right:: [a]
           }

new :: Int -> WorkspaceStack a
new n
  | n > 0 = WorkspaceStack t [] rs
  | _ = undefined
    where
      (t:rs) = [Workspace i Empty | i <- [0..n-1]]

with :: b -> (WindowStack a -> b) -> WorkspaceStack a -> b
with default_value f wss = case stack (current wss) of
  Empty -> default_value
  v  -> f v

peek :: WorkspaceStack a -> Maybe a
peek = with Nothing (return . focus)

index :: WorkspaceStack a -> [a]
index = with [] f
  where
    f (Window t l r) = reverse l ++ t:r
    f _ = []

modify :: WindowStack a -> (WindowStack a -> WindowStack a) -> WorkspaceStack a -> WorkspaceStack a
modify default_value f wss = wss {current = (current wss) {stack = with default_value f wss}}


main :: IO ()
main = do
  putStrLn "test"
