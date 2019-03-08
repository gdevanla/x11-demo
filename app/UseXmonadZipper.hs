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

data Workspace a = Workspace {
  tag:: Int,
  windowStack:: Stack a
  }

type Workspaces a = Stack (Workspace a)

-- Constructing a new window manager with 'n' virtual workspaces
new    :: Int -> Workspaces a
new x = Node {current=t, left=[], right=rs} where
  (t:rs) = [(Workspace i Empty) | i<-[0..n-1]]

-- Extract the currently visible window, or nothing if the workspace is empty
peek   :: Workspaces a -> Maybe a
peek workspaces = focus . windowStack .focus $ workspace

-- Index. Extract the windows on the current workspace as a list, for tiling
index  :: StackSet a -> [a]

-- Move focus to the left or right window on the current workspace
focusLeft, focusRight :: StackSet a -> StackSet a

-- Bring a new window under management
insert   :: a -> StackSet a -> StackSet a

-- Delete the currently focused window
delete :: -> StackSet a -> StackSet a

There are some additional operations we’d like to support, which operate on the workspace level:

-- View the virtual workspace to the left or right.
viewLeft, viewRight   :: StackSet a -> StackSet a

-- Move the currently focused window to workspace with tag 'n'
shift  :: Int -> StackSet a -> StackSet a
