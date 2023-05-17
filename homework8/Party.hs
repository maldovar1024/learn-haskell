{-# LANGUAGE UndecidableInstances #-}

module Party where

import Data.List (sort)
import Data.Tree (Tree (rootLabel, subForest))
import Employee (Employee (empFun, empName), GuestList (GL), testCompany, testCompany2)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

instance Semigroup GuestList where
  (GL e1 f1) <> (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Semigroup GuestList => Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z t = f (rootLabel t) $ map (treeFold f z) (subForest t)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss guests = (glCons boss $ foldMap snd guests, foldMap (uncurry moreFun) guests)

maxFun :: Tree Employee -> GuestList
maxFun employees = uncurry moreFun $ treeFold nextLevel (mempty, mempty) employees

showGuestList (GL es fun) = "Total fun: " ++ show fun ++ foldMap ("\n" ++) (sort $ map empName es)

main = readFile "./company.txt" >>= putStrLn . showGuestList . maxFun . read
