{-# LANGUAGE CPP #-}

module GHC.Core.Opt.PhaseOrder (getPhaseOrder) where

import qualified Data.Map.Strict as HashMap
import Data.List (sortBy, map)
import Data.Ord (comparing)
import GHC.Driver.Plugins ( withPlugins, installCoreToDos )
import GHC.Core.Opt.Monad
import Data.String
import Data.Int
import Data.Maybe (fromMaybe)

{-
1. How this code works: The original Haskell Code will perform runWhen and then place that value into a tuple, where the first value is a key for the optimization performed.
2. This list of tuples will be passed into this file, which will then select a HashMap<String, Int> based on a specific value.
3. Once the HashMap selection is made, we will create a list of size n. The value for our hashmap acts as the index for the order we want the optimization, represented by the key to go.
4. We then return this list back, and the Pipeline will continue as normal.
-}


createHashMap :: HashMap.Map String Int
createHashMap = HashMap.fromList []

insertIntoHashMap :: HashMap.Map String Int -> String -> Int -> HashMap.Map String Int
insertIntoHashMap hashMap key value = HashMap.insert key value hashMap

listToNewList :: [(String, CoreToDo)] -> HashMap.Map String Int -> [(Int, CoreToDo)]
listToNewList inputList hashMap = map getValue inputList
  where
    getValue :: (String, CoreToDo) -> (Int, CoreToDo)
    getValue (key, opt) = (fromMaybe 0 (HashMap.lookup key hashMap), opt)

orderTuples :: [(Int, CoreToDo)] -> [(Int, CoreToDo)]
orderTuples tuples = sortBy (comparing fst') tuples
  where
    fst' :: (Int, CoreToDo) -> Int
    fst' (x, _) = x

getPhaseOrder :: [CoreToDo] -> [CoreToDo]
getPhaseOrder coreToDoList = do

    let my_var = 0

    let my_hashmap = case my_var of
                        0 -> insertIntoHashMap (insertIntoHashMap createHashMap "OptB" 0) "OptA" 1
                        1 -> insertIntoHashMap createHashMap "One" 1
                        2 -> insertIntoHashMap createHashMap "Two" 2
                        3 -> insertIntoHashMap createHashMap "Three" 3
                        4 -> insertIntoHashMap createHashMap "Four" 4


    -- let list_of_strings = [("OptA", "FunnyMan"), ("OptB", "FunnyWoman")]
    -- let new_list = listToNewList list_of_strings my_hashmap
    -- let final_list = orderTuples new_list
    -- final_list
    -- print new_list
    -- print final_list
    let final_list = []
    final_list


