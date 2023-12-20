{-# LANGUAGE CPP #-}

module GHC.Core.Opt.PhaseOrder (getPhaseOrder) where

import qualified Data.Map.Strict as Map
import Data.List (sortBy, map)
import Data.Ord (comparing)
import GHC.Core.Opt.Monad
import Data.String
import Data.Int
import Data.Maybe (fromMaybe)

{-
1. How this code works: The original Haskell Code will perform runWhen and then place that value into a tuple, where the first value is a key for the optimization performed.
2. This list of tuples will be passed into this file, which will then select a Dictionary<String, Int> based on a specific value.
3. Once the Dictionary selection is made, we will create a list of size n. The value for our Dictionary acts as the index for the order we want the optimization, represented by the key to go.
4. We then return this list back, and the Pipeline will continue as normal.
-}


createDictionary :: Map.Map String Int
createDictionary = Map.fromList []

insertIntoDictionary :: Map.Map String Int -> String -> Int -> Map.Map String Int
insertIntoDictionary dictionary key value = Map.insert key value dictionary

listToNewList :: [(String, CoreToDo)] -> Map.Map String Int -> [(Int, CoreToDo)]
listToNewList inputList dictionary = map getValue inputList
  where
    getValue :: (String, CoreToDo) -> (Int, CoreToDo)
    getValue (key, opt) = (fromMaybe 0 (Map.lookup key dictionary), opt)

orderTuples :: [(Int, CoreToDo)] -> [(Int, CoreToDo)]
orderTuples tuples = sortBy (comparing fst') tuples
  where
    fst' :: (Int, CoreToDo) -> Int
    fst' (x, _) = x

flattenTuples :: [(Int, CoreToDo)] -> [CoreToDo]
flattenTuples tuples = map flatten tuples
  where
    flatten :: (Int, CoreToDo) -> CoreToDo
    flatten (_, x) = x

getPhaseOrder :: [(String, CoreToDo)] -> [CoreToDo]
getPhaseOrder coreToDoList = do

    let my_var = 0 -- 0 is the default order

    let my_Dictionary = case my_var of
                        0 -> (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary createDictionary "static_args" 0) "presimplify" 1) "specialise" 2) "full_laziness_1" 3) "simpl3" 4) "float_in_1" 5) "call_arity" 6) "strictness" 7) "exitification" 8) "full_laziness_2" 9) "cse" 10) "float_in_2" 11) "final" 12) "rule_check1" 13) "liberate_case" 14) "spec_constr" 15) "rule_check2" 16) "late_specialise" 17) "triple_combo" 18) "late_dmd_anal" 19) "strict_anal" 20) "rule_check3" 21) "add_caller" 22) "add_late" 23)
                        1 -> (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary createDictionary "static_args" 0) "presimplify" 1) "specialise" 2) "full_laziness_1" 8) "simpl3" 16) "float_in_1" 3) "call_arity" 9) "strictness" 18) "exitification" 20) "full_laziness_2" 12) "cse" 11) "float_in_2" 4) "final" 5) "rule_check1" 6) "liberate_case" 10) "spec_constr" 14) "rule_check2" 13) "late_specialise" 19) "triple_combo" 15) "late_dmd_anal" 7) "strict_anal" 17) "rule_check3" 22) "add_caller" 23) "add_late" 21)
                        2 -> (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary createDictionary "static_args" 0) "presimplify" 16) "specialise" 2) "full_laziness_1" 11) "simpl3" 6) "float_in_1" 13) "call_arity" 4) "strictness" 14) "exitification" 15) "full_laziness_2" 3) "cse" 21) "float_in_2" 1) "final" 20) "rule_check1" 8) "liberate_case" 7) "spec_constr" 12) "rule_check2" 9) "late_specialise" 10) "triple_combo" 19) "late_dmd_anal" 5) "strict_anal" 18) "rule_check3" 17) "add_caller" 22) "add_late" 23)
                        3 -> (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary createDictionary "static_args" 0) "presimplify" 5) "specialise" 17) "full_laziness_1" 2) "simpl3" 6) "float_in_1" 1) "call_arity" 16) "strictness" 4) "exitification" 15) "full_laziness_2" 8) "cse" 20) "float_in_2" 11) "final" 14) "rule_check1" 9) "liberate_case" 10) "spec_constr" 18) "rule_check2" 3) "late_specialise" 13) "triple_combo" 12) "late_dmd_anal" 21) "strict_anal" 7) "rule_check3" 22) "add_caller" 23) "add_late" 19)
                        4 -> (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary (insertIntoDictionary createDictionary "static_args" 18) "presimplify" 9) "specialise" 11) "full_laziness_1" 21) "simpl3" 8) "float_in_1" 4) "call_arity" 3) "strictness" 12) "exitification" 19) "full_laziness_2" 5) "cse" 15) "float_in_2" 7) "final" 10) "rule_check1" 14) "liberate_case" 6) "spec_constr" 1) "rule_check2" 2) "late_specialise" 20) "triple_combo" 16) "late_dmd_anal" 13) "strict_anal" 17) "rule_check3" 22) "add_caller" 23) "add_late" 0)


    flattenTuples (orderTuples (listToNewList coreToDoList my_Dictionary))

    -- let list_of_strings = [("OptA", "FunnyMan"), ("OptB", "FunnyWoman")]
    -- let new_list = listToNewList list_of_strings my_Dictionary
    -- let final_list = orderTuples new_list
    -- final_list
    -- print new_list
    -- print final_list
    -- let final_list = []
    -- final_list


