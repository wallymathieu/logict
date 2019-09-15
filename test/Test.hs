{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State.Strict

monadReader1 :: Assertion
monadReader1 = assertEqual "should be equal" [5 :: Int] $
  runReader (observeAllT (local (+ 5) ask)) 0

monadReader2 :: Assertion
monadReader2 = assertEqual "should be equal" [(5, 0)] $
  runReader (observeAllT foo) 0
  where
    foo :: MonadReader Int m => m (Int,Int)
    foo = do
      x <- local (5+) ask
      y <- ask
      return (x,y)

test_return :: Assertion
test_return =
  let c = mplus (return 1) (return 2) in
  let l = observeAll c in
  assertEqual "should be equal" [1,2] l

{--
test_delay :: Assertion
test_delay =
  let r = ref 0 in
  let rec generate () =
    incr r
    return !r
  in
  let c = C.delay generate in
  incr r;
  let l = List.sort (observeMany 3 c) in
  assertEqual "should be equal" [2] l
--}

test_from_fun :: Assertion
test_from_fun =
  let l = runState (observeManyT 5 inc) 0 in
  assertEqual "should be equal" ([1,2,3,4,5],5) l
  where
    inc :: MonadState Int m => m Int
    inc = state (\c -> let nc=c+1 in (nc, nc))

{--
test_bind :: Assertion
test_bind =
  let c1 = C.of_list [1,3] in
  let c2 = c1 >>= fun x -> C.of_list [x; x+1] in
  let l = List.sort (observeAll c2) in
  OUnit.assert_equal [1,2,3,4] l

test_interleave :: Assertion
test_interleave =
  let c1 = C.of_list [1;3;5] in
  let c2 = C.of_list [2;4;6] in
  let l = ref [] in
  C.iter (C.interleave c1 c2) (fun x -> l := !l @ [x]; true);
  OUnit.assert_equal [1;2;3;4;5;6] !l

test_ite1 :: Assertion
test_ite1 =
  let c = C.of_list [1;2] in
  let c' = C.ite c (fun x -> C.Return (x+1)) (C.Return 42) in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [2;3] l

test_ite2 :: Assertion
test_ite2 =
  let c = C.fail in
  let c' = C.ite c (fun x -> C.Return (x+1)) (C.Return 42) in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [42] l

test_map :: Assertion
test_map =
  let c = C.of_list [1;2;3;4;5] in
  let succ a=a+1
  let c' = C.map succ c in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [2;3;4;5;6] l

test_once :: Assertion
test_once =
  let c = C.of_list [1;2;3] in
  let c' = C.once c in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [1] l

test_guard :: Assertion
test_guard =
  let computation =
      C.of_list [1;2;3] >>= fun x ->
      C.guard (x <> 2) >>= fun () ->
      C.Return x
  in
  let l = List.sort (C.run_all computation) in
  OUnit.assert_equal [1;3] l
--}


main :: IO ()
main = defaultMain $ testGroup "All"
    [ testCase "Monad Reader 1" monadReader1
    , testCase "Monad Reader 2" monadReader2
    , testCase "Test return" test_return
    , testCase "From fun" test_from_fun
    ]
