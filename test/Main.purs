module Test.Main where

import Control.Monad.Free (Free)
import Data.Iterable (next)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import LRUCache (calculatedSize, clear, defaultCreateOptions, defaultGetOptions, defaultPeekOptions, delete, entries, get, has, keys, newCache, peek, rentries, rkeys, set, size, values)
import Prelude (Unit, bind, discard, eq, not, show, ($))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite


theSuite :: Free TestF Unit
theSuite = suite "LRU-basic" do

  test "set and get" do
    c1 <- liftEffect (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    mVal <- liftEffect $ get "1" defaultGetOptions c1
    assert "Element added with key should be retrieved by the same key" (eq mVal (Just 1))
    mVal2 <- liftEffect $ get "3" defaultGetOptions c1
    assert "get with key that has not been used with set should return nothing" (eq mVal2 Nothing)
  
  test "size" do
    c1 <- liftEffect (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    s <- liftEffect (size c1)
    assert "Cache should report size 2 after adding two elements." (eq s 2)

  test "calculatedsize" do
    c1 <- liftEffect (newCache defaultCreateOptions {maxSize = 1000, sizeCalculation = \s _ -> length s})
    _ <- liftEffect $ set "1" "aap" Nothing c1
    _ <- liftEffect $ set "2" "noot" Nothing c1
    s <- liftEffect (calculatedSize c1)
    assert "Cache should report size 7 after adding 'aap' and 'noot." (eq s 7)

  test "peek" do
    c1 <- liftEffect (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    mVal <- liftEffect $ peek "1" defaultPeekOptions c1
    assert "Element added with key should be retrieved when peeking with the same key" (eq mVal (Just 1))
    mVal2 <- liftEffect $ peek "2" defaultPeekOptions c1
    assert "peek with key that has not been used with set should return nothing" (eq mVal2 Nothing)

  test "has" do
    c1 <- liftEffect (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    has1 <- liftEffect $ has "1" {updateAgeOnHas: false} c1
    assert "has should return true on element added before" (eq has1 true)
    has2 <- liftEffect $ has "1" {updateAgeOnHas: false} c1
    assert "has should return false on element never added" (eq has2 true)

  test "delete" do
    c1 <- liftEffect (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    deletedP <- liftEffect $ delete "1" c1
    assert "Cache should report 'true' after deleting a recently inserted key into a cache that is not full." deletedP
    deletedP' <- liftEffect $ delete "1" c1
    assert "Cache should report 'false' after deleting a key for the second time." (not deletedP')
    s <- liftEffect (size c1)
    assert "Cache should report size 1 after deleting one of two added elements." (eq s 1)

  test "clear" do
    c1 <- liftEffect (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ clear c1
    s <- liftEffect (size c1)
    assert "Cache should report size 0 after clearing it." (eq s 0)

  test "keys" do
    c1 <- liftEffect (newCache defaultCreateOptions {ttl = 1000, updateAgeOnGet = true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    iterator <- liftEffect $ keys c1
    fst <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `keys` should first return `2`." (eq fst (Just "2"))
    snd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `keys` should first return `2` and then `1`." (eq snd (Just "1"))
    thrd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `keys` should first return `2` and then `1` and then nothing." (eq thrd Nothing)

  test "rkeys" do
    c1 <- liftEffect (newCache defaultCreateOptions {ttl = 1000, updateAgeOnGet = true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    iterator <- liftEffect $ rkeys c1
    fst <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `rkeys` should first return `1`." (eq fst (Just "1"))
    snd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `rkeys` should first return `1` and then `2`." (eq snd (Just "2"))
    thrd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `rkeys` should first return `1` and then `2` and then nothing." (eq thrd Nothing)

  test "values" do
    c1 <- liftEffect (newCache defaultCreateOptions {ttl = 1000, updateAgeOnGet = true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    iterator <- liftEffect $ values c1
    fst <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return 2." (eq fst (Just 2))
    snd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return 2 and then 1." (eq snd (Just 1))
    thrd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return 2 and then 1 and then nothing." (eq thrd Nothing)

  test "rvalues" do
    c1 <- liftEffect (newCache defaultCreateOptions {ttl = 1000, updateAgeOnGet = true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    iterator <- liftEffect $ rkeys c1
    fst <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `rvalues` should first return 1." (eq fst (Just "1"))
    snd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `rvalues` should first return 1 and then 2." (eq snd (Just "2"))
    thrd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `rvalues` should first return 1 and then 2 and then nothing." (eq thrd Nothing)

  test "entries" do
    c1 <- liftEffect (newCache defaultCreateOptions {ttl = 1000, updateAgeOnGet = true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    iterator <- liftEffect $ entries c1
    fst <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return (Tuple '2' 2)." (eq fst (Just (Tuple "2" 2)))
    snd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should then return 2 and then (Tuple '1' 1)." (eq snd (Just (Tuple "1" 1)))
    thrd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should then return nothing." (eq thrd Nothing)

  test "rentries" do
    c1 <- liftEffect (newCache defaultCreateOptions {ttl = 1000, updateAgeOnGet = true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    iterator <- liftEffect $ rentries c1
    fst <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return (Tuple '1' 1)." (eq fst (Just (Tuple "1" 1)))
    snd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` then return (Tuple '2' 2)." (eq snd (Just (Tuple "2" 2)))
    thrd <- liftEffect $ next iterator
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should finally return nothing." (eq thrd Nothing)
