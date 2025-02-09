module Main where

import TestesTarefa1
import TestesTarefa2
import TestesTarefa3
import TestesTarefa4
import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ test [testesTarefa1, testesTarefa2, testesTarefa3, testesTarefa4]
