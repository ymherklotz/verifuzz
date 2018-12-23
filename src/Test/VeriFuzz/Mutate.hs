{-|
Module      : Test.VeriFuzz.Mutation
Description : Functions to mutate the Verilog AST.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Functions to mutate the Verilog AST from "Test.VeriFuzz.VerilogAST" to generate
more random patterns, such as nesting wires instead of creating new ones.
-}

module Test.VeriFuzz.Mutate where

import           Control.Lens
import           Data.Maybe                    (catMaybes)
import           Test.VeriFuzz.Internal.Shared
import           Test.VeriFuzz.VerilogAST

-- | Return if the 'Identifier' is in a 'ModuleDecl'.
inPort :: Identifier -> ModuleDecl -> Bool
inPort id mod = any (\a -> a ^. portName == id) $ mod ^. modPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModuleItem] -> Maybe Expression
findAssign id items =
  safe last . catMaybes $ isAssign <$> items
  where
    isAssign (Assign ca)
      | ca ^. contAssignNetLVal == id = Just $ ca ^. contAssignExpr
      | otherwise = Nothing

-- | Nest expressions for a specific 'Identifier'. If the 'Identifier' is not found,
-- the AST is not changed.
nestId :: Identifier -> ModuleDecl -> ModuleDecl
nestId id mod
  | not $ inPort id mod = mod
  | otherwise = mod
