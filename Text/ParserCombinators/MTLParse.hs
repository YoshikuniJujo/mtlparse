--
-- MTLParse.hs
--
-- Author: Yoshikuni Jujo <PAF01143@nifty.ne.jp>
--
-- This file is part of mtlparse library
--
-- mtlparse is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or any later version.
--
-- mtlparse is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANGY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this program. If not, see
-- <http://www.gnu.org/licenses/>.

module Text.ParserCombinators.MTLParse (

  module Text.ParserCombinators.MTLParse.MTLParseCore

, tokens
, tokensBack
, build

, repeatParse
, optional
, list
, neList

, greedyRepeatParse
, greedyOptional
, greedyList
, greedyNeList

, beginningOfInput
, endOfInput

, apply2M
, (>++>)
, (>:>)

) where

import Text.ParserCombinators.MTLParse.MTLParseCore
import Control.Monad( replicateM )

tokens, tokensBack :: ( Eq a, MonadParse a m ) => [ a ] -> m [ a ]
tokens     = foldr ( (>:>).token     ) $ return []
tokensBack = foldr ( (>:>).tokenBack ) $ return []

build :: Monad m => m a -> ( a -> b ) -> m b
build = flip liftM

repeatParse, greedyRepeatParse ::
  MonadPlus m => Int -> Maybe Int -> m b -> m [ b ]

repeatParse mn ( Just mx ) p
  | mn == mx  = replicateM mn p
  | mn <  mx  = replicateM mn p
                `mplus`
                ( p >:> repeatParse mn (Just $ mx - 1) p )
  | otherwise = error "minimal larger than maximal"
repeatParse mn Nothing p
  = replicateM mn p
    `mplus`
    ( p >:> repeatParse mn Nothing p )

greedyRepeatParse mn ( Just mx ) p
  | mn == mx  = replicateM mn p
  | mn <  mx  = ( p >:> greedyRepeatParse mn (Just $ mx - 1) p )
                `mplus`
		replicateM mn p
  | otherwise = error "minimal larger than maximal"
greedyRepeatParse mn Nothing p
  = ( p >:> greedyRepeatParse mn Nothing p )
    `mplus`
    replicateM mn p

optional, greedyOptional, list, greedyList, neList, greedyNeList
  :: MonadPlus m => m a -> m [ a ]
optional       = repeatParse       0 $ Just 1
greedyOptional = greedyRepeatParse 0 $ Just 1
list           = repeatParse       0 Nothing
greedyList     = greedyRepeatParse 0 Nothing
neList         = repeatParse       1 Nothing
greedyNeList   = greedyRepeatParse 1 Nothing

-- beginning and end of input

beginningOfInput, endOfInput
  :: ( MonadPlus m, MonadParse a m ) => b -> m b
beginningOfInput x = do ( pre, _ ) <- getHere
                        if null pre then return x
			            else mzero
endOfInput       x = do ( _, post ) <- getHere
                        if null post then return x
			             else mzero

-- some tools for monad returning list

apply2M :: Monad m => ( a -> b -> c ) -> m a -> m b -> m c
apply2M op m1 m2 = do { r1 <- m1; r2 <- m2; return $ r1 `op` r2 }

(>++>) :: Monad m => m [a] -> m [a] -> m [a]
(>++>) = apply2M (++)

(>:>)  :: Monad m => m a -> m [a] -> m [a]
(>:>) = apply2M (:)
