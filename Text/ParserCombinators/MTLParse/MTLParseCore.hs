--
-- MTLParseCore.hs
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

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}

module Text.ParserCombinators.MTLParse.MTLParseCore (

  -- * MonadParse class
  MonadParse( spot, spotBack, still, parseNot, getHere, putHere,
              noBacktrack )

, token
, tokenBack

, getsHere
, modifyHere

, getForward
, getsForward
, putForward
, modifyForward

, getBack
, getsBack
, putBack
, modifyBack

  -- * The Parse Monad
, Parse(..)
, evalParse
, execParse
, mapParse
, withParse

  -- * The ParseT Monad
, ParseT(..)
, evalParseT
, execParseT
, mapParseT
, withParseT

, module Control.Monad
, module Control.Monad.Trans

) where

import Control.Monad        ( MonadPlus, mplus, mzero, liftM     )
import Control.Monad.Trans  ( MonadTrans( lift ),
                              MonadIO, liftIO                    )
import Control.Monad.Reader ( MonadReader( ask, local ),
                              ReaderT( ReaderT, runReaderT ),
                              mapReaderT                         )
import Control.Monad.Writer ( MonadWriter( tell, listen, pass ),
                              WriterT( WriterT, runWriterT ),
                              mapWriterT                         )
import Control.Monad.State  ( MonadState( get, put ),
                              StateT( StateT, runStateT ),
                              mapStateT                          )
import Control.Arrow        ( first, second                      )
import Data.Monoid          ( Monoid( mempty )                   )

class Monad m => MonadParse a m | m -> a where
  spot        :: ( a -> Bool ) -> m a
  spotBack    :: ( a -> Bool ) -> m a
  still       :: m b -> m b
  parseNot    :: c -> m b -> m c
  getHere     :: m ( [a], [a] )
  putHere     :: ( [a], [a] ) -> m ()
  noBacktrack :: m b -> m b

token, tokenBack :: ( Eq a, MonadParse a m ) => a -> m a
token     x = spot     (==x)
tokenBack x = spotBack (==x)

getsHere   :: MonadParse a m => ( ([a], [a]) -> b ) -> m b
modifyHere :: MonadParse a m => ( ([a], [a]) -> ([a], [a]) ) -> m ()
getsHere   f = liftM f getHere
modifyHere f = getHere >>= putHere . f

getBack, getForward   :: MonadParse a m => m [ a ]
getsBack, getsForward :: MonadParse a m => ( [a] -> [a] ) -> m [ a ]
getBack       = getsHere fst
getForward    = getsHere snd
getsBack    f = getsHere ( f.fst )
getsForward f = getsHere ( f.snd )

putBack, putForward       :: MonadParse a m => [ a ] -> m ()
modifyBack, modifyForward :: MonadParse a m => ( [a] -> [a] ) -> m ()
putBack    b  = getsHere snd >>= putHere . (,) b
putForward f  = getsHere fst >>= putHere . flip (,) f
modifyBack    = modifyHere . first
modifyForward = modifyHere . second

-- | A parse monad where /a/ is the type of the token to parse
-- and /b/ is the type of the /return value/.

newtype Parse a b
  = Parse { runParse :: ( [a], [a] ) -> [ ( b, ([a], [a]) ) ] }

-- Parse is instance of Functor Monad MonadPlus MonadReader MonadParse

instance Functor ( Parse p ) where
  fmap f m = Parse $ liftM ( first f ) . runParse m

instance Monad ( Parse a ) where
  return = Parse . \val inp -> [ (val, inp) ]
  Parse pr >>= f
         = Parse ( \st -> concat
	     [ runParse ( f a ) rest | ( a, rest ) <- pr st ] )

instance MonadPlus ( Parse a ) where
  mzero                     = Parse $ const []
  Parse p1 `mplus` Parse p2 = Parse $ \inp -> p1 inp ++ p2 inp

instance MonadReader ( [a], [a] ) ( Parse a ) where
  ask       = Parse $ \inp -> [ (inp, inp) ]
  local f m = Parse $ runParse m . f

instance MonadState ( [a], [a] ) ( Parse a ) where
  get     = Parse $ \inp -> [ (inp, inp) ]
  put inp = Parse $ const [ ((), inp) ]

instance MonadParse a ( Parse a ) where
  spot = Parse . spt
    where
    spt p ( pre, x:xs )
      | p x         = [ ( x, (x:pre, xs) ) ]
      | otherwise   = []
    spt _ ( _, [] ) = []
  spotBack = Parse . sptbck
    where
    sptbck p ( x:xs, post )
      | p x            = [ ( x, (xs, x:post) ) ]
      | otherwise      = []
    sptbck _ ( [], _ ) = []
  still p = Parse $ \inp -> do ( ret, _ ) <- runParse p inp
                               return ( ret, inp )
  parseNot x ( Parse p ) = Parse $ \inp -> case p inp of
                                                [] -> [ (x, inp) ]
					        _  -> []
  getHere = get
  putHere = put
  noBacktrack p = Parse $ (:[]) . head . runParse p

evalParse :: Parse a b -> ( [a], [a] ) -> [ b ]
evalParse m = map fst . runParse m

execParse :: Parse a b -> ( [a], [a] ) -> [ ([a], [a]) ]
execParse m = map snd . runParse m

mapParse :: ( ( b, ([a], [a]) ) -> ( c, ([a], [a]) ) ) -> Parse a b
                                                       -> Parse a c
mapParse f m = Parse $ map f . runParse m

withParse :: ( ([a], [a]) -> ([a], [a]) ) -> Parse a b -> Parse a b
withParse f m = Parse $ runParse m . f

-- | A parse monad for encaplulating an inner monad.

newtype ParseT a m b
  = ParseT { runParseT :: ( [a], [a] ) -> m [ ( b, ([a], [a]) ) ] }

instance Monad m => Functor ( ParseT a m ) where
  fmap f m = ParseT $ \a -> do
               rets <- runParseT m a
	       return [ ( f a', rst ) | ( a', rst ) <- rets ]

instance Monad m => Monad ( ParseT a m ) where
  return b = ParseT $ \a -> return [ (b, a) ]
  ParseT pr >>= f
    = ParseT $ \a ->
        pr a >>=
	  liftM concat . mapM ( \(a', rest) -> runParseT (f a') rest )

instance Monad m => MonadPlus ( ParseT a m ) where
  mzero                       = ParseT $ const $ return []
  ParseT p1 `mplus` ParseT p2 = ParseT $ \inp -> do ret1 <- p1 inp
                                                    ret2 <- p2 inp
						    return $ ret1 ++ ret2

instance Monad m => MonadParse a ( ParseT a m ) where
  spot = ParseT . spt
    where
    spt p ( pre, x:xs )
      | p x         = return [ ( x, (x:pre, xs) ) ]
      | otherwise   = return []
    spt _ ( _, [] ) = return []
  spotBack = ParseT . sptbck
    where
    sptbck p ( x:xs, post )
      | p x            = return [ ( x, (xs, x:post) ) ]
      | otherwise      = return []
    sptbck _ ( [], _ ) = return []
  still p = ParseT $ \inp -> do
    rets <- runParseT p inp
    return [ ( ret, inp ) | ( ret, _ ) <- rets ]
  parseNot x ( ParseT p ) = ParseT $ \inp -> do
    rets <- p inp
    case rets of
      [] -> return [ (x, inp) ]
      _  -> return []
  getHere = get
  putHere = put
  noBacktrack p = ParseT $ \inp -> do ret <- runParseT p inp
                                      return [ head ret ]

instance Monad m => MonadReader ( [a], [a] ) ( ParseT a m ) where
  ask       = ParseT $ \inp -> return [ (inp, inp) ]
  local f m = ParseT $ runParseT m . f

instance Monad m => MonadState ( [a], [a] ) ( ParseT a m ) where
  get     = ParseT $ \inp -> return [ (inp, inp) ]
  put inp = ParseT $ \_   -> return [ ((), inp) ]

instance MonadTrans ( ParseT a ) where
  lift m = ParseT $ \a -> do
             ret <- m
	     return [ (ret, a) ]

instance MonadIO m => MonadIO ( ParseT a m ) where
  liftIO = lift . liftIO

instance MonadWriter w m => MonadWriter w ( ParseT a m ) where
  tell     = lift . tell
  listen m = ParseT $ \inp -> do
    ( al, w ) <- listen ( runParseT m inp )
    return [ ( (ret, w), inp' ) | ( ret, inp' ) <- al ]
  pass m   = ParseT $ \inp -> pass $ do
    al <- runParseT m inp
    return
      ( [ ( ret, inp' ) | ( (ret, _), inp' ) <- al ] ,
        snd . fst $ head al )

evalParseT :: ( Monad m ) => ParseT a m b -> ( [a], [a] ) -> m [ b ]
evalParseT m inp = do
  al <- runParseT m inp
  return $ map fst al

execParseT
  :: ( Monad m ) => ParseT a m b -> ( [a], [a] ) -> m [ ([a], [a]) ]
execParseT m inp = do
  al <- runParseT m inp
  return $ map snd al

mapParseT
  :: ( m [ ( b, ([a], [a]) ) ] -> n [ (c, ( [a], [a]) ) ] )
       -> ParseT a m b -> ParseT a n c
mapParseT f m = ParseT $ f . runParseT m

withParseT :: ( ([a], [a]) -> ([a], [a]) ) -> ParseT a m b
                                           -> ParseT a m b
withParseT f m = ParseT $ runParseT m . f

-- MonadParse instance for other monad transformers

instance ( MonadParse a m ) => MonadParse a ( ReaderT s m ) where
  spot         = lift . spot
  spotBack     = lift . spotBack
  still        = mapReaderT still
  parseNot x p = ReaderT $ \r -> parseNot x ( runReaderT p r )
  getHere      = lift getHere
  putHere      = lift . putHere
  noBacktrack  = mapReaderT noBacktrack

instance ( MonadParse a m, Monoid w ) => MonadParse a ( WriterT w m )
  where
  spot        = lift . spot
  spotBack    = lift . spotBack
  still       = mapWriterT still
  parseNot x  = WriterT . parseNot (x, mempty) . runWriterT
  getHere     = lift getHere
  putHere     = lift . putHere
  noBacktrack = mapWriterT noBacktrack

instance ( MonadParse a m ) => MonadParse a ( StateT r m ) where
  spot         = lift . spot
  spotBack     = lift . spotBack
  still        = mapStateT still
  parseNot x p = StateT $ \s -> parseNot ( x, s ) ( runStateT p s )
  getHere      = lift getHere
  putHere      = lift . putHere
  noBacktrack  = mapStateT noBacktrack
