----------------------------------------------------------------------------
-- |
-- Module      :  Options.Applicative.Fxtra
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Additional combinators for handling options.
----------------------------------------------------------------------------
module Options.Applicative.Fxtra (
    withTransformations
  , lookupReader
  , module Options.Applicative
  ) where

import Data.Foldable
import Data.List
import Data.Monoid
import Options.Applicative
import Options.Applicative.Types (readerAsk)

-- | Helper to apply option parser transformations.
--
-- > import Options.Applicative
-- >
-- > data Sample = Sample
-- >   { hello :: String
-- >   , quiet :: Bool
-- >   }
-- >
-- > sample :: Parser Sample
-- > sample = Sample <$> strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting")
-- >                 <*> pure False
-- >
-- > sample' :: Parser Sample
-- > sample' = sample `withTransformations`
-- >   [ (\s -> s { quiet = True},  long "quiet" <> help "Be quiet")
-- >   , (\s -> s { quiet = False}, long "verbose" <> help "Keep")
-- >   ]
--
-- Or if you are like lens:
--
-- > import Control.Lens
-- > import Options.Applicative
-- >
-- > data Sample = Sample
-- >   { _hello :: String
-- >   , _quiet :: Bool
-- >   }
-- >
-- > makeLenses ''Sample
-- >
-- > sample :: Parser Sample
-- > sample = Sample <$> strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting")
-- >                 <*> pure False
-- >
-- > sample' :: Parser Sample
-- > sample' = sample `withTransformations`
-- >   [ set quiet True,  long "quiet" <> help "Be quiet")
-- >   , set quiet False, long "verbose" <> help "Keep")
-- >   ]
withTransformations :: Parser a -> [(a -> a, Mod FlagFields (a -> a))] -> Parser a
withTransformations p [] = p
withTransformations p ts = t <*> p
  where flag'' (v, m) = flag' v $ hidden <> m
        -- We need to Dual Endo, because we want first transformation applied first
        -- default Endo mappend applies first argument last:
        -- Endo f <> Endo g = Endo (f . g) -- but we want 'g . f'
        t = appEndo . getDual . foldMap (Dual . Endo) <$> (many . asum1 . map flag'' $ ts)

asum1 :: Alternative f => [f a] -> f a
asum1 = Data.Foldable.foldr1 (<|>)

-- | Enumerated option parser.
--
-- > data Color = Red | Green | Blue
-- >
-- > color :: Parser Color
-- > color = option (lookupReader t) (long "color" <> metavar "COLOR")
-- >   where t = [ ("red",   Red),
-- >             , ("green", Green)
-- >             , ("blue",  Blue)
-- >             ]
lookupReader :: [(String, a)] -> ReadM a
lookupReader opts = readerAsk >>= lookup'
  where lookup' s = maybe (readerError err) return $ lookup s opts
          where err = "Unknown option: " ++ s  ++ ", available: " ++ intercalate ", " (map fst opts)
