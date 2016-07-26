{-# LANGUAGE DeriveFunctor, RecordWildCards, TemplateHaskell,
             FlexibleContexts, DeriveDataTypeable #-}

import Data.Typeable
import Data.Data
import Data.List
import Data.Ord

import Control.Lens
import Control.Monad.State hiding (State)
import Data.Data.Lens (biplate)

data State = S
    { _gold    :: !Double
    , _income  :: !Double
    , _mobPage :: !MobPage
    } deriving (Show, Typeable, Data)

data Mob = M
    { _cost    :: !Double
    , _gain    :: !Double
    , _respawn :: !Double
    , _count   :: !Double
    } deriving (Show, Typeable, Data)

data MobPage = MP
    { _ident    :: !Int
    , _mobList  :: ![Mob]
    , _upCost   :: !Double
    , _upGain   :: !Double
    , _nextPage :: Maybe MobPage
    } deriving (Show, Typeable, Data)

makeLenses ''State
makeLenses ''Mob
makeLenses ''MobPage

initial :: State
initial = S 20 40 mobPages

initialEff :: State
initialEff = initial & biplate %~ sortMobs
    where sortMobs :: [Mob] -> [Mob]
          sortMobs = sortBy $ comparing efficiency

          efficiency m = m^.cost / m^.gain

-- Tick simulation

type Strategy = State -> State

roundDown :: Double -> Double
roundDown = fromIntegral . floor

naive :: Strategy
naive = execState $ do
    ml <- use $ mobPage.mobList
    mobPage.mobList <~ mapM buy ml
    checkUpgrade

  where buy m = do
            g <- use gold
            let avail  = roundDown $ m^.count
                afford = roundDown $ g / m^.cost
                num    = min avail afford
            gold   -= num * m^.cost
            income += num * m^.gain
            return (m & count -~ num)

        checkUpgrade = do
            g  <- use gold
            mp <- use mobPage
            when (g > mp^.upCost) $ case mp^.nextPage of
                Nothing -> return ()
                Just p  -> do mobPage .= p
                              gold   -= mp^.upCost
                              income += mp^.upGain

-- Simulate a mob refill tick
refill :: State -> State
refill = mobPage.mobList.mapped %~ refill
    where refill = execState $ do
            r <- use respawn
            count += tickDur / r
            count %= min 30

          tickDur = 15

-- Simulate a gold generation tick
payout :: State -> State
payout = execState $ do i <- use income; gold += i

-- Advance the state forward by a tick of time, while emplying a given strategy
tick :: Strategy -> State -> State
tick f = payout . f . refill

-- Mob page database

mobPages :: MobPage
mobPages = MP 1 t1 5000   500   . Just
         . MP 2 t2 50000  2500  . Just
         . MP 3 t3 500000 20000 . Just
         $ MP 4 t4 0 0 Nothing

t1, t2, t3, t4 :: [Mob]
t1 = [ M     5      1  3.0  0
     , M    10      2  4.0  0
     , M    25      4  5.0  0
     , M    50      8  8.0  0
     , M    80     12  9.0  0
     , M   125     18 10.0  0
     , M   200     28 10.0  0
     , M   300     38 14.5  0
     , M   450     55 14.5  0
     , M   700     80 14.5  0
     ]

t2 = [ M   1000    90  5.0  0
     , M   1500   130  5.0  0
     , M   2500   210  5.0  0
     , M   3000   240  6.0  0
     , M   4500   350  6.0  0
     , M   6000   440  8.0  0
     , M   8500   600  8.0  0
     , M  10500   700  8.0  0
     , M  13000   850  8.0  0
     , M  16000  1000  8.0  0
     ]

t3 = [ M  19000 12000  8.0  0
     , M  23000 14000 10.0  0
     , M  27000 16000 10.0  0
     , M  32000 18000 10.0  0
     , M  36000 20000 10.0  0
     , M  40000 22000 10.0  0
     , M  44000 24000 20.0  0
     , M  47000 25000 20.0  0
     , M  50000 26000 20.0  0
     , M  52000 27000 20.0  0
     , M  53000 28000 20.0  0
     ]

t4 = [ M  70000 40000  8.0  0
     , M  90000 50000  8.0  0
     , M 120000  7000  8.0  0
     , M 150000  9000  8.0  0
     , M 180000 10000  8.0  0
     , M 220000 12000  8.0  0
     , M 250000 14000  8.0  0
     , M 320000 17000  8.0  0
     , M 400000 21000  8.0  0
     , M 500000 25000  8.0  0
     , M 600000 30000  8.0  0
     ]
