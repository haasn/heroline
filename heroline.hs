{-# LANGUAGE RecordWildCards, BangPatterns, DeriveFunctor, ViewPatterns #-}

import Control.Applicative (liftA2)
import Control.Monad (replicateM, guard)

import Data.Ord (comparing)
import Data.List
import Data.Monoid
import Data.Foldable (foldMap, forM_)
import Data.Traversable (forM)
import Data.Maybe

import System.Random

-- | Type definitions

type Stats = StatsG Double
data StatsG n = Stats
    { wdmg   :: !n -- ^ Weapon damage
    , wepMul :: !n -- ^ Weapon damage increase (%)
    , dmgMul :: !n -- ^ Damage multiplier (%)
    , wspeed :: !n -- ^ Weapon speed increase (%)
    , wrange :: !n -- ^ Weapon bonus range (%)
    , cleave :: !n -- ^ Cleave (%)
    , cdr    :: !n -- ^ Ability cooldown speed (%)
    , cc     :: !n -- ^ Critical hit chance (%)
    , chd    :: !n -- ^ Critiacl hit damage (%)
    , life   :: !n -- ^ Bonus life
    , lifep  :: !n -- ^ Bonus life (%)
    , vamp   :: !n -- ^ Lifesteal (%)
    , bodies :: !n -- ^ Bodies provided (essentially tankiness)
    , multi  :: !n -- ^ Multistrike (additional hits per attack)
    , evade  :: !n -- ^ Chance to evade (%)
    , reduce :: !n -- ^ Damage reduction (%)
    } deriving (Show, Eq, Functor)

zstats :: Num a => StatsG a
zstats = pure 0

data Item = Item
    { itemName   :: !String      -- ^ Item name
    , itemStats  :: !Stats       -- ^ Item base stats
    , itemAuras  :: ![Aura]      -- ^ Item provided auras
    , itemFilter :: Hero -> Bool -- ^ Item relevance filter (performance)
    }

data Aura = Aura
    { auraName  :: !String   -- ^ Aura name
    , auraType  :: !AuraType -- ^ What the aura affects
    , auraStack :: !Bool     -- ^ Whether or not an aura stacks
    , auraStats :: !Stats    -- ^ Aura stats
    }

data AuraType = Self | Camp | Party
    deriving (Show, Eq)

data Hero = Hero
    { heroName  :: !String        -- ^ Hero name
    , heroStats :: !Stats         -- ^ Hero base stats (max level)
    , heroBonus :: Stats -> Stats -- ^ Bonus stats (conditional)
    , heroSpeed :: !Double        -- ^ Hero's base weapon speed
    , heroReq   :: Stats -> Bool  -- ^ Condition a hero requires to function
    , heroAuras :: ![Aura]        -- ^ Auras provided by the hero
    , heroCamp  :: !HeroCamp      -- ^ What camp a hero is standing in
    , heroItems :: ![Item]        -- ^ Hero base (required!) items
    , heroGlyph :: !Glyph         -- ^ Chosen hero glyph
    }

data HeroCamp = Melee | Range
    deriving (Show, Eq)

data Glyph = Glyph
    { glyphName  :: !String -- ^ Glyph name
    , glyphStats :: !Stats  -- ^ Glyph passive stats
    }

-- Some useful instances

instance Applicative StatsG where
    pure x = Stats x x x x x x x x x x x x x x x x
    Stats f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 <*>
        Stats x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
      = Stats (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7)
              (f8 x8) (f9 x9) (f10 x10) (f11 x11) (f12 x12) (f13 x13)
              (f14 x14) (f15 x15) (f16 x16)

instance Num a => Num (StatsG a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

instance Eq Aura where
    Aura{ auraStack = True } == _ = False
    _ == Aura{ auraStack = True } = False
    Aura na ta _ sa == Aura nb tb _ sb = (na,ta,sa) == (nb,tb,sb)

-- Local orphan to allow composing requirement functions
instance Monoid Bool where
    mempty  = False
    mappend = (&&)

-- Sum-like instance to allow foldMap
instance Num a => Monoid (StatsG a) where
    mempty  = 0
    mappend = (+)

-- | Helper functions

-- Turn a hero's base stats (dmg, HP) at level 1 into stats at max level
baseStats :: Double -> Double -> Stats
baseStats dmg hp = zstats { wdmg   = dmg + 2*perLevel + 3*bonusTrait
                          , wspeed = 5*perLevel + 6*bonusTrait
                          , life   = hp + 50*perLevel + 150*bonusTrait
                          }
    where perLevel   = 60 - 1
          bonusTrait = 30

-- An aura that just provides a number of bodies on the board
bodyAura :: String -> Double -> Aura
bodyAura n c = Aura { auraName  = n ++ "'s generic bodies"
                    , auraType  = Party
                    , auraStack = True
                    , auraStats = zstats { bodies = c }
                    }

-- Requirements that range chars need to survive (essentially just
-- the existence of something in the melee camp)
rangeReq :: Stats -> Bool
rangeReq s = bodies s > 0

-- Hero list itself

uptime :: Double -> Double -> Stats -> Double
uptime dur cd Stats{..} = min 1 $ dur / downtime
    where downtime = cd / (1 + cdr/100)

warden = Hero{..}
    where heroName  = "Warden"
          heroStats = baseStats 32 0 + zstats { cleave = 150, evade = 99.9 }
          heroBonus = const zstats
          heroSpeed = 1.2
          heroReq s = uptime 10 20 s >= 0.99 && bodies s > 0 -- permanent invisibility
          heroAuras = []
          heroCamp  = Melee
          heroItems = [ glaive, kang, alexandrite ]
          heroGlyph = frost

blademaster = Hero{..}
    where heroName  = "Blademaster"
          heroStats = baseStats 28 700 + zstats { cc = 25, chd = 1100, multi = 1.2 }
          heroBonus = const zstats
          heroSpeed = 1.2
          heroReq _ = True
          heroAuras = [ bodyAura heroName 4 ]
          heroCamp  = Melee
          heroItems = [ sunshatter, kang ]
          heroGlyph = holy

illidan = Hero{..}
    where heroName  = "Illidan"
          heroStats = baseStats 35 700 + zstats { cleave = 50, wepMul = 50, evade = 50 }
          heroBonus = \s -> zstats { wspeed = 475 * uptime 15 30 s }
          heroSpeed = 1.2
          heroReq _ = True
          heroAuras = [ bodyAura heroName 1 ]
          heroCamp  = Melee
          heroItems = [ sunshatter ]
          heroGlyph = shadow

nova = Hero{..}
    where heroName  = "Nova"
          heroStats = baseStats 31 500 + zstats { cc = 5, chd = 2500 }
          heroBonus = const zstats
          heroSpeed = 1.5
          heroReq _ = True
          heroAuras = []
          heroCamp  = Range
          heroItems = replicate 3 kang
          heroGlyph = fire

sylvanas = Hero{..}
    where heroName  = "Sylvanas"
          heroStats = baseStats 28 550
          heroBonus = \s -> let m = 5/45 in zstats { multi = m*2, wspeed = m*50 }
          heroSpeed = 1.4
          heroReq _ = True
          heroAuras = [ Aura heroName Party False zstats { wspeed = 55 } ]
          heroCamp  = Range
          heroItems = replicate 3 kang
          heroGlyph = frost

malfurion = Hero{..}
    where heroName  = "Malfurion"
          heroStats = baseStats 23 605 + zstats { multi = 10 }
          heroBonus = const zstats
          heroSpeed = 1.7
          heroReq   = rangeReq <> \s -> wrange s >= 40
          heroAuras = [ Aura heroName Party False zstats { wspeed = 50 } ]
          heroCamp  = Range
          -- Hard coded for efficiency
          heroItems = [ serendipity ] ++ replicate 4 kang ++ [ alexandrite ]
          heroGlyph = fire

arthas = Hero{..}
    where heroName  = "Arthas"
          heroStats = baseStats 30 750 + zstats
          heroBonus = const zstats
          heroSpeed = 1.3
          heroReq _ = True
          heroAuras = [ Aura heroName Party False zstats { wspeed = 50, cdr = 50 }
                      , bodyAura heroName 16 ]
          heroCamp  = Melee
          heroItems = []
          heroGlyph = holy

heroes :: [Hero]
heroes = [ -- mandatory
           malfurion --, illidan
           -- good
         , blademaster --, arthas
           -- deprecated
         -- , nova, warden, sylvanas
         ]

-- | Bonus glyph list

fire = Glyph{..}
    where glyphName  = "Fire Glyph"
          glyphStats = zstats { dmgMul = 5, wrange = 20 }

frost = Glyph{..}
    where glyphName  = "Frost Glyph"
          glyphStats = zstats { dmgMul = 5, cdr = 10 }

holy = Glyph{..}
    where glyphName  = "Holy Glyph"
          glyphStats = zstats { cdr = 10, lifep = 10 }

shadow = Glyph{..}
    where glyphName  = "Shadow Glyph"
          glyphStats = zstats { vamp = 15 }

glyphs :: [Glyph]
glyphs = [fire, frost, holy, shadow]

-- | Bonus aura list (useful for currently-unimplemented heroes etc.)

cdrAura :: Double -> Aura
cdrAura d = Aura
    { auraName  = "Generic (CDR)"
    , auraType  = Party
    , auraStack = True
    , auraStats = zstats { cdr = d }
    }

-- | Item list

kang = Item{..}
    where itemName  = "Kang"
          itemStats = zstats { wdmg = 900, cleave = 30 }
          itemAuras = [] -- This aura is useless
          itemFilter = const True

glaive = Item{..}
    where itemName  = "Firestrike Glaive"
          itemStats = zstats { wdmg = 700, cdr = 30 }
          -- 50% at 5/10 second uptime averages out to 25%
          itemAuras = [ Aura itemName Self False zstats { wepMul = 25 } ]
          itemFilter = const True

serendipity = Item{..}
    where itemName  = "Serendipity"
          itemStats = zstats { wdmg = 1000, wspeed = 100, wrange = 25 }
          itemAuras = [ Aura itemName Self False zstats { wspeed = 25 } ]
          itemFilter Hero{..} = heroCamp == Range

alexandrite = Item{..}
    where itemName  = "Alexandrite Coil"
          itemStats = zstats { wdmg = 750, wspeed = 200 }
          itemAuras = [] -- This aura is essentially useless for AoE
          itemFilter = const True

sunshatter = Item{..}
    where itemName  = "Sunshatter"
          itemStats = zstats { wdmg = 650, vamp = 5 }
          itemAuras = [] -- XXX Counter-attack (worth it?)
          itemFilter Hero{..} = heroCamp == Melee

bloodstone = Item{..}
    where itemName  = "Bloodstone Signet"
          itemStats = zstats { wdmg = 750, life = 25000 }
          itemAuras = [ Aura itemName Self False zstats { reduce = 50 } ]
          itemFilter Hero{..} = heroCamp == Melee

rhinestone = Item{..}
    where itemName  = "Rhinestone Seal"
          itemStats = zstats { wdmg = 450, wspeed = 100, life = 15000 }
          itemAuras = [] -- Essentially useless
          itemFilter Hero{..} = heroCamp == Melee

hellscream's = Item{..}
    where itemName  = "Hellscream's Shield Wall"
          itemStats = zstats { wdmg = 500, life = 40000 }
          itemAuras = [] -- Essentially useless
          itemFilter Hero{..} = heroCamp == Melee

blackfang = Item{..}
    where itemName  = "Blackfang Weave"
          itemStats = zstats { wspeed = 100, life = 40000 }
          --- TODO check aura stacking
          itemAuras = [ Aura itemName Party False zstats { dmgMul = 15 } ]
          itemFilter Hero{..} = heroCamp == Melee

immovable = Item{..}
    where itemName  = "The Immovable Object"
          itemStats = zstats { cdr = 30, life = 40000 }
          itemAuras = [] -- Essentially useless
          itemFilter Hero{..} = heroCamp == Melee

absolution = Item{..}
    where itemName  = "Shroud of Absolution"
          itemStats = zstats { life = 35000 }
          itemAuras = [ Aura itemName Camp False zstats { cdr = 15 } ]
          itemFilter Hero{..} = heroCamp == Melee

items :: [Item]

items = [kang, alexandrite, glaive, serendipity, sunshatter, blackfang,
         bloodstone, rhinestone, hellscream's, immovable, absolution]

-- | DPS calculation

data Config = Config
    { cleaveDensity    :: Double -- ^ Number of extra hit on average
    , ignoreConditions :: Bool   -- ^ Ignore conditions (DPS hypothetical limit)
    , bonusAuras       :: [Aura] -- ^ Extra auras to assume the party has
    }

defConf :: Config
defConf = Config { cleaveDensity = 0, ignoreConditions = False, bonusAuras = [] }

mixConf, aoeConf, sillyConf :: Config
mixConf = defConf { cleaveDensity = 1.5 }
aoeConf = defConf { cleaveDensity = 5.0 }
sillyConf = aoeConf { ignoreConditions = True }

-- DPS calculation

type DPS = Double

dps :: Config -> Stats -> Hero -> DPS
dps Config{..} Stats{..} Hero{..} = combinedDamage * (1 + dmgMul/100)
    where combinedDamage = weaponDamage + abilityDamage

          weaponDamage   = (1 + multi) * swingDamage / swingSpeed
          swingSpeed     = heroSpeed / (1 + wspeed/100)
          swingDamage    = (1 + cleaveDensity * cleave/100) * singleDamage
          singleDamage   = wdmg * (1 + wepMul/100) * critMul
          critMul        = 1 + cc/100 * (chd/100 - 1)

          -- TODO: implement ability damage
          abilityDamage  = 0

-- | Toughness estimation

type Toughness = Double

-- Effective health
toughness :: Stats -> Hero -> Toughness
toughness s@Stats{..} h = hptotal / dmgCoeff s h
    where hptotal = life * (1 + lifep/100)

-- Damage multiplier (factors into toughness)
dmgCoeff :: Stats -> Hero -> Double
dmgCoeff Stats{..} Hero{..} = soft * hard * mod
    where soft = 1 - evade/100
          hard = 1 - reduce/100
          mod  = case heroCamp of Melee -> 2.0; _ -> 1.0

-- Enemy DPS approximation
mobDamage :: Config -> Stats -> DPS
mobDamage Config{..} Stats{..} = mobCount * mobDPS / targetCount
    where mobDPS      = 8000 -- death revenant w/ buffs
          mobCount    = 1 + 3 * cleaveDensity -- decent estimation
          targetCount = bodies / 2 -- assume a worst case scenario

-- Healing estimation
healing :: Stats -> DPS -> Double
healing Stats{..} dps = dps * vamp/100

-- Survivability rating
tankiness :: Config -> Stats -> Hero -> DPS -> Double
tankiness c s h d = healing s d - mobDamage c s * dmgCoeff s h

-- Survivability verdict
canSurvive :: Config -> Stats -> Hero -> DPS -> Bool
canSurvive _ _ Hero { heroCamp = Range } _ = True
canSurvive c s h d = tankiness c s h d > 1000 && toughness s h >= mobDamage c s

-- | Group simulation

type Player = (Hero, [Item]) -- ^ Hero and the items they're carrying
type Party  = [Player]

-- All auras exported by a player (including self-auras only)
playerAuras :: Player -> [Aura]
playerAuras (h,is) = nub $ heroAuras h ++ concatMap itemAuras is

-- A player's own static stats (excluding auras and bonus)
playerStats :: Player -> Stats
playerStats (h,is) = heroStats h + foldMap itemStats is + glyphStats (heroGlyph h)

-- Party does not include the hero itself
allAuras :: Config -> Party -> Player -> [Aura]
allAuras c ps p@(h,_) = nub $ partyAuras ++ playerAuras p ++ bonusAuras c
    where partyAuras = [ a | p' <- ps, a <- playerAuras p', auraApplies p' a ]
          auraApplies (h',_) a = case auraType a of
                Self  -> False
                Party -> True
                Camp  -> heroCamp h' == heroCamp h || ignoreConditions c

partyStats :: Config -> Party -> [Stats]
partyStats c = go []
    where go _ []       = []
          go ps' (p:ps) = let auras = allAuras c (ps++ps') p
                              base  = playerStats p + foldMap auraStats auras
                              stats = base + heroBonus (fst p) base
                          in stats : go (p:ps') ps

-- | Brute force search space

type Choice m a = Int -> [a] -> m [a]

det :: Choice [] a
det 0 _      = [[]]
det _ []     = []
det n (x:xs) = do
    c <- [0..n]
    r <- det (n-c) xs
    return $ replicate c x ++ r

rnd :: Choice IO a
rnd n xs = replicateM n $ do
    i <- randomRIO (0,l-1)
    return $ xs !! i
    where l = length xs

-- length is always 6
inventory :: Functor m => Choice m Item -> Hero -> m [Item]
inventory choice h = fmap (base ++) $ choice (6 - length base) rest
    where base = heroItems h
          rest = filter (\Item{..} -> itemFilter h) items

partyChoice :: Monad m => Choice m Hero -> Choice m Item -> Int -> m Party
partyChoice ch chi n = do
    hs <- ch n heroes
    forM hs $ \h -> do
        is <- inventory chi h
        return (h,is)

type Result = ([DPS], Party, [Stats])

attempt :: Config -> Party -> Maybe Result
attempt c party = do
    -- Compute its stats/auras
    let !stats = partyStats c party
        !setup = zip (map fst party) stats
    -- Ensure the party is actually legal
    guard $ ignoreConditions c || and [ heroReq h s | (h,s) <- setup ]
    -- Compute the DPS values and ensure the party can survive
    let !ds = [ dps c s h | (h,s) <- setup ]
    guard $ ignoreConditions c || and [ canSurvive c s h d
                                      | (d, (h,s)) <- zip ds setup ]

    return (ds, party, stats)

bruteForce :: Config -> Int -> [Result]
bruteForce c = getRecords . catMaybes . map (attempt c) . partyChoice det det
    where getRecords [r] = [r]
          getRecords (a@(da,_,_) : b@(db,_,_) : rs)
            | db > da   = a : getRecords (b:rs)
            | otherwise = getRecords (a:rs)

guessLoop :: Config -> Int -> IO ()
guessLoop c n = go 0
    where go d = do
            party <- partyChoice rnd rnd n
            case attempt c party of
                Just r@(sum -> d',_,_) | d' > d -> printResult c r >> go d'
                _ -> go d

-- | Pretty printing and main program

showStats :: Stats -> String
showStats s = intercalate ", " [ show a ++ " " ++ n | (f, n) <- selectors
                               , let a = f s , a > 0 ]
    where selectors = [ (wdmg, "WD"), (wepMul, "WD%"), (dmgMul, "DMG%")
                      , (wspeed, "WS%"), (cleave, "CLV%"), (cdr, "AbS%")
                      , (cc, "CC%"), (chd, "CHD%"), (life, "HP")
                      , (vamp, "LfS"), (bodies, "BOD"), (multi, "MS")
                      , (wrange, "WRn"), (lifep, "HP%"), (evade, "Ddg%")
                      , (reduce, "Red%")
                      ]

printPlayer :: Player -> DPS -> IO ()
printPlayer p@(Hero{..}, is) dps = do
    putStrLn $ "--- " ++ heroName ++ " ---"
    putStrLn $ "Hero DPS:\t" ++ show dps
    putStrLn $ glyphName heroGlyph ++ ": " ++ showStats (glyphStats heroGlyph)
    forM_ is $ \Item{..} -> do
        putStrLn $ itemName ++ ": " ++ showStats itemStats
    putStrLn ""
    putStrLn "Auras provided:"
    forM_ (playerAuras p) $ \Aura{..} -> do
        putStrLn $ auraName ++ " (" ++ show auraType ++ "): " ++ showStats auraStats

printResult :: Config -> Result -> IO ()
printResult c (dps,party,stats) = do
    putStrLn "===== New best: ====="
    putStrLn $ "Total DPS:\t" ++ show (sum dps)
    putStrLn $ "Avg DPS per:\t" ++ show (sum dps / (1 + cleaveDensity c))
    forM_ (zip3 party stats dps) $ \(p,s,d) -> do
        printPlayer p d
        putStrLn ""
        putStrLn "Summary of stats:"
        putStrLn $ showStats s
    putStrLn "----------------"
    putStrLn "Bonus auras:"
    forM_ (bonusAuras c) $ \Aura{..} -> do
        putStrLn $ auraName ++ ": " ++ showStats auraStats

main :: IO ()
main = bruteForce conf 2 `forM_` printResult conf
--main = guessLoop conf 4
    where conf = aoeConf
