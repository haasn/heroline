{-# LANGUAGE RecordWildCards, BangPatterns #-}

import Control.Monad (replicateM, guard)
import Data.Ord (comparing)
import Data.List
import Data.Monoid
import Data.Foldable (foldMap, forM_)
import Data.Traversable (forM)

-- | Type definitions

data Stats = Stats
    { wdmg   :: !Double  -- ^ Weapon damage
    , wepMul :: !Double  -- ^ Weapon damage increase (%)
    , dmgMul :: !Double  -- ^ Damage multiplier (%)
    , wspeed :: !Double  -- ^ Weapon speed increase (%)
    , cleave :: !Double  -- ^ Cleave (%)
    , cdr    :: !Double  -- ^ Ability cooldown speed (%)
    , cc     :: !Double  -- ^ Critical hit chance (%)
    , chd    :: !Double  -- ^ Critiacl hit damage (%)
    , life   :: !Double  -- ^ Bonus life
    , vamp   :: !Double  -- ^ Lifesteal (%)
    , bodies :: !Double  -- ^ Bodies provided (essentially tankiness)
    , multi  :: !Double  -- ^ Multistrike (additional hits per attack)
    } deriving (Show, Eq)

stats :: Stats
stats = Stats { wdmg = 0, wepMul = 0, dmgMul = 0, wspeed = 0, cleave = 0,
                cdr = 0, cc = 0, chd = 0, life = 0, vamp = 0, bodies = 0,
                multi = 0 }

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
    , heroType  :: !HeroType      -- ^ What camp a hero is standing in
    }

data HeroType = Melee | Range
    deriving (Show, Eq)

-- Some useful instances

instance Monoid Stats where
    mempty = stats
    mappend a b = Stats
        { wdmg   = wdmg a + wdmg b
        , wepMul = wepMul a + wepMul b
        , dmgMul = dmgMul a + dmgMul b
        , wspeed = wspeed a + wspeed b
        , cleave = cleave a + cleave b
        , cdr    = cdr a + cdr b
        , cc     = cc a + cc b
        , chd    = chd a + chd b
        , life   = life a + life b
        , vamp   = vamp a + vamp b
        , bodies = bodies a + bodies b
        , multi  = multi a + multi b
        }

instance Eq Aura where
    Aura{ auraStack = True } == _ = False
    _ == Aura{ auraStack = True } = False
    Aura na ta _ sa == Aura nb tb _ sb = (na,ta,sa) == (nb,tb,sb)

-- | Helper functions

-- Turn a hero's base stats (dmg, HP) at level 1 into stats at max level
baseStats :: Double -> Double -> Stats
baseStats dmg hp = stats { wdmg   = dmg + 2*perLevel + 3*bonusTrait
                         , wspeed = 5*perLevel + 6*bonusTrait
                         , life   = hp -- XXX figure out the formula
                         }
    where perLevel   = 60 - 1
          bonusTrait = 30

-- An aura that just provides a number of bodies on the board
bodyAura :: String -> Double -> Aura
bodyAura n c = Aura { auraName  = n ++ "'s generic bodies"
                    , auraType  = Party
                    , auraStack = True
                    , auraStats = stats { bodies = c }
                    }

-- Requirement that essentially all melee chars need to survive (tankiness,
-- life steal of some sort)
meleeReq :: Stats -> Bool
meleeReq s = canSolo || canGroup
    where canSolo  = vamp s >= 10 && life s >= 40000
          canGroup = vamp s >  0  && life s >= 20000 && bodies s >= 5

-- Requirements that range chars need to survive (essentially just
-- the existence of something in the melee camp)
rangeReq :: Stats -> Bool
rangeReq s = bodies s > 0

-- Hero list itself

warden :: Hero
warden = Hero{..}
    where heroName  = "Warden"
          heroStats = baseStats 32 0 <> stats { cleave = 150 }
          heroBonus = const stats
          heroSpeed = 1.2
          heroReq   = \s -> cdr s >= 100 -- permanent invisibility
          heroAuras = []
          heroType  = Melee

blademaster :: Hero
blademaster = Hero{..}
    where heroName  = "Blademaster"
          heroStats = baseStats 28 0 <> stats { cc = 25, chd = 1100, multi = 1.2 }
          heroBonus = const stats
          heroSpeed = 1.2
          heroReq   = meleeReq
          heroAuras = [ bodyAura heroName 4 ]
          heroType  = Melee

heroes :: [Hero]
heroes = [blademaster, warden]

-- | Bonus trait list

-- TODO: implement traits

-- | Bonus aura list (useful for currently-unimplemented heroes etc.)

arthas :: Aura
arthas = Aura
    { auraName  = "Arthas's Unholy Frenzy"
    , auraType  = Party
    , auraStack = False
    , auraStats = stats { wspeed = 50, cdr = 50 }
    }

cdrAura :: Double -> Aura
cdrAura d = Aura
    { auraName  = "Generic (CDR)"
    , auraType  = Party
    , auraStack = True
    , auraStats = stats { cdr = d }
    }

-- | Item list

kang :: Item
kang = Item{..}
    where itemName  = "Kang"
          itemStats = stats { wdmg = 900, cleave = 30 }
          itemAuras = [] -- XXX add kang aura
          itemFilter = const True

glaive :: Item
glaive = Item{..}
    where itemName  = "Firestrike Glaive"
          itemStats = stats { wdmg = 700, cdr = 30 }
          -- 50% at 5/10 second uptime averages out to 25%
          itemAuras = [ Aura itemName Self False stats { wepMul = 25 } ]
          itemFilter = const True

serendipity :: Item
serendipity = Item{..}
    where itemName  = "Serendipity"
          itemStats = stats { wdmg = 1000, wspeed = 100 }
          itemAuras = [] -- XXX add serendipity aura
          itemFilter Hero{..} = heroType == Range

alexandrite :: Item
alexandrite = Item{..}
    where itemName  = "Alexandrite Coil"
          itemStats = stats { wdmg = 750, wspeed = 200 }
          itemAuras = [] -- This aura is essentially useless for AoE
          itemFilter = const True

sunshatter :: Item
sunshatter = Item{..}
    where itemName  = "Sunshatter"
          itemStats = stats { wdmg = 650, vamp = 5 }
          itemAuras = [] -- XXX add sunshatter aura
          itemFilter Hero{..} = heroType == Melee

bloodstone :: Item
bloodstone = Item{..}
    where itemName  = "Bloodstone Signet"
          itemStats = stats { wdmg = 750, life = 25000 }
          itemAuras = [] -- XXX add bloodstone aura
          itemFilter Hero{..} = heroType == Melee

rhinestone :: Item
rhinestone = Item{..}
    where itemName  = "Rhinestone Seal"
          itemStats = stats { wdmg = 450, wspeed = 100, life = 15000 }
          itemAuras = [] -- XXX add rhinestone aura
          itemFilter Hero{..} = heroType == Melee

hellscream's :: Item
hellscream's = Item{..}
    where itemName  = "Hellscream's Shield Wall"
          itemStats = stats { wdmg = 500, life = 40000 }
          itemAuras = [] -- XXX add hellscream's aura
          itemFilter Hero{..} = heroType == Melee

blackfang :: Item
blackfang = Item{..}
    where itemName  = "Blackfang Weave"
          itemStats = stats { wspeed = 100, life = 40000 }
          --- TODO check aura stacking
          itemAuras = [ Aura itemName Party False stats { dmgMul = 15 } ]
          itemFilter Hero{..} = heroType == Melee

items :: [Item]

items = [kang, alexandrite, glaive, serendipity, sunshatter, blackfang,
         bloodstone, rhinestone, hellscream's]

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

-- Actual damage estimation

type Player = (Hero, [Item]) -- ^ Hero and the items they're carrying
type Party  = [Player]

dps :: Config -> Stats -> Hero -> Double
dps Config{..} Stats{..} Hero{..} = combinedDamage * (1 + dmgMul/100)
    where combinedDamage = weaponDamage + abilityDamage

          weaponDamage   = (1 + multi) * swingDamage / swingSpeed
          swingSpeed     = heroSpeed / (1 + wspeed/100)
          swingDamage    = (1 + cleaveDensity * cleave/100) * singleDamage
          singleDamage   = wdmg * (1 + wepMul/100) * critMul
          critMul        = 1 + cc/100 * (chd/100 - 1)

          -- TODO: implement ability damage
          abilityDamage  = 0


-- All auras exported by a player (including self-auras only)
playerAuras :: Player -> [Aura]
playerAuras (h,is) = nub $ heroAuras h ++ concatMap itemAuras is

-- A player's own static stats (excluding auras and bonus)
playerStats :: Player -> Stats
playerStats (h,is) = heroStats h <> foldMap itemStats is

-- Party does not include the hero itself
allAuras :: Config -> Party -> Player -> [Aura]
allAuras c ps p@(h,_) = nub $ partyAuras ++ playerAuras p ++ bonusAuras c
    where partyAuras = [ a | p' <- ps, a <- playerAuras p', auraApplies p' a ]
          auraApplies (h',_) a = case auraType a of
                Self  -> False
                Party -> True
                Camp  -> heroType h' == heroType h || ignoreConditions c

partyStats :: Config -> Party -> [Stats]
partyStats c = go []
    where go _ []       = []
          go ps' (p:ps) = let auras = allAuras c (ps++ps') p
                              base  = playerStats p <> foldMap auraStats auras
                              stats = base <> heroBonus (fst p) base
                          in stats : go (p:ps') ps

-- | Brute force search space

-- length is always 6
inventory :: Hero -> [[Item]]
inventory h = choice 6 $ filter (\Item{..} -> itemFilter h) items
    where choice :: Int -> [a] -> [[a]]
          choice 0 _      = [[]]
          choice _ []     = []
          choice n (x:xs) = do
              c <- [0..n]
              r <- choice (n-c) xs
              return $ replicate c x ++ r


partyChoice :: Int -> [Party]
partyChoice n = do
    hs <- replicateM n heroes
    forM hs $ \h -> do
        is <- inventory h
        return (h,is)

optimize' :: Config -> Int -> [(Double, Party, [Stats])]
optimize' c n = getRecords $ do
    -- Pick a party and compute its stats/auras
    party <- partyChoice n
    let !stats = partyStats c party
        !setup = zip (map fst party) stats
    -- Ensure the party is actually legal
    guard $ ignoreConditions c || and [ heroReq h s | (h,s) <- setup ]
    -- Return the total DPS
    let !total = sum [ dps c s h | (h,s) <- setup ]
    return (total, party, stats)
    where getRecords [r] = [r]
          getRecords (a@(da,_,_) : b@(db,_,_) : rs)
            | db > da   = a : getRecords (b:rs)
            | otherwise = getRecords (a:rs)

-- | Pretty printing and main program

showStats :: Stats -> String
showStats s = intercalate ", " [ show a ++ " " ++ n | (f, n) <- selectors
                               , let a = f s , a > 0 ]
    where selectors = [ (wdmg, "WD"), (wepMul, "WD%"), (dmgMul, "DMG%")
                      , (wspeed, "WS%"), (cleave, "CLV%"), (cdr, "AbS%")
                      , (cc, "CC%"), (chd, "CHD%"), (life, "HP")
                      , (vamp, "LfS"), (bodies, "BOD"), (multi, "MS")
                      ]

printPlayer :: Player -> IO ()
printPlayer p@(Hero{..}, is) = do
    putStrLn $ "--- " ++ heroName ++ " ---"
    forM_ is $ \Item{..} -> do
        putStrLn $ itemName ++ ": " ++ showStats itemStats
    putStrLn ""
    putStrLn "Auras provided:"
    forM_ (playerAuras p) $ \Aura{..} -> do
        putStrLn $ auraName ++ " (" ++ show auraType ++ "): " ++ showStats auraStats

optimize :: Config -> Int -> IO ()
optimize c n = forM_ (optimize' c n) $ \(dps,party,stats) -> do
    putStrLn "===== New best: ====="
    putStrLn $ "Total DPS:\t" ++ show dps
    putStrLn $ "Avg DPS per:\t" ++ show (dps / (1 + cleaveDensity c))
    forM_ (zip party stats) $ \(p,s) -> do
        printPlayer p
        putStrLn ""
        putStrLn "Summary of stats:"
        putStrLn $ showStats s
    putStrLn "----------------"
    putStrLn "Bonus auras:"
    forM_ (bonusAuras c) $ \Aura{..} -> do
        putStrLn $ auraName ++ ": " ++ showStats auraStats

main :: IO ()
main = optimize conf 2
    where conf = mixConf {
            bonusAuras = [ arthas, cdrAura 15, cdrAura 10 ]
          }
