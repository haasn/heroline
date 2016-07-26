{-# LANGUAGE RecordWildCards, BangPatterns, DeriveFunctor, ViewPatterns,
             FlexibleInstances #-}

import Control.Applicative (liftA2)
import Control.Monad
import Control.Concurrent

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
    { wdmg    :: !n -- ^ Weapon damage
    , wepMul  :: !n -- ^ Weapon damage increase (%)
    , dmgMul  :: !n -- ^ Damage multiplier (%)
    , abilPow :: !n -- ^ Ability power (%)
    , abilMul :: !n -- ^ Ability damage multiplier (%)
    , wspeed  :: !n -- ^ Weapon speed increase (%)
    , wrange  :: !n -- ^ Weapon bonus range (%)
    , cleave  :: !n -- ^ Cleave (%)
    , cdr     :: !n -- ^ Ability cooldown speed (%)
    , cc      :: !n -- ^ Critical hit chance (%)
    , chd     :: !n -- ^ Critiacl hit damage (%)
    , life    :: !n -- ^ Bonus life
    , lifep   :: !n -- ^ Bonus life (%)
    , regen   :: !n -- ^ Life regen (/s)
    , regenp  :: !n -- ^ Life regen (%)
    , hpreg   :: !n -- ^ HP (%) per second
    , steal   :: !n -- ^ Lifesteal (%)
    , vamp    :: !n -- ^ Spell vamp (%)
    , bodies  :: !n -- ^ Bodies provided (essentially tankiness)
    , multi   :: !n -- ^ Multistrike (additional hits per attack)
    , reduce  :: !n -- ^ Damage reduction (%), multiplicative
    , debuff  :: !n -- ^ Damage debuff (slow %), multiplicative
    , drainp  :: !n -- ^ Enemy HP drain per second
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

data AuraType = Self | Camp | Party | Debuff | Drain
    deriving (Show, Eq)

data Hero = Hero
    { heroName  :: !String        -- ^ Hero name
    , heroStats :: !Stats         -- ^ Hero base stats (max level)
    , heroBonus :: Stats -> Stats -- ^ Bonus stats (conditional)
    , heroAbils :: ![Ability]     -- ^ Hero's usable abilities
    , heroSpeed :: !Double        -- ^ Hero's base weapon speed
    , heroReq   :: Stats -> Bool  -- ^ Condition a hero requires to function
    , heroAuras :: ![Aura]        -- ^ Auras provided by the hero
    , heroCamp  :: !HeroCamp      -- ^ What camp a hero is standing in
    , heroItems :: ![Item]        -- ^ Hero base (required!) items
    , heroGlyph :: !Glyph         -- ^ Chosen hero glyph
    , heroTags  :: ![HeroTag]     -- ^ Hero's extra tags
    }

data HeroTag = AbilityUser | WeaponUser | NeedsCD | NeedsReg | Tank
    deriving (Show, Eq)

data HeroCamp = Melee | Range
    deriving (Show, Eq)

data Ability = Ability
    { abilName  :: !String
    , abilDmg   :: Config -> Double -- ^ Damage dealt per hit (base)
    , abilSpeed :: Stats  -> Double -- ^ Hits per second
    }

data Glyph = Glyph
    { glyphName  :: !String -- ^ Glyph name
    , glyphStats :: !Stats  -- ^ Glyph passive stats
    }

-- Some useful instances

instance Applicative StatsG where
    pure x = Stats x x x x x x x x x x x x x x x x x x x x x x x

    Stats  f1  f2  f3  f4  f5  f6  f7  f8  f9  f10 f11 f12 f13 f14 f15 f16
           f17 f18 f19 f20 f21 f22 f23
     <*>
     Stats x1  x2  x3  x4  x5  x6  x7  x8  x9  x10 x11 x12 x13 x14 x15 x16
           x17 x18 x19 x20 x21 x22 x23
     = Stats (f1  x1 ) (f2  x2 ) (f3  x3 ) (f4  x4 ) (f5  x5 ) (f6  x6 )
             (f7  x7 ) (f8  x8 ) (f9  x9 ) (f10 x10) (f11 x11) (f12 x12)
             (f13 x13) (f14 x14) (f15 x15) (f16 x16) (f17 x17) (f18 x18)
             (f19 x19) (f20 x20) (f21 x21) (f22 x22) (f23 x23)

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
instance Fractional a => Monoid (StatsG a) where
    mempty  = 0
    -- Multiplicative effects are done separately
    mappend a b = (a + b) { reduce = coeff (reduce a) (reduce b)
                          , debuff = coeff (debuff a) (debuff b) }
        where coeff x y = 100 * (1 - (1 - x/100) * (1 - y/100))

-- Allow multiplying DPS figures
instance Num a => Num (a,a,a) where
    fromInteger (fromInteger -> x) = (x,x,x)
    (+) = liftT2 (+)
    (-) = liftT2 (-)
    (*) = liftT2 (*)
    negate = liftT negate
    abs    = liftT abs
    signum = liftT signum

liftT :: (a -> b) -> (a,a,a) -> (b,b,b)
liftT f (a,b,c) = (f a, f b, f c)

liftT2 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
liftT2 f (a,b,c) (x,y,z) = (f a x, f b y, f c z)

-- | Helper functions

-- Turn a hero's base stats (dmg, HP) at level 1 into stats at max level
baseStats :: Double -> Double -> Stats
baseStats dmg hp = zstats { wdmg   = dmg + 2*perLevel + 3*bonusTrait
                          , wspeed = 5*perLevel + 6*bonusTrait
                          , life   = hp + 50*perLevel + 150*bonusTrait
                          , regen  = 1 + 0.5*perLevel + 2*bonusTrait
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
          heroStats = baseStats 32 0 <> zstats { cleave = 150, reduce = 99.9 }
          heroBonus = const zstats
          heroAbils = [] -- XXX: implement instakill
          heroSpeed = 1.2
          heroReq s = uptime 10 20 s >= 0.99 && bodies s > 0 -- permanent invisibility
          heroAuras = []
          heroCamp  = Melee
          heroItems = [ glaive, kang, alexandrite ]
          heroGlyph = frost
          heroTags  = [WeaponUser, NeedsCD]

blademaster = Hero{..}
    where heroName  = "Blademaster"
          heroStats = baseStats 28 700 <> zstats { cc = 25, chd = 1100, multi = 0.2 }
          heroBonus = const zstats
          heroAbils = []
          heroSpeed = 1.2
          heroReq _ = True
          heroAuras = [ bodyAura heroName 1.2 ]
          heroCamp  = Melee
          heroItems = replicate 2 kang
          heroGlyph = shadow
          heroTags  = [WeaponUser, Tank]

illidan = Hero{..}
    where heroName  = "Illidan"
          heroStats = baseStats 35 700 <> zstats { cleave = 50, wepMul = 50
                                                 , reduce = 50, regen = 120 }
          heroBonus = \s -> zstats { wspeed = if uptime 15 30 s > 0.95
                                              then 475 else 0 }
          heroAbils = []
          heroSpeed = 0.9 -- demon form
          heroReq _ = True
          heroAuras = [ bodyAura heroName 1 ]
          heroCamp  = Melee
          heroItems = replicate 2 kang
          heroGlyph = shadow
          heroTags  = [WeaponUser, Tank, NeedsCD]

nova = Hero{..}
    where heroName  = "Nova"
          heroStats = baseStats 31 500 <> zstats { cc = 5, chd = 2500 }
          heroBonus = const zstats
          heroAbils = [] -- XXX: implement damaging abilities
          heroSpeed = 1.5
          heroReq   = rangeReq
          heroAuras = []
          heroCamp  = Range
          heroItems = replicate 3 kang
          heroGlyph = fire
          heroTags  = [WeaponUser]

sylvanas = Hero{..}
    where heroName  = "Sylvanas"
          heroStats = baseStats 28 550
          heroBonus = \s -> let m = 5/45 in zstats { multi = m*2, wspeed = m*50 }
          heroAbils = []
          heroSpeed = 1.4
          heroReq   = rangeReq
          heroAuras = [ Aura heroName Party False zstats { wspeed = 55 } ]
          heroCamp  = Range
          heroItems = replicate 3 kang
          heroGlyph = frost
          heroTags  = [WeaponUser]

malfurion = Hero{..}
    where heroName  = "Malfurion"
          heroStats = baseStats 23 605 <> zstats { multi = 10 }
          heroBonus = const zstats
          heroAbils = []
          heroSpeed = 1.7
          heroReq   = rangeReq <> \s -> wrange s >= 40
          heroAuras = [ Aura heroName   Party False zstats { wspeed = 50 }
                      , Aura "MalfStun" Party False zstats { reduce = 10 } ]
          heroCamp  = Range
          -- Hard coded for efficiency
          heroItems = [ serendipity, alexandrite ] ++ replicate 4 kang
          heroGlyph = fire
          heroTags  = [WeaponUser]

arthas = Hero{..}
    where heroName  = "Arthas"
          heroStats = baseStats 30 750
          heroBonus = const zstats
          heroAbils = [] -- XXX: implement heal?
          heroSpeed = 1.3
          heroReq _ = True
          heroAuras = [ Aura heroName Party False zstats { wspeed = 50, cdr = 50 }
                      , bodyAura heroName 1.5 ]
          heroCamp  = Melee
          heroItems = replicate 2 kang
          heroGlyph = shadow
          heroTags  = [WeaponUser, Tank, NeedsCD]

shieldbearer = Hero{..}
    where heroName  = "Shieldbearer"
          heroStats = baseStats 18 800 <> zstats { reduce = 25, lifep = 150
                                                 , regenp = 150 }
          heroBonus = const zstats
          heroAbils = [ barrier ]
          heroSpeed = 1.5
          heroReq _ = True
          heroAuras = [ bodyAura heroName 1
                      -- Generous estimation
                      , Aura "Knockback" Party True zstats { reduce = 50 }
                      ]
          heroCamp  = Melee
          heroItems = [ dreadstone, bloodstone ] ++ replicate 2 san'layn
          heroGlyph = shadow
          heroTags  = [AbilityUser, Tank, NeedsReg]

          -- Abilities
          barrier = Ability { abilName  = "Shield Barrier"
                            -- Really crude estimation (for now)
                            , abilDmg   = const $ 106 * 15
                            , abilSpeed = const $ 5
                            }

heroes :: [Hero]
heroes = [
           shieldbearer
         , malfurion
--       , illidan
--       , blademaster
--       , arthas
--       , nova
--       , warden
--       , sylvanas
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
          glyphStats = zstats { cdr = 10, lifep = 10, regenp = 10 }

shadow = Glyph{..}
    where glyphName  = "Shadow Glyph"
          glyphStats = zstats { steal = 12, vamp = 30 }

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

tag :: HeroTag -> Hero -> Bool
tag t Hero{..} = t `elem` heroTags

camp :: HeroCamp -> Hero -> Bool
camp c Hero{..} = c == heroCamp

kang = Item{..}
    where itemName  = "Kang"
          itemStats = zstats { wdmg = 900, cleave = 30 }
          itemAuras = [] -- This aura is useless
          itemFilter = tag WeaponUser

glaive = Item{..}
    where itemName  = "Firestrike Glaive"
          itemStats = zstats { wdmg = 700, cdr = 30 }
          -- 50% at 5/10 second uptime averages out to 25%
          itemAuras = [ Aura itemName Self False zstats { wepMul = 25 } ]
          itemFilter = tag WeaponUser

serendipity = Item{..}
    where itemName  = "Serendipity"
          itemStats = zstats { wdmg = 1000, wspeed = 100, wrange = 25 }
          itemAuras = [ Aura itemName Self False zstats { wspeed = 25 } ]
          itemFilter = camp Range <> tag WeaponUser

alexandrite = Item{..}
    where itemName  = "Alexandrite Coil"
          itemStats = zstats { wdmg = 750, wspeed = 200 }
          itemAuras = [] -- Only useful for Malfurion
          itemFilter = tag WeaponUser

sunshatter = Item{..}
    where itemName  = "Sunshatter"
          itemStats = zstats { wdmg = 650, steal = 5 }
          itemAuras = [] -- XXX Counter-attack (worth it?)
          itemFilter = tag Tank <> tag WeaponUser

tourmaline = Item{..}
    where itemName  = "Tourmaline Signet"
          itemStats = zstats { life = 5000, regen = 50, wdmg = 150 }
          itemAuras = [ Aura itemName Self False zstats { hpreg = 2 } ]
          itemFilter = tag NeedsReg <> tag Tank

bloodstone = Item{..}
    where itemName  = "Bloodstone Signet"
          itemStats = zstats { wdmg = 750, life = 25000, regen = 250 }
          itemAuras = [ Aura itemName Self False zstats { reduce = 50 } ]
          itemFilter = tag Tank

rhinestone = Item{..}
    where itemName  = "Rhinestone Seal"
          itemStats = zstats { wdmg = 450, wspeed = 100, life = 15000
                             , regen = 150, abilPow = 900 }
          itemAuras = [] -- Essentially useless
          itemFilter = tag AbilityUser <> tag Tank

hellscream's = Item{..}
    where itemName  = "Hellscream's Shield Wall"
          itemStats = zstats { wdmg = 500, life = 40000, regen = 500 }
          itemAuras = [ Aura itemName Drain False zstats { drainp = 2.5 } ]
          itemFilter = camp Melee <> tag Tank

blackfang = Item{..}
    where itemName  = "Blackfang Weave"
          itemStats = zstats { wspeed = 100, life = 40000, regen = 500 }
          itemAuras = [ Aura itemName Party False zstats { dmgMul = 15 } ]
          itemFilter = camp Melee

immovable = Item{..}
    where itemName  = "The Immovable Object"
          itemStats = zstats { cdr = 30, life = 40000, regen = 500 }
          itemAuras = [ Aura itemName Camp False zstats { regenp = 25 } ]
          itemFilter = tag Tank <> tag NeedsReg

absolution = Item{..}
    where itemName  = "Shroud of Absolution"
          itemStats = zstats { life = 35000, regen = 400 }
          itemAuras = [ Aura itemName Camp False zstats { cdr = 15 } ]
          itemFilter = tag Tank

bulwark = Item{..}
    where itemName  = "Blackrock Bulwark"
          itemStats = zstats { life = 7500, regen = 100, wdmg = 100 }
          itemAuras = [ Aura itemName Party False zstats { reduce = 50 } ]
          itemFilter = camp Melee <> tag Tank

emerald = Item{..}
    where itemName  = "Emerald Coil"
          itemStats = zstats { wspeed = 75, wdmg = 150 }
          itemAuras = [ Aura itemName Party False zstats { reduce = 50 } ]
          itemFilter = camp Melee <> tag Tank

aegis = Item{..}
    where itemName  = "Royal Aegis"
          itemStats = zstats { life = 1500, regen = 20, cdr = 10 }
          itemAuras = [ Aura itemName Self False zstats { reduce = 20 } ] -- average-ish
          itemFilter = camp Melee <> tag Tank

dreadstone = Item{..}
    where itemName  = "Dreadstone Band"
          itemStats = zstats { life = 25000, regen = 250, abilPow = 1500 }
          itemAuras = [ Aura itemName Drain False zstats { drainp = 1 } ]
          itemFilter = camp Melee <> tag Tank <> tag AbilityUser

skullflame = Item{..}
    where itemName  = "Skullflame Shield"
          itemStats = zstats { life = 20000, regen = 900, abilPow = 1000
                             , wdmg = 400 }
          itemAuras = [ Aura itemName Party False zstats { abilMul = 15 } ]
          itemFilter = camp Melee <> tag Tank <> tag AbilityUser

san'layn = Item{..}
    where itemName  = "San'layn Stave"
          itemStats = zstats { abilPow = 1500, wdmg = 500, vamp = 6 }
          itemAuras = []
          itemFilter = tag AbilityUser

items = [kang, alexandrite, glaive, serendipity, sunshatter, blackfang,
         bloodstone, rhinestone, hellscream's, immovable, absolution, bulwark,
         emerald, aegis, dreadstone, tourmaline, skullflame, san'layn]

-- | DPS calculation

type DPS = (Double, Double, Double) -- Physical, Magical, Drain

-- Physical, Magical, Drain
dps :: Config -> Stats -> Hero -> DPS
dps c@Config{..} s@Stats{..} Hero{..} = (weaponDamage, abilityDamage, drainDamage)
    where weaponDamage   = (1 + multi) * swingDamage / swingSpeed
          swingSpeed     = heroSpeed / (1 + wspeed/100) * (1 - debuff/100)
          swingDamage    = (1 + cleaveDensity * cleave/100) * singleDamage
          singleDamage   = wdmg * (1 + wepMul/100) * critMul * totMul
          critMul        = 1 + cc/100 * (chd/100 - 1)

          abilityDamage  = totMul * (1 + abilMul/100) * (1 + abilPow/100) * abilities
          abilities      = sum [ admg * aspeed | Ability{..} <- heroAbils
                               , let admg   = abilDmg c
                               , let aspeed = abilSpeed s
                               ]

          drainDamage    = (1 + mobHitDensity) * mobHP * (drainp/100)
          mobHP          = 500000

          totMul         = 1 + dmgMul/100

-- | Toughness estimation

type Toughness = Double

-- Effective health
toughness :: Stats -> Hero -> Toughness
toughness s@Stats{..} h = hptotal / dmgCoeff s h
    where hptotal = life * (1 + lifep/100)

-- Damage multiplier (factors into toughness)
dmgCoeff :: Stats -> Hero -> Double
dmgCoeff Stats{..} Hero{..} = mod * (1 - reduce/100)
    where mod  = case heroCamp of Melee -> 2.0; _ -> 1.0

-- Enemy DPS approximation
mobDamage :: Config -> Stats -> Double
mobDamage Config{..} Stats{..} = mobCount * mobDPS / targetCount
    where mobDPS      = 8000 -- death revenant-ish w/ buffs
          mobCount    = mobHitDensity
          targetCount = bodies / 2 -- assume a worst case scenario

-- Healing estimation
healing :: Stats -> DPS -> Double
healing Stats{..} (pdps, mdps, _) = lifesteal + totalreg * (1 + regenp/100)
    where totalreg  = regen + totalhp * (hpreg/100)
          totalhp   = life * (1 + lifep/100)
          lifesteal = pdps * steal/100 + mdps * vamp/100

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
playerStats (h,is) = heroStats h <> foldMap itemStats is <> glyphStats (heroGlyph h)

-- Party does not include the hero itself
allAuras :: Config -> Party -> Player -> [Aura]
allAuras c ps p@(h,_) = nub $ partyAuras ++ playerAuras p ++ bonus
    where partyAuras = [ a | p' <- ps, a <- playerAuras p', auraApplies p' a ]
          auraApplies (h',_) a = case auraType a of
                Self   -> False
                Party  -> True
                Camp   -> heroCamp h' == heroCamp h || ignoreConditions c
                Debuff -> debuffApplies a
                Drain  -> False

          bonus = filter debuffApplies $ bonusAuras c
          debuffApplies a = not (ignoreConditions c) && case auraType a of
                Debuff -> heroCamp h == Melee
                _      -> True

partyStats :: Config -> Party -> [Stats]
partyStats c = go []
    where go _ []       = []
          go ps' (p:ps) = let auras = allAuras c (ps++ps') p
                              base  = playerStats p <> foldMap auraStats auras
                              stats = base <> heroBonus (fst p) base
                          in stats : go (p:ps') ps

-- | Brute force search space

type Choice m a = Int -> [a] -> m [a]

det :: Choice [] a
det 0 _      = [[]]
det _ []     = []
det n (x:xs) = do
    c <- [n,n-1..0]
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
            | total db > total da = a : getRecords (b:rs)
            | otherwise = getRecords (a:rs)

          total = sum . map (\(x,y,z) -> x+y+z)

guessLoop :: Config -> Int -> IO ()
guessLoop c n = do replicateM_ 3 . forkIO $ go 0
                   go 0
    where go d = do
            party <- partyChoice rnd rnd n
            case attempt c party of
                Just r@(sum -> d',_,_) | total d' > total d -> do printResult c r
                                                                  go d'
                _ -> go d

          total (x,y,z) = x+y+z

-- | Pretty printing and main program

showStats :: Stats -> String
showStats s = intercalate ", " [ show a ++ " " ++ n | (f, n) <- selectors
                               , let a = f s , a /= 0 ]
    where selectors = [ (wdmg, "WD"), (wepMul, "WD%"), (dmgMul, "DMG%")
                      , (wspeed, "WS%"), (cleave, "CLV%"), (cdr, "AbS%")
                      , (cc, "CC%"), (chd, "CHD%"), (life, "HP")
                      , (steal, "LfS"), (vamp, "SpV"), (bodies, "BOD")
                      , (multi, "MS") , (wrange, "WRn"), (lifep, "HP%")
                      , (reduce, "Red%"), (regen, "HP/s"), (regenp, "HP/s%")
                      , (debuff, "Dbf%"), (hpreg, "HP%/s"), (abilPow, "AbP%")
                      , (abilMul, "AbD%"), (drainp, "Dr%")
                      ]

showDPS :: DPS -> String
showDPS (p,m,d) = intercalate " / " [show p, show m, show d] ++ " = " ++ show t
    where t = p+m+d

printPlayer :: Player -> DPS -> IO ()
printPlayer p@(Hero{..}, is) dps = do
    putStrLn $ "--- " ++ heroName ++ " ---"
    putStrLn $ "Hero DPS:\t" ++ showDPS dps
    putStrLn $ glyphName heroGlyph ++ ": " ++ showStats (glyphStats heroGlyph)
    forM_ is $ \Item{..} -> do
        putStrLn $ itemName ++ ": " ++ showStats itemStats
    putStrLn ""
    putStrLn "Auras provided:"
    forM_ (playerAuras p) $ \Aura{..} -> do
        putStrLn $ auraName ++ " (" ++ show auraType ++ "): " ++ showStats auraStats

printResult :: Config -> Result -> IO ()
printResult c (dps,party,stats) = do
    let total@(tp,tm,td) = sum dps
        per   = (tp / cd, tm / cd, td / cd) where cd = 1 + cleaveDensity c
    putStrLn "===== New best: ====="
    putStrLn $ "Total DPS:\t" ++ showDPS total
    putStrLn $ "Avg DPS per:\t" ++ showDPS per
    forM_ (zip3 party stats dps) $ \(p,s,d) -> do
        printPlayer p d
        putStrLn ""
        putStrLn "Summary of stats:"
        putStrLn $ showStats s
    putStrLn "----------------"
    putStrLn "Bonus auras:"
    forM_ (bonusAuras c) $ \Aura{..} -> do
        putStrLn $ auraName ++ ": " ++ showStats auraStats

    let pool = 500000 * 50
        dsum (p,m,d) = p+m+d
    putStrLn $ "Time needed to kill 50 DRs: " ++ show (pool / dsum total)

-- Configuration

data Config = Config
    { cleaveDensity    :: Double -- ^ Number of extra hit on average
    , mobHitDensity    :: Double -- ^ Number of mobs that hit you
    , ignoreConditions :: Bool   -- ^ Ignore conditions (DPS hypothetical limit)
    , bonusAuras       :: [Aura] -- ^ Extra auras to assume the party has
    }

defConf :: Config
defConf = Config { cleaveDensity = 0
                 , mobHitDensity = 1
                 , ignoreConditions = False
                 , bonusAuras = [] }

midConf, lateConf, sillyConf :: Config
midConf   = defConf { cleaveDensity = 1.5,  mobHitDensity = 5   }
lateConf  = defConf { cleaveDensity = 10.0, mobHitDensity = 100 }
sillyConf = lateConf { ignoreConditions = True }

main :: IO ()
main = bruteForce conf 1 `forM_` printResult conf
--main = guessLoop conf 2
    where conf = lateConf {
            bonusAuras = [ Aura "Slow"     Debuff False zstats { debuff = 25 }
                         , Aura "Curse"    Debuff False zstats { debuff = 33 }
                         , Aura "Poison"   Debuff False zstats { debuff = 10 }
                         , Aura "Mind Rot" Debuff False zstats { cdr = -33 }
                         ]
          }
