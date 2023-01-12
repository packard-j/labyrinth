{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Data.JSON.Board (JSONBoard(..), Gem, allGems) where
import Data.Aeson
import Data.Aeson.Types
import Data.Vector ((!))
import qualified Data.Vector as Vector (toList)
import Maze.Board
import Maze.Tile
import Maze.Orientation
import Maze.Connector
import Maze.Coordinate
import Data.Set (Set, fromList, toList, member, size)
import Data.Text (unpack)
import Data.Tuple
import Test.QuickCheck

newtype JSONBoard = JSONBoard (Board Treasure) deriving (Show, Eq)

instance FromJSON JSONBoard where
  parseJSON value = JSONBoard <$> parseBoard value where
    parseBoard = withObject "Board" $ \board -> do
      connectors <- board .: "connectors"
      treasures  <- board .: "treasures"
      gems       <- parseMatrix parseTreasure treasures
      enforceUnique gems
      treasureAt <- parseCoordinateMap rows cols gems
      tileAt     <- parseMatrix parseConnector connectors >>= 
                    parseCoordinateMap rows cols
      pure $ newBoard (\coord -> tileAt coord $ treasureAt coord) rows cols
    rows = 7
    cols = 7
    enforceUnique gems = if uniqueTreasures (concat gems)
                            then pure () 
                            else fail "treasures are not mutually distinct"

instance ToJSON JSONBoard where
  toJSON (JSONBoard board) = object
    [ "connectors" .= (map show <$> tiles board),
      "treasures" .= (map contents <$> tiles board)]

instance Arbitrary JSONBoard where
  arbitrary = do
    f <- suchThat arbitrary unique
    return $ JSONBoard (newBoard f cols rows) where
      unique f = uniqueTreasures (treasures $ newBoard f cols rows)
      treasures board = contents <$> concat (tiles board)
      rows = 7
      cols = 7
            
type Treasure = (Gem, Gem)
newtype Gem = Gem String deriving (Show, Eq, Ord)

instance ToJSON Gem where
  toJSON (Gem name) = toJSON name

instance Arbitrary Gem where
  arbitrary = elements (toList allGems)

type Matrix a = [[a]]

newtype UnorderedPair a = UnorderedPair (a, a)

instance (Ord a) => Ord (UnorderedPair a) where
  compare (UnorderedPair a) (UnorderedPair b) 
    | UnorderedPair a == UnorderedPair b = EQ
    | otherwise = compare a b

instance (Eq a) => Eq (UnorderedPair a) where
  (UnorderedPair a) == (UnorderedPair b) = a == b || a == swap b

-- | Parse a [[`a']] from a JSON Array of JSON Arrays.
parseMatrix :: (Value -> Parser a) -> Value -> Parser (Matrix a)
parseMatrix parseElement = parseListOf "rows" (parseListOf "cols" parseElement)

-- | Parse a [`a'] from a JSON Array 
parseListOf :: String -> (Value -> Parser a) -> Value -> Parser [a]
parseListOf name parseElement = withArray name $ \elems -> sequenceA (Vector.toList $ parseElement <$> elems)

-- | Parse a mapping of Coordinates to to elements in the given Matrix of size (rows x cols)
parseCoordinateMap :: Integer -> Integer -> Matrix a -> Parser (Coordinate -> a)
parseCoordinateMap rows cols matrix
  | validRows && validCols = pure access 
  | otherwise = fail "invalid size"
 where
   validRows = validLength rows matrix
   validCols = all (validLength cols) matrix
   validLength expected list = length list == fromIntegral expected
   access (Coordinate col row) = matrix !! fromIntegral row !! fromIntegral col

-- | Parse a connector (a tile without a contained `a').
parseConnector :: Value -> Parser (a -> Tile a)
parseConnector = withText "connector" tile where
  tile "│" = pure $ Tile Bar North
  tile "─" = pure $ Tile Bar East
  tile "└" = pure $ Tile L North
  tile "┌" = pure $ Tile L East
  tile "┐" = pure $ Tile L South
  tile "┘" = pure $ Tile L West
  tile "┬" = pure $ Tile T North
  tile "┤" = pure $ Tile T East
  tile "┴" = pure $ Tile T South
  tile "├" = pure $ Tile T West
  tile "┼" = pure $ Tile Plus North
  tile _   = fail "invalid connector" :: Parser (a -> Tile a)

-- | Parse a Treasure, which is a pairing of two Gems
parseTreasure :: Value -> Parser Treasure
parseTreasure = withArray "treasure" gems where
  gems array
    | length array == 2 = do
       gem1 <- parseGem $ array ! 0
       gem2 <- parseGem $ array ! 1
       return (gem1, gem2)
    | otherwise = fail "invalid treasure size"
  parseGem = withText "gem" $ \text -> 
    let name = unpack text in 
    if member (Gem name) allGems
       then pure $ Gem name
       else fail "invalid gem"

uniqueTreasures :: [Treasure] -> Bool
uniqueTreasures treasures = size (fromList $ UnorderedPair <$> treasures) == length treasures

-- | The set of valid gem names
allGems :: Set Gem
allGems = fromList $ Gem <$> [ 
    "alexandrite-pear-shape",
    "alexandrite",
    "almandine-garnet",
    "amethyst",
    "ametrine",
    "ammolite",
    "apatite",
    "aplite",
    "apricot-square-radiant",
    "aquamarine",
    "australian-marquise",
    "aventurine",
    "azurite",
    "beryl",
    "black-obsidian",
    "black-onyx",
    "black-spinel-cushion",
    "blue-ceylon-sapphire",
    "blue-cushion",
    "blue-pear-shape",
    "blue-spinel-heart",
    "bulls-eye",
    "carnelian",
    "chrome-diopside",
    "chrysoberyl-cushion",
    "chrysolite",
    "citrine-checkerboard",
    "citrine",
    "clinohumite",
    "color-change-oval",
    "cordierite",
    "diamond",
    "dumortierite",
    "emerald",
    "fancy-spinel-marquise",
    "garnet",
    "gems",
    "gems",
    "gems2",
    "gems2",
    "gems3",
    "golden-diamond-cut",
    "goldstone",
    "grandidierite",
    "gray-agate",
    "green-aventurine",
    "green-beryl-antique",
    "green-beryl",
    "green-princess-cut",
    "grossular-garnet",
    "hackmanite",
    "heliotrope",
    "hematite",
    "iolite-emerald-cut",
    "jasper",
    "jaspilite",
    "kunzite-oval",
    "kunzite",
    "labradorite",
    "lapis-lazuli",
    "lemon-quartz-briolette",
    "magnesite",
    "mexican-opal",
    "moonstone",
    "morganite-oval",
    "moss-agate",
    "orange-radiant",
    "padparadscha-oval",
    "padparadscha-sapphire",
    "peridot",
    "pink-emerald-cut",
    "pink-opal",
    "pink-round",
    "pink-spinel-cushion",
    "prasiolite",
    "prehnite",
    "purple-cabochon",
    "purple-oval",
    "purple-spinel-trillion",
    "purple-square-cushion",
    "raw-beryl",
    "raw-citrine",
    "red-diamond",
    "red-spinel-square-emerald-cut",
    "rhodonite",
    "rock-quartz",
    "rose-quartz",
    "ruby-diamond-profile",
    "ruby",
    "sphalerite",
    "spinel",
    "star-cabochon",
    "stilbite",
    "sunstone",
    "super-seven",
    "tanzanite-trillion",
    "tigers-eye",
    "tourmaline-laser-cut",
    "tourmaline",
    "unakite",
    "white-square",
    "yellow-baguette",
    "yellow-beryl-oval",
    "yellow-heart",
    "yellow-jasper",
    "zircon",
    "zoisite" ]
