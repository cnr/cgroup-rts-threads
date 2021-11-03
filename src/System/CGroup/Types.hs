-- | Parsers and types related to cgroups and mounts
module System.CGroup.Types (
  -- * CPU quotas
  CPUQuota (..),

  -- * raw cgroups as viewed in \/proc\/$PID\/cgroup
  RawCGroup (..),
  parseCGroups,

  -- * mounts as viewed in \/proc\/$PID\/mountinfo
  Mount (..),
  parseMountInfo,

  -- * Parser type
  Parser,
  parseFile,
) where

import Control.Exception (throwIO)
import Data.Char (isSpace)
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Path
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

---------- CPU quotas

-- | A CPU quota is the ratio of CPU time our process can use relative to the
-- scheduler period
--
-- For example:
--
-- @
-- | ratio            | description |
-- | ---------------- | ----------- |
-- |  100000 / 100000 |         (1) |
-- |  200000 / 100000 |         (2) |
-- |   50000 / 100000 |         (3) |
-- |     max / 100000 |         (4) |
-- @
--
-- (1): we can use up to a single CPU core
--
-- (2): we can use up to two CPU cores
--
-- (3): the scheduler will give us a single CPU core for up to 50% of the time
--
-- (4): we can use all available CPU resources (there is no quota)
data CPUQuota
  = CPUQuota (Ratio Int)
  | NoQuota
  deriving (Eq, Ord, Show)

---------- Raw cgroups

-- | A cgroup, as viewed within \/proc\/[pid]\/cgroup
--
-- see cgroups(7): \/proc\/[pid]\/cgroup section
data RawCGroup = RawCGroup
  { rawCGroupId :: Text
  , rawCGroupControllers :: [Text]
  , rawCGroupPath :: Path Abs Dir
  }
  deriving (Show)

-- | Parse an entire \/proc\/[pid]\/cgroup file into a list of cgroups
parseCGroups :: Parser [RawCGroup]
parseCGroups = some parseSingleCGroup <* eof

-- | Parse a single cgroup line within \/proc\/[pid]\/cgroup
--
-- hierarchyID:list,of,controllers:path
--
-- In cgroups version 1, a comma-separated list of controllers exists within each group
--
-- In cgroups version 2, the "controllers" section is always an empty string
--
-- see cgroups(7): \/proc\/[pid]\/cgroup section
parseSingleCGroup :: Parser RawCGroup
parseSingleCGroup =
  RawCGroup
    <$> takeUntil1P ':' -- hierarchy ID number
    <*> (splitOnIgnoreEmpty "," <$> takeUntilP ':') -- comma-separated list of controllers
    <*> (parseIntoAbsDir =<< takeUntil1P '\n') -- path

-- return the prefix of the input until reaching the supplied character.
-- the character is also consumed as part of this parser.
--
-- this parser succeeds even when the character does not exist in the input
takeUntilP :: Char -> Parser Text
takeUntilP c = takeWhileP Nothing (/= c) <* optional (char c)

-- like 'takeUntilP', but expects a non-empty prefix before the character
takeUntil1P :: Char -> Parser Text
takeUntil1P c = takeWhile1P Nothing (/= c) <* optional (char c)

-- Data.Text.splitOn, but returns empty list on empty haystack, rather than [""]
--
-- >>> Data.Text.splitOn "foo" ""
-- [""]
--
-- >>> splitOnIgnoreEmpty "foo" ""
-- []
splitOnIgnoreEmpty :: Text -> Text -> [Text]
splitOnIgnoreEmpty _ "" = []
splitOnIgnoreEmpty s str = Text.splitOn s str

---------- Mounts

-- | A mount, as viewed within \/proc\/[pid]\/mountinfo
--
-- see proc(5): \/proc\/[pid]\/mountinfo section
data Mount = Mount
  { mountId :: Text
  , mountParentId :: Text
  , mountStDev :: Text
  , mountRoot :: Text
  , mountPoint :: Text
  , mountOptions :: Text
  , mountTags :: [Text]
  , mountFilesystemType :: Text
  , mountSource :: Text
  , mountSuperOptions :: [Text]
  }
  deriving (Show)

-- | Parse an entire \/proc\/[pid]\/mountinfo file into a list of mounts
parseMountInfo :: Parser [Mount]
parseMountInfo = some parseSingleMount <* eof

-- | Parse a single mount line within \/proc\/[pid]\/mountinfo
--
-- Fields are space-separated
--
-- see proc(5): \/proc\/[pid]\/mountinfo section
parseSingleMount :: Parser Mount
parseSingleMount =
  Mount
    <$> field -- id
    <*> field -- parent id
    <*> field -- st_dev
    <*> field -- mount root
    <*> field -- mount point
    <*> field -- mount options
    <*> field `manyTill` separator -- optional mount tags, terminated by "-"
    <*> field -- filesystem type
    <*> field -- mount source
    <*> (splitOnIgnoreEmpty "," <$> field) -- super options
    <* optional (char '\n')

-- a field in the mountinfo file, terminated by whitespace
field :: Parser Text
field = lexeme $ takeWhile1P Nothing (not . isSpace)

-- separator after optional mount tags ("-")
separator :: Parser Char
separator = lexeme $ char '-'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (char ' '))

parseIntoAbsDir :: Text -> Parser (Path Abs Dir)
parseIntoAbsDir = either (fail . show) pure . parseAbsDir . Text.unpack

-- | Megaparsec Parser
type Parser = Parsec Void Text

-- | Parse a file
parseFile :: Parser a -> Path b File -> IO a
parseFile parser file = either throwIO pure . parse parser (toFilePath file) =<< TIO.readFile (toFilePath file)
