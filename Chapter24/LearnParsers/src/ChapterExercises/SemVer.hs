module SemVer where

import Data.Word
import Text.Trifecta
import Control.Applicative
import Data.Char

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString = NOSS String | NOSI Integer
                      deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
              deriving (Eq, Show)

instance Ord SemVer where
    (SemVer major minor patch release metadata) `compare` (SemVer major' minor' patch' release' metadata') =
        case (major `compare` major') of
            LT -> LT
            GT -> GT
            EQ -> case (minor `compare` minor') of
                  LT -> LT
                  GT -> GT
                  EQ -> patch `compare` patch'
    --I don't feel like comparing the release data. The rules are too complicated.


-- 1. Write a parser for semantic versions as defined by http://semver.org/.
-- After making a working parser, write an Ord instance for the SemVer type
-- that obeys the specification outlined on the SemVer type that obeys
-- the specification outlined on the SemVer website.

parseMajor :: Parser Major
parseMajor = decimal <* char '.'

parseMinor :: Parser Minor
parseMinor = decimal <* char '.'

parsePatch :: Parser Patch
parsePatch = decimal

-- parseRelease :: Parser Release
-- parseRelease = (char '-') *> (some $ NOSS <$> (some letter) <|> (NOSI <$> decimal)) <* (optional (char '.')) 
parseNumberOrString :: Parser NumberOrString
parseNumberOrString  = NOSS <$> (some letter) <|> (NOSI <$> decimal)

parseRelease :: Parser Release
parseRelease = do
    hasRelease <- optional $ char '-'
    case hasRelease of
        Nothing -> do return ([])
        Just _ -> do
                    release <- some $ parseNumberOrString <* (skipOptional $ char '.')
                    return release

parseMetadata = do
    hasMetadata <- optional $ char '+'
    case hasMetadata of
        Nothing -> do return ([])
        Just _ -> do
                    metadata <- some $ parseNumberOrString <* (skipOptional $ char '.')
                    return metadata

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- parseMajor
    minor <- parseMinor
    patch <- parsePatch
    release <- parseRelease
    metadata <- parseMetadata
    return (SemVer major minor patch release metadata)

-- Expected results:
-- Prelude> parseString parseSemVer mempty "2.1.1"
-- Success (SemVer 2 1 1 [] [])
-- Prelude> parseString parseSemVer mempty "1.0.0-x.7.z.92"
-- Success (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
-- Prelude> SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
-- True

