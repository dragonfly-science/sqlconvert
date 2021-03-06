{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
--
module Main where
-- SQL Server ouput converted into PostgreSQL

import           Control.Applicative              hiding (many)
import           Control.Monad                    (mzero)
import           Data.Time
import           Numeric                          (readHex)
import           System.Locale

import           Data.Attoparsec.ByteString       as A hiding (takeWhile1)
import           Data.Attoparsec.ByteString.Char8 as AC (char, endOfLine,
                                                         inClass, isEndOfLine,
                                                         satisfy, skipSpace,
                                                         takeWhile1)
import qualified Data.Attoparsec.ByteString.Lazy  as AL

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import           Data.ByteString.Lazy             hiding (map)

import           Prelude                          hiding (concat, getContents,
                                                   interact, putStr, takeWhile)
import           System.IO                        (stderr)

-- Entry captures something interesting, or Nothing
type Entry = Either (Maybe ByteString) ByteString
-- the names of 'current' insert fields
type InsertState = Maybe [ BS.ByteString ]

main :: IO ()
main =
  do
    contents <- getContents
    let parsed = parseSQL Nothing contents
    mapM_ (either writeErrors putStr) parsed
    where
        writeErrors (Just err) = hPutStr stderr err
        writeErrors Nothing    = return ()

-- Lazy parser, converts a lazy bytestring into a lazy string of Entries
parseSQL :: InsertState -> ByteString -> [Entry]
parseSQL insertState contents =
  case AL.parse (parseEntry insertState) contents of
    AL.Fail {} -> []
    AL.Done contents' (insertState', entry) -> entry : parseSQL insertState' contents'

-- Main parser, tries to find something interesting
parseEntry :: InsertState -> Parser (InsertState, Entry)
parseEntry insertState  =  (insertState,) <$> createTable
                       <|> (insertState,) <$> alterTable
                       <|> insertStatement insertState
                       <|> finishInserts insertState
                       <|> (insertState,) <$> ignoreInsert
                       <|> (insertState,) <$> ignoreLine


-----------------------------------
-- INSERT TO COPY

-- when starting an insert sequence, convert it into a copy....
-- First one sets up some state
-- After that it gets checked...
insertStatement :: InsertState -> Parser (InsertState, Entry)
insertStatement insertState =
  do
    tableName <- string "INSERT [dbo]." *> identifier <* char ' '
    columns <- char '(' *> identifier `sepBy` (string ", ") <* char ')'
    values <- string " VALUES (" *> value `sepBy` (string ", ") <* char ')' <* endOfLine
    let copyLine = concat ["COPY ", showIdentifier tableName, " (", cols, ") FROM STDIN;\n" ]
        cols = intercalate "," (map showIdentifier columns)
    case insertState of
        Nothing -> return (Just columns, Right (concat [copyLine, writeVals values]))
        Just _  -> return (Just columns, Right (writeVals values))

    where
        writeVals :: [BS.ByteString] -> ByteString
        writeVals vals = concat [intercalate "\t" $ map fromStrict vals, "\n"]
        value  =  (string "NULL" >> return "\\N")
              <|> parseString
              <|> parseCast
              <|> parseDateTime
              <|> parseNumber


ignoreInsert :: Parser Entry
ignoreInsert =
  do
    start <- string "INSERT [dbo]."
    rest <- AL.takeWhile (not . isEndOfLine)
    endOfLine
    return $ Left (Just ( concat [fromStrict start, fromStrict rest, "\n"]))

parseDateTime :: Parser BS.ByteString
parseDateTime =
  do
    (hex1, hex2) <- string "CAST(0x" *> ((,) <$> eighthex <*> eighthex) <* " AS DateTime)"
    let [(days, _)] = readHex hex1
        timen :: Double
        [(timen, _)] = readHex hex2
        day = addDays days (fromGregorian 1900 1 1)
        timeDiff = picosecondsToDiffTime (floor (timen * 10000000000/3))
    if days > 106680740  -- largest date 294276 AD
        then return "294275-01-01 00:00:00"
        else return $ BSC.pack $ formatTime defaultTimeLocale "%F %T" $ UTCTime day timeDiff
    where
        eighthex = A.count 8 (AC.satisfy (AC.inClass "0-9A-F"))

parseString :: Parser BS.ByteString
parseString =
  do
    res <- string "N'" *> contents <* char '\''
    return $ BSC.concatMap escape res
    where
        contents = BSC.concat <$> many' (takeWhile1 (/= '\'') <|> (string "''" >> return "'"))
        -- http://www.postgresql.org/docs/9.1/static/sql-copy.html
        escape '\n' = BSC.pack "\\n"
        escape '\t' = BSC.pack "\\t"
        escape '\r' = BSC.pack "\\r"
        escape '\b' = BSC.pack "\\b"
        escape '\f' = BSC.pack "\\f"
        escape '\\' = BSC.pack "\\\\"
        escape c    = BSC.pack [c]

parseCast :: Parser BS.ByteString
parseCast =
  do
    string "CAST(" *> parseNumber <* " AS Decimal(" <* parseNumber <* ", " <* parseNumber <* "))"

parseNumber :: Parser BS.ByteString
parseNumber =
  do
    sign <- option "" (string "-")
    nums <- AL.takeWhile $ AL.inClass "0-9.xA-F"
    return $ BSC.concat [sign , nums]

finishInserts :: InsertState -> Parser (InsertState, Entry)
finishInserts Nothing = mzero
finishInserts (Just _) = (string "/****" <|> string "SET") >> ignoreLine >> return (Nothing, Right "\\.\n")

-----------------------------------
-- CREATE TABLE
createTable :: Parser Entry
createTable =
  do
    tableName <- string "CREATE TABLE [dbo]." *> identifier <* string "(" <* endOfLine
    columns <- many1 (column <* ignoreLine)
    let cs = intercalate ",\n" $ map showColumn columns
    let result = concat $ ["CREATE TABLE ", showIdentifier tableName, " (", "\n",  cs, "\n);\n"]
    return (Right result)

-- ALTER TABLE
alterTable :: Parser Entry
alterTable =
  do
    tableName <- string "ALTER TABLE [dbo]." *> identifier <* string " ADD "
    c <- column
    endOfLine
    let result = concat $ ["ALTER TABLE ", showIdentifier tableName, " ADD COLUMN ", showColumn c, ";\n"]
    return (Right result)


identifier :: Parser BS.ByteString
identifier = char '[' *> (AL.takeWhile $ AL.inClass "A-Za-z0-9_-")  <* char ']'
showIdentifier :: BS.ByteString -> ByteString
showIdentifier ident = concat ["\"", fromStrict ident, "\""]

cqualifier :: Parser BS.ByteString
cqualifier  =  string "NOT NULL"
           <|> string "NULL"
           <|> (string "IDENTITY(1,1)" >> return "PRIMARY KEY")


-- Columns
data ColumnType = ColumnType BS.ByteString (Maybe BS.ByteString) deriving Eq
ctype :: Parser ColumnType
ctype = ColumnType <$> identifier <*> option Nothing nums
    where
        nums = Just <$> (char '(' *> takeWhile1 (/= ')') <* char ')')

data Column = Column { columnName :: BS.ByteString, columnType :: ColumnType, columnQualifiers :: [BS.ByteString] } deriving Eq
column :: Parser Column
column =  Column <$> (skipSpace *> identifier  <* char ' ') <*> (ctype <* char ' ' ) <*> (many' cqualifier)
showColumn :: Column -> ByteString
-- Handle some fields specially
showColumn (Column "TimeStamp" _ _) = intercalate " " $ ["    ", "\"TimeStamp\"", "TEXT", "NULL"]
showColumn (Column "Photo" _ _)     = intercalate " " $ ["    ", "\"Photo\"",     "TEXT", "NULL"]
showColumn (Column cn ct cqs) =
    let  col = showIdentifier cn
         cty = fixType ct
         cqs' = intercalate " " $ map fromStrict cqs
         indent = "    "
    in  intercalate " " $ [indent, col, cty, cqs']

    where
        fixType (ColumnType t Nothing)  = fromStrict (pgType t)
        fixType (ColumnType t (Just n)) = concat [fromStrict (pgType t), "(", fromStrict n, ")"]
        pgType "datetime"   = "TIMESTAMP"
        pgType "char"       = "CHAR"
        pgType "varchar"    = "VARCHAR"
        pgType "nvarchar"   = "VARCHAR"
        pgType "int"        = "INT"
        pgType "bigint"     = "BIGINT"
        pgType "boolean"    = "BOOLEAN"
        pgType "timestamp"  = "BIGINT"
        pgType "text"       = "TEXT"
        pgType "image"      = "BYTEA"
        pgType "money"      = "MONEY"
        pgType t            = t

-- End CREATE TABLE
--------------------------------------------------

-- Allow lines to be ignored
ignoreLine :: Parser Entry
ignoreLine = do skipWhile (not . isEndOfLine)
                endOfLine
                return $ Left Nothing


