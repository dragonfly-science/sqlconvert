{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
--
module Main where
-- SQL Server ouput converted into PostgreSQL

import           Control.Applicative hiding (many)
import           Control.Monad (mzero)
import           Numeric (readHex)
import           Data.Maybe (catMaybes)
import           Data.Time
import           System.Locale

import           Data.Attoparsec.ByteString as A hiding (takeWhile1)
import           Data.Attoparsec.ByteString.Char8 as AC (endOfLine, isEndOfLine, skipSpace, char, isDigit, takeWhile1, satisfy, inClass)
import qualified Data.Attoparsec.ByteString.Lazy as AL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy hiding (map)

import           Prelude hiding (takeWhile, concat, interact)

-- Entry captures something interesting, or Nothing
type Entry = Maybe ByteString
-- the names of 'current' insert fields
type InsertState = Maybe [ BS.ByteString ] 

main :: IO ()
main = interact ( concat . catMaybes . parseSQL Nothing)

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
        Nothing -> return (Just columns, Just (concat [copyLine, writeVals values]))
        Just _  -> return (Just columns, Just (writeVals values))

    where
        writeVals :: [BS.ByteString] -> ByteString
        writeVals vals = concat [intercalate "\t" $ map fromStrict vals, "\n"]
        value  =  (string "NULL" >> return "\\N")  
              <|> parseDateTime
              <|> parseString
              <|> (AL.takeWhile $ AL.inClass "0-9.")


parseDateTime :: Parser BS.ByteString
parseDateTime =
  do 
    (hex1, hex2) <- string "CAST(0x" *> ((,) <$> eighthex <*> eighthex) <* " AS DateTime)"   
    let [(days, _)] = readHex hex1
        timen :: Double
        [(timen, _)] = readHex hex2
        day = addDays days (fromGregorian 1900 1 1)
        timeDiff = picosecondsToDiffTime (floor (timen * 10000000000/3))
    if days > (300*365)
        then return "\\N"
        else return $ BSC.pack $ formatTime defaultTimeLocale "%F %T" $ UTCTime day timeDiff 
    where
        eighthex = A.count 8 (AC.satisfy (AC.inClass "0-9A-F"))
            
parseString :: Parser BS.ByteString
parseString = (string "N''" >> return "") <|>
  do
    res <- string "N'" *> takeWhile1 (/= '\'') <* char '\''
    return $ BSC.concatMap escape res
    where   -- http://www.postgresql.org/docs/9.1/static/sql-copy.html
        escape '\n' = BSC.pack "\\n"
        escape '\t' = BSC.pack "\\t"
        escape '\r' = BSC.pack "\\r"
        escape '\b' = BSC.pack "\\b"
        escape '\f' = BSC.pack "\\f"
        escape '\\' = BSC.pack "\\\\"
        escape c    = BSC.pack [c]

finishInserts :: InsertState -> Parser (InsertState, Entry)
finishInserts Nothing = mzero
finishInserts (Just _) = (string "/****" <|> string "SET") >> ignoreLine >> return (Nothing, Just "\\.\n")

-----------------------------------
-- CREATE TABLE 
createTable :: Parser Entry
createTable = 
  do
    tableName <- string "CREATE TABLE [dbo]." *> identifier <* string "(" <* endOfLine
    columns <- many1 (column <* ignoreLine)
    let cs = intercalate ",\n" $ map showColumn columns
    let result = concat $ ["CREATE TABLE ", showIdentifier tableName, " (", "\n",  cs, "\n);\n"]
    return (Just result)

-- ALTER TABLE 
alterTable :: Parser Entry
alterTable =
  do
    tableName <- string "ALTER TABLE [dbo]." *> identifier <* string " ADD "
    c <- column
    endOfLine
    let result = concat $ ["ALTER TABLE ", showIdentifier tableName, " ADD COLUMN ", showColumn c, ";\n"]
    return (Just result)


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
ctype = ColumnType <$> identifier <*> option Nothing ( Just <$> (char '(' *> takeWhile1 isDigit <* char ')'))

data Column = Column { columnName :: BS.ByteString, columnType :: ColumnType, columnQualifiers :: [BS.ByteString] } deriving Eq
column :: Parser Column
column =  Column <$> (skipSpace *> identifier  <* char ' ') <*> (ctype <* char ' ' ) <*> (many' cqualifier)
showColumn :: Column -> ByteString
-- Handle some fields specially
showColumn (Column "TimeStamp" _ _) = intercalate " " $ ["    ", "\"TimeStamp\"", "BIGINT", "NULL"]
showColumn (Column "Photo" _ _)     = intercalate " " $ ["    ", "\"Photo\"",     "TEXT",   "NULL"]
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
                return Nothing


