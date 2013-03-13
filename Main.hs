{-# LANGUAGE OverloadedStrings #-}
{- SQL Server ouput converted into PostgreSQL
 -
 - Uses Parsec to do the parsing.
 - Reads standard input, writes to standard output
 -
 -}

module Main where

import           Control.Applicative hiding (many)
import           Data.Attoparsec.ByteString hiding (takeWhile1)
import           Data.Attoparsec.ByteString.Char8 (endOfLine, isEndOfLine, skipSpace, char, isDigit, takeWhile1)
import qualified Data.Attoparsec.ByteString.Lazy as AL

import qualified Data.ByteString as BS
import           Data.ByteString.Lazy hiding (map)
import           Data.Maybe (catMaybes)

import           Prelude hiding (takeWhile, concat, interact)

-- Entry captures something interesting, or Nothing
type Entry = Maybe ByteString

main :: IO ()
main = interact ( concat . map (flip append "\n") . catMaybes . parseSQL)

-- Lazy parser, converts a lazy bytestring into a lazy string of Entries 
parseSQL :: ByteString -> [Entry]
parseSQL contents =
  case AL.parse parseEntry contents of
    AL.Fail {} -> []
    AL.Done contents' entry -> entry : parseSQL contents'

-- Main parser, tries to find something interesting
parseEntry :: Parser Entry
parseEntry = createTable <|> alterTable <|> ignoreLine


-----------------------------------
-- INSERT TO COPY

-- when starting an insert sequence, convert it into a copy....



-----------------------------------
-- CREATE TABLE 
createTable :: Parser Entry
createTable = 
  do
    tableName <- string "CREATE TABLE [dbo]." *> identifier <* string "(" <* endOfLine
    columns <- many1 (column <* ignoreLine)
    let cs = intercalate ",\n" $ map showColumn columns
    let result = concat $ ["CREATE TABLE \"", fromStrict tableName, "\" (", "\n",  cs, "\n);\n"]
    return (Just result)

-- ALTER TABLE 
alterTable :: Parser Entry
alterTable =
  do
    tableName <- string "ALTER TABLE [dbo]." *> identifier <* string " ADD "
    c <- column
    endOfLine
    let result = concat $ ["ALTER TABLE \"", fromStrict tableName, "\" ADD COLUMN ", showColumn c, ";\n"]
    return (Just result)


identifier :: Parser BS.ByteString
identifier = char '[' *> (AL.takeWhile $ inClass "A-Za-z0-9_-")  <* char ']'

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
-- Handle TimeStamp special. Set NULL
showColumn (Column "TimeStamp" _ _) = intercalate " " $ ["    ", "\"TimeStamp\"", "BIGINT", "NULL"]
showColumn (Column cn ct cqs) =
    let  col = concat $ ["\"", fromStrict cn, "\""]
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


