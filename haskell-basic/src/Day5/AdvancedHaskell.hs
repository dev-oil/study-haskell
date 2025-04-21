{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

-- 📦 기본 import
import Control.Applicative ((<*>), (<$>))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Control.Monad (forM_)
import GHC.Generics (Generic)
import Control.Lens ((&), (.~))
import Data.Text (Text)
import qualified Data.Text as T

-- 📘 ExceptT 관련
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO) 
import Control.Error.Util (note)

-- 📅 시간 관련
import Data.Time (UTCTime, getCurrentTime)
import Data.ULID (ULID, getULID)

--------------------------------
-- ✅ 1. 어플리커티브 실습
--------------------------------

ages :: [(Int, Int)]
ages = [(32, 100), (42, 50)]

sumAges :: Int -> Int -> Maybe Int
sumAges id1 id2 = (+) <$> lookup id1 ages <*> lookup id2 ages

--------------------------------
-- ✅ 2. Semigroup / Monoid
--------------------------------

semigroupExample :: String
semigroupExample = "Hello" <> " " <> "World"

monoidExample :: [Int]
monoidExample = mempty <> [1, 2, 3]

--------------------------------
-- ✅ 3. Traverse 실습
--------------------------------

ids :: [(String, Int)]
ids = [("John", 1), ("Jane", 2), ("Jim", 3)]

names :: [String]
names = ["John", "Jane", "Jim"]

lookupAges :: [Maybe Int]
lookupAges = map (`lookup` ids) names

traverseAges :: Maybe [Int]
traverseAges = sequence lookupAges

--------------------------------
-- ✅ 4. Record Dot Syntax 실습
--------------------------------

data Author = Author
  { id :: String,
    name :: String
  } deriving (Show, Eq, Generic)

data Book = Book
  { id :: String,
    name :: String,
    author :: Author
  } deriving (Show, Eq, Generic)

author1 :: Author
author1 = Author "1" "Robert Alan Dahl"

book1 :: Book
book1 = Book "1" "On Democracy" author1

-- Getter 예시: author1.name
-- Setter 예시: book1 & #name .~ "New Name"

--------------------------------
-- ✅ 5. Task 타입 및 스마트 생성자
--------------------------------

data Task = Task
  { id :: ULID,
    content :: Text,
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime,
    completed :: Bool
  } deriving (Show, Eq)

mkTask :: ULID -> Text -> UTCTime -> Maybe Task
mkTask taskId content createdAt =
  if T.length content > 12
    then Nothing
    else Just $
      Task
        { id = taskId,
          content = content,
          createdAt = createdAt,
          completed = False,
          updatedAt = Nothing
        }

--------------------------------
-- ✅ 6. ExceptT 기반 createTask
--------------------------------

createTask :: Text -> ExceptT Text IO Task
createTask content = do
  taskId <- liftIO getULID
  now <- liftIO getCurrentTime
  liftEither $ note "Content is too long" $ mkTask taskId content now

runCreateTask :: Text -> IO ()
runCreateTask content = do
  result <- runExceptT (createTask content)
  case result of
    Left err -> putStrLn ("❌ Error: " ++ T.unpack err)
    Right task -> print task

--------------------------------
-- ✅ main 함수
--------------------------------

main :: IO ()
main = do
  putStrLn "\n--- Applicative Example ---"
  print $ sumAges 32 42

  putStrLn "\n--- Semigroup Example ---"
  putStrLn semigroupExample

  putStrLn "\n--- Monoid Example ---"
  print monoidExample

  putStrLn "\n--- Traverse Example ---"
  print lookupAges
  print traverseAges

  putStrLn "\n--- Record Dot Example ---"
  -- print $ author1.name  -- 🔴 name 필드 중복으로 인해 ambiguous error 발생

  -- let book2 = book1 & #name .~ "민주주의"        -- 🔴 모호한 #name (Book, Author 중복)
  -- let book3 = book1 & #author . #name .~ "devoil" -- 🔴 중첩 필드도 name이 겹치므로 에러 발생
  -- print book2
  -- print book3

  putStrLn "\n--- Smart Constructor + ExceptT ---"
  runCreateTask "Hello"         -- 정상
  runCreateTask "This is too long!" -- 에러