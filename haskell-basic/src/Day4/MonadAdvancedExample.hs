{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- main 모듈 정의
module Main where

-- 필요한 모듈 import
import Control.Monad.IO.Class (MonadIO, liftIO)          -- IO 리프팅
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)  -- Reader 모나드
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell) -- Writer 모나드

-- 1. 사용자 정의 타입 클래스 선언
-- 이 타입 클래스는 "UserRepository 기능을 제공하는 모나드"라는 뜻
class UserRepository m where
  findNameById :: Int -> m String  -- ID로 이름을 찾아주는 인터페이스

-- 2. App 모나드 정의 (Reader + Writer + IO)
-- 이 App 타입은 여러 모나드 효과를 조합한 '모나드 스택'
newtype App a = App { unApp :: ReaderT Int (WriterT String IO) a }
  -- App은 ReaderT Int → WriterT String → IO 순서로 쌓인 구조
  deriving (Functor, Applicative, Monad, MonadReader Int, MonadWriter String, MonadIO)
  -- 자동으로 모나드 관련 기능을 파생시킴

-- 3. UserRepository 인스턴스 구현
-- App 모나드는 UserRepository 역할도 할 수 있어야 하니까 여기서 정의
instance UserRepository App where
  findNameById _ = pure "DevOil"
  -- 여기선 단순히 항상 "DevOil"을 반환함 (실제 DB라면 ID로 조회했겠지!)

-- 4. 비즈니스 로직 함수
-- Reader, Writer, IO, UserRepository 기능을 모두 지원하는 모나드에서 실행 가능
app :: (MonadReader Int m, MonadWriter String m, MonadIO m, UserRepository m) => m Int
app = do
  x <- ask                 -- Reader로부터 환경값(Int)을 읽어옴
  userId <- liftIO readLn -- IO 모나드로 사용자 입력 받기 (liftIO로 끌어올림)
  userName <- findNameById userId -- 타입 클래스에서 제공하는 이름 찾기
  tell $ "userId: " <> show userId <> ", userName: " <> userName
  -- Writer에 로그를 기록함
  pure $ x + userId        -- x(환경값) + 입력받은 ID를 결과로 반환

-- 5. main 함수
-- Reader + Writer 모나드를 실행하고 결과를 출력하는 함수
main :: IO ()
main = do
  putStrLn "Enter user ID:" -- 안내 문구 출력
  let writer = runReaderT (unApp app) 40
  -- ReaderT 실행: 환경값 40을 넘김
  (result, logs) <- runWriterT writer
  -- WriterT 실행: 로그와 결과 분리해서 받음
  putStrLn "\n[LOG]"
  putStrLn logs
  putStrLn "[RESULT]"
  print result
