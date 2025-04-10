{-# LANGUAGE DeriveAnyClass #-}

-- 1. 타입 변수와 identity 함수
-- identity
-- 들어온 값을 그대로 돌려주는 함수
identity :: a -> a
identity x = x

-- second
-- 두 개의 값을 받되, 두 번째 값만 반환하는 함수
-- a와 b는 서로 다른 타입이어도 됨
second :: a -> b -> b
second _ y = y

-- 2. 타입 클래스 예시
inc :: Num a => a -> a -- Num a => 부분이 바로 타입 클래스 제약 즉, a는 반드시 Num 타입 클래스의 인스턴스여야 함
inc x = x + 1

-- 3. 타입 클래스 인스턴스 만들기
data MyValue = MyValue Int deriving Show

instance Num MyValue where
  (+) (MyValue x) (MyValue y) = MyValue (x + y)
  -- 나머지 함수는 생략했지만, 경고만 나고 컴파일은 됨

value1 :: MyValue
value1 = MyValue 1

value2 :: MyValue
value2 = MyValue 2

-- 4. Point에 Num 인스턴스 만들기
data Point = Point Int Int deriving Show

instance Num Point where
  (+) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- 5. 타입 클래스 직접 선언
class YesNo a where
  yesno :: a -> Bool
  yesno _ = False

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

-- 6. Optional 타입 정의 및 일반화
-- Optional 은 마치.. Maybe 같은것.. (null | T)
data Optional a = Value a | Null deriving (Show)

instance (Num a) => Num (Optional a) where
  (+) (Value x) (Value y) = Value (x + y)
  (+) _ _ = Null

-- 7. Optional 응용
incOptional :: (Num a) => Optional a -> Optional a
incOptional = mapOptional inc

hello :: String -> String
hello = ("Hello " ++)

helloOptionalString :: Optional String -> Optional String
helloOptionalString = mapOptional hello

-- 8. mapOptional 구현
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Null = Null
mapOptional f (Value x) = Value (f x)

main :: IO ()
main = do
  -- identity & second
  print $ identity "Hi"
  print $ second 42 "World"

  -- inc 함수
  print $ inc 10
  print $ inc 3.14

  -- MyValue
  print $ value1 + value2

  -- Point (+)
  print $ Point 1 2 + Point 3 4

  -- YesNo 타입클래스
  print $ yesno (0 :: Int)
  print $ yesno (123 :: Int)
  print $ yesno True
  print $ yesno ([] :: [Int])
  print $ yesno [1, 2, 3]

  -- Optional + mapOptional
  print $ Value 1 + Value 2
  print $ Value 1 + Null
  print $ incOptional (Value 3)
  print $ incOptional Null
  print $ helloOptionalString (Value "Haskell")
  print $ helloOptionalString Null
