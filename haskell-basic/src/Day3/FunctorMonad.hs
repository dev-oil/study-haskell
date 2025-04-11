-- Functor & Monad 실습 정리

-- Optional 타입 정의 (Maybe와 유사)
data Optional a
  = Value a
  | Null
  deriving (Show)

-- Optional에 함수 적용 (Functor 역할)
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional f (Value x) = Value (f x)
mapOptional _ Null      = Null

-- 리스트에 함수 적용 (map과 동일)
mapList :: (a -> b) -> [a] -> [b]
mapList = map

-- Functor 타입 클래스 흉내내기
class Mapable f where
  mapAll :: (a -> b) -> f a -> f b

-- Optional에 대한 Mapable 인스턴스
instance Mapable Optional where
  mapAll = mapOptional

-- 리스트에 대한 Mapable 인스턴스
instance Mapable [] where
  mapAll = mapList

-- 실제 하스켈 Functor 타입 클래스
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Maybe는 하스켈에서 Optional과 같음
-- data Maybe a = Just a | Nothing
-- fmap = mapOptional 역할 수행

-- Either 타입: 실패 정보를 함께 담을 수 있음
data MyEither a b = MyLeft a | MyRight b deriving (Show)

-- 예시 Either 값들
failValue :: MyEither String Int
failValue = MyLeft "Invalid Number"

successValue :: MyEither String Int
successValue = MyRight 1

-- Config 타입 정의 (Reader 모나드 기반)
newtype Config r a = Config { runConfig :: r -> a }

-- 항상 42를 반환하는 Config 예시
constConfig :: Config r Int
constConfig = Config { runConfig = \_ -> 42 }

-- 진짜 값을 꺼내보기
testConfigResult :: Int
testConfigResult = runConfig constConfig "hello" -- 결과: 42

-- Functor 한계 예시: 중첩된 Maybe
phones :: [(String, String)]
phones = [("123-123-123", "Todd"), ("000-000-000", "Eunmin")]

ids :: [(String, Int)]
ids = [("Todd", 32), ("Eunmin", 424)]

ages :: [(Int, Int)]
ages = [(32, 100), (42, 50)]

-- 펑터로 구현 (중첩 발생)
getAgeByPhone_functor :: String -> Maybe (Maybe (Maybe Int))
getAgeByPhone_functor phone =
  ( \name ->
      ( \id ->
          lookup id ages
      ) <$> lookup name ids
  ) <$> lookup phone phones

-- 모나드로 간단하게 구현
getAgeByPhone_monad :: String -> Maybe Int
getAgeByPhone_monad phone =
  lookup phone phones >>= \name ->
    lookup name ids >>= \id ->
      lookup id ages

-- 모나드 do 구문 버전
getAgeByPhone_do :: String -> Maybe Int
getAgeByPhone_do phone = do
  name <- lookup phone phones
  id <- lookup name ids
  age <- lookup id ages
  return age

-- inc 함수
type Name = String
inc :: Num a => a -> a
inc x = x + 1

-- 실습 13: inc 적용하여 getAge'
getAge' :: Name -> Maybe Int
getAge' name = do
  id <- lookup name ids
  let id' = inc id
  age <- lookup id' ages
  return age

-- 실습 14: 리스트를 모나드로 다루기
foo :: [Int] -> [String]
foo xs = xs >>= (\x -> return (show (inc x)))
-- 또는 fmap / do 스타일로도 가능
-- foo xs = do { x <- xs; return (show (inc x)) }

-- 실습 19: Config로 외부 환경에서 값 꺼내기
myConfig :: Config String Int
myConfig = Config (\_ -> 42)

useMyConfig :: Int
useMyConfig = runConfig myConfig "외부환경"

-- Functor 기본 예제
-- 값을 감싼 컨테이너에 함수 적용하기
exampleFunctor1 :: Maybe Int
exampleFunctor1 = fmap inc (Just 1)  -- 결과: Just 2

exampleFunctor2 :: [Int]
exampleFunctor2 = fmap inc [1, 2, 3] -- 결과: [2, 3, 4]

-- Monad 기본 예제
-- 값을 꺼내서 함수 적용 후 다시 감싸기
exampleMonad1 :: Maybe Int
exampleMonad1 = Just 1 >>= (\x -> Just (inc x)) -- 결과: Just 2

exampleMonad2 :: [Int]
exampleMonad2 = [1, 2, 3] >>= (\x -> [x, x + 10]) -- 결과: [1,11,2,12,3,13]


main :: IO ()
main = do
  putStrLn "hello world"
