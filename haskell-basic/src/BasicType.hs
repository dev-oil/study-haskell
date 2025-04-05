-- 1. 정수형 (정수형: Int, 큰 수용: Integer)
valueInt :: Int 
valueInt = 42

valueInteger :: Integer
valueInteger = 21354646978654654

-- 2. 실수 (Float, Double)
valueFloat :: Float
valueFloat = 3.14

valueDouble :: Double
valueDouble = 2.47654654

-- 3. 문자열 (String), 문자 (Char) (c랑 비슷하군...)
valueChar :: Char
valueChar = '가'

valueString :: String
valueString = "하스켈 짱!!!"

-- 4. 불리언 (Bool)
valueBool :: Bool
valueBool = True

-- 5. 리스트 (List) - 동일한 타입의 요소들을 순서대로 나열한 구조
listChars :: [Char]
listChars = ['가', '나', '다']

listInts :: [Int]
listInts = [1, 2, 3, 4, 5]

listStrings :: [String]
listStrings = ["하스켈", "짱", "배워요"]

-- 6. 튜플 (Tuple) - 서로 다른 타입의 값들을 한 묶음으로 만드는 구조
-- 리스트와 달리, 원소의 개수와 타입이 고정되어 있음
pair :: (String, Int)
pair = ("하스켈", 2025)

triple :: (Int, Bool, String)
triple = (1, True, "하이")

main :: IO () 
main = do
  print valueInt -- print는 어떤 값이든 문자열로 바꿔서 출력해주는 IO 함수 / (putStrLn은 오직 String만 받을 수 있음)
  print valueInteger
  print valueFloat
  print valueDouble
  putStrLn [valueChar]
  putStrLn valueString
  print valueBool

  -- 리스트
  putStrLn listChars
  print listInts
  mapM_ putStrLn listStrings -- mapM_ 은 리스트 안의 각 요소에 putStrLn 을 적용시켜줌 -- 부작용이 있는 함수를 리스트에 적용하고, 결과는 필요 없을 때 사용함 forEach 사용할 때 vs map 사용할 때 생각해보면 되겠슴

  -- 튜플
  -- pair
  let (name, year) = pair
  putStrLn ("이름: " ++ name)
  putStrLn ("년도: " ++ show year)

  -- triple
  let (n, b, msg) = triple
  putStrLn ("숫자: " ++ show n) -- show ? 값을 사람이 읽을 수 있는 문자열로 바꿔주는 함수
  putStrLn ("불리언: " ++ show b)
  putStrLn ("문자열: " ++ msg)