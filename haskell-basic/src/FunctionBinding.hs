-- 1. $ 연산자
-- 괄호() 대신 쓰는 연산자

data Shape = Rect Double Double | Circle Double deriving (Show, Eq)

area :: Shape -> Double
area (Rect w h) = w * h
area (Circle r) = pi * r * r

-- 2. where 구문
-- 함수 정의의 맨 아래에서 보조 변수들을 정의하는 방식 (함수 밖)
-- 함수 전체에서 사용
-- 함수 정의 단순화
-- 예시: 원의 면적 2배 구하기
doubleArea :: Double -> Double
doubleArea r =
  area * 2
  where
    area = pi * r * r
    pi = 3.14

-- 3. let-in 구문
-- 표현식 안에서 값을 정의한 다음에, 그걸 이용하는 방식 (함수 안)
-- in 뒤의 표현식에서만 사용
-- 계산 순서를 명확히 하고 싶을 때
-- 예시: 반지름 주고 면적 계산
area2 :: Double -> Double
area2 r = 
  let pi = 3.14
  in pi * r * r

-- 조건 분기
-- 1. if ... then ... else ...
foo :: Int -> String
foo x =
  if x > 10
    then "많다"
    else "적다" -- else는 항상 필수임! (하스켈에선 if도 표현식이기 때문)

-- 2. 가드
-- 여러 조건을 정리해서 분기할 때 아주 가독성 좋음 | 기호로 조건을 나열하고, 마지막은 otherwise
foo2 :: Int -> String
foo2 x
  | x > 10     = "많다"
  | x > 5      = "보통"
  | otherwise  = "적다" -- otherwise는 항상 True인 값 (즉, 자바스크립트의 else)

-- 3. case 구문
-- 값을 직접 분해해서 조건 분기 특히 패턴 매칭이 필요한 경우에 사용함
describeBool :: Bool -> String
describeBool b =
  case b of -- 변수 b의 값이 True면 "참이야"를 반환 False 면 "거짓이야" 반환
    True  -> "참이야"
    False -> "거짓이야"

-- 리스트 함수
-- head [1 .. 4]
-- -- 1
-- last [1 .. 4]
-- -- 4
-- tail [1 .. 4]
-- -- [2,3,4]
-- init [1 .. 4]
-- -- [1,2,3]
-- take 2 [1 .. 4]
-- -- [1,2]
-- drop 2 [1 .. 4]
-- -- [3,4]
-- null []
-- -- True
-- 1 `elem` [1 .. 4]
-- -- True
-- [1 .. 4] !! 3
-- -- 4
-- concat [[1, 2], [], [3, 4]]
-- -- [1,2,3,4]
-- map (+1) [1 .. 4]
-- -- [2,3,4,5]
-- filter (>2) [1 .. 4]
-- -- [3,4]
-- foldr (+) 0 [1 .. 4]
-- -- 10

-- 리스트 컴프리헨션
-- 기본 구조
-- [ 결과식 | 변수 <- 리스트, 조건 ]


main :: IO ()
main = do
  -- 기본 예시
  print (sqrt (3 + 4 + 5))      -- 괄호 많음
  print $ sqrt (3 + 4 + 5)      -- 괄호 하나 제거
  print $ sqrt $ 3 + 4 + 5      -- 괄호 모두 제거

  -- 중첩된 함수 호출
  print (abs (negate (10)))        -- 원형
  print $ abs (negate 10)          -- $ 한 번 사용
  print $ abs $ negate 10          -- $ 두 번 사용

  -- area와 결합해서 사용하기
  print $ area $ Rect 1.0 1.0 -- 원래는 이런식으로 사용 print (area (Rect 2.0 3.0))
  print $ area $ Circle 2.0

  -- 리스트 함수
  print $ head [10, 20, 30] -- 첫 번째 요소
  print $ tail [10, 20, 30] -- 첫 번째 제외 나머지
  print $ take 2 [1..5] -- 앞에서 n개
  print $ drop 2 [1..5] -- 앞에서 n개 버림
  print $ null [] -- 비었는지 검사
  print $ 3 `elem` [1, 2, 3, 4] -- 포함여부
  print $ [1, 2, 3, 4] !! 2 -- 인덱스 접근

  

  -- 고차 함수들
  print $ map (*2) [1,2,3] -- 맵 함수
  print $ filter odd [1..10] -- 필터 함수 + odd (홀수만)
  print $ foldr (+) 0 [1,2,3,4] -- 오른쪽부터 누적 계산	

  -- 리스트 컴프리헨션 (리스트 통합)
  print [ x * 2 | x <- [1..5] ]                    -- [2,4,6,8,10]
  print [ x | x <- [1..10], odd x ]                -- 홀수만 필터링 [1,3,5,7,9]
  print [ (x, y) | x <- [1,2], y <- [3,4] ]         -- 모든 조합 (중첩 for문) [(1,3),(1,4),(2,3),(2,4)]
  print [ x | x <- [1..20], x `mod` 3 == 0 ]        -- 3의 배수 [(1,3),(1,4),(2,3),(2,4)]

