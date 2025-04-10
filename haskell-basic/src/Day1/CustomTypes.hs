-- 사용자 정의 타입만들기
-- data, newtype을 활용해 내가 원하는 새로운 타입을 정의하고, 그 타입을 이용해 의미 있는 데이터를 표현

-- 1-1. data 키워드로 새 타입 만들기
-- data → 새로운 타입을 만든다.
-- Point → 새로 만든 타입의 이름이다.
-- Point Double Double → 이 타입을 만들 때 사용할 생성자 함수의 이름과 그에 필요한 인자들이다.
-- deriving (Show, Eq) → 이 타입은 출력 가능하고(Show), 비교 가능하게(Eq) 만들어달라는 요청이다.
data Point = Point Double Double deriving (Show, Eq)

value :: Point
value = Point 1.0 1.0

-- 1-2. 생성자가 여러 개인 타입
-- Shape 정의
-- Shape 타입은 Rect, Circle 두 개의 생성자를 가짐
-- 각각 다른 인자 구조를 가짐 (Rect는 두 개, Circle은 하나)
data Shape
  = Rect Double Double
  | Circle Double
  deriving (Show, Eq)

area :: Shape -> Double
area (Rect w h) = w * h -- 디스트럭처링
area (Circle r) = pi * r * r -- 디스트럭처링

-- 1-3. newtype 사용 예제
-- newtype: 단 하나의 생성자 + 단 하나의 필드만 있을 때 최적화된 방식
newtype Value = Value { unValue :: Int } deriving (Show, Eq)

-- 1-4. 생성자 함수에 인자가 없는 타입
-- 열거형(enum)처럼, 미리 정해진 여러 가지 "값" 중 하나를 선택하는 타입을 만들 때 자주 사용한다고 함
data MyBool = MyTrue | MyFalse deriving (Show, Eq)
-- MyTrue, MyFalse는 값이자 생성자이고, 인자를 받지 않음

-- 함수의 패턴 매칭 & 디스트럭처링
-- 패턴 매칭이란?
-- 함수를 정의할 때 입력값의 구조에 따라 분기하는 문법 즉, 인자값을 “구조대로 쪼개서” 다르게 처리하는 것

-- 2-1. 기본 예시
crazyAdd :: Int -> Int -> Int
crazyAdd 0 0 = 42              -- 두 인자가 모두 0일 때
crazyAdd 1 x = x               -- 첫 인자가 1일 때
crazyAdd x y = x + y           -- 그 외에는 그냥 더함

-- 2-2. 리스트에서의 패턴 매칭 (디스트럭처링 포함)
length' :: [a] -> Int
length' [] = 0                   -- 빈 리스트
length' (_:xs) = 1 + length' xs  -- 첫 요소 무시하고 나머지 재귀 처리
-- js 에서 const [_, ...xs] = [1,2,3] // 첫 원소 무시, 나머지 가져오기

-- length' (1 : [2,3]) = 1 + length' [2,3]
-- length' (2 : [3])   = 1 + length' [3]
-- length' (3 : [])    = 1 + length' []
-- length' []          = 0   -- base case 도달
-- 최종 결과는 1 + 1 + 1 + 0 = 3

-- 2-3. 튜플에서의 패턴 매칭
fst' :: (a, b) -> a
fst' (x, _) = x -- (x, _) → 튜플에서 첫 번째 요소만 가져오고 두 번째는 무시

-- 2-4. 와일드 카드 _
describeShape :: Shape -> String
describeShape (Rect _ 0) = "높이 0인 직사각형"
describeShape (Rect 0 _) = "너비 0인 직사각형"
describeShape _          = "그 외 도형"

main :: IO ()
main = do
  print value                    -- Point 1.0 1.0
  print (value == Point 1.0 1.0) -- true
  
  print (area (Rect 2.0 3.0)) -- 6.0
  print (area (Circle 1.0))   -- 3.14159...

  let val = Value 42
  print val             -- Value 42
  print (unValue val)   -- 42

  let a = MyTrue
  let b = MyFalse
  print a               -- MyTrue
  print (a == b)        -- False
  