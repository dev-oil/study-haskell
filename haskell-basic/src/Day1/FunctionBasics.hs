-- 1. 기본 함수 정의
add :: Int -> Int -> Int -- 두 개의 Int를 받아서 Int를 반환한다는 의미
add x y = x + y

-- 2. 부분 적용
-- 커링이 기본이라서, 아래처럼도 쓸 수 있다고 함
-- 커링 이란 ? "여러 개의 인자를 받는 함수를, 하나의 인자만 받는 함수들의 연속으로 바꾸는 것"
add2 :: Int -> Int
add2 = add 2 -- x에 2를 고정시킨 함수

-- 3. 람다(lamda) 함수로 정의하기
add3 :: Int -> Int -> Int
add3 = \x y -> x + y

-- 4. 중위 함수로 사용하기
result :: Int
result = 1 `add` 2 -- 이렇게 backtick()으로 감싸면 add`를 중위 연산자 처럼 사용할 수 있음

-- 5. 중위 함수 직접 만들기
-- 하스켈은 기호로 된 함수(예: +++)를 중위 연산자처럼 정의할 수 있음
(+++) :: Int -> Int -> Int
(+++) x y = x + y

result2 :: Int
result2 = 1 +++ 2   -- 중위 사용

result3 :: Int
result3 = (+++) 3 4 -- 전위 함수처럼도 가능

-- 6. 섹션 (Section)
-- 중위 함수의 부분 적용
-- 하스켈에서는 중위 연산자 양쪽 중 한쪽만 고정해서 함수처럼 만드는 문법이 있다.
addOne :: Integer -> Integer
addOne = (+ 1)   -- 오른쪽 1을 고정
onePlus :: Integer -> Integer
onePlus = (1 +)  -- 왼쪽 1을 고정


main :: IO ()
main = do
  print (add 2 3) 
  print (add2 3) -- add2는 y만 받는 함수
  print (result) -- 3
  print (result2) -- 3
  print (result3) -- 7
  print (addOne 2)   -- 3
  print (onePlus 2)  -- 3

