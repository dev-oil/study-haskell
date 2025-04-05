module Main (main) where -- 이 파일이 Main이라는 모듈이라는 뜻 / 괄호 안의 main은 이 모듈에서 외부에 노출(export) 되는 함수 / 보통 프로그램의 시작점이라 반드시 main이라는 이름을 씀

main :: IO () -- IO는 "입출력이 있는" 연산이라는 뜻 / ()는 "아무 값도 반환하지 않는다"는 뜻 (void와 비슷한 느낌)
main = do -- do는 IO 작업을 여러 줄로 나열할 때 사용하는 문법
  putStrLn "hello world" -- putStrLn은 문자열을 콘솔에 출력해주는 함수
