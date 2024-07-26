-- TODO: Remove this because it will be explained during the modules and multiple file tutorial
-- module Test (func, func2) where -- export func and func2
-- module Test where -- export everything (if module line not present, everything is available but :browse will only show main)

func2 :: Int -> Int
func2 x = x + 2

func :: Int -> Int
func x = x + 1

main :: IO ()
main = do
  putStrLn "Hello World"
  putStrLn "Hello World, again"
  print $ func2 2
