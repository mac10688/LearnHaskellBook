module GreetifCool1 where

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyy. What's chakin'?"
  else  putStrLn "pshhh."
  where cool = coolness == "downright frosty yo"



