import Process
import System

data Notify : Type -> Type where
     Answer : Integer -> Notify ()

factWorker : Integer -> (pid : ProcID Notify) -> Worker [pid] ()
factWorker x pid = do usleep 5000000
                      Send pid (Answer (cast (fact (cast x))))
                      Disconnect pid

instance Cast String Nat where
    cast orig = cast (the Integer (cast orig))

GetWork : Maybe Integer -> Running (Maybe Integer) Notify 
GetWork res = do 
             res' <- TimeoutRespond {res = Maybe Integer} 1 Nothing
                          (\val => case val of
                                        Answer res => Pure ((), Just res))
             putStrLn ("Result: " ++ show res')
             c <- CountClients
             case c of
                  Z => Pure (res <+> res')
                  S k => Loop (GetWork (res <+> res'))
                          

testSlowWorker : Program () Notify
testSlowWorker = do putStr "Number> "
                    x <- getLine
                    res <- Work (factWorker (cast x))
                                (GetWork Nothing)
                    putStrLn $ "The answer was " ++ show res
                    Quit ()

main : IO ()
main = run testSlowWorker

