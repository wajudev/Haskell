seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return(x:xs)

-- seqn :: [IO a] -> IO [a]
seqn :: Monad m => [m a] -> m [a]