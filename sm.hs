module StateM where
data SM s a = SM (s -> (a,s))  -- The monadic type

instance Monad (SM s) where
  -- defines state propagation
  SM c1 >>= fc2         =  SM (\s0 -> let (r,s1) = c1 s0 
                                          SM c2 = fc2 r in
                                         c2 s1)
  return k              =  SM (\s -> (k,s))

 -- extracts the state from the monad
readSM                  :: SM s
readSM                  =  SM (\s -> (s,s))

 -- updates the state of the monad
updateSM                :: (s -> s) -> SM ()  -- alters the state
updateSM f              =  SM (\s -> ((), f s)) 

-- run a computation in the SM monad
runSM                   :: s -> SM a -> (a,s)
runSM s0 (SM c)         =  c s0

