do x <- [1,2,3]
   y <- [1,2,3]
   True <- return (x /= y)
   return (x,y)
