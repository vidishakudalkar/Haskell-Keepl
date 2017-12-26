keepl k (x:xs) 
  |k==0 = error "Cannot slice on 0" -- Slicing at 0 should not return any value or an empty list but we have displayed an error message
  |k==1 = (x:xs) -- Every element will be returned as step is 1
  |k>1 = let m=k in keeps k m (x:xs) -- Creating a new function so as to keep the variable k intact and decrementation is performed on m
  |otherwise = error "Number cannot be negative" -- Negative number is not accepted as k


len [] = 0  -- Length function 
len (x:xs) = 1 + len xs

keeps k m (x:xs) 
  |k==0 = []
  |k==1 = if (len(xs)/m)>=1 then x:[]++ keepl m xs else x:[]
  |otherwise = keeps (k-1) m xs
