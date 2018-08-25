module Util where
  
takeBy cnt [] = []
takeBy cnt xs = (take cnt xs : takeBy cnt (drop cnt xs))
