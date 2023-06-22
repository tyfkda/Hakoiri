module Util
    ( breadthFirstSearch
    ) where
import Data.List (unfoldr)

breadthFirstSearch :: (n -> s -> ([n], [n], s)) -> s -> [n] -> [n]
breadthFirstSearch expfn state nodes = unfoldr f (nodes, state)
    where
        f ([], _)    = Nothing
        f (n: nn, s) =
            let (bef, aft, s') = expfn n s
             in Just (n, (bef ++ nn ++ aft, s'))
