import GHC.Base (TrName(TrNameD))
import Language.Haskell.TH (safe)
import Data.List (subsequences)

diffs [] = []
diffs xs = zipWith (-) (tail xs) xs

signsMatch :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> Bool
signsMatch a b = (a < 0) == (b < 0)

safeReport [] = True
safeReport [x]  = True
safeReport xs = all (\a -> signsMatch d a && abs a < 4 && a /= 0) (d:ds)
    where (d:ds) = diffs xs

tryAllSubs xs =
    any safeReport . filter (\l -> length l == length xs - 1) $ subsequences xs

-- Function to try safeReport on all sub lists missing one element



main = 
    print . length . filter (\l -> safeReport l || tryAllSubs l) . map (map readInt . words) . lines =<< readFile "input.txt" 

readInt :: String -> Int
readInt = read