import Control.Monad
import Data.List
import GHC.Exts

main :: IO ()
main = print solves

solves :: [[Int]]
solves = do
    s1 <- [(1,1),(2,2),(3,3),(4,4)]
    let s1' = fst s1
    s2 <- [(1,3),(2,4),(3,1),(4,2)]
    let s2' = fst s2
    s3 <- [(1,3),(2,6),(3,2),(4,4)]
    let s3' = fst s3
    s4 <- [(1,(1,5)),(2,(2,7)),(3,(1,9)),(4,(6,10))]
    let s4' = fst s4
    s5 <- [(1,8),(2,4),(3,9),(4,7)]
    let s5' = fst s5
    s6 <- [(1,(2,4)),(2,(1,6)),(3,(3,10)),(4,(5,9))]
    let s6' = fst s6
    s7 <- [(1,3),(2,2),(3,1),(4,4)]
    let s7' = fst s7
    s8 <- [(1,7),(2,5),(3,2),(4,10)]
    let s8' = fst s8
    s9 <- [(1,6),(2,10),(3,2),(4,9)]
    let s9' = fst s9
    s10 <- [(1,3),(2,2),(3,4),(4,1)]
    let s10' = fst s10
    let ss = [0, s1',s2',s3',s4',s5',s6',s7',s8',s9',s10']
    guard $ snd s1 == fst s1
    guard $ snd s2 == fst s5
    guard $ notElem (ss !! snd s3) $ map (ss !!) $ filter (/= snd s3) [3,6,2,4]
    guard $ ss !! fst (snd s4) == ss !! snd (snd s4)
    guard $ fst s5 == ss !! snd s5
    guard $ s8' == ss !! fst (snd s6) && s8' == ss !! snd (snd s6)
    guard $ head (head $ sortWith length $ group $ sort $ tail ss) == snd s7
    guard $ (ss !! snd s8 - s1') `notElem` [1, -1]
    guard $ (s1' == s6') /= (s5' == ss !! snd s9)
    guard $ let sss = (sortWith length $ group $ sort $ tail ss) 
            in length (last sss) - length (head sss) == snd s10
    pure $ tail ss
