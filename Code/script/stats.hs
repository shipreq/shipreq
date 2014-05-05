#!/usr/bin/runghc

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import System.Directory
import System.Posix.Files
import System.Process
import Text.Printf

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a,c) = (f a, c)

------------------------------------------------------------------------------------------------------------------------
-- Dirs & cloc

joinDirs :: FilePath -> FilePath -> FilePath
joinDirs "." b = b
joinDirs a   b = a ++ "/" ++ b

dirEntriesIn :: FilePath -> IO [FilePath]
dirEntriesIn d = (addPrefix . ignoreDots) <$> getDirectoryContents d
  where addPrefix = map $ joinDirs d
        ignoreDots = filter $ not . flip elem [".",".."]

dirsIn :: FilePath -> IO [FilePath]
dirsIn = (filterM isDir =<<) . dirEntriesIn

isDir :: FilePath -> IO Bool
isDir = (isDirectory <$>) . getFileStatus

cloc :: [FilePath] -> IO Stat
cloc = (parseCloc <$>) . runClocS

runClocS :: [FilePath] -> IO String
runClocS fs = p <$> readProcessWithExitCode "cloc" fs [] where p (_,stdout,_) = stdout

parseCloc :: String -> Stat
parseCloc a = parse $ listToMaybe $ filter (isPrefixOf "Scala") $ lines a
  where parse Nothing  = emptyStat
        parse (Just l) = Stat (col 1) (col 4) where col = read . (words l !!)

------------------------------------------------------------------------------------------------------------------------
-- Stats gathering

groups = ["base", "taskman", "webapp"]
mainPaths = ["src/main/scala"]
testPaths = ["src/test-lib/scala", "src/test/scala"]

type Group = String
type Module = String
type Stats = (Stat,Stat)

data GroupD = GroupD { gname :: Group, modstats :: [(Module, Stats)] } deriving (Show)

data Stat = Stat { files :: Int, loc :: Int } deriving (Show, Eq)
emptyStat = Stat 0 0
instance Monoid Stat where
  mappend a b = Stat (files a + files b) (loc a + loc b)
  mempty = emptyStat

isEmpty :: Stat -> Bool
isEmpty (Stat a b) = a==0 && b==0

areEmpty :: Stats -> Bool
areEmpty (a,b) = isEmpty a && isEmpty b

modulesFor :: Group -> [FilePath] -> [Module]
modulesFor g = sort . filter (isPrefixOf g)

statForModule :: [FilePath] -> Module -> IO Stat
statForModule dirs m = cloc $ map (joinDirs m) dirs

statsForModule :: Module -> IO Stats
statsForModule m = do a <- statForModule mainPaths m
                      b <- statForModule testPaths m
                      return $ if isSuffixOf "-test" m
                        then (emptyStat, mappend a b)
                        else (a,b)

groupD :: [FilePath] -> Group -> IO GroupD
groupD dirs g = let
  a = modulesFor g dirs
  b = mapM statsForModule a -- IO [Stats]
  c = zip a <$> b           -- IO [(Module, Stats)]
  d = filter (not . areEmpty . snd) <$> c
  in GroupD g <$> d

gatherAllStats :: IO [GroupD]
gatherAllStats = do dirs <- dirsIn "."
                    mapM (groupD dirs) groups

------------------------------------------------------------------------------------------------------------------------
-- Printing stats

header = "                      |       Files     |            LoC\n"
       ++"                      |    M    T    ∑  |      M      T      ∑  (T:M)\n"
sepLine= "----------------------+-----------------+----------------------------\n"

float i = fromIntegral i :: Float

testRatio (Stat _ m, Stat _ t) = float t / float m

testRatioS (Stat _ 0, _) = " - "
testRatioS s = printf "%.1f" $ testRatio s

fmtGroup (GroupD _ ms) = map fmtMS ms
fmtMS (m,s) = printf "%-21s | %s  | %s  (%s)\n" m (fmtPF s) (fmtPL s) (testRatioS s)

fmtP :: String -> (Stat -> Int) -> Stats -> String
fmtP prec f (a,b) =
  let d = "%"++prec++"d"
      p = d++" "++d++" "++d
      t = mappend a b
  in printf p (f a) (f b) (f t)

fmtPF = fmtP "4" files
fmtPL = fmtP "6" loc

fmtGroups = (concat . fmtGroup =<<)

groupStats (GroupD _ ms) = mconcat $ map snd ms

singleModuleGroup name stats = GroupD name [(name,stats)]

consolidateGroup gd @ (GroupD g _) = singleModuleGroup g $ groupStats gd

totalStatsForGroups gs = mconcat (map groupStats gs)
consolidateGroups = singleModuleGroup "∑" . totalStatsForGroups

fmtBreakdowns = intercalate sepLine

-- Logic vs Impl

headerIL = "Logic & Impl          |    L    I    ∑  |      L      I      ∑  (I:L)\n"

modulesWithSuffix suf = filter (isSuffixOf suf . fst) . concatMap modstats

dropr n = reverse . drop n . reverse

modulesWithSuffix' suf = map (mapFst $ dropr $ length suf) . modulesWithSuffix suf

logicAndImpl gs = let l = modulesWithSuffix' "-logic" gs
                      i = modulesWithSuffix' "-impl" gs
                      keys = sort $ nub $ map fst $ l ++ i
                      get xs k = s where Just (_,(s,_)) = find ((== k) . fst) xs
                      getLI k = singleModuleGroup k (get l k, get i k)
                  in  map getLI keys

logicAndImplWithTotal gs = a ++ [consolidateGroups a] where a = logicAndImpl gs

------------------------------------------------------------------------------------------------------------------------

sampleData = [GroupD "base" [("base-db",(Stat {files = 5, loc = 287},Stat {files = 0, loc = 0})),("base-test",(Stat {files = 0, loc = 0},Stat {files = 3, loc = 115})),("base-util",(Stat {files = 12, loc = 769},Stat {files = 3, loc = 244}))],GroupD "taskman" [("taskman-api-impl",(Stat {files = 3, loc = 103},Stat {files = 4, loc = 72})),("taskman-api-logic",(Stat {files = 7, loc = 136},Stat {files = 2, loc = 95})),("taskman-server-impl",(Stat {files = 15, loc = 959},Stat {files = 5, loc = 408})),("taskman-server-logic",(Stat {files = 11, loc = 587},Stat {files = 5, loc = 413}))],GroupD "webapp" [("webapp",(Stat {files = 133, loc = 8388},Stat {files = 70, loc = 8239}))]]

main :: IO ()
main = do putStrLn "Analysing..."
          -- gs <- pure sampleData
          gs <- gatherAllStats
          -- putStrLn $ show gs
          putStrLn $ fmtBreakdowns [
            header
            ,fmtGroups gs
            ,fmtGroups $ map consolidateGroup gs
            ,fmtGroups [consolidateGroups gs]
            ]
          putStrLn "\n"
          putStrLn $ fmtBreakdowns [
            headerIL
            ,fmtGroups $ logicAndImpl gs
            ,fmtGroups [consolidateGroups $ logicAndImpl gs]
            ]

