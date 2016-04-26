#!/usr/bin/runghc

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import System.Directory
import System.Posix.Files
import System.Process
import Text.Printf

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapFst f (a,c) = (f a, c)
mapSnd f (c,a) = (c, f a)

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

groups = ["base", "taskman", "webapp", "utils"]
pathPrefixes = ["", "shared/", "jvm/", "js/"]
mainPaths = fmap (\x -> x ++ "src/main/scala") pathPrefixes
testPaths = fmap (\x -> x ++ "src/test/scala") pathPrefixes

type Group = String
type Module = String
type Stats = (Stat,Stat)

data GroupD = GroupD { gname :: Group, modstats :: [(Module, Stats)] } deriving (Show)

data Stat = Stat { files :: Int, loc :: Int } deriving (Show, Eq)
emptyStat = Stat 0 0
instance Monoid Stat where
  mappend a b = Stat (files a + files b) (loc a + loc b)
  mempty = emptyStat

isEmpty     :: Stat -> Bool
areEmpty    :: Stats -> Bool
mergeStatsL :: Stats -> Stats
mergeStatsR :: Stats -> Stats

mergeStatsL (a,b) = (mappend a b, emptyStat)
mergeStatsR (a,b) = (emptyStat, mappend a b)

isEmpty (Stat a b) = a==0 && b==0

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
-- Top-level module stats

deps = M.fromList [
         ("webapp-server",         ["webapp-base-test", "base-db", "taskman-api"]) ,
         ("webapp-client-ww-api",  ["webapp-base"]) ,
         ("webapp-client-ww",      ["webapp-base-test", "webapp-client-ww-api"]) ,
         ("webapp-client",         ["webapp-base-test", "base-util", "webapp-client-ww-api"]) ,
         ("webapp-base-test",      ["webapp-base-server"]) ,
         ("webapp-base-server",    ["webapp-base"]),
         ("webapp-base",           ["webapp-macro", "base-util"]) ,
         ("webapp-macro",          ["base-util"]) ,
         ("taskman",               ["taskman-api", "taskman-server"]) ,
         ("taskman-api",           ["taskman-api-impl", "taskman-api-logic"]) ,
         ("taskman-api-impl",      ["taskman-api-logic"]) ,
         ("taskman-api-logic",     ["base-util"]) ,
         ("taskman-server",        ["taskman-server-logic", "taskman-server-schema", "taskman-server-impl"]) ,
         ("taskman-server-impl",   ["taskman-server-logic", "taskman-server-schema", "taskman-api"]) ,
         ("taskman-server-schema", ["base-db"]) ,
         ("taskman-server-logic",  ["taskman-api-logic"]) ,
         ("base-db",               ["base-util"]) ,
         ("base-util",             ["base-macro"]) ]

topLevelModules = ["taskman", "webapp-client", "webapp-client-ww", "webapp-server"]

tdeps :: String -> [String]
tdeps d = sort $ tdeps' [] [d]

tdeps' :: [String] -> [String] -> [String]
tdeps' done [] = done
tdeps' done (q:qs) = let
    (d',q') = tdeps'' done q
  in tdeps' d' (nub $ q' ++ qs)

tdeps'' :: [String] -> String -> ([String],[String])
tdeps'' done dep = let
    t     = M.findWithDefault [] dep deps
    done' = done ++ [dep]
    new   = filter (flip notElem done') t
  in (done', new)

extractModuleStats :: [GroupD] -> String -> Stats
extractModuleStats gs name = mconcat $ map snd $ filter ((name ==) . fst) $ concatMap modstats gs

extractModuleStatsT :: [GroupD] -> String -> Stats
extractModuleStatsT gs name = mconcat $ map (extractModuleStats gs) $ tdeps name

topLevelModuleStats gs = map (ap (,) (fst . extractModuleStatsT gs)) topLevelModules

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

topLevelModuleStatReportS = "--------------------------------\n"
topLevelModuleStatReportH = "Module            Files      LoC\n"++topLevelModuleStatReportS
fmtTopLevelModuleStat (m,s) = printf "%-16s  %5d  %7d\n" m (files s) (loc s)
topLevelModuleStatReport gs =
  topLevelModuleStatReportH ++ (concatMap fmtTopLevelModuleStat $ topLevelModuleStats gs) ++ topLevelModuleStatReportS

------------------------------------------------------------------------------------------------------------------------

sampleData = [GroupD {gname = "base", modstats = [("base-db",(Stat {files = 6, loc = 330},Stat {files = 0, loc = 0})),("base-test",(Stat {files = 0, loc = 0},Stat {files = 3, loc = 167})),("base-util",(Stat {files = 9, loc = 529},Stat {files = 2, loc = 231})),("base-util-sjs",(Stat {files = 13, loc = 859},Stat {files = 1, loc = 14}))]},GroupD {gname = "taskman", modstats = [("taskman-api-impl",(Stat {files = 3, loc = 102},Stat {files = 4, loc = 75})),("taskman-api-logic",(Stat {files = 6, loc = 148},Stat {files = 2, loc = 109})),("taskman-server-impl",(Stat {files = 16, loc = 1144},Stat {files = 6, loc = 443})),("taskman-server-logic",(Stat {files = 13, loc = 816},Stat {files = 6, loc = 517}))]},GroupD {gname = "webapp", modstats = [("webapp-base",(Stat {files = 38, loc = 2912},Stat {files = 0, loc = 0})),("webapp-base-test",(Stat {files = 0, loc = 0},Stat {files = 11, loc = 1178})),("webapp-client",(Stat {files = 65, loc = 3983},Stat {files = 15, loc = 828})),("webapp-server",(Stat {files = 131, loc = 8577},Stat {files = 69, loc = 8152}))]}]

customiseDetailedView  :: [GroupD] -> [GroupD]
customiseDetailedView' :: GroupD -> GroupD

customiseDetailedView gs = let f g@ GroupD {gname="webapp"} = customiseDetailedView' g
                               f g@ GroupD {}               = g
                            in map f gs

customiseDetailedView' g = let named n     = (\x -> n == (fst x))
                               get name    = head $ filter (named name) (modstats g)
                               nBaseTest   = "webapp-base-test"
                               nBase       = "webapp-base"
                               sBaseTest   = mergeStatsR $ snd $ get nBaseTest
                               sBase       = snd $ get nBase
                               merged      = ("webapp-base{,-test}", mappend sBase sBaseTest)
                               removeOld   = filter (\x -> not $ or $ map (\n -> named n x) [nBaseTest, nBase]) (modstats g)
                            in GroupD {gname = "webapp", modstats = [merged] ++ removeOld}

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do putStrLn "Analysing..."
          -- gs <- pure sampleData
          gs <- gatherAllStats
          -- putStrLn $ show gs
          putStrLn $ fmtBreakdowns [
            header
            ,fmtGroups $ customiseDetailedView gs
            ,fmtGroups $ map consolidateGroup gs
            ,fmtGroups [consolidateGroups gs]
            ]
          putStrLn "\n"
          putStrLn $ fmtBreakdowns [
            headerIL
            ,fmtGroups $ logicAndImpl gs
            ,fmtGroups [consolidateGroups $ logicAndImpl gs]
            ]
          putStrLn "\n"
          putStrLn $ topLevelModuleStatReport gs
