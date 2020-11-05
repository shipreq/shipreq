#!/usr/bin/runghc

import Control.Monad
import Data.List
import Debug.Trace (trace)
import qualified Data.Map.Strict as M
import Data.Maybe
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

groups = ["base", "benchmark", "taskman", "utils", "webapp"]
pathPrefixes = ["", "shared/", "jvm/", "js/"]
mainPaths = fmap (++"src/main/scala") pathPrefixes
testPaths = fmap (++"src/test/scala") pathPrefixes

type Group = String
type Module = String
type Stats = (Stat,Stat)

data GroupD = GroupD { gname :: Group, modstats :: [(Module, Stats)] } deriving (Show)

data Stat = Stat { files :: Int, loc :: Int } deriving (Show, Eq)
emptyStat = Stat 0 0
instance Semigroup Stat where
  a <> b = Stat (files a + files b) (loc a + loc b)
instance Monoid Stat where
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
                      return $ if "-test" `isSuffixOf` m
                        then (emptyStat, mappend a b)
                        else (a,b)

groupD :: [FilePath] -> Group -> IO GroupD
groupD dirs g = let
  a = modulesFor g dirs
  b = mapM statsForModule a -- IO [Stats]
  c = zip a <$> b           -- IO [(Module, Stats)]
  d = filter (not . areEmpty . snd) <$> c
  in GroupD g <$> d

modifyGroupDName :: (Group -> Group) -> GroupD -> GroupD
modifyGroupDName f g = g { gname = f $ gname g }

gatherAllStats :: IO [GroupD]
gatherAllStats = do dirs <- dirsIn "."
                    mapM (groupD dirs) groups

------------------------------------------------------------------------------------------------------------------------
-- Top-level module stats

-- Only considers Compile scope, not Test
-- Also doesn't distinguish between JVM/JS

type TopLevelDeps = M.Map String [String]

depsJvm = M.fromList [
         ("webapp-ssr",              ["webapp-member", "webapp-client-public"]),
         ("webapp-server-logic",     ["taskman-api-logic", "webapp-ssr"]) ,
         ("webapp-server",           ["base-db", "taskman-api", "webapp-server-logic", "webapp-gen"]) ,
         ("webapp-client-public",    ["webapp-base"]) ,
         ("webapp-member",           ["webapp-base"]) ,
         ("webapp-base",             ["webapp-macro", "base-util"]) ,
         ("webapp-macro",            ["base-util", "base-db"]) ,
         ("taskman",                 ["taskman-server"]) ,
         ("taskman-api",             ["taskman-api-logic", "base-db"]) ,
         ("taskman-api-logic",       ["base-util"]) ,
         ("taskman-server",          ["taskman-server-logic", "taskman-server-schema", "taskman-api"]) ,
         ("taskman-server-schema",   ["base-db"]) ,
         ("taskman-server-logic",    ["taskman-api-logic"]) ,
         ("base-db",                 ["base-ops"]) ,
         ("base-ops",                ["base-util"]) ,
         ("base-util",               ["base-predef"]),
         ("base-predef",             []) ]
depsJs = M.fromList [
         ("webapp-ssr",              ["webapp-client-public", "webapp-client-loaders"]),
         ("webapp-server-logic",     ["webapp-member"]) ,
         ("webapp-client-public",    ["webapp-base"]) ,
         ("webapp-client-loaders",   ["webapp-member"]) ,
         ("webapp-client-home",      ["webapp-client-loaders"]) ,
         ("webapp-client-ww-api",    ["webapp-member"]) ,
         ("webapp-client-ww",        ["webapp-client-ww-api"]) ,
         ("webapp-client-project",   ["webapp-client-loaders"]) ,
         ("webapp-member",           ["webapp-base"]) ,
         ("webapp-base",             ["webapp-macro", "base-util"]) ,
         ("webapp-macro",            ["base-util"]) ,
         ("base-util",               []) ]

topLevelJvmModules = ["taskman", "webapp-server"]
topLevelJsModules = [
  "webapp-client-public",
  "webapp-client-home",
  "webapp-client-ww",
  "webapp-client-project"]

tdeps :: TopLevelDeps -> String -> [String]
tdeps deps d = sort $ tdeps' deps [] [d]

tdeps' :: TopLevelDeps -> [String] -> [String] -> [String]
tdeps' deps done [] = done
tdeps' deps done (q:qs) = let
    (d',q') = tdeps'' deps done q
  in tdeps' deps d' (nub $ q' ++ qs)

tdeps'' :: TopLevelDeps -> [String] -> String -> ([String],[String])
tdeps'' deps done dep = let
    t     = M.findWithDefault [] dep deps
    done' = done ++ [dep]
    new   = filter (`notElem` done') t
  in (done', new)

extractModuleStats :: [GroupD] -> String -> Stats
extractModuleStats gs name = mconcat $ map snd $ filter ((name ==) . fst) $ concatMap modstats gs

extractModuleStatsT :: TopLevelDeps -> [GroupD] -> String -> Stats
extractModuleStatsT deps gs name = mconcat $ map (extractModuleStats gs) $ tdeps deps name

topLevelModuleStats :: [GroupD] -> TopLevelDeps -> [String] -> [(String, Stat)]
topLevelModuleStats gs deps = map (ap (,) (fst . extractModuleStatsT deps gs))

------------------------------------------------------------------------------------------------------------------------
-- Printing stats

header = "                            |       Files     |            LoC\n"
       ++"                            |    M    T    ∑  |      M      T      ∑  (T:M)\n"
sepLine= "----------------------------+-----------------+----------------------------\n"

float i = fromIntegral i :: Float

testRatio (Stat _ m, Stat _ t) = float t / float m

testRatioS (Stat _ 0, _) = " - "
testRatioS s = printf "%.1f" $ testRatio s

fmtGroup (GroupD _ ms) = map fmtMS ms
fmtMS (m,s) = printf "%-27s | %s  | %s  (%s)\n" m (fmtPF s) (fmtPL s) (testRatioS s)

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

headerIL = "Logic & Impl                |    L    I    ∑  |      L      I      ∑  (I:L)\n"

modulesWithSuffix :: String -> [GroupD] -> [(Module, Stats)]
modulesWithSuffix suf = filter (isSuffixOf suf . fst) . concatMap modstats

modulesWithSuffix' :: String -> [GroupD] -> [(Module, Stats)]
modulesWithSuffix' suf = map (mapFst $ dropr $ length suf) . modulesWithSuffix suf

dropr :: Int -> [a] -> [a]
dropr n = reverse . drop n . reverse

logicAndImpl :: [GroupD] -> [GroupD]
logicAndImpl gs = let l = modulesWithSuffix' "-logic" gs
                      keys = sort $ map fst $ l
                      get xs k = s where Just (_,(s,_)) = find ((== k) . fst) xs
                      getLI k = singleModuleGroup k (get l k, get (concatMap modstats gs) k)
                  in  map getLI keys

logicAndImplWithTotal gs = a ++ [consolidateGroups a] where a = logicAndImpl gs

topLevelModuleStatReportS = "-------------------------------------\n"
topLevelModuleStatReportH = "Module                 Files      LoC\n"++topLevelModuleStatReportS
fmtTopLevelModuleStat (m,s) = printf "%-21s  %5d  %7d\n" m (files s) (loc s)
topLevelModuleStatReport gs =
  topLevelModuleStatReportH ++
  concatMap fmtTopLevelModuleStat (topLevelModuleStats gs depsJvm topLevelJvmModules) ++
  concatMap fmtTopLevelModuleStat (topLevelModuleStats gs depsJs topLevelJsModules) ++
  topLevelModuleStatReportS

------------------------------------------------------------------------------------------------------------------------

customiseDetailedView :: [GroupD] -> [GroupD]
customiseDetailedView gs =
  let w1                           = customiseDetailedView' "webapp" "webapp-base"
      f g@ GroupD {gname="webapp"} = w1 g
      f g@ GroupD {}               = g
  in map f gs

customiseDetailedView' :: String -> String -> GroupD -> GroupD
customiseDetailedView' gName' nBase g =
  let named n x   = n == fst x
      get name    = head $ filter (named name) (modstats g)
      nBaseTest   = nBase ++ "-test"
      sBaseTest   = mergeStatsR $ snd $ get nBaseTest
      sBase       = snd $ get nBase
      merged      = (nBase ++ "{,-test}", mappend sBase sBaseTest)
      removeOld   = filter (\x -> not $ any (`named` x) [nBaseTest, nBase]) (modstats g)
  in GroupD {gname = gName', modstats = merged : removeOld}

-- customiseDetailedView' g = let named n x   = n == fst x
--                                get name    = head $ filter (named name) (modstats g)
--                                nBaseTest   = "webapp-base-test"
--                                nBase       = "webapp-base"
--                                sBaseTest   = mergeStatsR $ snd $ get nBaseTest
--                                sBase       = snd $ get nBase
--                                merged      = ("webapp-base{,-test}", mappend sBase sBaseTest)
--                                removeOld   = filter (\x -> not $ any (`named` x) [nBaseTest, nBase]) (modstats g)
--                             in GroupD {gname = "webapp", modstats = merged : removeOld}

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
