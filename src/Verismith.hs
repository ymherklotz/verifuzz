{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : Verismith
-- Description : Verismith
-- Copyright   : (c) 2018-2022, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
module Verismith
  ( defaultMain,

    -- * Types
    Opts (..),
    SourceInfo (..),

    -- * Run functions
    runEquivalence,
    runSimulation,
    runReduce,
    draw,

    -- * Verilog generation functions
    procedural,
    proceduralIO,
    proceduralSrc,
    proceduralSrcIO,
    randomMod,

    -- * Extra modules
    module Verismith.Verilog,
    module Verismith.Config,
    module Verismith.Circuit,
    module Verismith.Tool,
    module Verismith.Fuzz,
    module Verismith.Report,
  )
where

import Control.Concurrent
import Control.Lens hiding ((<.>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Internal (unpackChars)
import qualified Data.ByteString.Lazy as L
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Dot as G
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Time
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Hog
import Hedgehog.Internal.Seed (Seed)
import Options.Applicative
import Paths_verismith (getDataDir)
import Shelly hiding (command)
import Shelly.Lifted (liftSh)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Random (randomIO)
import Verismith.Circuit
import Verismith.Config
import Verismith.EMI
import Verismith.Fuzz
import Verismith.Generate
import Verismith.OptParser
import Verismith.Reduce
import Verismith.Report
import Verismith.Result
import Verismith.Shuffle
import Verismith.Tool
import Verismith.Tool.Internal
import Verismith.Utils (generateByteString)
import Verismith.Verilog
import Verismith.Verilog.Distance
import Verismith.Verilog.Parser (parseSourceInfoFile)
import qualified Verismith.Verilog2005 as V2
import Prelude hiding (FilePath)

toFP :: String -> FilePath
toFP = fromText . T.pack

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\_ -> putMVar mvar ())
  return mvar

logMsg :: Text -> Text -> IO ()
logMsg level msg = do
  currentTime <- getZonedTime
  T.putStrLn $
    "["
      <> level
      <> "] "
      <> T.pack (formatTime defaultTimeLocale "%H:%M:%S " currentTime)
      <> msg

logInfo :: Text -> IO ()
logInfo = logMsg "INFO"

logWarn :: Text -> IO ()
logWarn = logMsg "WARN"

logFatal :: Text -> IO ()
logFatal = logMsg "FATAL"

getConfig' :: Bool -> Maybe FilePath -> IO Config
getConfig' strict s = do
  config <- maybe def (parseConfigFile . T.unpack . toTextIgnore) s
  case config of
    Left errFst -> do
      when strict $ mapM_ logFatal (T.lines errFst) >> exitFailure
      relaxedConfig <- maybe def (parseConfigFileRelaxed . T.unpack . toTextIgnore) s
      case relaxedConfig of
        Left errSnd -> do
          mapM_ logFatal $ T.lines errSnd
          exitFailure
        Right x -> do
          mapM_ logWarn (T.lines errFst)
          logWarn "Ignoring additional fields"
          return x
    Right x -> return x
  where
    def = return (Right defaultConfig)

getConfig = getConfig' True

getGenerator :: Config -> Text -> Maybe FilePath -> IO (Gen (SourceInfo ()))
getGenerator config top s =
  maybe (return $ proceduralSrc top config) (fmap return . parseSourceInfoFile top) $
    toTextIgnore <$> s

-- | Randomly remove an option by setting it to 0.
randDelete :: Int -> IO Int
randDelete i = do
  r <- randomIO
  return $ if r then i else 0

randomise :: Config -> IO Config
randomise config@(Config emi a _ c d e f) = do
  mia <- return $ cm ^. probModItemAssign
  misa <- return $ cm ^. probModItemSeqAlways
  mica <- return $ cm ^. probModItemCombAlways
  mii <- return $ cm ^. probModItemInst
  ssb <- return $ cs ^. probStmntBlock
  ssnb <- return $ cs ^. probStmntNonBlock
  ssc <- return $ cs ^. probStmntCond
  ssf <- return $ cs ^. probStmntFor
  en <- return $ ce ^. probExprNum
  keep_out <- return $ cmo ^. probModDropOutput
  drop_out <- randDelete $ cmo ^. probModDropOutput
  ei <- randDelete $ ce ^. probExprId
  ers <- randDelete $ ce ^. probExprRangeSelect
  euo <- randDelete $ ce ^. probExprUnOp
  ebo <- randDelete $ ce ^. probExprBinOp
  ec <- randDelete $ ce ^. probExprCond
  eco <- randDelete $ ce ^. probExprConcat
  estr <- randDelete $ ce ^. probExprStr
  esgn <- randDelete $ ce ^. probExprSigned
  eus <- randDelete $ ce ^. probExprUnsigned
  return $
    Config
      emi
      a
      ( Probability
          (ProbModItem mia misa mica mii)
          (ProbStatement ssb ssnb ssc ssf)
          (ProbExpr en ei ers euo ebo ec eco estr esgn eus)
          (ProbMod drop_out keep_out)
      )
      c
      d
      e
      f
  where
    cm = config ^. configProbability . probModItem
    cs = config ^. configProbability . probStmnt
    ce = config ^. configProbability . probExpr
    cmo = config ^. configProbability . probMod

handleOpts :: Opts -> IO ()
handleOpts (Fuzz o configF f k n nosim noequiv noreduction file top cc checker) = do
  config <- getConfig configF
  gen <- getGenerator config top file
  datadir <- getDataDir
  _ <-
    runFuzz
      ( FuzzOpts
          (Just $ fromText o)
          f
          k
          n
          nosim
          noequiv
          noreduction
          config
          (toFP datadir)
          cc
          checker
      )
      defaultYosys
      (fuzzMultiple gen)
  return ()
handleOpts (EMIOpts o configF f k n nosim noequiv noreduction top file) = do
  config <- getConfig configF
  datadir <- getDataDir
  src <- parseSourceInfoFile top (T.pack file) :: IO (SourceInfo ())
  let gen = proceduralEMI src config
  _ <-
    runFuzz
      ( FuzzOpts
          (Just $ fromText o)
          f
          k
          n
          nosim
          noequiv
          noreduction
          config
          (toFP datadir)
          False
          Nothing
      )
      defaultYosys
      (fuzzMultipleEMI gen)
  return ()
handleOpts (Generate f c sf popts) = do
  config <- getConfig c
  if sf
    then do
      source <- V2.runGarbageGeneration config
      maybe L.putStr L.writeFile f $ V2.genSource (Just 80) popts source
    else do
      source <- proceduralIO "top" config :: IO (Verilog ())
      maybe T.putStrLn T.writeFile (T.unpack . toTextIgnore <$> f) $ genSource source
handleOpts (Parse f o s popts) = do
  (ast, warns) <- V2.parseVerilog2005 (T.unpack (toTextIgnore f))
  mapM_ (hPutStrLn stderr) warns
  if null warns || not s
    then pure ()
    else error "Input file does not comply strictly with the Verilog 2005 standard"
  ast' <- case V2.resolveInsts ast of
    Left err -> error err
    Right x -> pure x
  maybe L.putStr L.writeFile o $ V2.genSource (Just 80) popts ast'
handleOpts (ShuffleOpt f t o nshuffle nrename noequiv equivdir checker) = do
  datadir <- getDataDir
  verilogSrc <- T.readFile file
  case parseVerilog (T.pack file) verilogSrc of
    Left l -> print l
    Right v -> do
      let sv = SourceInfo t v
      sv' <- runShuffle nshuffle nrename sv
      let gv = GenVerilog sv' :: GenVerilog (SourceInfo ())
      if noequiv
        then return ()
        else do
          shelly $ do
            mkdir equivdir
            cp file (equivdir </> fn1)
          writeFile (equivdir </> fn2) $ show gv
          res <- shelly . runResultT $
            pop equivdir $ do
              runEquiv checker (toFP datadir) (mkid fn1) (mkid fn2) sv'
          case res of
            Pass _ -> putStrLn "Equivalence check passed"
            Fail (EquivFail _) -> putStrLn "Equivalence check failed"
            Fail TimeoutError -> putStrLn "Equivalence check timed out"
            Fail _ -> putStrLn "Equivalence check error"
      case o of
        Nothing -> print gv
        Just o' -> writeFile (T.unpack $ toTextIgnore o') $ show gv
  where
    file = T.unpack . toTextIgnore $ f
    fn1 = "rtl1.v"
    fn2 = "rtl2.v"
    mkid f = Verismith.Tool.Identity "" (fromText f)
handleOpts (Reduce f t _ ls' False) = do
  src <- parseSourceInfoFile t (toTextIgnore f)
  datadir <- getDataDir
  case descriptionToSynth <$> ls' of
    a : b : _ -> do
      putStrLn "Reduce with equivalence check"
      shelly $ do
        make dir
        pop dir $ do
          src' <- reduceSynth Nothing (toFP datadir) a b src :: Sh (SourceInfo ())
          writefile (fromText ".." </> dir <.> "v") $ genSource src'
    a : _ -> do
      putStrLn "Reduce with synthesis failure"
      shelly $ do
        make dir
        pop dir $ do
          src' <- reduceSynthesis a src
          writefile (fromText ".." </> dir <.> "v") $ genSource src'
    _ -> do
      putStrLn "Not reducing because no synthesiser was specified"
      return ()
  where
    dir = fromText "reduce"
handleOpts (Reduce f t _ ls' True) = do
  src <- parseSourceInfoFile t (toTextIgnore f) :: IO (SourceInfo ())
  datadir <- getDataDir
  case descriptionToSynth <$> ls' of
    a : b : _ -> do
      putStrLn "Starting equivalence check"
      res <- shelly . runResultT $ do
        make dir
        pop dir $ do
          runSynth a src
          runSynth b src
          runEquiv Nothing (toFP datadir) a b src
      case res of
        Pass _ -> putStrLn "Equivalence check passed"
        Fail (EquivFail _) -> putStrLn "Equivalence check failed"
        Fail TimeoutError -> putStrLn "Equivalence check timed out"
        Fail _ -> putStrLn "Equivalence check error"
      return ()
    as -> do
      putStrLn "Synthesis check"
      _ <- shelly . runResultT $ mapM (flip runSynth src) as
      return ()
  where
    dir = fromText "equiv"
handleOpts (ConfigOpt c conf r) = do
  config <- if r then getConfig conf >>= randomise else getConfig conf
  maybe (T.putStrLn . encodeConfig $ config) (`encodeConfigFile` config) $
    T.unpack
      . toTextIgnore
      <$> c
handleOpts (DistanceOpt v1 v2) = do
  src1 <- parseSourceInfoFile (T.pack v1) (toTextIgnore v1)
  src2 <- parseSourceInfoFile (T.pack v2) (toTextIgnore v2)
  let d = distance src1 src2
  putStrLn ("Distance: " <> show d)
handleOpts (Equiv o v1 v2 top checker) = do
  datadir <- getDataDir
  src <- parseSourceInfoFile top (toTextIgnore v1) :: IO (SourceInfo ())
  shelly $ do
    mkdir o
    cp v1 (o </> fn1)
    cp v2 (o </> fn2)
  res <- shelly . runResultT $
    pop o $ do
      runEquiv checker (toFP datadir) (mkid fn1) (mkid fn2) src
  case res of
    Pass _ -> putStrLn "Equivalence check passed"
    Fail (EquivFail _) -> putStrLn "Equivalence check failed"
    Fail TimeoutError -> putStrLn "Equivalence check timed out"
    Fail _ -> putStrLn "Equivalence check error"
  where
    fn1 = "rtl1.v"
    fn2 = "rtl2.v"
    mkid f = Verismith.Tool.Identity "" (fromText f)

defaultMain :: IO ()
defaultMain = do
  optsparsed <- execParser opts
  handleOpts optsparsed

makeSrcInfo :: (ModDecl ann) -> (SourceInfo ann)
makeSrcInfo m = SourceInfo (getIdentifier $ m ^. modId) (Verilog [m])

-- | Draw a randomly generated DAG to a dot file and compile it to a png so it
-- can be seen.
draw :: IO ()
draw = do
  gr <- Hog.sample $ rDups . getCircuit <$> Hog.resize 10 randomDAG
  let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  writeFile "file.dot" dot
  shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]

-- | Function to show a bytestring in a hex format.
showBS :: ByteString -> Text
showBS = decodeUtf8 . L.toStrict . toLazyByteString . byteStringHex

-- | Run a simulation on a random DAG or a random module.
runSimulation :: IO ()
runSimulation = do
  -- gr <- Hog.generate $ rDups <$> Hog.resize 100 (randomDAG :: Gen (G.Gr Gate ()))
  -- let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  -- writeFile "file.dot" dot
  -- shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]
  -- let circ =
  --       head $ (nestUpTo 30 . generateAST $ Circuit gr) ^.. getVerilog . traverse . getDescription
  rand <- generateByteString Nothing 32 20
  rand2 <- Hog.sample (randomMod 10 100) :: IO (ModDecl ())
  val <- shelly . runResultT $ runSim defaultIcarus (makeSrcInfo rand2) rand
  case val of
    Pass a -> T.putStrLn $ showBS a
    _ -> T.putStrLn "Test failed"

-- | Code to be executed on a failure. Also checks if the failure was a timeout,
-- as the timeout command will return the 124 error code if that was the
-- case. In that case, the error will be moved to a different directory.
onFailure :: Text -> RunFailed -> Sh (Result Failed ())
onFailure t _ = do
  ex <- lastExitCode
  case ex of
    124 -> do
      logger "Test TIMEOUT"
      chdir ".." $ cp_r (fromText t) $ fromText (t <> "_timeout")
      return $ Fail EmptyFail
    _ -> do
      logger "Test FAIL"
      chdir ".." $ cp_r (fromText t) $ fromText (t <> "_failed")
      return $ Fail EmptyFail

checkEquivalence :: (Show ann) => SourceInfo ann -> Text -> IO Bool
checkEquivalence src dir = shellyFailDir $ do
  mkdir_p (fromText dir)
  curr <- toTextIgnore <$> pwd
  datadir <- liftIO getDataDir
  setenv "VERISMITH_ROOT" curr
  cd (fromText dir)
  catch_sh
    ((runResultT $ runEquiv Nothing (toFP datadir) defaultYosys defaultVivado src) >> return True)
    ((\_ -> return False) :: RunFailed -> Sh Bool)

-- | Run a fuzz run and check if all of the simulators passed by checking if the
-- generated Verilog files are equivalent.
runEquivalence ::
  Maybe Seed ->
  -- | Generator for the Verilog file.
  Gen (Verilog ()) ->
  -- | Name of the folder on each thread.
  Text ->
  -- | Name of the general folder being used.
  Text ->
  -- | Keep flag.
  Bool ->
  -- | Used to track the recursion.
  Int ->
  IO ()
runEquivalence seed gm t d k i = do
  (_, m) <- shelly $ sampleSeed seed gm
  let srcInfo = SourceInfo "top" m
  rand <- generateByteString Nothing 32 20
  datadir <- getDataDir
  shellyFailDir $ do
    mkdir_p (fromText d </> fromText n)
    curr <- toTextIgnore <$> pwd
    setenv "VERISMITH_ROOT" curr
    cd (fromText "output" </> fromText n)
    _ <-
      catch_sh
        ( runResultT $
            runEquiv Nothing (toFP datadir) defaultYosys defaultVivado srcInfo
              >> liftSh (logger "Test OK")
        )
        $ onFailure n
    _ <-
      catch_sh
        ( runResultT $
            runSim (Icarus "iverilog" "vvp") srcInfo rand
              >>= (\b -> liftSh $ logger ("RTL Sim: " <> showBS b))
        )
        $ onFailure n
    cd ".."
    unless k . rm_rf $ fromText n
  when (i < 5 && isNothing seed) (runEquivalence seed gm t d k $ i + 1)
  where
    n = t <> "_" <> T.pack (show i)

runReduce :: (SourceInfo ()) -> IO (SourceInfo ())
runReduce s =
  shelly $ reduce "reduce.v" (\s' -> not <$> liftIO (checkEquivalence s' "reduce")) s
