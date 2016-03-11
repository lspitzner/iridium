module Development.Iridium.Config
  ( parseConfigs
  , configIsTrue
  , configIsTrueM
  , configIsEnabled
  , configIsEnabledM
  , configReadString
  , configReadStringM
  , configReadStringMaybe
  , configReadStringMaybeM
  , configReadList
  , configReadListM
  , configReadStringWithDefaultM
  , configDecideStringM
  , mergeConfigs
  )
where



import           Prelude hiding ( FilePath )

import qualified Data.Yaml             as Yaml
import qualified Data.Yaml.Pretty      as YamlPretty
import qualified Turtle.Prelude        as Turtle
import qualified Data.HashMap.Strict   as HM
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.Text             as Text
import qualified Data.Vector           as DV
import qualified Data.List             as List
import qualified Data.ByteString       as BS
import qualified Data.Vector

import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Filesystem.Path.CurrentOS
import           Control.Monad.Trans.MultiRWS
import           Data.Text ( Text )
import           Control.Monad
import           Data.Monoid
import           Data.Maybe
import           Data.Ord ( comparing )

import           Development.Iridium.UI.Console
import           Development.Iridium.Types
import           Paths_iridium



readConfFile
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadPlus m
     )
  => FilePath
  -> m Config
readConfFile path = do
  pushLog LogLevelInfoVerbose $ "Reading config file " ++ encodeString path
  eitherValue <- liftIO $ Yaml.decodeFileEither $ encodeString path
  case eitherValue of
    Left e -> do
      pushLog LogLevelError $ "Error reading config file " ++ encodeString path
      pushLog LogLevelError $ show e
      mzero
    Right o@Yaml.Object{} -> return o
    Right _ -> do
      pushLog LogLevelError $ "Error reading config file: expecting YAML object."
      pushLog LogLevelError $ "(Parsing was successful but returned something else,\nlike a list. or smth.)"
      mzero

writeConfigToFile :: String -> Config -> IO ()
writeConfigToFile path config =
  writeFile
    path
    (headerComment ++ "\n---\n" ++ unlines (go Nothing 0 config) ++ "...\n")
  where
    headerComment :: String
    headerComment = unlines
                  $ map ("# " ++)
                  $ [ "see https://github.com/lspitzner/iridium"
                    , ""
                    , "note that you can add a user-global .iridium.yaml"
                    , "into $HOME, containing e.g."
                    , ""
                    , "---"
                    , "setup:"
                    , "  compiler-paths:"
                    , "    ghc-7.10.3: /opt/ghc-7.10.3/bin/ghc"
                    , "    ghc-7.8.4:  /opt/ghc-7.8.4/bin/ghc"
                    , ""
                    , "  hackage:"
                    , "    username: user"
                    , "..."
                    , ""
                    ]
    -- The reason for this custom pretty-printing is that encodePretty from
    -- the yaml package formats strings horribly, which
    -- makes the documentation elements more annoying to parse than they
    -- are helpful.
    go :: Maybe String -> Int -> Config -> [String]
    go firstLine indent (Yaml.Object m)
      = maybe id (:) firstLine -- (firstLine:)
      $ List.sortBy (comparing fst) (HM.toList m) >>= \(k, v) ->
        go (Just $ replicate indent ' ' ++ Text.unpack k ++ ":") (indent+2) v
    go firstLine indent (Yaml.Array  a)
      = maybe id (:) firstLine
      $ Data.Vector.toList a >>= \v ->
        case go Nothing 0 v of
          [] -> []
          (x:xr) -> (replicate indent ' ' ++ "- " ++ x)
                  : (fmap ((replicate (indent+2) ' ')++) xr)        
    go firstLine indent (Yaml.String s)
      = case (lines $ Text.unpack s, firstLine) of
          ([], Just l)  ->
            [l ++ " \"\""]
          ([x], Just l) | '"' `notElem` x -> -- " this editor has highlighting problems..
            [l ++ " " ++ show x]
          (xs, Just l)  ->
            ((l ++ " |"):)
            $ fmap ((replicate indent ' ') ++)
            $ xs
          (xs, Nothing) ->
            fmap ((replicate indent ' ') ++) xs
    go firstLine indent (Yaml.Number i)
      = case firstLine of
          Just l -> [l ++ " " ++ show i]
          Nothing -> [replicate indent ' ' ++ show i]
    go firstLine indent (Yaml.Bool b)
      = case firstLine of
          Just l -> [l ++ " " ++ show b]
          Nothing -> [replicate indent ' ' ++ show b]
    go _firstLine _indent Yaml.Null
      = error "Null"

determineConfFromStuff
  :: ( MonadIO m
     , MonadMultiState LogState m
     )
  => m Config
determineConfFromStuff = do
  return $ Yaml.Object $ HM.empty -- TODO

parseConfigs
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     )
  => m Yaml.Value
parseConfigs = do
  pushLog LogLevelInfo "Reading config files.."

  home <- Turtle.home
  cwd  <- Turtle.pwd
  let userConfPath        = home </> decodeString ".iridium.yaml"
  let userDefaultConfPath = home </> decodeString ".iridium-default.yaml"
  let localConfPath       = cwd  </> decodeString "iridium.yaml"
  staticDefaultPath   <- liftIO $ getDataFileName "default-iridium.yaml"
  userConfExists        <- Turtle.testfile $ userConfPath
  userDefaultConfExists <- Turtle.testfile $ userDefaultConfPath
  localConfExists       <- Turtle.testfile $ localConfPath

  userConf <- if userConfExists
    then do
      pushLog LogLevelInfoVerbose $ "Reading user config file from "
                                 ++ encodeString userConfPath
      readConfFile userConfPath
    else return $ Yaml.Object $ HM.empty

  localConf <- if localConfExists
    then readConfFile localConfPath
    else do
      userDefaultConf <- if userDefaultConfExists
        then do
          pushLog LogLevelInfoVerbose $ "Reading user default config from "
                                     ++ encodeString userDefaultConfPath
          readConfFile userDefaultConfPath
        else return $ Yaml.Object $ HM.empty
      calculatedConf <- determineConfFromStuff
      staticDefaultConf <- do
        pushLog LogLevelInfoVerbose $ "Reading static default config from "
                                   ++ staticDefaultPath
        readConfFile (decodeString staticDefaultPath)

      let combinedConfig = mergeConfigs
                           userDefaultConf   -- 1. priority
                         $ mergeConfigs
                           calculatedConf    -- 2. priority
                           staticDefaultConf -- 3. priority

      pushLog LogLevelInfo $ "Creating default iridium.yaml."
      liftIO $ writeConfigToFile (encodeString localConfPath) combinedConfig

      readConfFile localConfPath

  let final = mergeConfigs localConf userConf
  let displayStr = unlines
                 $ fmap ("  " ++)
                 $ lines
                 $ BSChar8.unpack
                 $ YamlPretty.encodePretty YamlPretty.defConfig final
  pushLog LogLevelInfoVerboser $ "Parsed config: \n" ++ displayStr
  return $ final

-- left-preferring merge; deep merge for objects/arrays
mergeConfigs :: Yaml.Value -> Yaml.Value -> Yaml.Value
mergeConfigs (Yaml.Object o1) (Yaml.Object o2) = Yaml.Object $ HM.unionWith mergeConfigs o1 o2
mergeConfigs (Yaml.Array a1)  (Yaml.Array a2)  = Yaml.Array  $ a1 <> a2
mergeConfigs Yaml.Null x = x
mergeConfigs x _         = x

configIsTrueM
  :: MonadMultiReader Config m
  => [String]
  -> m Bool
configIsTrueM ps'' = configIsTrue ps'' `liftM` mAsk

configIsTrue :: [String] -> Yaml.Value -> Bool
configIsTrue ps'' = go ps''
  where
    go :: [String] -> Yaml.Value -> Bool
    go []     v = case v of
                    Yaml.Bool b -> b
                    _ -> error $ "error in yaml data: expected Bool, got " ++ show v
    go (p:pr) v = case v of
                    Yaml.Object hm -> case HM.lookup (Text.pack p) hm of
                      Just v' -> go pr v'
                      Nothing -> error $ "error in yaml data: no find element " ++ show p ++ " when looking for config " ++ show ps''
                    _ -> error $ "error in yaml data: expected Object, got " ++ show v

configIsTrueMaybe :: [String] -> Yaml.Value -> Maybe Bool
configIsTrueMaybe ps'' = go ps''
  where
    go :: [String] -> Yaml.Value -> Maybe Bool
    go []     v = case v of
                    Yaml.Bool b -> Just b
                    _ -> Nothing
    go (p:pr) v = case v of
                    Yaml.Object hm -> case HM.lookup (Text.pack p) hm of
                      Just v' -> go pr v'
                      Nothing -> Nothing
                    _ -> Nothing

configIsEnabledM
  :: MonadMultiReader Config m
  => [String]
  -> m Bool
configIsEnabledM ps = configIsEnabled ps `liftM` mAsk

configIsEnabled :: [String] -> Yaml.Value -> Bool
configIsEnabled ps v = fromMaybe False $ configIsTrueMaybe (ps ++ ["enabled"]) v

configReadStringM
  :: MonadMultiReader Config m
  => [String]
  -> m String
configReadStringM ps'' = configReadString ps'' `liftM` mAsk

configReadString :: [String] -> Yaml.Value -> String
configReadString ps'' = go ps''
  where
    go :: [String] -> Yaml.Value -> String
    go []     v = case v of
                    Yaml.String b -> Text.unpack b
                    _ -> error $ "error in yaml data: expected String, got " ++ show v
    go (p:pr) v = case v of
                    Yaml.Object hm -> case HM.lookup (Text.pack p) hm of
                      Just v' -> go pr v'
                      Nothing -> error $ "error in yaml data: no find element " ++ show p ++ " when looking for config " ++ show ps''
                    _ -> error $ "error in yaml data: expected Object, got " ++ show v

configReadStringMaybeM
  :: MonadMultiReader Config m
  => [String]
  -> m (Maybe String)
configReadStringMaybeM ps'' = configReadStringMaybe ps'' `liftM` mAsk

configReadStringMaybe :: [String] -> Yaml.Value -> Maybe String
configReadStringMaybe ps'' = go ps''
  where
    go :: [String] -> Yaml.Value -> Maybe String
    go []     v = case v of
                    Yaml.String b -> Just $ Text.unpack b
                    _             -> Nothing
    go (p:pr) v = case v of
                    Yaml.Object hm -> go pr =<< HM.lookup (Text.pack p) hm
                    _              -> Nothing

configReadStringWithDefaultM
  :: MonadMultiReader Config m
  => String
  -> [String]
  -> m String
configReadStringWithDefaultM def ps = do
  liftM (fromMaybe def) $ configReadStringMaybeM ps

configReadListM
  :: MonadMultiReader Config m
  => [String]
  -> m [Yaml.Value]
configReadListM ps'' = configReadList ps'' `liftM` mAsk

configReadList :: [String] -> Yaml.Value -> [Yaml.Value]
configReadList ps'' = go ps''
  where
    go :: [String] -> Yaml.Value -> [Yaml.Value]
    go []     v = case v of
                    Yaml.Array a -> DV.toList a
                    _ -> error $ "error in yaml data: expected Array, got " ++ show v
    go (p:pr) v = case v of
                    Yaml.Object hm -> case HM.lookup (Text.pack p) hm of
                      Just v' -> go pr v'
                      Nothing -> error $ "error in yaml data: no find element " ++ show p ++ " when looking for config " ++ show ps''
                    _ -> error $ "error in yaml data: expected Object, got " ++ show v

configDecideStringM
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Config m
     , MonadMultiState LogState m
     )
  => [String]
  -> [(String, m a)]
  -> m a
configDecideStringM ps opts = do
  str <- configReadStringM ps
  case List.lookup str opts of
    Nothing -> do
      pushLog LogLevelError $ "Error looking up config value "
                           ++ show ps
                           ++ "; expecting one of "
                           ++ show (fmap fst opts)
                           ++ "."
      mzero
    Just k -> k
