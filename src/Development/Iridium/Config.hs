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

import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Filesystem.Path.CurrentOS
import           Control.Monad.Trans.MultiRWS
import           Data.Text ( Text )
import           Control.Monad
import           Data.Monoid
import           Data.Maybe

import           Development.Iridium.UI.Console
import           Development.Iridium.Types
import           Paths_iridium



readConfFile
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadPlus m
     )
  => FilePath
  -> m (HM.HashMap Text Yaml.Value)
readConfFile path = do
  pushLog LogLevelInfoVerbose $ "Reading config file " ++ encodeString path
  eitherValue <- liftIO $ Yaml.decodeFileEither $ encodeString path
  case eitherValue of
    Left e -> do
      pushLog LogLevelError $ "Error reading config file " ++ encodeString path
      pushLog LogLevelError $ show e
      mzero
    Right (Yaml.Object m) -> return m
    Right _ -> do
      pushLog LogLevelError $ "Error reading config file: expecting YAML object."
      pushLog LogLevelError $ "(Parsing was successful but returned something else,\nlike a list. or smth.)"
      mzero

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
  let userConfPath  = home </> decodeString ".iridium.yaml"
  let localConfPath = cwd  </> decodeString "iridium.yaml"
  userConfExists  <- Turtle.testfile $ userConfPath
  localConfExists <- Turtle.testfile $ localConfPath

  userConf <- if userConfExists
    then readConfFile userConfPath
    else return $ HM.empty

  localConf <- if localConfExists
    then readConfFile localConfPath
    else do
      defaultPath <- liftIO $ getDataFileName "default-iridium.yaml"
      pushLog LogLevelInfo $ "Creating default iridium.yaml."
      Turtle.cp (decodeString defaultPath) localConfPath
      readConfFile localConfPath

  let final = HM.unionWith mergeConfigs localConf userConf
  let displayStr = unlines
                 $ fmap ("  " ++)
                 $ lines
                 $ BSChar8.unpack
                 $ YamlPretty.encodePretty YamlPretty.defConfig final
  pushLog LogLevelInfoVerboser $ "Parsed config: \n" ++ displayStr
  return $ Yaml.Object final

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

configIsEnabledM
  :: MonadMultiReader Config m
  => [String]
  -> m Bool
configIsEnabledM ps = configIsTrueM $ ps ++ ["enabled"]

configIsEnabled :: [String] -> Yaml.Value -> Bool
configIsEnabled ps = configIsTrue $ ps ++ ["enabled"]

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
