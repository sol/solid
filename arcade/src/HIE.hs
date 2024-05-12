{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module HIE (
  HieM
-- , runHie
, definition
, references
{-
, rename
, getType
, addTypeSignature
-- exported for testing
, findHieFile
-}
) where

import Prelude ()
import HaskellPrelude

import           Data.Maybe
import           Data.List
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           System.Process

import           System.Directory
import           System.FilePath

import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set


import GHC.Plugins hiding (empty)
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Iface.Ext.Ast as GHC
import GHC.Iface.Ext.Binary as GHC hiding (readHieFile)
use GHC.Iface.Ext.Binary as GHC
import GHC.Builtin.Utils (knownKeyNames)
import GHC.Iface.Ext.Debug as GHC
import GHC.Iface.Ext.Fields as GHC
import GHC.Iface.Ext.Types as GHC hiding (Span)
use GHC.Iface.Ext.Types as GHC
import GHC.Iface.Ext.Utils as GHC hiding (selectPoint)
use GHC.Iface.Ext.Utils as GHC
import           GHC.Types.Name.Cache

-- import           GHC.Orphans ()
-- import           GHC.Compat hiding (Env(..))
-- import           GHC.Iface.Ext.Compat as GHC hiding (Span, readHieFile, selectPoint)
-- import qualified GHC.Iface.Ext.Compat as GHC

import qualified System.Logging.Facade as Log

-- import           T (Text)
-- import qualified T

-- import           Scribe.Config
import Data.Location
import Config
-- import qualified Edit

type HieM = ReaderT Env IO

data Env = Env {
  envConfig :: Config
-- , envDynFlags :: DynFlags
, envNameCacheUpdater :: NameCache -- FIXME: rename to nameCache
, envHieCache :: IORef (Map FilePath HieFile)
}

runHie :: Config -> HieM a -> IO a
runHie config action = do
  -- dynFlags <- mkDynFlags -- FIXME?

  -- nameCache <- flip initNameCache [] <$> mkSplitUniqSupply 's' >>= newIORef
  nameCache <- initNameCache 'r' knownKeyNames

  ref <- newIORef mempty
  runReaderT action Env {
    envConfig = config
  -- , envDynFlags = dynFlags
  , envNameCacheUpdater = nameCache
  , envHieCache = ref
  }

readHieFile :: FilePath -> HieM HieFile
readHieFile name = do
  ref <- asks (.envHieCache)
  cache <- liftIO $ readIORef ref
  case Map.lookup name cache of
    Nothing -> do
      nameCacheUpdater <- asks (.envNameCacheUpdater)
      hieFile <- liftIO $ hie_file_result <$> GHC.readHieFile nameCacheUpdater name
      liftIO $ writeIORef ref (Map.insert name hieFile cache)
      return hieFile
    Just hieFile -> return hieFile

findHieFile :: FilePath -> HieM (Maybe FilePath)
findHieFile source = do
  dirs <- asks $ (.configSourceDirs) . (.envConfig)
  liftIO $ case listToMaybe $ mapMaybe (`stripDirectoryPrefix` source) dirs of
    Nothing -> return Nothing
    Just file -> do
      let hieFile = hiedir </> dropExtension file ++ ".hie"
      doesFileExist hieFile >>= \ case
        False -> return Nothing
        True -> return (Just hieFile)
  where
    stripDirectoryPrefix :: FilePath -> FilePath -> Maybe FilePath
    stripDirectoryPrefix dir file = guarded (file /=) path
      where
        path = makeRelative dir file

getHieFile :: Location -> HieM (Maybe HieFile)
getHieFile location = findHieFile location.locationFile >>= \ case
  Nothing -> return Nothing
  Just hieFile -> Just <$> readHieFile hieFile

findHieFiles :: HieM [FilePath]
findHieFiles = liftIO $ find_ [hiedir, "-name", "*.hie"]
  where
    find_ args = words <$> readProcess "find" args ""

readHieFiles :: HieM [HieFile]
readHieFiles = findHieFiles >>= mapM readHieFile

globalRefMap :: HieM (RefMap TypeIndex)
globalRefMap = do
  refMaps <- map (generateReferencesMap . getAsts . hie_asts) <$> readHieFiles
  return $ foldr (Map.unionWith mappend) mempty refMaps

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

selectPoint :: HieFile -> Location -> Maybe (HieAST TypeIndex)
selectPoint hieFile location = GHC.selectPoint hieFile (location.locationLine, location.locationColumn)

{-

recoverType :: HieFile -> TypeIndex -> IfaceType
recoverType hieFile = hieTypeToIface . flip recoverFullType (hie_types hieFile)
-}

getNodeInfo :: NodeOrigin -> HieAST a -> Maybe (NodeInfo a)
getNodeInfo origin = Map.lookup origin . getSourcedNodeInfo . sourcedNodeInfo

identifiersAt :: HieFile -> Location -> [(Identifier, IdentifierDetails TypeIndex)]
identifiersAt hieFile location = maybe [] (Map.toList . nodeIdentifiers) node
  where
    node :: Maybe (NodeInfo TypeIndex)
    node = selectPoint hieFile location >>= getNodeInfo SourceInfo

identifierAt :: HieFile -> Location -> Maybe Identifier
identifierAt hieFile = fmap fst . listToMaybe . filter notEvidenceVarUse . identifiersAt hieFile
  where
    notEvidenceVarUse =
#if __GLASGOW_HASKELL__ >= 900
      Set.notMember EvidenceVarUse . identInfo . snd
#else
      const True
#endif

nameAt :: HieFile -> Location -> Maybe Name
nameAt hieFile = identifierAt hieFile >=> either (const Nothing) Just

definitionSite :: Name -> Maybe Location
definitionSite = srcLocToLocation . nameSrcLoc

definition :: Location -> HieM (Maybe Location)
definition location = getHieFile location >>= \ case
  Nothing -> return Nothing
  Just hieFile -> do
    let name = nameAt hieFile location
    -- Log.debug (show name)
    return $ name >>= definitionSite

references :: Location -> HieM [(Location, String)]
references location = references_ location >>= \ case
  Nothing -> return []
  Just (_, xs) -> return $ map format xs
  where
    format (srcSpan, _) = (realSrcLocToLocation $ realSrcSpanStart srcSpan, "reference")

references_ :: Location -> HieM (Maybe (Identifier, [(GHC.Span, IdentifierDetails TypeIndex)]))
references_ location = getHieFile location >>= \ case
  Nothing -> return Nothing
  Just hieFile -> do
    refMap <- globalRefMap
    return $ do
      name <- identifierAt hieFile location
      details <- Map.lookup name refMap
      return (name, details)

{-
rename :: Text -> Location -> HieM ()
rename new location = references_ location >>= \ case
  Nothing -> return ()
  Just (name, xs) -> do
    let replace = T.replace (identifierToText name) new
    liftIO $ mapM_ (Edit.modifySpan replace . realSrcSpanToSpan . fst) $ reverse xs

identifierToText :: Identifier -> Text
identifierToText = \ case
  Left name -> unpackFastString $ moduleNameFS name
  Right name -> unpackFastString . occNameFS $ occName name

realSrcSpanToSpan :: RealSrcSpan -> Span
realSrcSpanToSpan srcSpan = Span (unpackFS $ srcSpanFile srcSpan) (srcSpanStartLine srcSpan, srcSpanStartCol srcSpan) (srcSpanEndLine srcSpan, srcSpanEndCol srcSpan)

getType :: Location -> HieM [String]
getType location = getHieFile location >>= \ case
  Nothing -> return []
  Just hieFile -> case selectPoint hieFile location >>= getNodeInfo SourceInfo of
    Nothing -> return []
    Just node -> do
      let
        nodeTypes = nodeType node
        identifierTypes = [t | (Right _, IdentifierDetails (Just t) _) <- Map.toList $ nodeIdentifiers node]
        types = nub $ nodeTypes ++ identifierTypes

      dynFlags <- asks envDynFlags
      return $ map (renderType dynFlags hieFile) types

addTypeSignature :: Location -> HieM ()
addTypeSignature location = getHieFile location >>= \ case
  Nothing -> return ()
  Just hieFile -> do
    case identifiersAt hieFile location of
      [(Right name, IdentifierDetails (Just t) (isValBind -> True))] | Just loc <- definitionSite name -> do
        dynFlags <- asks envDynFlags
        let signature = T.pack (occNameString $ occName name) <> " :: " <> (T.pack . renderType dynFlags hieFile $ t)
        liftIO $ Edit.addSignature signature loc
      _ -> return ()

isValBind :: Set ContextInfo -> Bool
isValBind xs = or [True | ValBind RegularBind _ _ <- Set.toList xs]

renderType :: DynFlags -> HieFile -> TypeIndex -> String
renderType dynFlags hieFile = showSDocDumpOneLine dynFlags . ppr . recoverType hieFile
  -}

realSrcLocToLocation :: RealSrcLoc -> Location
realSrcLocToLocation loc = Location (unpackFS $ srcLocFile loc) (srcLocLine loc) (srcLocCol loc)

srcLocToLocation :: SrcLoc -> Maybe Location
srcLocToLocation = \ case
  RealSrcLoc srcLoc _ -> Just (realSrcLocToLocation srcLoc)
  UnhelpfulLoc _ -> Nothing
