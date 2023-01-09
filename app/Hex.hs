{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}

-- | Module to handle generation of hexagonal grid meshes and game-level calculations wrt hexagonal coordinates
module Hex where

import GHC.Float
import GHC.Records
import Ghengin.Component.Mesh.Hex
import qualified Data.List as List

import Ghengin.Asset.Texture
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad
import Data.IORef
import Ghengin.Vulkan
import Ghengin.Scene.Graph
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.Transform.Animation
import Ghengin.Component.Mesh
import Ghengin.Component hiding (get)
import Ghengin.Component.UI
import Ghengin.Render.Packet
import Ghengin.Utils (get)
import Ghengin
import Numeric.Noise

import Math.Geometry.Grid hiding (size)
import Math.Geometry.Grid.Hexagonal

type HexMaterial = '[Vec3, Texture2D]

data Player = Player Int Int -- ^ Position where it's at
instance Component Player where type Storage Player = Unique Player

-- This world is pretty bothersome, we gotta check where it's better to have it live.
data World = World { renderPackets :: !(Storage RenderPacket)
                   , transforms    :: !(Storage Transform)
                   , transformAnimations :: !(Storage (TransformAnimation World))
                   , modelMatrices :: !(Storage ModelMatrix)
                   , cameras       :: !(Storage Camera)
                   , uiwindows     :: !(Storage (UIWindow World))
                   , entityParents :: !(Storage Parent)
                   , entityCounter :: !(Storage EntityCounter)
                   , hexes         :: !(Storage Hexagon)
                   , player        :: !(Storage Player)
                   }

makeHexMeshes :: Float -> Float -> Ghengin w [Mesh]
makeHexMeshes size percent = do
  flatMesh <- lift $
    case makeHexFace size percent (0,0) of
      (HexFace _ _ verts ixs) -> do
        createMeshWithIxs
            (zipWith3 Vertex verts (List.repeat $ vec3 0 (-1) 0)
                                   (List.cycle [vec3 1 1 1,
                                          vec3 1 1 1, vec3 1 1 1, vec3 1 1 1, vec3 1 1 1, vec3 1 1 1, vec3 1 1 1, -- Inner hex
                                          vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, -- Outer hex for borders
                                          vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0])) ixs
  pure [flatMesh, flatMesh, flatMesh]


data Hexagon = Hexagon Int Int
  deriving Show
instance Component Hexagon where type Storage Hexagon = Map Hexagon

-- | Create a hexagonal grid with given size centered on the axial coordinates (0,0)
createHexGrid :: Int -> HexHexGrid
createHexGrid = hexHexGrid

-- | Generate a renderable mesh from a HexGrid.
--
-- We will always create our mesh from a (0,0) centered hex-grid despite our
-- actual player coordinates possibly being any (x,y) coordinate
--
-- We need to:
--
-- (1) Compute Hex coord to 2D world space coord (x,y) of the hex center (get vertex C)
-- (2) Compute offset of corners and add them to the hex center coordinates
--      (C1, C2, C3, C4, C5, C6)
-- (3) Create 3d vertex for each corner and center with fixed y value using the
--      2D coordinates, and duplicate them to create the bottom face of the hexagon 
-- (4) Create faces of hexagon using the 3d vertices
-- (5) Create bottom faces
-- (6) Create side faces using vertices from bottom and top faces
hexGridMeshes :: Float -> [Mesh] -> HexHexGrid -> Ghengin w (M.Map (Int,Int) (Transform,Mesh))
hexGridMeshes size (head -> flatMesh) hgrid = do
  let faceixs = indices hgrid
  pure $ foldr (\(q,r) -> M.insert (q,r) (Transform (liftHexCoord size (q,r)) (vec3 1 1 1) (vec3 0 0 0), flatMesh)) mempty faceixs

-- noiseVal (WithVec3 x y z) = ((double2Float $ coherentNoise 0 (float2Double x * 20, float2Double y * 20, float2Double z * 20)) + 1)
-- setNoiseY v@(WithVec3 x _ z) = vec3 x (noiseVal v) z

-- * Hex settings

-- The settings thing is very confusing. We need a better design for this on the
-- engine side.
-- Settings are a terrible design

-- data HexSettings = HexSettings { hexSize :: !(IORef Float)
--                                , gridSize :: !(IORef Int)
--                                , innerHexPercent :: !(IORef Float)
--                                }

-- instance UISettings HexSettings where
--   type ReactivityInput  HexSettings = M.Map (Int,Int) (Transform,Mesh) -> SceneGraph World ()
--   type ReactivityOutput HexSettings = ()
--   type ReactivityConstraints HexSettings w = (w ~ World, HasField "renderPackets" w (Storage RenderPacket), Has w (Renderer ()) Hexagon)
  
--   makeSettings = HexSettings <$> newIORef 1 <*> newIORef 2 <*> newIORef 0.9

--   makeComponents s@(HexSettings hs gs ip) makeRenderTiles = do
--     b1 <- sliderFloat "Hex Size" hs 0 15
--     b2 <- sliderInt   "Grid Size" gs 1 15
--     b3 <- sliderFloat "Inner Hex" ip 0 1

--     -- When changed:
--     when (V.or [b1,b2,b3]) $ do

--       newMeshes <- gridFromSettings s

--       -- For now, we simply delete all hexagons but the center which has the special mat and re-do them from scratch

--       cmapM $ \(Hexagon _ _, (RenderPacket oldMesh _ _ _)) -> lift (freeMesh oldMesh) >> pure (Nothing :: Maybe RenderPacket)
      
--       sceneGraph $ makeRenderTiles newMeshes

--       pure ()

-- gridFromSettings :: HexSettings -> Ghengin w (M.Map (Int,Int) (Transform, Mesh))
-- gridFromSettings (HexSettings hs gs ip) = do
--   hexS  <- liftIO $ readIORef hs
--   gridS <- liftIO $ readIORef gs
--   ipS   <- liftIO $ readIORef ip

--   hexGridMeshes hexS undefined (createHexGrid gridS)

