{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Module to handle generation of hexagonal grid meshes and game-level calculations wrt hexagonal coordinates
module Hex where

import GHC.Records
import qualified Data.List as List

import Control.Monad
import Data.IORef
import Ghengin.Component.Mesh
import Ghengin.Component
import Ghengin.Component.UI
import Ghengin.Utils hiding (Index)
import Ghengin.Render.Packet
import Ghengin

import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal

-- | Create a hexagonal grid with given size centered on the axial coordinates
-- (0,0)
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
hexGridMesh :: Float -> HexHexGrid -> Ghengin w Mesh
hexGridMesh size hgrid = do
  let faceixs = indices hgrid

      -- | (1) and (2) and (3)
      hexfaces = map (makeHexFace size) faceixs

      -- | (4)
      hexTriFacesIxs = [1, 2, 0,
                        2, 3, 0,
                        3, 4, 0,
                        4, 5, 0,
                        5, 6, 0,
                        6, 1, 0]

      verts = concatMap (\(HexFace c0 c1 c2 c3 c4 c5 c6) -> [c0, c1, c2, c3, c4, c5, c6]) hexfaces
      ixs   = concatMap (\i -> map (+(i*7)) hexTriFacesIxs) [0..length hexfaces]
      
  lift $ createMeshWithIxs (zipWith3 Vertex verts (List.repeat $ vec3 0 1 0) (List.repeat $ vec3 0 0 0)) ixs

-- | Hex face: center vertex and all 6 corner vertices (C1,C2,C3,C4,C5,C6), and triangular faces
data HexFace = HexFace !Vec3 !Vec3 !Vec3 !Vec3 !Vec3 !Vec3 !Vec3
  deriving Show

-- | ...
--
-- q basis: (0,0) -> (1,0): {x=√3, y=0}
-- r basis: (0,0) -> (0,1): {x=(width/2)=(√3/2), y=3/2}
--
-- [ x ] =  size * [ √3 √3/2 ] [ q ]
-- [ y ]           [ 0   3/2 ] [ r ]
hexGridTo2DWorld :: Float -> Index HexHexGrid -> (Float, Float)
hexGridTo2DWorld size (fromIntegral -> q, fromIntegral -> r) = (size * (sqrt 3 * q + (sqrt 3/2) * r), size * (3/2) * r)

-- Corners are working fine
hexCornerOffset :: Float -- ^ Size
                -> Int   -- ^ Hex corner ix ∈ [0,6[
                -> (Float, Float)
hexCornerOffset size ix =
  let angle = case ix of
                0 -> 2*pi*0.5/6
                1 -> 2*pi*1.5/6
                2 -> 2*pi*2.5/6
                3 -> 2*pi*3.5/6
                4 -> 2*pi*4.5/6
                5 -> 2*pi*5.5/6
                i -> error $ "Unexpected corner index " <> show i
   in (size * cos angle, size * sin angle)

-- | A Hex face is composed of one center, 6 corners, and 6 triangular faces composed of them
makeHexFace :: Float -> Index HexHexGrid -> HexFace
makeHexFace size (q, r) =
  let
    c0@(cx,cz) = hexGridTo2DWorld size (q,r)
    [c1,c2,c3,c4,c5,c6] = map ((\(cnx,cnz) -> vec3 (cnx + cx) 1 (cnz + cz)) . hexCornerOffset size) [0..5]
  in
    HexFace (vec3 cx 1 cz) c1 c2 c3 c4 c5 c6



data HexSettings = HexSettings { hexSize :: !(IORef Float)
                               , gridSize :: !(IORef Int)
                               }

instance UISettings HexSettings where
  type ReactivityInput  HexSettings = ()
  type ReactivityOutput HexSettings = ()
  type ReactivityConstraints HexSettings w = (HasField "renderPackets" w (Storage RenderPacket))
  
  makeSettings = HexSettings <$> newIORef 1 <*> newIORef 1

  makeComponents s@(HexSettings hs gs) () = do
    b1 <- sliderFloat "Hex Size" hs 0 15
    b2 <- sliderInt   "Grid Size" gs 0 15

    -- When changed:
    when (or [b1,b2]) $ do

      newMesh <- gridMeshFromSettings s
      -- Update all hexagons
      cmap $ \(RenderPacket _oldMesh mat pipeline k) -> RenderPacket newMesh mat pipeline k

      pure ()

gridMeshFromSettings :: HexSettings -> Ghengin w Mesh
gridMeshFromSettings (HexSettings hs gs) = do
  hexS <- liftIO $ readIORef hs
  gridS <- liftIO $ readIORef gs

  hexGridMesh hexS (createHexGrid gridS)
