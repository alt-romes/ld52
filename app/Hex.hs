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
hexGridMesh :: Float -> Float -> HexHexGrid -> Ghengin w Mesh
hexGridMesh size innerPercent hgrid = do
  let faceixs = indices hgrid

      -- | (1) and (2) and (3)
      hexfaces = map (makeHexFace size innerPercent) faceixs

      -- | (4) now done in the hex face
      verts = concatMap (\(HexFace vs _) -> vs) hexfaces
      ixs   = concatMap (\(HexFace _ ixs, i) -> map (+(i*13)) ixs) (zip hexfaces [0..length hexfaces]) -- 13 is number of vertices per face
      
  lift $ createMeshWithIxs (zipWith3 Vertex verts (List.repeat $ vec3 0 1 0) (List.cycle [vec3 1 1 1, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0, vec3 0 0 0])) ixs
  -- The first of the seven colors is white and represents the center of the
  -- hexagon. The corners are black and the interpolation will make a black
  -- border.

-- | Hex face: the vertices and indices to form a face
data HexFace = HexFace ![Vec3] -- ^ vertices in this face
                       ![Int]  -- ^ indices
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
--
-- We will need to duplicate each corner per corner ix.
-- One for the inner hex and other for the outer one
hexCornerOffset :: Float -- ^ Size
                -> Float -- ^ Inner percent
                -> Int   -- ^ Hex corner ix ∈ [0,6[
                -> ((Float, Float), (Float, Float)) -- ^ The outer and inner hex coords respectively
hexCornerOffset size percent ix =
  let angle = case ix of
                0 -> 2*pi*0.5/6
                1 -> 2*pi*1.5/6
                2 -> 2*pi*2.5/6
                3 -> 2*pi*3.5/6
                4 -> 2*pi*4.5/6
                5 -> 2*pi*5.5/6
                i -> error $ "Unexpected corner index " <> show i
   in ((size * cos angle, size * sin angle), (size * cos angle * percent, size * sin angle * percent))

-- | A Hex face is composed of one center, 6 corners, and 6 triangular faces composed of them
makeHexFace :: Float -> Float -> Index HexHexGrid -> HexFace
makeHexFace size percent (q, r) =
  let
    c0@(cx,cz) = hexGridTo2DWorld size (q,r)
    corners@[(c1,c1i),(c2,c2i),(c3,c3i),(c4,c4i),(c5,c5i),(c6,c6i)]
      = map ((\((cnx,cnz), (cnix,cniz)) -> (vec3 (cnx + cx) 1 (cnz + cz), vec3 (cnix + cx) 1 (cniz + cz)) ) . hexCornerOffset size percent) [0..5]
    
  in
    --        C0           1    2     3   4     5   6    7    8   9  10  11  12
    HexFace [vec3 cx 1 cz, c1i, c2i, c3i, c4i, c5i, c6i, c1, c2, c3, c4, c5, c6]
            ([1, 2, 0, -- Inner hex triangle faces
             2, 3, 0,
             3, 4, 0,
             4, 5, 0,
             5, 6, 0,
             6, 1, 0]
             <> [ 7, 2, 1 -- Quads between outer and inner triangle
                , 7, 8, 2
                , 8, 3, 2
                , 8, 9, 3
                , 9, 4, 3
                , 9, 10, 4
                , 10, 5, 4
                , 10, 11, 5
                , 11, 6, 5
                , 11, 12, 6
                , 12, 1, 6
                , 12, 7, 1
                ])



data HexSettings = HexSettings { hexSize :: !(IORef Float)
                               , gridSize :: !(IORef Int)
                               , innerHexPercent :: !(IORef Float)
                               }

instance UISettings HexSettings where
  type ReactivityInput  HexSettings = ()
  type ReactivityOutput HexSettings = ()
  type ReactivityConstraints HexSettings w = (HasField "renderPackets" w (Storage RenderPacket))
  
  makeSettings = HexSettings <$> newIORef 1 <*> newIORef 1 <*> newIORef 0.9

  makeComponents s@(HexSettings hs gs ip) () = do
    b1 <- sliderFloat "Hex Size" hs 0 15
    b2 <- sliderInt   "Grid Size" gs 1 15
    b3 <- sliderFloat "Inner Hex" ip 0 1

    -- When changed:
    when (or [b1,b2,b3]) $ do

      newMesh <- gridMeshFromSettings s

      -- Update all hexagons
      cmapM $ \(RenderPacket oldMesh mat pipeline k) -> do
        lift $ freeMesh oldMesh
        pure $ RenderPacket newMesh mat pipeline k

      pure ()

gridMeshFromSettings :: HexSettings -> Ghengin w Mesh
gridMeshFromSettings (HexSettings hs gs ip) = do
  hexS <- liftIO $ readIORef hs
  gridS <- liftIO $ readIORef gs
  ipS   <- liftIO $ readIORef ip

  hexGridMesh hexS ipS (createHexGrid gridS)
