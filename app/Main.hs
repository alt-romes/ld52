{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Proxy
import Ghengin.Vulkan
import Ghengin.Vulkan.Sampler
import Control.Monad
import qualified Data.Map as M
import qualified Data.Vector as V
import Ghengin
import Ghengin.Utils
import Ghengin.Vulkan.Sampler
import Ghengin.Component.Mesh
import Ghengin.Component.Mesh.Sphere hiding (UnitFace(..), UnitSphere(..))
import Ghengin.Component.Mesh.Hex
import Ghengin.Component.Mesh.Obj
import Ghengin.Asset.Texture
import Ghengin.Component
import Ghengin.Component.Transform
import Ghengin.Component.Transform.Animation
import Ghengin.Component.UI
import Ghengin.Component.Camera
import Ghengin.Component.Material
import Ghengin.Scene.Graph
import Ghengin.Render.Packet
import Ghengin.Input

import Math.Geometry.Grid.HexagonalInternal (HexDirection(..))
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid hiding (size)

import qualified Shader
import Map hiding (HexMaterial)
import Hex

pattern SIZE :: Float
pattern SIZE = 1

instance Monad m => Has World m Hexagon where getStore = SystemT (asks (.hexes))
instance Monad m => Has World m Player where getStore = SystemT (asks (.player))

data Game p = Game { meshes :: [Mesh '[Vec3, Vec3, Vec3]]
                   , materials :: [Material HexMaterial]
                   , textures :: [Texture2D]
                   , pipeline :: RenderPipeline p
                   , sampler  :: Sampler
                   , terrain  :: Terrain
                   }

init :: Ghengin World (Game _)
init = do

  sampler <- lift $ createSampler FILTER_LINEAR SAMPLER_ADDRESS_MODE_REPEAT
  sand    <- lift $ texture "/Users/romes/ld52/assets/Sand.jpg" sampler
  grass   <- lift $ texture "/Users/romes/ld52/assets/Grass.jpg" sampler
  water   <- lift $ texture "/Users/romes/ld52/assets/Water.jpg" sampler

  human   <- lift $ loadObjMesh "assets/human.obj"

  meshes <- makeHexMeshes SIZE 0.95

  pipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  sandMat  <- lift $ material (StaticBinding (vec3 0.9 0.75 0) . Texture2DBinding sand . Done) pipeline
  grassMat <- lift $ material (StaticBinding (vec3 0.2 1 0.2) . Texture2DBinding grass . Done) pipeline
  waterMat <- lift $ material (StaticBinding (vec3 0.2 0.2 1) . Texture2DBinding water . Done) pipeline

  let mats = [waterMat, grassMat, sandMat]

  let terrain = makeTerrain (indices $ hexHexGrid 200) mats meshes

  sceneGraph do

    makeRenderTiles pipeline terrain ((0,0):neighbours UnboundedHexGrid (0,0))

    player <- renderPacket human waterMat pipeline

    newEntity' ( player, Player 0 0, Transform (vec3 0 0 0) (vec3 1.5 1.5 1.5) (vec3 0 0 0)) do
      -- newEntity ( player, Transform (vec3 0 0 0.3) (vec3 0.5 0.5 0.5) (vec3 0 0 0))

      newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
                , Transform (vec3 0 (-2) (-5)) (vec3 1 1 1) (vec3 (-0.6) 0 0))

    -- newEntityUI "Hex Grid" (makeComponents settings makeRenderTiles')

  pure $ Game meshes [waterMat, grassMat, sandMat] [sand, grass, water] pipeline sampler terrain

makeRenderTiles :: RenderPipeline _ -> Terrain -> [(Int,Int)] -> SceneGraph World ()
makeRenderTiles gridPipeline terr ixs =
  forM_ ixs $ \(q,r) ->
    let mat = fst $ terr ! (q,r)
        mesh = snd $ terr ! (q,r)
     in do
      rp <- renderPacket mesh mat gridPipeline
      newEntity (rp, Hexagon q r, makeHexTransform (q,r))

makeHexTransform :: (Int,Int) -> Transform
makeHexTransform (q,r) = Transform (liftHexCoord SIZE (q,r)) (vec3 1 1 1) (vec3 0 0 0)


movePlayer :: Game _ -> HexDirection -> Ghengin World ()
movePlayer g hexDir = do
  -- If player is moving
  --  (1) Keep moving in the same direction by altering the transform of the player (the camera follows the player) (handled by transform animation)
  --
  -- At the beginning of a movement (if we weren't already moving)
  --  (1) Add 3 tiles in the direction we are travelling in
  --  (2) Set the player to moving
  --  (3) Move along the hex axis (handled by transform animation)
  --
  -- Handled by transform animation:
  -- When we reach the destination tile
  --  (1) End the movement
  --  (2) Drop the three tiles we left behind (animate through the transform by setting to "dropping")
  --
  cmapM \(Player px py, manim :: Maybe (TransformAnimation World), Transform pos scale rot) ->
    case manim of
      Nothing -> do
        -- We weren't in an animation, so we start one to the next tile
        --  * A player movement towards the chosen tile
        --  * A drop movement on the three tiles we leave behind
        --  * A rise movement on the three tiles we approach ahead
        
        let toAdd = coordsToAdd (px,py) hexDir
            toRem = coordsToRem (px,py) hexDir

        sceneGraph $ do
          forM_ toAdd $ \(q,r) -> do
            rp <- renderPacket (snd $ g.terrain ! (q,r)) (fst $ g.terrain ! (q,r)) g.pipeline
            let tr = makeHexTransform (q,r)
                tr' = tr{Ghengin.Component.Transform.position = withVec3 tr.position (\x _ z -> vec3 x 0.5 z)} :: Transform
            newEntity (rp, Hexagon q r, tr', transformAnimation @World (vec3 0 (-0.5) 0) tr'.position 2 (pure ()))
          

        -- Dropping tiles
        cmapM \(Hexagon q r, Not @(TransformAnimation World), Transform pos' _ _, e :: Entity) -> do -- On all tiles without an animation

          let
              fin :: Ghengin World ()
              fin = destroy e (Proxy @(Hexagon, TransformAnimation World, Transform, RenderPacket))

          if (q,r) `elem` toRem
             then pure $ Just $ TransformAnimation' (vec3 0 0.5 0) (pos' + vec3 0 10 0) 2 fin
             else pure Nothing


-- Rename transformAnimation to positionAnimation
        pure (uncurry Player (newCoords (px,py) hexDir), Just (transformAnimation (vecFromHexDir 1 hexDir) pos 1 (pure ())), Transform pos scale rot) -- (angleFromHexDir hexDir))
      Just ta -> do
        pure (Player px py, Just ta, Transform pos scale rot)

  pure ()

vecFromHexDir :: Float -> HexDirection -> Vec3
vecFromHexDir size = \case
  Northwest -> liftHexCoord size (-1,1) - center
  Northeast -> liftHexCoord size (0,1)  - center
  West      -> liftHexCoord size (-1,0) - center
  East      -> liftHexCoord size (1,0)  - center
  Southwest -> liftHexCoord size (0,-1) - center
  Southeast -> liftHexCoord size (1,-1) - center
  where
    center = liftHexCoord size (0,0)

angleFromHexDir :: HexDirection -> Vec3
angleFromHexDir = \case
  Northwest -> 2*pi*0.5/6
  Northeast -> 2*pi*1.5/6
  East      -> 2*pi*2.5/6
  Southeast -> 2*pi*3.5/6
  Southwest -> 2*pi*4.5/6
  West      -> 2*pi*5.5/6

newCoords :: (Int,Int) -> HexDirection -> (Int,Int)
newCoords (x,y) = \case
  Northwest -> (x-1, y+1)
  Northeast -> (x,y+1)
  West      -> (x-1,y)
  East      -> (x+1,y)
  Southwest -> (x,y-1)
  Southeast -> (x+1,y-1)
  
coordsToAdd :: (Int,Int) -> HexDirection -> [(Int,Int)]
coordsToAdd (x,y) dir = case dir of
  Northwest -> [w,nw,ne]
  Northeast -> [nw,ne,e]
  West      -> [sw,w,nw]
  East      -> [ne,e,se]
  Southwest -> [se,sw,w]
  Southeast -> [e,se,sw]
  where
    [w,nw,ne,e,se,sw] = neighbours UnboundedHexGrid (newCoords (x,y) dir) -- only works for grid size = 2

coordsToRem :: (Int,Int) -> HexDirection -> [(Int,Int)]
coordsToRem (x,y) = \case
  Southeast -> [w,nw,ne]
  Southwest -> [nw,ne,e]
  East      -> [sw,w,nw]
  West      -> [ne,e,se]
  Northeast -> [se,sw,w]
  Northwest -> [e,se,sw]
  where
    [w,nw,ne,e,se,sw] = neighbours UnboundedHexGrid (x,y) -- only works for grid size = 2

update :: Game _ -> DeltaTime -> Ghengin World Bool
update g dt = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  -- Left up
  ifPressed Key'Y (movePlayer g Northwest) (pure ())

  -- Right up
  ifPressed Key'U (movePlayer g Northeast) (pure ())

  -- Left
  ifPressed Key'G (movePlayer g West) (pure ())

  -- Right
  ifPressed Key'J (movePlayer g East) (pure ())

  -- Left down
  ifPressed Key'B (movePlayer g Southwest) (pure ())

  -- Right down
  ifPressed Key'N (movePlayer g Southeast) (pure ())

  -- Always update transform animations
  transformAnimationUpdate dt

  pure False

end :: _ -> Ghengin w ()
end Game{} = do
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  -- setLogLevel LogTrace
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w Main.init undefined update end

radians :: Floating a => a -> a
radians d = d * (pi/180)
