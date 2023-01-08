{-# LANGUAGE BlockArguments #-}
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
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.Mesh.Hex
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
import Numeric.Noise

import Math.Geometry.Grid.HexagonalInternal (HexDirection(..))
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid hiding (size)

import qualified Shader
import Hex

pattern SIZE :: Float
pattern SIZE = 1

instance Monad m => Has World m Hexagon where getStore = SystemT (asks (.hexes))
instance Monad m => Has World m Player where getStore = SystemT (asks (.player))

data Game p = Game { meshes :: [Mesh]
                   , materials :: [Material HexMaterial]
                   , textures :: [Texture2D]
                   , pipeline :: RenderPipeline p
                   , sampler  :: Sampler
                   }

init :: Ghengin World (Game _)
init = do

  sampler <- lift $ createSampler FILTER_LINEAR SAMPLER_ADDRESS_MODE_REPEAT
  sand    <- lift $ texture "assets/Sand.jpg" sampler
  grass   <- lift $ texture "assets/Grass.jpg" sampler -- heavy image...

  -- settings <- liftIO $ makeSettings @HexSettings

  meshes <- makeHexMeshes SIZE 0.92
  -- gridMeshes <- gridFromSettings settings

  pipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  sandMat  <- lift $ material (StaticBinding (vec3 0.9 0.75 0) . Texture2DBinding sand . Done) pipeline
  grassMat <- lift $ material (StaticBinding (vec3 0.2 1 0.2) . Texture2DBinding grass . Done) pipeline

  pl <- lift $ newSphereMesh 20 Nothing

  sceneGraph do

    makeRenderTiles pipeline [sandMat, grassMat] meshes ((0,0):neighbours UnboundedHexGrid (0,0))

    rp <- renderPacket pl grassMat pipeline

    newEntity' ( rp, Player 0 0, Transform (vec3 0 (-1) 0) (vec3 0.5 0.5 0.5) (vec3 0 0 0)) do
      newEntity ( rp, Transform (vec3 0 (-1) 0) (vec3 0.5 0.5 0.5) (vec3 2 0 0))

      newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
                , Transform (vec3 0 (-1) (-3)) (vec3 1 1 1) (vec3 0 0 0))

    -- newEntityUI "Hex Grid" (makeComponents settings makeRenderTiles')

  pure $ Game meshes [sandMat, grassMat] [sand, grass] pipeline sampler

makeRenderTiles :: RenderPipeline _ -> [Material HexMaterial] -> [Mesh] -> [(Int,Int)] -> SceneGraph World ()
makeRenderTiles gridPipeline [grassMat, sandMat] gmeshes ixs =
  forM_ (zip ixs (cycle gmeshes)) $ \((q,r),gridMesh) ->
    let mat = if r == 0 || q `mod` r == 0 then grassMat else sandMat in do
      liftIO $ print(q,r)
      rp <- renderPacket gridMesh mat gridPipeline
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
  cmapM \(Player px py, manim :: Maybe (TransformAnimation World), Transform pos _ _) ->
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
            rp <- renderPacket (head g.meshes) (head g.materials) g.pipeline
            let tr = makeHexTransform (q,r)
                tr' = tr{Ghengin.Component.Transform.position = withVec3 tr.position (\x y z -> vec3 x 1 z)} :: Transform
            newEntity (rp, Hexagon q r, tr', transformAnimation @World (vec3 0 (-1) 0) tr'.position (pure ()))
          

        -- Dropping tiles
        cmapM \(Hexagon q r, Not @(TransformAnimation World), Transform pos' _ _, e :: Entity) -> do -- On all tiles without an animation

          let
              fin :: Ghengin World ()
              fin = destroy e (Proxy @(Hexagon, TransformAnimation World, Transform, RenderPacket))

          if (q,r) `elem` toRem
             then pure $ Just $ transformAnimation (vec3 0 1 0) pos' fin
             else pure Nothing


        pure (uncurry Player (newCoords (px,py) hexDir), Just (transformAnimation (vecFromHexDir 1 hexDir) pos (pure ())))
      Just ta -> do
        pure (Player px py, Just ta)

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
  transformAnimationUpdate 1 dt

  pure False

end :: _ -> Ghengin w ()
end Game{textures=texs, sampler=samp} = do
  dev <- lift getDevice
  -- Double freeing the sampler
  forM_ texs $ liftIO . freeTexture dev
  liftIO $ destroySampler dev samp
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  -- setLogLevel LogTrace
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w Main.init undefined update end

radians :: Floating a => a -> a
radians d = d * (pi/180)
