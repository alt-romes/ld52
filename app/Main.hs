{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Ghengin.Vulkan
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

import qualified Shader
import Hex

instance Monad m => Has World m Hexagon where getStore = SystemT (asks (.hexes))
instance Monad m => Has World m Player where getStore = SystemT (asks (.player))


init :: Ghengin World _
init = do

  sampler <- lift $ createSampler FILTER_LINEAR SAMPLER_ADDRESS_MODE_REPEAT
  sand    <- lift $ texture "assets/Sand.jpg" sampler
  grass   <- lift $ texture "assets/Grass.jpg" sampler -- heavy image...

  settings <- liftIO $ makeSettings @HexSettings

  gridMeshes <- gridFromSettings settings

  gridPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  sandMat <- lift $ material (StaticBinding (vec3 0.9 0.75 0) . Texture2DBinding sand . Done) gridPipeline
  grassMat <- lift $ material (StaticBinding (vec3 0.2 1 0.2) . Texture2DBinding grass . Done) gridPipeline

  pl <- lift $ newSphereMesh 20 Nothing

  sceneGraph do

    let makeRenderTiles :: M.Map (Int,Int) (Transform, Mesh) -> SceneGraph World ()
        makeRenderTiles gmeshes =
          forM_ (M.toList gmeshes) $ \((q,r),(tr,gridMesh)) ->
            let mat = if r == 0 || q `mod` r == 0 then grassMat else sandMat in do
              liftIO $ print(q,r)
              rp <- renderPacket gridMesh mat gridPipeline
              newEntity (rp, Hexagon q r, tr)

    makeRenderTiles gridMeshes

    newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
              , Transform (vec3 0 0 (-3)) (vec3 1 1 1) (vec3 0 0 0))

    rp <- renderPacket pl grassMat gridPipeline
    newEntity ( rp
              , Player, Transform (vec3 0 0 0) (vec3 0.5 0.5 0.5) (vec3 2 0 0))

    newEntityUI "Hex Grid" (makeComponents settings makeRenderTiles)

  pure (sand, grass,sampler)


movePlayer :: DeltaTime -> HexDirection -> Ghengin World ()
movePlayer dt hexDir = do
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
  cmapM \(Player, manim :: Maybe (TransformAnimation World), Transform pos _ _) ->
    case manim of
      Nothing -> do
        -- We weren't in an animation, so we start one to the next tile
        pure (Just (transformAnimation (vecFromHexDir 1 hexDir) pos (pure ())))
      Just ta -> do
        pure (Just ta)

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

update :: _ -> DeltaTime -> Ghengin World Bool
update _ dt = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  -- Left up
  ifPressed Key'U (movePlayer dt Northwest) (pure ())

  -- Right up
  ifPressed Key'I (movePlayer dt Northeast) (pure ())

  -- Left
  ifPressed Key'H (movePlayer dt West) (pure ())

  -- Right
  ifPressed Key'K (movePlayer dt East) (pure ())

  -- Left down
  ifPressed Key'N (movePlayer dt Southwest) (pure ())

  -- Right down
  ifPressed Key'M (movePlayer dt Southeast) (pure ())

  -- Always update transform animations
  transformAnimationUpdate 1 dt

  pure False

end :: _ -> Ghengin w ()
end (sand,grass,samp) = do
  dev <- lift getDevice
  -- Double freeing the sampler
  liftIO $ freeTexture dev sand
  liftIO $ freeTexture dev grass
  liftIO $ destroySampler dev samp
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  -- setLogLevel LogTrace
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w Main.init undefined update end

radians :: Floating a => a -> a
radians d = d * (pi/180)
