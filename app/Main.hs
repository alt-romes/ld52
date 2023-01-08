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
import Ghengin.Asset.Texture
import Ghengin.Component
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Component.Camera
import Ghengin.Component.Material
import Ghengin.Scene.Graph
import Ghengin.Render.Packet
import Ghengin.Input

import Math.Geometry.Grid.HexagonalInternal (HexDirection(..))

import qualified Shader
import Hex

instance Monad m => Has World m Hexagon where getStore = SystemT (asks (.hexes))
instance Monad m => Has World m Player where getStore = SystemT (asks (.player))


init :: Ghengin World _
init = do

  -- TODO: Sampler should be freed separately so we can share it (apparently not?)
  sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_REPEAT
  sand    <- lift $ texture "assets/IMG_7472.JPG" sampler
  grass   <- lift $ texture "assets/IMG_5873.JPG" sampler

  settings <- liftIO $ makeSettings @HexSettings
  -- TODO Lens would be good here to access fields of settings directly

  gridMeshes <- gridMeshesFromSettings settings

  gridPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  gridMaterial <- lift $ material (StaticBinding (vec3 0 0 0) . Texture2DBinding sand . Done) gridPipeline
  selectedMaterial <- lift $ material (StaticBinding (vec3 1 0.74 0) . Texture2DBinding grass . Done) gridPipeline

  pl <- lift $ newSphereMesh 20 Nothing

  sceneGraph do

    let makeRenderTiles :: M.Map (Int,Int) Mesh -> SceneGraph World ()
        makeRenderTiles gmeshes = 
          forM_ (M.toList gmeshes) $ \((q,r),gridMesh) ->
            let mat = if q == 0 && r == 0 then selectedMaterial else gridMaterial in do
              newEntity (renderPacket gridMesh mat gridPipeline, Hexagon q r, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))

    makeRenderTiles gridMeshes

    newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
              , Transform (vec3 0 0 (-3)) (vec3 1 1 1) (vec3 0 0 0))

    newEntity ( renderPacket pl selectedMaterial gridPipeline
              , Player, Transform (vec3 0 0.5 0) (vec3 0.5 0.5 0.5) (vec3 2 0 0))

    newEntityUI "Hex Grid" (makeComponents settings makeRenderTiles)

  pure (sand, grass,sampler)


movePlayer :: DeltaTime -> Maybe HexDirection -> Ghengin World ()
movePlayer dt d = do
  -- If player is moving
  --  (1) Keep moving in the same direction by altering the transform of the player (the camera follows the player)
  --
  -- At the beginning of a movement (if we weren't already moving)
  --  (1) Add 3 tiles in the direction we are travelling in
  --  (2) Set the player to moving
  --  (3) Move along the hex axis
  --
  -- When we reach the destination tile
  --  (1) End the movement
  --  (2) Drop the three tiles we left behind (animate through the transform by setting to "dropping")
  --
  -- Actually, the last two bits could fit in a generic "update entities" that moves if the things are moving, and stops moving if reached
  liftIO $ print d
  pure ()

  

update :: _ -> DeltaTime -> Ghengin World Bool
update _ dt = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  -- Left up
  ifPressed Key'U (movePlayer dt $ Just Northwest) (pure ())

  -- Right up
  ifPressed Key'I (movePlayer dt $ Just Northeast) (pure ())

  -- Left
  ifPressed Key'H (movePlayer dt $ Just West) (pure ())

  -- Right
  ifPressed Key'L (movePlayer dt $ Just East) (pure ())

  -- Left down
  ifPressed Key'N (movePlayer dt $ Just Southwest) (pure ())

  -- Right down
  ifPressed Key'M (movePlayer dt $ Just Southeast) (pure ())

  -- Move player if it has started moving but not finished, even if nothing was pressed
  movePlayer dt Nothing

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
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w Main.init undefined update end

radians :: Floating a => a -> a
radians d = d * (pi/180)
