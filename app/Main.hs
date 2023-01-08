{-# LANGUAGE BlockArguments #-}
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
import Ghengin.Asset.Texture
import Ghengin.Component
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Component.Camera
import Ghengin.Component.Material
import Ghengin.Scene.Graph
import Ghengin.Render.Packet

import qualified Shader
import Hex

instance Monad m => Has World m Hexagon where getStore = SystemT (asks (.hexes))


init :: Ghengin World _
init = do

  -- TODO: Sampler should be freed separately so we can share it
  sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_REPEAT
  sand    <- lift $ texture "assets/IMG_7472.JPG" sampler
  grass   <- lift $ texture "assets/IMG_5873.JPG" sampler

  settings <- liftIO $ makeSettings @HexSettings
  -- TODO Lens would be good here to access fields of settings directly

  gridMeshes <- gridMeshesFromSettings settings

  gridPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  gridMaterial <- lift $ material (StaticBinding (vec3 0 0 0) . Texture2DBinding sand . Done) gridPipeline
  selectedMaterial <- lift $ material (StaticBinding (vec3 1 0.74 0) . Texture2DBinding grass . Done) gridPipeline

  sceneGraph do

    let makeRenderTiles :: M.Map (Int,Int) Mesh -> SceneGraph World ()
        makeRenderTiles gmeshes = 
          forM_ (M.toList gmeshes) $ \((q,r),gridMesh) ->
            let mat = if q == 0 && r == 0 then selectedMaterial else gridMaterial in do
              newEntity (renderPacket gridMesh mat gridPipeline, Hexagon q r, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))

    makeRenderTiles gridMeshes

    newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
              , Transform (vec3 0 0 (-3)) (vec3 1 1 1) (vec3 0 0 0))

    newEntityUI "Hex Grid" (makeComponents settings makeRenderTiles)

  pure (sand, grass)

update :: _ -> DeltaTime -> Ghengin World Bool
update _ dt = do
  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr
  pure False

end :: _ -> Ghengin w ()
end (sand,grass) = do
  dev <- lift getDevice
  liftIO $ freeTexture dev sand
  liftIO $ freeTexture dev grass
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  -- setLogLevel LogTrace
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w Main.init undefined update end

radians :: Floating a => a -> a
radians d = d * (pi/180)
