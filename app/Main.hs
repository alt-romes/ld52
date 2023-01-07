{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Ghengin
import Ghengin.Utils
import Ghengin.Component
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Component.Camera
import Ghengin.Component.Material
import Ghengin.Scene.Graph
import Ghengin.Render.Packet
import Ghengin.Render.Pipeline

import qualified Shader
import Hex

data World = World { renderPackets :: !(Storage RenderPacket)
                   , transforms    :: !(Storage Transform)
                   , modelMatrices :: !(Storage ModelMatrix)
                   , cameras       :: !(Storage Camera)
                   , uiwindows     :: !(Storage (UIWindow World))
                   , entityParents :: !(Storage Parent)
                   , entityCounter :: !(Storage EntityCounter)
                   }

init :: Ghengin World ()
init = do

  settings <- liftIO $ makeSettings @HexSettings
  -- TODO Lens would be good here to access fields of settings directly

  gridMesh <- gridMeshFromSettings settings

  gridPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  gridMaterial <- lift $ material Done gridPipeline

  let rp1 = renderPacket gridMesh gridMaterial gridPipeline

  sceneGraph do
    newEntity (rp1, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))

    newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
              , Transform (vec3 0 0 (-3)) (vec3 1 1 1) (vec3 0 0 0))

    newEntityUI "Hex Grid" (makeComponents settings ())

  pure ()

update :: () -> DeltaTime -> Ghengin World Bool
update () dt = do
  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr
  pure False

end :: Ghengin w ()
end = liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  setLogLevel LogTrace
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w Main.init undefined update end

radians :: Floating a => a -> a
radians d = d * (pi/180)
