{-# LANGUAGE DataKinds #-}
module Map where
import GHC.Float
import Numeric.Noise
import Numeric.Noise.Ridged
import Ghengin
import Ghengin.Component.Material
import Ghengin.Asset.Texture
import Ghengin.Component.Mesh
import qualified Data.Set as S
import qualified Data.Map as M

type HexMaterial = '[Vec3, Texture2D]

newtype Terrain = Terrain (M.Map (Int,Int) (Material HexMaterial, Mesh))

tileNoise :: Vec3 -> Int
tileNoise (normalize -> WithVec3 x y z) = step (noiseValue ridgedNoise (float2Double x, float2Double y, float2Double z)) dist
  where
    step :: Double -> [(Double,Int)] -> Int
    step _ [] = 0
    step w ((t,i):xs) = if w > t then i else step w xs

    dist :: [(Double, Int)]
    dist = [(0.5, 2), (0, 1)]

    seed        = 1
    octaves     = 2
    scale       = 1
    frequency   = 1
    lacunarity  = 2
    ridgedNoise = ridged seed octaves scale frequency lacunarity

makeTerrain :: [(Int,Int)] -> [Material HexMaterial] -> [Mesh] -> Terrain
makeTerrain ixs mats meshes =
  Terrain $ M.fromSet (\(q,r) -> let i = tileNoise (vec3 (fromIntegral q) 1 (fromIntegral r)) in (mats !! i, meshes !! i)) (S.fromList ixs)

(!) :: Terrain -> (Int, Int) -> (Material HexMaterial, Mesh)
(!) (Terrain m) = (m M.!)

