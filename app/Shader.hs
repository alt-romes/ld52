{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE QualifiedDo      #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Shader where

import Ghengin.Shaders.FIR
import Ghengin.Shaders.Lighting
import Ghengin.Shaders.Fixed
import Ghengin.Shaders

shaderPipeline :: GShaderPipeline _
shaderPipeline
  = StructInput @VertexData @(Triangle List)
    :>-> (vertex, ())
    :>-> (fragment, ())

type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 1 0 ':-> V 3 Float -- in normal
   , Slot 2 0 ':-> V 3 Float -- in color
   ]

type VertexDefs
  = '[ "main"        ':-> EntryPoint '[] Vertex

     , "out_col" ':-> Output '[ Location 0 ] (V 3 Float)
     , "2d_pos"  ':-> Output '[ Location 1 ] (V 2 Float)

     , "out_position" ':-> Output '[ Location 2 ] (V 4 Float)
     , "out_normal"   ':-> Output '[ Location 3 ] (V 4 Float)
     ]
     :++: FixedDescriptorSetZero
     :++: FixedPushConstant
     :++: FixedVertices


type FragmentDefs
      -- Always required to output a color from the fragment shader
  =  '[ "out_col"  ':-> Output '[ Location 0 ] (V 4 Float)
      , "in_color" ':-> Input  '[ Location 0 ] (V 3 Float)
      , "2d_pos"   ':-> Input  '[ Location 1 ] (V 2 Float)

      -- Fragment shader
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment

      , "tile_tex" ':-> Texture2D '[ DescriptorSet 1, Binding 0 ] (RGBA8 UNorm)
      , "border" ':-> Uniform '[ DescriptorSet 1, Binding 1 ] (Struct '[ "color" ':-> V 3 Float ])

      -- Blinn phong
      , "in_position" ':-> Input '[ Location 2 ] (V 4 Float)
      , "in_normal"   ':-> Input '[ Location 3 ] (V 4 Float)
      ]
      :++: FixedDescriptorSetZero


vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"
    ~(Vec3 cx cy cz) <- get @"in_color"

    -- Blinn Phong
    modelM <- use @(Name "push" :.: Name "model")
    put @"out_position" (modelM !*^ (Vec4 x y z 1))
    put @"out_normal"   (modelM !*^ (Vec4 nx ny nz 0)) -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)

    put @"out_col" (Vec3 cx cy cz)
    put @"2d_pos"  (Vec2 x z)

    put @"gl_Position" =<< applyMVP (Vec4 x y z 1)

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    ~(Vec2 px pz)       <- get @"2d_pos"
    ~(Vec3 icx icy icz) <- get @"in_color"
    ~(Vec3 bcr bcg bcb) <- use @(Name "border" :.: Name "color")

    ~(Vec4 tx ty tz _) <- use @(ImageTexel "tile_tex") NilOps (Vec2 px pz)

    let isBorder = icx /= 1

    let col = if isBorder then (Vec3 bcr bcg bcb)
                          else (Vec3 (tx) (ty) (tz))
    put @"out_col" . add1 =<< blinnPhong 32 col

add1 :: Code (V 3 Float) -> Code (V 4 Float)
add1 (Vec3 x y z) = Vec4 x y z 1

-- TODO: Sinusoidal animations

