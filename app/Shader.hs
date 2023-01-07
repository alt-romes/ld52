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
     -- For now the three locations (0,1,2) are fixed on the mesh, but later they will not be fixed
  = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"   ':-> Input '[ Location 1 ] (V 3 Float)
     , "in_color"    ':-> Input '[ Location 2 ] (V 3 Float)

     -- Entry point, this is the vertex shader
     , "main"        ':-> EntryPoint '[] Vertex

     -- Push constant is always passed per-model with the model transform matrix according to the scene graph
     , "push"        ':-> PushConstant '[] (Struct '[ "model" ':-> M 4 4 Float ])

     -- Descriptor set #0 is fixed for now, and the engine always passes these parameters in the descriptor set #0
     , "ubo"         ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                                  ( Struct '[ "view" ':-> M 4 4 Float
                                            , "proj" ':-> M 4 4 Float ] )

     , "out_col" ':-> Output '[ Location 0 ] (V 3 Float)
     ]


type FragmentDefs
      -- Always required to output a color from the fragment shader
  =  '[ "out_col"  ':-> Output '[ Location 0 ] (V 4 Float)
      , "in_color" ':-> Input  '[ Location 0 ] (V 3 Float)

      -- Camera position always passed by the engine (so it's in the descriptor set #0)
      , "camera_pos" ':-> Uniform '[ DescriptorSet 0, Binding 1 ]
                                    ( Struct '[ "val" ':-> V 3 Float ] )

      -- Fragment shader
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]


vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"
    ~(Vec3 cx cy cz) <- get @"in_color"
    modelM <- use @(Name "push" :.: Name "model")
    viewM  <- use @(Name "ubo" :.: Name "view")
    projM  <- use @(Name "ubo" :.: Name "proj")

    put @"out_col" (Vec3 cx cy cz)

    put @"gl_Position" ((projM !*! viewM !*! modelM) !*^ (Vec4 x y z 1))

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    ~(Vec3 icx icy icz) <- use @(Name "in_color")
    ~(Vec3 cx cy cz) <- use @(Name "camera_pos" :.: Name "val")

    put @"out_col" (Vec4 icx icy icz 1)

