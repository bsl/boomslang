{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Vector where

import Graphics.Rendering.OpenGL

data Vec = V
  { getX :: !GLfloat
  , getY :: !GLfloat
  } deriving (Show, Eq, Ord)

infixl 7 ^*.
infixl 7 .*^
infixl 7 ^/.
infixl 7 `dot`
infixl 7 `cross`
infixl 6 ^+^
infixl 6 ^-^

class Vector2D v c | v -> c where
  (^+^) :: v -> v -> v
  (^-^) :: v -> v -> v
  (^*.) :: v -> c -> v
  (.*^) :: c -> v -> v
  (^/.) :: v -> c -> v
  vnull :: v
  dot   :: v -> v -> c
  cross :: v -> v -> c
  vlen  :: v -> c
  mul   :: v -> v -> v

instance Vector2D Vec GLfloat where
  V x1 y1 ^+^ V x2 y2     = V (x1+x2) (y1+y2)
  V x1 y1 ^-^ V x2 y2     = V (x1-x2) (y1-y2)
  V x y ^*. t             = V (x*t) (y*t)
  t .*^ V x y             = V (x*t) (y*t)
  V x y ^/. t             = V (x/t) (y/t)
  vnull                   = V 0 0
  V x1 y1 `dot` V x2 y2   = x1*x2+y1*y2
  V x1 y1 `cross` V x2 y2 = x1*y2-x2*y1
  vlen (V x y)            = sqrt (x*x+y*y)
  V x1 y1 `mul` V x2 y2   = V (x1*x2) (y1*y2)
