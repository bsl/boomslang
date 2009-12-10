module Graphics.Color
  ( -- * Types
    Color
    -- * Accessors
  , redChannel
  , greenChannel
  , blueChannel
  , alphaChannel
    -- * Constructors
  , make
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Accessor.Template (deriveAccessors)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Color a = Color
  { redChannel_   :: a
  , greenChannel_ :: a
  , blueChannel_  :: a
  , alphaChannel_ :: a
  }
  deriving (Eq, Show)

$(deriveAccessors ''Color)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

make :: a -> a -> a -> a -> Color a
make = Color
