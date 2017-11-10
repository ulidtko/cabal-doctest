module Example where

type CBool = forall a. a -> a -> a

t :: CBool
t x _y = x

f :: CBool
f _x y = y

-- | Convert to conrete 'Bool'
--
-- >>> toBool t
-- True
--
-- >>> toBool f
-- False
toBool :: CBool -> Bool
toBool b = b True False

-- | Convert from concrete 'Bool'.
--
-- prop> toBool (fromBool b) == b
fromBool :: Bool -> CBool
fromBool True  = t
fromBool False = f
