-- fmap (++ "!") getLine

-- `(->) r` is `r ->` so a partially applied ->
-- `->` is a type constructor too [:optimizer:](https://cdn.discordapp.com/emojis/1522362343512866959.webp?size=96)

-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))
--
-- instance Functor ((->) r) where
--     fmap = (.)

