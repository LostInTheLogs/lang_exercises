data Point = Point Float Float deriving (Show)
data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

-- Record
data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String
  }
  deriving (Show)
