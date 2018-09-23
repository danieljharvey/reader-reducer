module AppData where

data DogState = DogState {
    dogName :: String,
    dogAge  :: Integer
} deriving (Show)

data CatState = CatState {
    catName :: String,
    catAge  :: Integer
} deriving (Show)

data AllState = AllState {
    dogState  :: DogState,
    catState  :: CatState,
    globalInt :: Integer
} deriving (Show)

initialDogState :: DogState
initialDogState = DogState {
    dogName = "Steve",
    dogAge = 100
}

initialCatState :: CatState
initialCatState = CatState {
    catName = "Peeboo",
    catAge = 7
}

initialState :: AllState
initialState = AllState {
    dogState = initialDogState,
    catState = initialCatState,
    globalInt = 69
}
