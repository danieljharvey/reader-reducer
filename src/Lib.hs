module Lib
    ( someFunc
    ) where

import           Control.Lens
import           Control.Monad.Reader as R
import           Control.Monad.Trans
import           Control.Monad.Writer

someFunc :: IO ()
someFunc = do
    print initialState
    print newState

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

data Action = IncrementDogAge | IncrementCatAge | SetAgeToGlobal deriving (Show)

type ReadWrite a = ReaderT AllState (Writer [String]) a

type ReducerFunction a = Action -> a -> ReadWrite a

dogReducer :: ReducerFunction DogState
dogReducer action state = case action of
    IncrementDogAge -> do
        tell ["DogReducer: " <> show IncrementDogAge]
        pure $ state { dogAge = dogAge state + 1 }
    _               -> pure state

catReducer :: ReducerFunction CatState
catReducer action state = do
    allState <- R.ask
    case action of
        IncrementCatAge -> do
            tell ["CatReducer: " <> show IncrementCatAge]
            pure $ state { catAge = catAge state + globalInt allState }
        SetAgeToGlobal  -> do
            tell ["CatReducer: " <> show SetAgeToGlobal]
            pure $ state { catAge = globalInt allState}
        _               -> pure state

dogLens :: Lens' AllState DogState
dogLens = lens dogState (\st d -> st { dogState = d } )

catLens :: Lens' AllState CatState
catLens = lens catState (\st c -> st { catState = c } )

doDogReducer :: Action -> AllState -> ReadWrite AllState
doDogReducer action state = dogReducer action (view dogLens state)
                        >>= (\d -> pure $ set dogLens d state )

doCatReducer :: Action -> AllState -> ReadWrite AllState
doCatReducer action state = catReducer action (view catLens state)
                        >>= (\s -> pure $ set catLens s state )

type Reducer = Action -> AllState -> ReadWrite AllState

reducers :: [Reducer]
reducers = [doDogReducer, doCatReducer]

reduce :: Action -> AllState -> ReadWrite AllState
reduce action allState = doCatReducer action allState
                     >>= doDogReducer action

-- run many actions
newerState :: ReadWrite AllState
newerState =
    pure initialState
    >>= reduce IncrementCatAge
    >>= reduce SetAgeToGlobal
    >>= reduce IncrementDogAge

newState :: (AllState, [String])
newState = runWriter $ runReaderT newerState initialState

