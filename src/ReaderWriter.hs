module ReaderWriter
    ( testReducer
    ) where

import           AppData
import           Control.Lens
import           Control.Monad.Reader as R
import           Control.Monad.Writer

testReducer :: IO ()
testReducer = do
    print initialState
    print newState

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

