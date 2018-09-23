module StateWriter
    ( stateReducer
    ) where

import           AppData
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bifunctor

stateReducer :: IO ()
stateReducer = do
    print initialState
    print newState

data Action = IncrementDogAge
            | IncrementCatAge
            | SetAgeToGlobal
            | SetDogName String
            | SetCatName String
            deriving (Show)

type StateWrite a = StateT AllState (Writer [String]) a

type Reducer a = Action -> StateWrite a

type DoReducer = Action -> AllState -> StateWrite AllState

type StateWithLog = (AllState, [String])

dogReducer :: Reducer DogState
dogReducer action = do
    allState <- get
    let localState = view dogLens allState
    case action of
        IncrementDogAge -> do
            tell ["DogReducer: " <> show IncrementDogAge]
            pure $ localState { dogAge = dogAge localState + 1 }
        (SetDogName name) -> do
            tell ["DogReducer: " <> show (SetDogName name)]
            pure $ localState { dogName = name }
        _               -> pure localState

catReducer :: Reducer CatState
catReducer action = do
    allState <- get
    let localState = view catLens allState
    case action of
        IncrementCatAge -> do
            tell ["CatReducer: " <> show IncrementCatAge]
            pure $ localState { catAge = catAge localState + globalInt allState }
        SetAgeToGlobal  -> do
            tell ["CatReducer: " <> show SetAgeToGlobal]
            pure $ localState { catAge = globalInt allState}
        (SetCatName name) -> do
                tell ["CatReducer: " <> show (SetCatName name)]
                pure $ localState { catName = name }
        _               -> pure localState

dogLens :: Lens' AllState DogState
dogLens = lens dogState (\st d -> st { dogState = d } )

catLens :: Lens' AllState CatState
catLens = lens catState (\st c -> st { catState = c } )

doDogReducer :: DoReducer
doDogReducer action st = dogReducer action
                       >>= (\d -> pure $ set dogLens d st)

doCatReducer :: DoReducer
doCatReducer action st = catReducer action
                       >>= (\c -> pure $ set catLens c st)

-- run reducer action and put new result into state
reduceAndSave :: Action -> DoReducer -> AllState -> StateWrite AllState
reduceAndSave action reducer st = reducer action st
                                 >>= (\newSt -> do
                                        put newSt
                                        pure newSt)

reduce :: [DoReducer] -> Action -> AllState -> StateWrite AllState
reduce reducers action firstState = foldr (\red s -> s >>= reduceAction red) (pure firstState) reducers
    where reduceAction = reduceAndSave action

testReduce :: DoReducer
testReduce = reduce [doCatReducer, doDogReducer]

testActions :: [Action]
testActions = [IncrementCatAge, SetAgeToGlobal, IncrementDogAge, SetDogName "John"]

runActions :: DoReducer -> [Action] -> AllState -> StateWithLog
runActions reducerFunc actions allState = first fst $ runWriter $ runStateT newerState allState
    where newerState = foldr (\a s -> s >>= reducerFunc a) (pure allState) revActions
          revActions = reverse actions

newState :: StateWithLog
newState = runActions testReduce testActions initialState
