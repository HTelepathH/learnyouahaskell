double :: Type
double = FloatingPointType 64 IEEE

data CodeGenState = CodeGenState {
      currentBlock :: Name
    , blocks       :: Map.Map Name BlockState
    , symtab       :: SymbleTable
    , blockCount   :: Int
    , count        :: Word
    , names        :: Names
    } deriving Show

data BlockState = BlockState {
    idx   :: Int
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator) 
} deriving Show
