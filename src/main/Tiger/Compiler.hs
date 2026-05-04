module Tiger.Compiler(compile) where


data CompilationError
  = SomeError
  deriving Show

data Program
  = Program
  deriving Show

compile :: Text -> Validation (NonEmpty CompilationError) Program
compile _ = Success Program
