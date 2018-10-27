module Pipeline where

import Extras
import Expense
import Commands
import Errors

-- filtered -> message/result
type ACmdFunc = [Expense] -> Either String [Expense]

createPL :: [String] -> [Expense] -> [Either ACmdFunc ErrString]
createPL []   _ = [Right NoArgsGiven]
createPL args l = foldl genFunction [] $ splitWith "--" args
  where genFunction acc []    = acc
        genFunction acc (c:a) = acc ++ [
          case getCommandFunction c of
            Nothing -> Right $ UnknownCmd c
            Just x  -> Left $ x a l]

execPL :: [Expense] -> [ACmdFunc] -> Either String [Expense]
execPL _    []   = Left $ show PipelineEmpty
execPL expL cmds = foldl __execPL (Right expL) cmds

__execPL :: Either String [Expense] -> ACmdFunc -> Either String [Expense]
__execPL (Left s)  _ = Left s
__execPL (Right e) f = f e
