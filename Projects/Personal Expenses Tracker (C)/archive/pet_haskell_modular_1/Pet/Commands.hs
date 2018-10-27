module Pet.Commands where

import Pet.Arguments
import Pet.Errors
import Pet.Expense

type CommandResult   = Either [Expense] String
type CommandFunction = Options   ->
                       [Expense] ->
                       [Expense] ->
                       Either CommandResult CommandError
