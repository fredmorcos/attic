module Errors (badElemErrorString, badDateErrorString) where

-- Builds the structure 'template' for an error message. It is
-- composed of a header, an empty line and a body.
buildErrorString :: String -> [String] -> String
buildErrorString header body = unlines $ (header : "" : []) ++ body

-- Error message when an expense element cannot be parsed.
badElemErrorString :: String
badElemErrorString = buildErrorString "Bad element."
  ["Make sure that:",
   "  1. All elements have 3 columns separated by spaces.",
   "  2. No date, amount or category contains any spaces."
  ]

-- Error message when a date cannot be parsed.
badDateErrorString :: String
badDateErrorString = buildErrorString "Bad date format."
  ["Make sure all dates follow the YYYY-MM-DD format where",
   "YYYY refers to the 4-digit year number, MM refers to",
   "the 2-digit month number and DD refers to the 2-digit",
   "day number."
  ]
