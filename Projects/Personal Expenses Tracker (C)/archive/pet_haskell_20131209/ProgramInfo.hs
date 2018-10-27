module ProgramInfo where

programName :: String
programName = "pet"

versionInfo :: String
versionInfo = unlines [ programName ++ " - Personal Expense Tracker - version 0.2"
                      , "Copyright (c) 2012-2013 - Fred Morcos <fred.morcos@gmail.com>"
                      , "https://github.com/fredmorcos/pet.git"
                      ]

usageHeader :: String
usageHeader = "Usage: " ++ programName ++ " [ARGUMENTS]"
