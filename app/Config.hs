{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Text as T

import DataTypes ( Colors(..), FetchField(..) )
import Line (line)

import Info.Os           ( fetchOs )
import Info.Shell        ( fetchShell )
import Info.Uptime       ( fetchUptime )
import Info.Kernel       ( fetchKernel )
import Info.Terminal     ( fetchTerminal )
import Info.Wm           ( fetchWm )
import Info.Editor       ( fetchEditor )
import Info.ShellCommand ( fetchShellCommand )
import Info.Hostname     ( fetchHostname)

-- | This field defines all data which will be shown
-- | Line word adds a line
-- | Other word and their functions must be defined in the fetchFields function
config :: T.Text
config = "host os kernel wm line terminal shell editor line uptime"

-- | Separator used between the title and the info
-- | The spaces will be added after separator to make all lines have the same size
separator :: T.Text
separator = " ~ "

-- | Colors of every elements
-- | Commonly the color should be specified like "blue", "yellow", "brightRed", etc.
-- | But the colors are used in "\ESC[1;" ++ color ++ "m" expression,
-- | so they can be like "31", "35", etc.
-- | Also more complex like "31m\ESC[1;41" can be used to make multiple formats.
colors :: Colors
colors = Colors { borderColor    = "blue"
                , titleColor     = "yellow"
                , separatorColor = "yellow"
                , infoColor      = "brightWhite"}
            
-- | This function is example of creating the fetch command which just takes output from shell command
-- | fetchShellCommand function is used to safely get output from the shell command
-- | First argument of fetchShell command is command, which must be the full path to executable script
-- | or the name of the script if its directory in $PATH
-- | Second arguments is options
-- | For more complex commands like "$ free -h | awk '/^Mem:/ {print $3} "/" $2'"
-- | it is possible to create a script, and here only run that script.
fetchVolume :: IO T.Text
fetchVolume = fetchShellCommand "pamixer" ["--get-volume-h"]

-- | This is the list of FetchField types
-- | The FetchField has type (Text, IO Text), 
-- | where the first value is the title (which is used to identify the word from config)
-- | and the second value is the fetch command, all of them must have type IO Text
fetchFields :: [FetchField]
fetchFields = [ FetchField ("line",     line) -- This can be used as separator between sections
              , FetchField ("kernel",   fetchKernel)
              , FetchField ("host",     fetchHostname)
              , FetchField ("os",       fetchOs ) 
              , FetchField ("wm",       fetchWm)
              , FetchField ("terminal", fetchTerminal) 
              , FetchField ("uptime",   fetchUptime)
              , FetchField ("shell",    fetchShell)
              , FetchField ("editor",   fetchEditor)  
              , FetchField ("volume",   fetchVolume)
              ]
