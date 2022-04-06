{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Text as T

import DataTypes ( Colors(..), FetchField(..), FieldAlign (..) )
import Utils     ( add )

import Info.Os           ( fetchOs )
import Info.Shell        ( fetchShell )
import Info.Uptime       ( fetchUptime )
import Info.Kernel       ( fetchKernel )
import Info.Terminal     ( fetchTerminal )
import Info.Wm           ( fetchWm )
import Info.Editor       ( fetchEditor )
import Info.Cmd          ( fetchCmd )
import Info.Hostname     ( fetchHostname)
import Info.Cpu          ( fetchCpu )
import Info.User         ( fetchUser )
import Info.DiskUsage    ( fetchDiskUsage, fetchAvailSpace )

-- | This field defines all data which will be shown
-- | "line" word adds a line
-- | "emptyLine" word adds an empty line
-- | Other word and their functions must be defined in the fetchFields function
config :: T.Text
config = "host os kernel wm line term shell uptime line root home"

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
colors = Colors { borderColor    = "black"
                , titleColor     = "red"
                , separatorColor = "black"
                , infoColor      = ""}
            
-- | This function is example of creating the fetch command which just takes output from shell command
-- | fetchCmd function is used to safely get output from the shell command
-- | First argument of fetchCmd command is command, which must be the full path to executable script
-- | or the name of the script if its directory in $PATH
-- | Second arguments is options
-- | For more complex commands like "$ free -h | awk '/^Mem:/ {print $3} "/" $2'"
-- | it is possible to create a script, and here only run that script.
-- fetchVolume :: IO T.Text
-- fetchVolume = fetchCmd "pamixer" ["--get-volume-h"]

-- | This is the list of FetchField type
-- | The FetchField has type (Text, IO Text), 
-- | where the first value is the title (which is used to identify the word from config)
-- | and the second value is the fetch command, all of them must have type IO Text
fetchFields :: [FetchField]
fetchFields = [ add "kernel"    fetchKernel
              , add "host"      fetchHostname
              , add "os"        fetchOs
              , add "wm"        fetchWm
              , add "term"      fetchTerminal
              , add "uptime"    fetchUptime
              , add "shell"     fetchShell
              , add "editor"    fetchEditor
              , add "cpu"       fetchCpu
              , add "user"      fetchUser
              , add "root"      (fetchDiskUsage "/") -- This can be used to get free space in format "used/total"
              , add "home"      (fetchAvailSpace "/home") -- Free space in format "available"
              , add "volume"    (fetchCmd "pamixer" ["--get-volume-h"])
              ]

-- | This variable defines how the final text will be aligned
-- | NoAlign value means that no spaces will be added
-- | Separator value means that spaces will be added after separator
-- | Title means that spaces will be added between title and separator
fieldAlign :: FieldAlign
fieldAlign = Title
