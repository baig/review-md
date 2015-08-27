module CommandParser(
      Critic (..)
    , desc
    , critic
) where

import Options.Applicative
import Options.Applicative.Help.Chunk

release   = "critic 0.0.1"
copyright = "Copyright (C) 2015 Wasif Hasan Baig"
web       = "https://github.com/baig/critic"
note      = "This is free software. See the LICENSE file." ++
            "There is no warranty, not even for merchantability or fitness" ++
            "for a particular purpose."

data Critic = Critic
            { showw       :: Bool
            , accept      :: Bool
            , reject      :: Bool
            , interactive :: Bool
            , json        :: Bool
            , yaml        :: Bool
            , documents   :: String
            }

desc =  fullDesc
     <> header   "Critic is a CriticMarkup processing tool."
     <> footerDoc (unChunk . vcatChunks $ stringChunk <$> [copyright, web, note])

acceptSwitch =  switch
             $  long  "accept-all"
             <> short 'a'
             <> help  "Accept all changes"

rejectSwitch =  switch
             $  long  "reject-all"
             <> short 'r'
             <> help  "Reject all changes"

interactiveSwitch =  switch
                  $  long  "interactive"
                  <> short 'i'
                  <> help  "Accept or reject changes in interactive mode"

jsonSwitch =  switch
           $  long  "json"
           <> short 'j'
           <> help  "Output as JSON"
           
yamlSwitch =  switch
           $  long  "yaml"
           <> short 'y'
           <> help  "Output as YAML"

showSwitch =  switch
           $  long  "show"
           <> short 's'
           <> help  "Show all changes"
           
filename = strArgument (metavar "MARKDOWN_FILE")
           
critic :: Parser Critic
critic =  Critic
      <$> showSwitch
      <*> acceptSwitch
      <*> rejectSwitch
      <*> interactiveSwitch
      <*> jsonSwitch
      <*> yamlSwitch
      <*> filename