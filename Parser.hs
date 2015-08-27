{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
      Review (..)
    , parseReviews
    , parseFromFile
    , reviewToStr
) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

type Old    = String
type File   = String
type Row    = Int
type Col    = Int
type New    = String
type Author = String

data Review = Addition     Row Col String
            | Deletion     Row Col String
            | Substitution Row Col Old    New
            | Comment      Row Col Author String
            | Highlight    Row Col String Row    Col
            | Text         String
            deriving (Eq, Show)
            
data Thread = Empty
            | Commment String Thread
            deriving (Eq, Show)

delimiters open end = between (string open) (string end)

isDelimiter open = lookAhead (string open)

till end = manyTill anyChar (try (lookAhead $ string end))

addition :: Parsec String () Review
addition =  try 
         $  Addition
        <$> (sourceLine   <$> getPosition)
        <*> (sourceColumn <$> getPosition)
        <*> (isDelimiter "{++" *> (delimiters "{++" "++}" $ till "++}"))

deletion :: Parsec String () Review
deletion =  try
         $  Deletion
        <$> (sourceLine   <$> getPosition)
        <*> (sourceColumn <$> getPosition)
        <*> (isDelimiter "{--" *> (delimiters "{--" "--}" $ till "--}"))

highlight :: Parsec String () Review
highlight =  try
          $  Highlight
         <$> (sourceLine   <$> getPosition)
         <*> (sourceColumn <$> getPosition)
         <*> (isDelimiter "{==" *> (delimiters "{==" "==}" $ till "==}"))
         <*> (sourceLine   <$> getPosition)
         <*> (sourceColumn <$> getPosition)

comment :: Parsec String () Review
comment =  try
        $  Comment
       <$> (sourceLine   <$> getPosition)
       <*> (sourceColumn <$> getPosition)
       <*> (isDelimiter "{>>" *> string "{>>" *> option "" author)
       <*> till "<<}"
       <*  string "<<}"

regular :: Parsec String () Review
regular =  try
        $  Text
       <$> many (noneOf "{+-=~><}")

author :: Parsec String () String
author =  string "@"
       *> many (noneOf " ")
      <*  char ' '

target :: Parsec String () String
target =  try
       $  isDelimiter "{~~"
       *> string "{~~"
       *> many (noneOf "~")

replacement :: Parsec String () String
replacement =  try
            $  isDelimiter "~>"
            *> delimiters "~>" "~~}" ( till "~~}" )

substitution :: Parsec String () Review
substitution =  try
             $  Substitution
            <$> (sourceLine   <$> getPosition)
            <*> (sourceColumn <$> getPosition)
            <*> target
            <*> replacement

review :: Parser Review
review =  addition
      <|> deletion
      <|> substitution
      <|> highlight
      <|> comment
      <|> regular
      
parseReviews :: Parsec String () [Review]
parseReviews =  manyTill review eof

reviewToStr :: Review -> String
reviewToStr (Addition _ _ str)         = "{++" ++ str ++ "++}"
reviewToStr (Deletion _ _ str)         = "{--" ++ str ++ "--}"
reviewToStr (Substitution _ _ old new) = "{~~" ++ old ++ "~>" ++ new ++ "~~}"
reviewToStr (Highlight _ _ str _ _)    = "{==" ++ str ++ "==}"
reviewToStr (Comment _ _ author str)   = "{>>" ++ ifAuthor ++ str ++ "<<}"
                                       where ifAuthor
                                                 | null author = "" 
                                                 | otherwise   = "@" ++ author 
reviewToStr (Text str)                 = str

