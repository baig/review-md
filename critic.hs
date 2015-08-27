#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}

import Parser
import Builder
import CommandParser
import Options.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Yaml.Pretty as Y
import qualified Data.Yaml.Builder as YB
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

type Filename = String

data Change = Accept -- accept ins/del/subs or keep highlight/comment
            | Reject -- reject ins/del/subs or delete highlight/comment
            | Leave  -- leave intact
            deriving (Eq, Show)
      
acceptChange :: Review -> Review
acceptChange (Addition _ _ str)       = Text str
acceptChange (Deletion _ _ _)         = Text ""
acceptChange (Substitution _ _ _ new) = Text new
acceptChange (Highlight _ _ str _ _)  = Text $ "{==" ++ str ++ "==}"
acceptChange (Comment _ _ author str) = Text $ "{>>" ++ (if (not . null $ author) then "@" ++ author ++ " " else "") ++ str ++ "<<}"
acceptChange x                        = x

rejectChange :: Review -> Review
rejectChange (Addition _ _ _)         = Text ""
rejectChange (Deletion _ _ str)       = Text str
rejectChange (Substitution _ _ old _) = Text old
rejectChange (Highlight _ _ str _ _)  = Text ""
rejectChange (Comment _ _ author str) = Text ""
rejectChange x                        = x

printPrompt :: Review -> IO String
printPrompt (Addition row col str) = do putStrLn $ "Insertion on Line " ++
                                                   show row ++
                                                   " Column " ++
                                                   show col
                                        putStrLn $ "  Before: " ++ "before"
                                        putStrLn $ "  After : " ++ "after"
                                        putStrLn $ "(a=accept, r=reject, u=unchanged): "
                                        getLine
printPrompt (Deletion row col str)              = do putStr $ "Deletion on Line " ++ show row ++ " Column " ++ show col ++ "\n" ++ "    Before: " ++ "before\n" ++ "    After : " ++ "after\n" ++ "(a=accept, r=reject, u=unchanged): "
                                                     getLine
printPrompt (Substitution row col old new)      = do putStr $ "Substitution on Line " ++ show row ++ " Column " ++ show col ++ "\n" ++ "    Before: " ++ "before\n" ++ "    After : " ++ "after\n" ++ "(a=accept, r=reject, u=unchanged): "
                                                     getLine
printPrompt (Highlight row1 col1 str row2 col2) = do putStr $ "Highlight ranging from Line " ++ show row1 ++ " Column " ++ show col1 ++ " to Line " ++ show row2 ++ " Column " ++ show col2 ++ "\n" ++ "    Before: " ++ "before\n" ++ "    After : " ++ "after\n" ++ "(k=keep, d=delete): "
                                                     getLine
printPrompt (Comment row col author str)        = do putStr $ "Comment on Line " ++ show row ++ " Column " ++ show col ++ "\n" ++ "    Before: " ++ "before\n" ++ "    After : " ++ "after\n" ++ "(k=keep, d=delete): "
                                                     getLine
printPrompt _                                   = return ""


getChange :: String -> Change
getChange s
    | s == "a"  = Accept
    | s == "r"  = Reject
    | s == "k"  = Accept
    | s == "d"  = Reject
    | otherwise = Leave
           
applyChange :: (Change,Review) -> Review
applyChange (Accept, r) = acceptChange r
applyChange (Reject, r) = rejectChange r
applyChange (_,      r) = r

run :: Critic -> IO ()
run (Critic _ True _ _ _ _ file) = do reviews <- getReviewsFromFile file
                                      x <- map acceptChange <$> pure reviews
                                      print $ mconcat $ map reviewToStr x
                                      return ()
run (Critic _ _ True _ _ _ file) = do reviews <- getReviewsFromFile file
                                      x <- map rejectChange <$> pure reviews
                                      print $ mconcat $ map reviewToStr x
                                      return ()
run (Critic _ _ _ True _ _ file) = do reviews <- getReviewsFromFile file
                                      x <-  map applyChange
                                        <$> (flip zip) reviews
                                        <$> map getChange
                                        <$> sequence (printPrompt <$> reviews)
                                      print $ mconcat $ map reviewToStr x
                                      return ()
run (Critic _ _ _ _ True _ file) = do reviews <- getReviewsFromFile file
                                      putStrLn $ BSL.unpack $ encodePretty $ toJSON reviews
                                      return ()
run (Critic _ _ _ _ _ True file) = do reviews <- getReviewsFromFile file
                                      BS.putStrLn $ Y.encodePretty Y.defConfig $ toJSON reviews
                                      return ()
run _ = return ()

main :: IO ()
main =  execParser opts >>= run
     where
        opts = info (helper <*> critic) desc
        
getReviewsFromFile :: Filename -> IO [Review]
getReviewsFromFile f = parseFromFile parseReviews f
                   >>= \case (Left  _ ) -> return []
                             (Right rs) -> return rs
