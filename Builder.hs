{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Builder (
      toJSON
    , Y.toYaml
) where

import Parser (Review (..))
import Data.Aeson.Types
import Data.Text (pack)
import Data.Vector hiding (map)
import qualified Data.Yaml.Builder as Y

instance ToJSON Review where
    toJSON (Addition row col str)         = object [("type"    , String "addition"         ),
                                                     "line"   .= row                        ,
                                                     "char"   .= col                        ,
                                                     "text"   .= str                        ]
    toJSON (Deletion row col str)         = object [("type"    , String "deletion"         ),
                                                     "line"   .= row                        ,
                                                     "char"   .= col                        ,
                                                     "text"   .= str                        ]
    toJSON (Substitution row col old new) = object [("type"    , String "substitution"     ),
                                                     "line"   .= row                        ,
                                                     "char"   .= col                        ,
                                                     "old"    .= old                        ,
                                                     "new"    .= new                        ]
    toJSON (Comment row col author str)   = object [("type"    , String "comment"          ),
                                                     "author" .= author                     ,
                                                     "line"   .= row                        ,
                                                     "char"   .= col                        ,
                                                     "text"   .= str                        ]
    toJSON (Highlight r1 c1 str r2 c2)    = object [("type"    , String "highlight"        ),
                                                     "range"  .= object ["start"   .=
                                                                   object [ "line" .= r1,  
                                                                            "char" .= c1]   , 
                                                                         "end"     .=      
                                                                   object [ "line" .= r2,  
                                                                            "char" .= c2]]  ,
                                                     "text"   .= str                        ]
    toJSON (Text _)                       = emptyObject

instance ToJSON [Review] where
    toJSON rs = Array $ fromList $ map toJSON rs

instance Y.ToYaml Review where
    toYaml (Addition row col str)         = Y.mapping [("type"      , Y.string "addition"          ),
                                                        "line"   Y..= row                           ,
                                                        "char"   Y..= col                           ,
                                                        "text"   Y..= (pack str)                    ]
    toYaml (Deletion row col str)         = Y.mapping [("type"      , Y.string "deletion"          ),
                                                        "line"   Y..= row                           ,
                                                        "char"   Y..= col                           ,
                                                        "text"   Y..= (pack str)                    ]
    toYaml (Substitution row col old new) = Y.mapping [("type"      , Y.string "substitution"      ),
                                                        "line"   Y..= row                           ,
                                                        "char"   Y..= col                           ,
                                                        "old"    Y..= (pack old)                    ,
                                                        "new"    Y..= (pack new)                    ]
    toYaml (Comment row col author str)   = Y.mapping [("type"      , Y.string "comment"           ),
                                                        "author" Y..= (pack author)                 ,
                                                        "line"   Y..= row                           ,
                                                        "char"   Y..= col                           ,
                                                        "text"   Y..= (pack str)                    ]
    toYaml (Highlight r1 c1 str r2 c2)    = Y.mapping [("type"      , Y.string "highlight"         ),
                                                        "range"  Y..= Y.mapping ["start"  Y..=      
                                                                       Y.mapping [ "line" Y..= r1,  
                                                                                   "char" Y..= c1]  ,
                                                                                 "end"    Y..=       
                                                                       Y.mapping [ "line" Y..= r2,  
                                                                                   "char" Y..= c2]] ,
                                                        "text"   Y..= (pack str)                    ]
    toYaml (Text _)                       = Y.mapping [("text"      , Y.null)                       ]
