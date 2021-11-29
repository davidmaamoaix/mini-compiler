module TestData where

import Parser

astBasics =        NProg [ NFDef NIntType "add" 
                                 [NParam NIntType "a", NParam NIntType "b"]
                                 (NBlock [NRet $ NBinExp (NIdExp "a") "+" (NIdExp "b")])
                         , NFDef NBoolType "and_gate"
                                 [NParam NBoolType "a", NParam NBoolType "b"]
                                 (NBlock [NRet $ NBinExp (NIdExp "a") "&&" (NIdExp "b")])
                         ]

astStructs =       NProg [ NSDef "Foo"
                                 [NField (NPtrType NIntType) "ptr", NField NBoolType "name"]
                         , NFDef NIntType "main"
                                 [ NSimpStmt $ NDecl (NSIdType "Foo") "a"
                                 , NSimpStmt $ NAsnSimp () "=" ()
                                 ]
                         ]
