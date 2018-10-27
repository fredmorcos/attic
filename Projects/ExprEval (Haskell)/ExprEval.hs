-- Nada Victor, Fatma Ziwar, Fred Morcos

data LogicExpr = Prop Char | Neg LogicExpr | And LogicExpr LogicExpr | Or LogicExpr LogicExpr | Impl LogicExpr LogicExpr deriving Show

evaluate :: LogicExpr->[(Char,Bool)]->Bool

evaluate (Prop p) []=error "Element Not in List"
evaluate (Prop p) ((x,y):xs)	| p==x =y
			    	| otherwise =evaluate (Prop p) xs

evaluate (Neg l) []=error "Empty List"
evaluate (Neg l) ((x,y):xs)	| evaluate (l) ((x,y):xs)==True =False
				| otherwise =True

evaluate (And l n) []=error "Empty List"
evaluate (And l n) ((x,y):xs)	| evaluate (l) ((x,y):xs)==True && evaluate (n) ((x,y):xs)==True  =True
				| otherwise =False

evaluate (Or l n) []=error "Empty List"
evaluate (Or l n) ((x,y):xs)	| evaluate (l) ((x,y):xs)==False && evaluate (n) ((x,y):xs)==False =False
				| otherwise =True

evaluate (Impl l n) []=error "Empty List"
evaluate (Impl l n) ((x,y):xs)	| evaluate (l) ((x,y):xs)==False && evaluate (n) ((x,y):xs)==False =True
				| evaluate (l) ((x,y):xs)==True && evaluate (n) ((x,y):xs)==True =True
				| evaluate (l) ((x,y):xs)==False && evaluate (n) ((x,y):xs)==True =True
				| otherwise =False

clausalNF :: LogicExpr -> LogicExpr

causalNF e = simplify e

--clausalNF e = (simplifyDist (simplifyDeMorg (simplifyDeMor (simplifyNeg (simplifyImp e)))))
--clausalNF e	| (checkSimplify e)==True = clausalNF (simplify e)
--		| otherwise =e

simplifyImp :: LogicExpr -> LogicExpr

simplifyImp (Impl l n)	=(Or (Neg (simplifyImp l)) (simplifyImp n))
simplifyImp (Prop p)	=(Prop p)
simplifyImp (Neg l)		=(Neg (simplifyImp l))
simplifyImp (And l n)	=(And (simplifyImp l) (simplifyImp n))
simplifyImp (Or l n)	=(Or (simplifyImp l) (simplifyImp n))

simplifyNeg :: LogicExpr -> LogicExpr

simplifyNeg (Neg (Neg l))	=(simplifyNeg l)
simplifyNeg (Prop p)		=(Prop p)
simplifyNeg (Neg l)			=(Neg (simplifyNeg l))
simplifyNeg (And l n)		=(And (simplifyNeg l) (simplifyNeg n))
simplifyNeg (Or l n)		=(Or (simplifyNeg l) (simplifyNeg n))
simplifyNeg (Impl l n)		=(Impl (simplifyNeg l) (simplifyNeg n))

simplifyDeMor :: LogicExpr -> LogicExpr

simplifyDeMor (Neg (Or l n))	=(And (Neg (simplifyDeMor l)) (Neg (simplifyDeMor n)))
simplifyDeMor (Prop p)		=(Prop p)
simplifyDeMor (Neg l)		=(Neg (simplifyDeMor l))
simplifyDeMor (And l n)		=(And (simplifyDeMor l) (simplifyDeMor n))
simplifyDeMor (Or l n)		=(Or (simplifyDeMor l) (simplifyDeMor n))
simplifyDeMor (Impl l n)	=(Impl (simplifyDeMor l) (simplifyDeMor n))

simplifyDeMorg :: LogicExpr -> LogicExpr

simplifyDeMorg (Neg (And l n))	=(Or (Neg (simplifyDeMorg l)) (Neg (simplifyDeMorg n)))
simplifyDeMorg (Prop p)		=(Prop p)
simplifyDeMorg (Neg l)		=(Neg (simplifyDeMorg l))
simplifyDeMorg (And l n)	=(And (simplifyDeMorg l) (simplifyDeMorg n))
simplifyDeMorg (Or l n)		=(Or (simplifyDeMorg l) (simplifyDeMorg n))
simplifyDeMorg (Impl l n)	=(Impl (simplifyDeMorg l) (simplifyDeMorg n))

simplifyDist :: LogicExpr -> LogicExpr

simplifyDist (Or (And p c) d)	=(And (Or (simplifyDist p) (simplifyDist d)) (Or (simplifyDist c) (simplifyDist d)))
simplifyDist (Prop p)		=(Prop p)
simplifyDist (Neg l)		=(Neg (simplifyDist l))
simplifyDist (And l n)		=(And (simplifyDist l) (simplifyDist n))
simplifyDist (Or l n)		=(Or (simplifyDist l) (simplifyDist n))
simplifyDist (Impl l n)		=(Impl (simplifyDist l) (simplifyDist n))

simplify :: LogicExpr-> LogicExpr

simplify x= (simplifyDist (simplifyDeMorg (simplifyDeMorg (simplifyNeg (simplifyImp x)))))
--simplify (Prop p)		=(Prop p)
--simplify (Impl l n)		=(Or (Neg (simplify l)) (simplify n))
--simplify (Neg (Neg l))		= simplify l
--simplify (Neg (Or l n))		=(And (Neg (simplify l)) (Neg (simplify n)))
--simplify (Neg (And l n))	=(Or (Neg (simplify l)) (Neg (simplify n)))
--simplify (Or (And p c) d)	=(And (Or (simplify p) (simplify d)) (Or (simplify c) (simplify d)))


--checkSimplify :: LogicExpr -> Bool

--checkSimplify (Impl l n)=True
--checkSimplify (Neg (Neg l))=True
--checkSimplify (Neg (Or l n))=True
--checkSimplify (Neg (And l n))=True
--checkSimplify (Or (And p c) d)=True
--checkSimplify (Prop p)= False

countVar :: LogicExpr-> [Char]

countVar (Prop p)=[p]
countVar (Neg l)= countVar l
countVar (And l n)= countVar l ++ countVar n
countVar (Or l n)= countVar l ++ countVar n
countVar (Impl l n)= countVar l ++ countVar n

--getBinDig:: Int->Int

--getBinDig n=length(convertToBin n)

fillSpace :: Int->[Int]->[Int]

fillSpace n l	| (length l)==n =l
		| (length l)<n =fillSpace n (0:l)

convertToBin :: Int ->[Int]

convertToBin 0=[0]
convertToBin 1 =[1]
convertToBin n = convertToBin (div n 2)++[mod n 2]

convertToBool :: [Int]->[Bool]

convertToBool [0]=[False]
convertToBool [1]=[True]
convertToBool (x:xs)= convertToBool [x] ++ convertToBool xs

numOfVar :: Int-> Int

numOfVar 0=0
numOfVar 1=1
numOfVar n= (2^n)-1

count ::Int -> [Int]

count 0=[0]
count n= (count (n-1))++[n]

--generate :: [Int]->[[Bool]]
generateTTbool :: Int->[[Bool]]
--generateTTbool x= map (convertToBool) (map (convertToBin) (count (numOfVar x)))

generateTTbool x= map (convertToBool) (map (fillSpace x) (map (convertToBin) (count (numOfVar x))))
--generateTTbool x= map (convertToBool) map (fillSpace (map (convertToBin) (fillSpace (count (numOfVar x)) (numOfVar x)))

generateTT :: LogicExpr->[[(Char,Bool)]]
generateTT x=(map (zip (countVar x)) ((generateTTbool (length (countVar x)))))

generate :: LogicExpr->[Bool]
generate x=(map (evaluate x) (generateTT x))

getR :: Int-> [Char]
getR 0=[]
getR 1=['r']
getR n=['r']++(getR (n-1))

generating :: LogicExpr->[(Char,Bool)]
generating x= (zip (getR (length (generate x)))) (generate x)

--getHead :: [(Char,Bool)]->(Char,Bool)

--goThrough [x]=x
--sget

generateTruthTable :: LogicExpr -> [[(Char,Bool)]]
generateTruthTable x= makeresult (generating x) (generateTT x)
--generateTruthTable x=map (++ generating x) (generateTT x)


makeresult :: [(Char,Bool)] -> [[(Char,Bool)]] -> [[(Char,Bool)]]
makeresult [] []=[]
makeresult (x:xs) ((y:ys):l) = ((y:ys)++[x]) : makeresult xs l

--generate x=map (zip ['r']) (map (evaluate x) (generateTT x))

--generateTT x= (map (: ('r',evaluate x (l))) (l:ls)) where (l:ls)=(map (zip (countVar x)) ((generateTTbool (length (countVar x)))))
--generateTT (Prop x)= (generateTTbool 1)++(evaluate )
--generateTT x= map (++(evaluate x (head (generateTTbool (length (countVar x)))))) generateTTbool (length (countVar x))

--clausalNF(And(Or(Prop 'p')(Or(Prop 'q')(Neg(Prop 's'))))(And (Impl (Prop 'q') (Prop 'r')) (Neg(Impl(Neg(Prop 'q')) (Prop 's')))))
