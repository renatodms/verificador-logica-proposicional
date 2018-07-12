module Parser (compareProp) where

import Verify

-- Constantes, operadores e variáveis pré definidas
opList = ['&','|','>']
constList = ['T','F']
varList = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']

-- Converte uma string para o tipo Prop (do módulo Verify), verificando sua sintaxe.
parsing :: String -> Prop
parsing "" = Error "Expressão inválida."
parsing (x:"")
 | x == 'T' = Const True
 | x == 'F' = Const False
 | elem x varList = Var x
 | otherwise = Error "Expressão inválida."
parsing (x:(y:""))
 | x == '!' && y == 'T' = Neg (Const True)
 | x == '!' && y == 'F' = Neg (Const False)
 | x == '!' && elem y varList = Neg (Var y)
 | otherwise = Error "Expressão inválida."
parsing (x:(y:(z:xs)))
 | x == 'T' && y == '&' = Conj (Const True) (parsing (z:xs))
 | x == 'F' && y == '&' = Conj (Const False) (parsing (z:xs))
 | x == 'T' && y == '|' = Disj (Const True) (parsing (z:xs))
 | x == 'F' && y == '|' = Disj (Const False) (parsing (z:xs))
 | x == 'T' && y == '>' = Imp (Const True) (parsing (z:xs))
 | x == 'F' && y == '>' = Imp (Const False) (parsing (z:xs))
 | elem x varList && y == '&' = Conj (Var x) (parsing (z:xs))
 | elem x varList && y == '|' = Disj (Var x) (parsing (z:xs))
 | elem x varList && y == '>' = Imp (Var x) (parsing (z:xs))
 | x == '(' && (isJustObject (y:(z:xs)) 1) = parsing (removeLastChar (y:(z:xs)))
 | x == '(' && (isOpAfterObject (y:(z:xs)) '&' 1) = Conj (parsing (removeLastChar (getLeftExp (y:(z:xs)) '&' 1 ""))) (parsing (getRightExp (y:(z:xs)) '&' 1))
 | x == '(' && (isOpAfterObject (y:(z:xs)) '|' 1) = Disj (parsing (removeLastChar (getLeftExp (y:(z:xs)) '|' 1 ""))) (parsing (getRightExp (y:(z:xs)) '|' 1))
 | x == '(' && (isOpAfterObject (y:(z:xs)) '>' 1) = Imp (parsing (removeLastChar (getLeftExp (y:(z:xs)) '>' 1 ""))) (parsing (getRightExp (y:(z:xs)) '>' 1))
 | x == '!' && y == '(' && (isJustObject xs 1) = Neg (parsing (removeLastChar (z:xs)))
 | x == '!' && y == '(' && (isOpAfterObject (z:xs) '&' 1) = Conj (Neg (parsing (removeLastChar (getLeftExp (z:xs) '&' 1 "")))) (parsing (getRightExp (z:xs) '&' 1))
 | x == '!' && y == '(' && (isOpAfterObject (z:xs) '|' 1) = Disj (Neg (parsing (removeLastChar (getLeftExp (z:xs) '|' 1 "")))) (parsing (getRightExp (z:xs) '|' 1))
 | x == '!' && y == '(' && (isOpAfterObject (z:xs) '>' 1) = Imp (Neg (parsing (removeLastChar (getLeftExp (z:xs) '>' 1 "")))) (parsing (getRightExp (z:xs) '>' 1))
 | x == '!' && y == 'T' && z == '&' = Conj (Neg (Const True)) (parsing xs)
 | x == '!' && y == 'F' && z == '&' = Conj (Neg (Const False)) (parsing xs)
 | x == '!' && y == 'T' && z == '|' = Disj (Neg (Const True)) (parsing xs)
 | x == '!' && y == 'F' && z == '|' = Disj (Neg (Const False)) (parsing xs)
 | x == '!' && y == 'T' && z == '>' = Imp (Neg (Const True)) (parsing xs)
 | x == '!' && y == 'F' && z == '>' = Imp (Neg (Const False)) (parsing xs)
 | x == '!' && elem y varList && z == '&' = Conj (Neg (Var y)) (parsing xs)
 | x == '!' && elem y varList && z == '|' = Disj (Neg (Var y)) (parsing xs)
 | x == '!' && elem y varList && z == '>' = Imp (Neg (Var y)) (parsing xs)
 | otherwise = Error "Expressão inválida."

-- Verifica duas expressões (strings) e retorna o status 
compareProp :: String -> String -> String
compareProp expA expB
 | (hasErrorProp (parsing expA)) /= "" = "Expressão da esquerda inválida."
 | (hasErrorProp (parsing expB)) /= "" = "Expressão da direita inválida."
 | varsVerify (parsing expA) (parsing expB) == False = "Quantidade de variáveis diferente nas expressões."
 | otherwise = show (equiv (parsing expA) (parsing expB))

-- Verifica se há erro no parsing
hasErrorProp :: Prop -> String
hasErrorProp (Error x) = x
hasErrorProp (Const _) = ""
hasErrorProp (Var _) = ""
hasErrorProp (Neg exp) = hasErrorProp exp
hasErrorProp (Conj expA expB) = (hasErrorProp expA) ++ (hasErrorProp expB)
hasErrorProp (Disj expA expB) = (hasErrorProp expA) ++ (hasErrorProp expB)
hasErrorProp (Imp expA expB) = (hasErrorProp expA) ++ (hasErrorProp expB)
hasErrorProp (Xor expA expB) = (hasErrorProp expA) ++ (hasErrorProp expB)
hasErrorProp (Sse expA expB) = (hasErrorProp expA) ++ (hasErrorProp expB)

-- Verifica se a quantidade de variáveis é a mesma nas duas expressões
varsVerify :: Prop -> Prop -> Bool
varsVerify expA expB = (length leftExpression) == (length rightExpression)
  where
    leftExpression = remDupl (vars expA)
    rightExpression = remDupl (vars expB)

-- Verifica se a string passada é um envolto de parênteses estruturado, sem mais caracteres em seguida
isJustObject :: String -> Int -> Bool
isJustObject "" 0 = True
isJustObject _ 0 = False
isJustObject "" _ = False
isJustObject (x:xs) counter
 | x == '(' = isJustObject xs (counter+1)
 | x == ')' = isJustObject xs (counter-1)
 | x == '!' = isJustObject xs counter
 | elem x opList = isJustObject xs counter
 | elem x constList = isJustObject xs counter
 | elem x varList = isJustObject xs counter
 | otherwise = False

-- Verifica se após o primeiro objeco estruturado da string há um operador
isOpAfterObject :: String -> Char -> Int -> Bool
isOpAfterObject "" _ 0 = False
isOpAfterObject (x:_) op 0
 | x == op = True
 | otherwise = False
isOpAfterObject "" _ _ = False
isOpAfterObject (x:xs) op counter
 | x == '(' = isOpAfterObject xs op (counter+1)
 | x == ')' = isOpAfterObject xs op (counter-1)
 | x == '!' = isOpAfterObject xs op counter
 | elem x opList = isOpAfterObject xs op counter
 | elem x constList = isOpAfterObject xs op counter
 | elem x varList = isOpAfterObject xs op counter
 | otherwise = False

-- Recupera parte da esquerda da expressão
getLeftExp :: String -> Char -> Int -> String -> String
getLeftExp (x:_) op 0 backlog
 | x == op = reverse backlog
getLeftExp (x:xs) op counter backlog
 | x == '(' = getLeftExp xs op (counter+1) (x:backlog)
 | x == ')' = getLeftExp xs op (counter-1) (x:backlog)
 | x == '!' = getLeftExp xs op counter (x:backlog)
 | elem x opList = getLeftExp xs op counter (x:backlog)
 | elem x constList = getLeftExp xs op counter (x:backlog)
 | elem x varList = getLeftExp xs op counter (x:backlog)

-- Recupera parte da direita da expressão
getRightExp :: String -> Char -> Int -> String
getRightExp (x:xs) op 0
 | x == op = xs
getRightExp (x:xs) op counter
 | x == '(' = getRightExp xs op (counter+1)
 | x == ')' = getRightExp xs op (counter-1)
 | x == '!' = getRightExp xs op counter
 | elem x opList = getRightExp xs op counter
 | elem x constList = getRightExp xs op counter
 | elem x varList = getRightExp xs op counter

-- Remove o último caractere da string
removeLastChar :: String -> String
removeLastChar "" = ""
removeLastChar (x:"") = ""
removeLastChar (x:(y:xs))
 | xs == "" = [x]
 | otherwise = (x:(removeLastChar (y:xs)))

-- Testes
test = do
 print (parsing "a")
 print (parsing "T")
 print (parsing "(F)")
 print (parsing "!b")
 print (parsing "!T")
 print (parsing "!(c)")
 print (parsing "!d&e")
 print (parsing "!(f)&g")
 print (parsing "!h&(F)")
 print (parsing "!(i)|j")
 print (parsing "(!(!(i)|j)>!(F))")
 print (parsing "(T)&(k)")
 print (parsing "T&l")
 print (parsing "m>(!(!(n)|o)|!(F))")
 print "ERROS:"
 print (parsing "T4")
 print (parsing "!(f)&g)")