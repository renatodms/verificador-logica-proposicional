module Verify
  (Prop (..),
   Subst,
   eval,
   vars,
   bools,
   substs,
   remDupl,
   equiv
  ) where

-- Tipo algébrico que define uma proposição da lógica proposicional.
data Prop = Const Bool | Var Char | Neg Prop | Conj Prop Prop | Disj Prop Prop |
            Xor Prop Prop | Imp Prop Prop | Sse Prop Prop | Error String
            deriving (Eq, Show)

-- Tipo definido para representar a associação entre variável e valor booleano.
type Subst = [(Prop, Bool)]

-- Função que calcula o valor verdade de uma expressão utilizando uma lista
-- que associa cada variável com um valor booleano.
eval :: Subst -> Prop -> Bool
eval _ (Const a) = a
eval (x:xs) (Var a) = 
 if (fst x) == (Var a)
  then snd x
  else eval xs (Var a)
eval t (Neg a) = not (eval t a)
eval t (Conj a b) = (eval t a) && (eval t b)
eval t (Disj a b) = (eval t a) || (eval t b)
eval t (Xor a b) = not (eval t (Conj a b)) && (eval t (Disj a b))
eval t (Imp a b) =
 if (eval t a)
  then (eval t b)
  else True
eval t (Sse a b) = (eval t (Imp a b)) && (eval t (Imp b a))

-- Função que retorna uma lista com todas as variáveis existentes em uma dada proposição.
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var a) = [a]
vars (Neg a) = vars a
vars (Conj a b) = vars a ++ vars b
vars (Disj a b) = vars a ++ vars b
vars (Xor a b) = vars a ++ vars b
vars (Imp a b) = vars a ++ vars b
vars (Sse a b) = vars a ++ vars b

-- Função que retorna todas as possíveis combinações de valores booleano para um dado n.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools x = [y:ys | y <- [True, False], ys <- bools (x-1)]

-- Função que associa cada possível valor booleano a cada variável da proposição.
substs :: Prop -> [Subst]
substs a =  map (zip (map Var (remDupl (vars a)))) (bools (length (remDupl (vars a))))

-- Função que remove elementos repetidos de uma lista.
remDupl :: Eq a => [a] -> [a]
remDupl [] = []
remDupl [x] = [x]
remDupl (x:xs) = x : (remDupl (filter (/= x) xs))

-- Função que compara se duas proposições são equivalentes.
equiv :: Prop -> Prop -> Bool
equiv a b = and [(eval x a) == (eval x b) | x <- substs a]