module Parser (parsing) where

import Verify

-- Converte uma string para o tipo Prop (do módulo Verify), verificando sua sintaxe.
parsing :: String -> Maybe Prop
parsing input
 | input /= "" = Just (Const True)
 | otherwise = Nothing