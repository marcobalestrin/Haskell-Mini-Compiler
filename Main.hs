module Main where 

import Lexer 
import Parser 
import Interpreter
import TypeChecker

-- main = getContents >>= print .eval . typecheck . parser . lexer

--Mostra todas as etapas
main = do
  input <- getContents
  let tokens = lexer input
  print tokens
  let ast = parser tokens
  print ast
  print (eval (typecheck ast))