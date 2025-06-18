# Haskell-Mini-Compiler

Este projeto é um interpretador funcional escrito em **Haskell**. Ele permite a avaliação de expressões aritméticas, booleanas, funções lambda, `let`, e **tuplas aninhadas** com suporte a operações `fst` e `snd`.

Desenvolvido como parte de um trabalho acadêmico para a disciplina de Linguagens de Programação.

---

## Funcionalidades Implementadas

- Números e booleanos (`true`, `false`)
- Operações aritméticas (`+`, `-`, `*`)
- Operações lógicas (`&&`, `||`, `xor`, `not`)
- Comparações (`>`, `<`, `==`)
- Condicionais (`if ... then ... else ...`)
- `let` bindings
- Funções lambda com aplicação
- **Tuplas aninhadas**, como `(1, 2, 3)` → representadas como `Tuple 1 (Tuple 2 3)`
- Acesso com `fst` e `snd`

---

## Requisitos

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Happy](https://www.haskell.org/happy/) (gerador de parser para Haskell)

---

##  Como compilar e executar

### 1. Gere o parser com `Happy`:

```bash
happy Parser.y
```

> Isso gera o arquivo `Parser.hs` automaticamente.

---

### 2. Execute um exemplo usando `runghc`

#### Windows

```powershell
Get-Content .\Examples\ex1.txt | runghc Main.hs
```
#### Linux/macOS

```bash
runghc Main.hs < Examples/ex1.txt
```

---

## Exemplo

Arquivo: `Examples/ex1.txt`

```haskell
let x = 10 in let y = 20 in (x, 1, y)
```

### Resultado esperado:

```
Tuple (Num 10) (Tuple (Num 1) (Num 20))
```

Isso mostra uma tupla aninhada de três elementos.

---


## Autor

#### Marco Antonio 
#### Universidade Federal da Fronteira Sul  
#### 2025
