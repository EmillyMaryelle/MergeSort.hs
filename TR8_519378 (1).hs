--Emilly Maryelle Xavier Pereira 519378
-- 1 passo: Criar um MergeSorte 
mergesort :: [Int] -> [Int]
mergesort = \list ->
    case list of
        [] -> []
        [x] -> [x]
        _ ->
            let (evens, odds) = divi list
            in juntar (mergesort evens) (mergesort odds) 
-- 2 passo: dividir o MS em duas partes             
divi :: [Int] -> ([Int], [Int])
divi = \list -> 
-- colocar alguma base para comparar pode ser if ou ate uma case só para organizar e observar a criação
--usar a mesma ordenação de um bubble sort
    case list of
        [] -> ([], [])
        x:xs -> 
            let (odds, evens) = divi xs
            in (x:evens, odds)

-- 3 passo: Juntar as duas partes e retornar ao passo 1 levando ao passo 4
juntar :: [Int] -> [Int] -> [Int]
juntar = \a -> \b ->
    case a of
        [] -> b
        x:xs ->
            case b of
                [] -> a
                c:c1 | x>c -> c:juntar a c1
                _ -> x:juntar xs b

-- 4 passo: retornar o resultado 
main = print (mergesort [1252, 346, 8786, 87])

-- Neste trabalho os alunos devem apresentar uma função em haskell que realiza a ordenação dos elementos de uma lista usando o algoritmo de Merge-Sort
