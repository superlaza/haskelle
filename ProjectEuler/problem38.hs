quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

n = head [x | x<-[9999,9998..1000], (quicksort ((show x) ++ (show (x*2)))) == "123456789"]
answer = (show n) ++ (show (n*2))