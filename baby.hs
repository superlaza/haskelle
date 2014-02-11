max' :: (Ord a) => [a]->a
max' [x] = x
max' (x:xs) 
	| x > m = x
	| otherwise = m
	where m = max' xs

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

parts :: Int -> Int -> [[Int]]
parts 0 p = [[]]
parts x p = [(y:ys) | y <-[p..x], ys <- (parts (x - y) y)]

p 0 = [[]]
p n = [(x:xs) | x<-[x|x<-[1..n], (x==1)||(x==2)], xs<-(p (n-x)), (null xs || x<=head xs)]

count [] = 0
count (_:xs) = 1 + (count xs)

p' x 
	| x<0 = 0
	| otherwise = sum [(p' (x-k))+(p' (x-k))| k<-[1..x]]


g k = k*(3*k-1)/2

getk x = floor ((1+sqrt (1+24*x))/6)