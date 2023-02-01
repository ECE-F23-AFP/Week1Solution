namespace StarterExercises

open System

module Library =
    // 1.2
    let h x y = Math.Sqrt(x*x + y*y)

    // 1.5
    let rec fibonacci = function
        | 0 -> 0
        | 1 -> 1
        | n -> fibonacci(n-1) + fibonacci(n-2)
        
    // 1.6
    let rec sum = function
        | (m, 0) -> m
        | (m, n) -> m+n + sum(m, n-1)
    
    // 2.2
    let rec pow = function
        | (s: string, 1) -> s
        | (s: string, n) -> s + pow (s, n-1)
        
    // 2.3
    let isIthChar (s: string, i, ch) = if (s.[i-1] = ch) then true else false
    
    // 2.5
    let rec occInString = function
        | (s: string, _) when s.Length = 0 -> 0
        | (s, ch) -> (if s.[0] = ch then 1 else 0) + occInString(s.Substring(1), ch)
        
    // 2.8
    let rec bin = function
        | (_, 0) -> 1
        | (n, k) when n = k -> 1
        | (n, k) -> bin(n-1, k-1) + bin(n-1, k)
    
    // 4.1
    let upto n = 
        let rec uptoRec = function
            | i when i = n -> [i]
            | i            -> i :: uptoRec (i+1)
        uptoRec 1
    
    // 4.2
    let rec downto1 = function
        | i when i = 1 -> [i]
        | i            -> i :: downto1 (i-1)
    
    // 4.4
    let altsum2 xs =
        let rec altsum2Rec = function
            | (_, [])        -> 0
            | (neg, x :: xs) -> (if neg then -x else x) + (altsum2Rec (not neg, xs))
        altsum2Rec (false, xs)
