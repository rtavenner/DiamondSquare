module DiamondSquare exposing (diamondSquare)

import Array exposing (Array)
import Random
import Random.Array
import Lazy

diamondSquare : Int -> (Float,Float,Float,Float) -> Random.Generator (Array (Array Float))
diamondSquare detail corners =
    Random.map 
        (diamondSquareLazy corners (2^detail+1) 
            >> Array.map (Array.map Lazy.force)) 
        (Random.Array.array (2^detail+1) 
            <| Random.Array.array (2^detail+1) 
            <| Random.float -1 1)

diamondSquareLazy : (Float,Float,Float,Float) -> Int -> Array (Array Float) -> Array (Array (Lazy.Lazy Float))
diamondSquareLazy (a,b,c,d) size randomness = 
    let f : Int -> Int -> Float -> Lazy.Lazy Float
        f x y rand = 
            if x == 0 && y == 0
            then Lazy.lazy <| \_ -> a
            else if x == 0 && y == size - 1
            then Lazy.lazy <| \_ -> b
            else if x == size - 1 && y == 0
            then Lazy.lazy <| \_ -> c
            else if x == size - 1 && y == size - 1
            then Lazy.lazy <| \_ -> d
            else 
                basedOn x y
                |> (\(dist,diag) ->
                    if diag 
                    then 
                        ( sqrt 2 * toFloat dist
                        , [(x+dist,y+dist)
                          ,(x+dist,y-dist)
                          ,(x-dist,y+dist)
                          ,(x-dist,y-dist)])
                    else
                        ( toFloat dist
                        , [(x+dist,y)
                          ,(x-dist,y)
                          ,(x,y+dist)
                          ,(x,y-dist)]))
                |> (\(scale,lst) -> Lazy.lazy (\_ -> 
                    lst
                    |> List.filterMap 
                        (\(x,y) -> 
                            answer 
                            |> Array.get x 
                            |> Maybe.andThen (Array.get y) 
                            |> Maybe.map Lazy.force)
                    |> (\nearby -> List.sum nearby / toFloat (List.length nearby)) 
                    |> (+) (rand * scale / toFloat size)))
        answer : Array (Array (Lazy.Lazy Float))
        answer = Array.indexedMap (\x -> Array.indexedMap (\y -> f x y)) randomness
        basedOn : Int -> Int -> (Int,Bool)
        basedOn x y = 
            case (x % 2, y % 2) of
                (0,0) -> Tuple.mapFirst ((*) 2) <| basedOn (x//2) (y//2)
                (0,_) -> (1,False)
                (_,0) -> (1,False)
                (_,_) -> (1,True)
    in answer