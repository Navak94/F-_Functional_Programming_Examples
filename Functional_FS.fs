// max list
let rec max list =
    match list with
        | [x] -> x
        | head::tail ->
            let maximum = max tail
            if head > maximum then head
            else maximum
printfn "highest number is is %d" (max [1;2562;32;5;85;300;])


//search
let fn a =
    if a=0 then true
    else false

let rec search f list =
    match list with
        | [] -> -1
        | head::tail ->
            if f head then 0
            else 
                let searchValue = search f tail
                if searchValue = -1 then searchValue
                else searchValue + 1
printfn " search index %d" (search fn [1;0;3;5;2;-1])


//nth
let rec nth n list = 
    match list with
    | [n] -> n
    | head::tail -> 
        if n = 0 then head
        else nth (n-1) tail 
printfn " selection is %d" (nth 3 [1;2;3;4])



//zip
let rec zip pair =
    let left = fst(pair)
    let right = snd(pair)
    match left with 
        | [] -> []
        | _ ->
            let leftHead = List.head left
            let leftTail = List.tail left
            let rightHead = List.head right
            let rightTail = List.tail right
            [(leftHead, rightHead)] @ zip (leftTail, rightTail)
printfn " selection is %A" (zip (["a"; "b"; "c"; "d"], [1; 2; 3; 4]))

