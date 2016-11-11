let rec last l = 
match l with 
| [] -> failwith "liste vide"
| [a] -> a
| _ :: t -> last t;;

last [1;2;3];;

let rec list_produit l =
match l with 
| [] -> 1
| h :: t -> h * list_produit t;; 

list_produit [2;2;3];;

let rec mem x l = 
match l with
| [] -> false
| [a] -> if a = x then true else false
| h :: t -> if h = x then true else mem x t;;

mem 27[12;27;1];;
mem 3 [12;27;1];;

let rec map f l = 
match l with
| [] -> failwith "liste vide"
| [a] -> [f(a)]
| h :: t -> [f(h)] @ map f t;;

let succ x = x + 1 in map succ [1;2;3];;

let rec liste_min l =
match l with 
| [] -> failwith "liste vide"
| [a] -> a
| h :: t -> if h < liste_min t then h else liste_min t;;

liste_min [-30; 0; 549];;

let rec is_sorted l = 
match l with 
| [] -> failwith "liste vide"
| [a] -> true
| h :: n :: t -> if h < n then is_sorted ([n]@t) else false;;

is_sorted [1;3;4;6;5];;
is_sorted [1;2;3;4;5];;

let rec append l1 l2 =
match l1 with 
| [] -> l2
| [a] -> a :: l2
| h :: t -> h::append t l2;;

append [1;2;3] [4;5];;

let rec rev l = 
match l with 
| [] -> failwith "liste vide"
| [a] -> [a]
| h :: t -> rev(t) @ [h];;

let rev2 l =
  let rec aux l acc =
match l with
|[] -> acc
| h :: t -> aux t(h::acc)
  in aux l [];;

rev [1;2;3];;

let rec flatten l = 
match l with
|[] -> [] 
|[a] -> a
|h :: t -> h @ flatten(t);;

flatten [[2];[];[3;4;5]];;

let rotation_d l = 
match l with 
|[] -> failwith "liste vide"
|[a] -> [a]
|h :: t -> match rev(l) with
	   |[] -> l  
	   |h :: t -> h::rev(t) ;;

rotation_d [1;2];;
rotation_d [1;2;3;4;5];;

let rec moyenne l =
match l with 
|[] -> failwith "liste vide"
|[a] -> a
|h :: t -> h + moyenne t / 2;;

moyenne [1;2;3];;

let rec somme l = 
match l with 
[] -> 0
|h :: t -> h = (somme t)
    in 
    let moyenne1 l = (somme l) / (List.lenth(l)) ;;

let moyenne2 l = 
  let rec aux l s lg = 
match l with 
[] -> (s, lg)
| h :: t -> aux t (s + h) (lg + 1)
  in let r = aux l 0 0 
     in match r with ( s, lg) -> s / lg

let rec kelem l k = 
match l with 
|[] -> failwith "liste vide"
|[a] -> if k = 1 then a else failwith "liste trop courte"
|h :: t -> if k = 1 then h else kelem t (k-1);;

kelem [3;3;4;5] 3;;

let rec range a z =
if a = z then [z] else if a < z then a::(range(a+1) z) else a::(range(a-1) z);;

range 3 6;;
range 6 3;;

let rec choose l = 
  kelem l (Random.int (List.length(l)) + 1);;

choose [3;4;5;6];;

let rec filter f l = 
match l with 
|[] -> []
|[a] -> if (f a)=true then [a] else []
|h :: t -> if (f h) =true then [h]@ filter f t else filter f t ;;

filter(function x -> x > 3)  [4; 2; 10];; 
