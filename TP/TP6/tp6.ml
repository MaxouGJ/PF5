let liste1 n = 
  let rec aux n l = 
    match n with 
    |0 -> 0 :: l
    |_ -> aux (n-1) (n::l)
  in aux n []
;;

let length1 l = 
  let rec aux l2 s = 
    match l2 with
    |[] -> s
    |_ :: t -> aux t (1+s)
  in aux l 0
;;

let fib n = 
  let rec fibAux n a b = 
    match n with 
    |0 -> a
    |1 -> b
    |_ -> fibAux (n-1) b (a+b)  
  in fibAux n 1 1  
;;

fib 45;;

let append' l r =
  let rec aux l r = 
    match l with 
    |[] -> r
    |h :: t -> aux t (h :: r)
  in aux l r
;;

append' [1;2;3] [4;5;6];;

let append l1 l2 = 
  let l3 = append' l1 [] in 
  append' l3 l2;;

append [1;2;3] [4;5;6];;

let rec compter_lignes_c c n = 
try 
  let l = input_line c in compter_lignes_c c (n+1)
with End_of_file -> n;;

let compterLignes f =
  let c = open_in f in compter_lignes_c c 0;;

let rec compterLignes_c c n = 
  let l = 
    try 
      input_line c 
    with 
      End_of_file -> ""
  in match l with 
     |"" -> n
     |l -> compterLignes_c c (n+1)
;;

let compterLignes f =
  let c = open_in f in compterLignes_c c 0;;

print_int (compterLignes "mots");;

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree;;

let rec taille a = 
match a with 
|Nil -> 0
|Node(a, x, y) -> 1 + taille x + taille y
;;

let rec taille' l n = 
  match l with
  |[] -> 0
  |[a] -> taille a + n 
  |h :: t -> taille h + n + taille' t n
;;

let a1 = Node(0, Nil, Nil);;
let a2 = Node(5, Node(7, Nil, Nil), Node(6, Nil, Nil));;

taille' [a1;a2] 4;;

let rec tailleT a =
  match a with
  |Nil -> 0
  |Node(x, g, d) -> taille' [g;d] 1 - 1
;; 

tailleT a1;;
tailleT a2;;

let insert l x = 
  let rec insertAux l x prec = 
    match l with
    |[] ->  rev (x :: prec)
    |[n] -> if n = x then rev (n :: prec) 
	    else if n > x then rev (n :: x :: prec) 
	    else rev(x :: n :: prec)
    |h :: t -> if x = h then (rev(prec)) @ l 
	       else if x < h then rev (x::prec) @ l  
	       else insertAux t x (h :: prec)
  in insertAux l x [];; 

let rec rev l = 
  match l with 
    [] -> []
  |[x] -> [x]
  |h :: t -> rev t @ [h];;

rev [12;15;954];; 
insert [1;5;9;25;27;48] 12;;

let rec sort l = 
  match l with 
  |[] -> []
  |[x] -> [x]
  |h :: t -> insert (sort t) h;;
 
sort [12;15;954;2;54;36;914;65;21;15];;

let rec union_sorted l1 l2 = 
  match l1 with 
    [] -> l2
  |[a] -> insert l2 a
  |h :: t -> union_sorted t (insert l2 h);;

union_sorted [1;3;5] [2;5;8];;

let inter_sorted l1 l2 = 
  let rec aux l1 l2 res
  match l1 with 
    [] -> res
  |[a] -> 
  |h :: t -> match l2 with 
	     |[] -> []
	     |[x] -> if x = h then [x] else []
	     |y :: q -> if h = y then inter_sorted t l2
			else if h < y then inter_sorted t l2
			else inter_sorted l1 q
  in aux l1 l2 []
;;

inter_sorted [1;3;5] [2;5;8];;
