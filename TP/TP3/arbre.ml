type 'a tree = 
  |Nil 
  |Node of 'a *' a tree *'a tree;;

let rec space k = 
  if k <= 0 then ""
  else " "^(space (k - 1))

let rec line k =  
  if k <= 0 then ""
  else "-"^(line (k - 1))
;;


let croisement l1 ls1 ln l2 ls2 =
  let spc_center = space ln in
  let spc_left   = (space l1)^spc_center
  and spc_right  = spc_center^(space l2) in
  let rec aux ls1 ls2 = match ls1, ls2 with
      [], [] -> []
    | s1::ls1', [] -> (s1^spc_right)::(aux ls1' [])
    | [], s2::ls2' -> (spc_left^s2)::(aux [] ls2')
    | s1::ls1', s2::ls2' -> (s1^spc_center^s2)::(aux ls1' ls2')
  in aux ls1 ls2
;;


let rec levels_of_tree t = match t with 
    Nil -> ((1, 0), ["*"])
  | Node(n, g, d) -> 
    let sn = string_of_int n in
    let ln = String.length sn in 
    let (l1, ofs1), ls1 =  levels_of_tree g 
    and (l2, ofs2), ls2 =  levels_of_tree d in 
    let sr_top = 
      (space (ofs1 + 1))^
      (line (l1 - ofs1 - 1))^
      (sn)^
      (line ofs2)^
      (space (l2 - ofs2))  in
    let sr_bot = 
      (space ofs1)^"|"^
      (space (l1 - ofs1 - 1))^
      (space ln)^
      (space ofs2)^"|"^
      (space (l2 - ofs2 - 1))  in

    let lc = croisement l1 ls1 ln l2 ls2  in 
    ((l1 + ln + l2, l1), sr_top::sr_bot::lc)
;;

let print_tree tree = 
    List.iter (fun s -> print_string (s^"\n")) (snd (levels_of_tree tree));;

let arbre  = Node(42, Node(10, Nil, Node(25, Nil, Nil)), Node(1, Nil, Nil));;
let arbrestr = Node("abc", Nil, Node("def", Nil, Nil));;
let acomplet = Node(10, Node(5, Nil, Nil), Node(18, Nil, Nil));;
let abr = Node(42, Node(10, Nil, Node(39, Nil, Nil)), Nil);;

let rec taille a =
match a with 
|Nil -> 0
|Node(x, g, d) -> 1 + taille(g) + taille(d);;

taille(arbre);;
taille arbrestr;;

let rec hauteur a =
match a with 
|Nil -> 0
|Node(x, g, d) -> 1 + max(hauteur g) (hauteur d);;

hauteur arbre;;
hauteur arbrestr;;
p
let rec mem n a = 
match a with 
|Nil -> false
|Node(x, g, d) -> if x = n then true else (mem n g) || (mem n d);;

mem 1 arbre;;
mem 12 arbre;;

let rec complet a =
match a with 
|Nil -> true
|Node(x, g, d) -> if(hauteur g = hauteur d) then complet g && complet d else false;; 

complet arbre;;
complet acomplet;;

let rec elements a = 
match a with 
|Nil -> []
|Node(x, g, d) -> elements g @ [x] @ elements d;;

elements arbre;;
elements acomplet;;

let rec mem_abr n a = 
match a with 
|Nil -> false
|Node(x, g, d) -> if(x = n) then true else if(x > n) then mem_abr n g else mem_abr n d;;

mem_abr 39 abr;;
mem_abr 12 abr;;

let rec add_abr n a = 
match a with 
|Nil -> Node(n, Nil, Nil)
|Node(x, g, d) -> if(x = n) then a  else if(x > n) then Node(x, add_abr n g, d) else Node(x, g, add_abr n d);;

add_abr 59 abr;;
print_tree (add_abr 59 abr);;

let rec min_abr a = 
match a with
|Node(x, g, d) -> if(g = Nil) then x else min_abr g;;

let rec max_abr a = 
match a with
|Node(x, g, d) -> if(d = Nil) then x else max_abr d;;

let rec is_abr a min max= 
match a with 
|Nil -> true
|Node(x, g, d) -> if(x<min || max<x) then false else (is_abr g min x && is_abr d x max);; 

is_abr abr (min_abr abr) (max_abr abr);;
is_abr arbre (min_abr arbre) (max_abr arbre);;

let rec make_abr l =
match l with 
|[a] -> Node(a, Nil, Nil)
|h :: t -> add_abr h (make_abr t);;

print_tree (make_abr [5;4;87;6;8;2]);;
elements (make_abr [5;4;87;6;8;2]);;

let rec forall_labels p a =
match a with 
|Nil -> true
|Node(x, g, d) -> (p x && forall_labels p g && forall_labels p d);;

let f x = x > 50;;
forall_labels f abr;;

let is_uniform v a = 
forall_labels (function x -> x = v) a;;

is_uniform 10 abr;;
is_uniform 10 (Node(10, Nil, Nil));;

let rec forall_subtrees p a = 
match a with 
|Nil -> true
|Node(x, g, d) -> p x g d && forall_subtrees p g && forall_subtrees p d;;

let est_peigne_droit a = 
match a with 
|Nil -> true
|Node(x, g, d) -> forall_subtrees (fun x g d -> g = Nil) a;;

est_peigne_droit (Node(10, Nil, Node(17, Nil, Node(7, Nil, Nil))));;
est_peigne_droit abr;;

let rec fold_tree fn vf a =
match a with 
|Nil -> vf
|Node(x, g, d) -> fn x (fold_tree fn vf g) (fold_tree fn vf d);;

(*Le type de fold_tree est celui de vf*)

let somme_etiquettes a = 
match a with 
|Nil -> 0
|Node(x, g, d) -> fold_tree (fun x y z -> x + y + z) 0 a;;
