type 'a tree =
 | Nil
 | Node of 'a * 'a tree * 'a tree;;

(*quelques exemples d'arbres ; noter que a1, a7, a8, a9, a10 sont des abr,
et que les autres n'en sont pas *)

let a1=Node (10, Node(1, Nil, Nil), Nil);;
let a2=Node(3,a1,a1);;
let a3=Node(3,a1,a2);;
let a4=Node(10,a3,a3);;
let a5= Node(1, Node(2, Node(3,Nil,Nil),Node(4,Nil,Nil)),
	      Node(2, Node(3,Nil,Nil),Node(4,Nil,Nil)));;
let a6=Node(32,a4,a4);;
let a7=Node(42,Node(10,Nil,Node(39,Nil,Nil)),Nil);;
let a8=Node(20,Node(10,Node(5,Nil,Nil),Node(15,Nil,Nil)),
 Node(30,Node(25,Nil,Nil),Node(40,Nil,Nil)));;
let a9=Node(70,Node(60,Node(55,Nil,Nil),Node(65,Nil,Nil)),
 Node(80,Node(75,Nil,Nil),Node(90,Nil,Nil)));;
let a10=Node(50,a8,a9);;
let a11=Node(42,Node(10,Nil,Node(51,Nil,Nil)),Nil);;
let a12=Node(3,Node(4,Nil,Nil), a11);;

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

(*quelque exemple d'affichage*)
print_endline "Quelque exemple d'affichage";;
print_tree a1;;
print_tree a2;;
print_tree a3;;
print_tree a4;;
print_tree a5;;
print_tree a6;;
print_tree a7;;
print_tree a8;;
print_tree a9;;
print_tree a10;;
print_tree a11;;
print_tree a12;;
