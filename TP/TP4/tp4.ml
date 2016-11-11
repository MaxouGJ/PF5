type formule = Vrai | Faux
	       |Var of string
	       |Neg of formule
	       |Et of formule * formule
	       |Ou of formule * formule;;

let f = Et(Vrai, Neg(Ou(Var "q", Var "p")));;
let f2 = Et(Var "p", Ou(Faux, Faux));; 

let rec string_of_formule = 
function
|Vrai -> "Vrai"
|Faux -> "Faux"
|Var x -> x
|Et (x, y) -> "(" ^ (string_of_formule x) ^ " Et " ^ (string_of_formule y) ^ ")"
|Ou (x, y) -> "(" ^ (string_of_formule x) ^ " Ou " ^ (string_of_formule y) ^ ")"
|Neg y -> " Neg " ^ "(" ^ (string_of_formule y) ^ ")";;

string_of_formule f;;

let rec insert x l =
match l with 
|[] -> [x]
|h :: t -> if (h > x) then x :: l else if(h = x) then l else h :: insert x t;; 

let rec sort l = 
match l with 
|[] -> []
|[a] -> [a]
|h :: t -> insert h (sort t);;

let rec list_of_vars = 
function
|Vrai | Faux-> []
|Var x -> [x]
|Et(x, y) | Ou(x, y) -> sort (list_of_vars x @ list_of_vars y)
|Neg x -> list_of_vars x;;

list_of_vars f;;
list_of_vars (Et (Ou(Var "p",Var "q"), Var "q"));;

let rec eval_formule formule env =
match formule with 
|Vrai -> true
|Faux -> false
|Var x -> List.assoc x env
|Et(x, y) -> eval_formule x env && eval_formule y env
|Ou(x, y) -> eval_formule x env || eval_formule y env
|Neg x -> if (eval_formule x env) then false else true;; 

eval_formule f [("q",true);("p",false)];;
eval_formule f [("q", false); ("p", false)];;

let rec eval_sous_formule f =
match f with 
|Vrai |Faux |Var _ -> f
|Neg x -> begin match(eval_sous_formule x) with 
	  |Vrai -> Faux
	  |Faux -> Vrai
	  |y -> Neg y
	  end
|Ou(x, y) ->begin match(eval_sous_formule x, eval_sous_formule y)with 
		  |Vrai, _| _, Vrai -> Vrai
		  |Faux, Faux -> eval_sous_formule y
		  |a, b -> Ou(a, b)
	    end
|Et(x, y)->begin match(eval_sous_formule x, eval_sous_formule y)with
		 |Vrai, Vrai -> Vrai
		 |Faux,  _| _,Faux -> Faux
		 |a, b -> Et(a, b)
	   end 
;;
(*
let rec eval_sous_formule f =
match f with 
|Vrai -> Vrai
|Faux -> Faux
|Var x -> f
|Et(x, y) -> if((eval_sous_formule x) == Vrai) then (eval_sous_formule y) 
	     else if((eval_sous_formule y) == Vrai) then (eval_sous_formule x) 
	     else if((eval_sous_formule x) == Faux) then Faux 
	     else if((eval_sous_formule y) == Faux) then Faux 
	     else Et((eval_sous_formule x), (eval_sous_formule y))
|Ou(x, y) -> if((eval_sous_formule x) == Vrai) then Vrai 
	     else if((eval_sous_formule y) == Vrai) then Vrai
	     else if(((eval_sous_formule x) == Faux) && ((eval_sous_formule y) == Faux)) then Faux
	     else Ou((eval_sous_formule x),(eval_sous_formule y))		      
|Neg(x) -> if((eval_sous_formule x) == Vrai) then Faux 
	   else if((eval_sous_formule x) == Faux) then Vrai
	   else Neg((eval_sous_formule x));;
*)
eval_sous_formule f;;
eval_sous_formule f2;;
(*
let rec fnc f =
match f with 
|Vrai -> Vrai
|Faux -> Faux
|Var x -> f
|Ou(x, y) -> eval_sous_formule (fnc(Ou(x, y)))
|Et(x, y) -> eval_sous_formule (fnc(Et(x, y)))
|Neg(x) -> begin match((eval_sous_formule x)) with 
		 |Vrai -> Faux
		 |Faux -> Vrai
		 |Neg(a) -> a
		 |Ou(a, b) -> Et(fnc(Neg a), fnc(Neg b))
		 |Et(a, b) -> Ou(fnc(Neg a), fnc(Neg b))
		 |a -> Neg(a)		 
	   end
;;
 *)

let rec desc_n f = 
match f with
|Neg(Neg x) -> desc_n x
|Neg(Et(x, y)) -> Ou((desc_n x),(desc_n y))
|Neg(Ou(x, y)) -> Et((desc_n x),(desc_n y))
|Et(x, y) -> Et((desc_n x),(desc_n y))
|Ou(x, y) -> Ou((desc_n x),(desc_n y))
|_ -> f;;

let rec desc_ou f = 
match f with 
|Et(x, y) -> Et((desc_ou x),(desc_ou y))
|Ou(x, y) -> let g1 = (desc_ou x) 
	     and g2 = (desc_ou y) 
	     in match (g1, g2) with 
		|Et(a, b), _ -> Et((desc_ou (Ou(a, g2)), (desc_ou Ou(b, g2))))
;;

let fnc f = desc_ou(desc_n f);;

fnc (Ou(Neg(Et(Var "p", Var "q")), Et(Var "q", Var "r")));;
fnc (Neg(Neg(Var "p")));;
fnc (Neg(Ou(Var "p", Neg(Var "p"))));;
fnc f2;;

let ajoutElem e lenv = 

;;

let environnement f = 
  let lv = list_of_vars f in 
  let ajoutTous l ltemp = 
    match l with
    |[] -> ltemp
    |h :: t -> ajoutTous t (ajoutElem (h, false) ltemp) @ (ajoutElem(h, true) ltemp)
  in ajoutTous lv [[]]
;;
    
