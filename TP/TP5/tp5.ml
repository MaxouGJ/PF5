type joueur = Croix | Rond;;
type pion = Joueur of joueur | Vide;;
type ligne = (pion * pion * pion);;
type grille = (ligne * ligne * ligne);; 
type position = (int * char);;

let init = ((Vide, Vide, Vide), (Vide, Vide, Vide), (Vide, Vide, Vide));;
let g = (( Joueur Croix,Vide, Joueur Rond),(Joueur Croix, Joueur Rond,Vide),(Joueur Croix,Vide,Joueur Rond));;

let stringPion p = 
match p with 
|Vide -> " "
|Joueur Rond -> "O"
|Joueur Croix -> "X"
;;

let affichePion p = print_string (stringPion p);;

let stringLigne l = 
match l with (p1, p2, p3) -> "| " ^ stringPion(p1) ^ " | " ^ stringPion(p2) ^ " | " ^ stringPion(p3) ^ " |";;

let afficheLigne l = print_string (stringLigne l);;

let afficheGrille g = 
match g with (l1, l2, l3) -> print_string("+---+---+---+");
			     print_newline();
			     afficheLigne l1;
			     print_newline();
			     print_string("+---+---+---+");
			     print_newline();
			     afficheLigne l2;
			     print_newline();
			     print_string("+---+---+---+");
			     print_newline();
			     afficheLigne l3;
			     print_newline();
			     print_string("+---+---+---+");
			     print_newline();;
  
afficheGrille init;;
afficheGrille g;;

exception MoveForbidden ;;
exception NoMoreMove ;;

let move p g j = 
match p with 
(l, r) -> 
match g with
(l1, l2, l3) -> if(l = 1) then 
		  if(r = 'a') then 
		    match l1 with (p1, p2, p3) -> if (p1 = Vide) 
						  then ((Joueur j, p2, p3), l2, l3)
						  else raise MoveForbidden
		  else if(r == 'b') then
		    match l1 with (p1, p2, p3) -> if (p2 = Vide) 
						  then ((p1, Joueur j, p3), l2, l3)
						  else raise MoveForbidden
		  else if (r = 'c') then
		    match l1 with (p1, p2, p3) -> if (p3 = Vide) 
						  then ((p1, p2, Joueur j), l2, l3)
						  else raise MoveForbidden
		  else raise MoveForbidden
		else if(l = 2) then 
		  if(r = 'a') then 
		    match l2 with (p1, p2, p3) -> if (p1 = Vide) 
						  then (l1, (Joueur j, p2, p3), l3)
						  else raise MoveForbidden
		  else if(r = 'b') then
		    match l2 with (p1, p2, p3) -> if (p2 = Vide) 
						  then (l1, (p1, Joueur j, p3), l3)
						  else raise MoveForbidden
		  else if (r = 'c') then
		    match l2 with (p1, p2, p3) -> if (p3 = Vide) 
						  then (l1, (p1, p2, Joueur j), l3)
						  else raise MoveForbidden
		  else raise MoveForbidden
		else if(l = 3) then 
		  if(r = 'a') then 
		    match l3 with (p1, p2, p3) -> if (p1 = Vide) 
						  then (l1, l2, (Joueur j, p2, p3))
						  else raise MoveForbidden
		  else if(r = 'b') then
		    match l3 with (p1, p2, p3) -> if (p2 = Vide) 
						  then (l1, l2, (p1, Joueur j, p3))
						  else raise MoveForbidden
		  else if (r = 'c') then
		    match l3 with (p1, p2, p3) -> if (p3 = Vide) 
						  then (l1, l2, (p1, p2, Joueur j))
						  else raise MoveForbidden
		  else raise MoveForbidden
		else raise MoveForbidden
;;

afficheGrille(move (2, 'b') init Croix);;
afficheGrille(move (2, 'b') g Croix);;
afficheGrille(move (2, 'b') g Croix);;

(*
let aVide ((p1,p2,p3),(p4,p5,p6),(p7,p8,p9)) =
if (p1 = Vide) then true
else if (p2 = Vide) then true
else if (p3 = Vide) then true
else if (p4 = Vide) then true
else if (p5 = Vide) then true
else if (p6 = Vide) then true
else if (p7 = Vide) then true
else if (p8 = Vide) then true
else if (p9 = Vide) then true
else false;; 
 *)

let aVideLigne (p1, p2, p3) = (p1 = Vide || p2 = Vide || p3 = Vide);;

let aVide (l1, l2, l3) = (aVideLigne l1 || aVideLigne l2 || aVideLigne l3);;

aVide init;;
aVide g;;

let gagne ((p1, p2, p3), (p4, p5, p6), (p7, p8, p9)) =
match p1 with
|Joueur j when ((p1 = p2 && p1 = p3) || (p1 = p4 && p1 = p7) || (p1 = p5 && p1 = p9)) -> Joueur j 
|_ -> match p2 with 
      |Joueur j when (p2 = p5 && p2 = p8) -> Joueur j
      |_ -> match p3 with 
	    |Joueur j when ((p3 = p6 && p3 = p9) || (p3 = p5 && p3 = p7)) -> Joueur j
	    |_ -> match p4 with 
		  |Joueur j when (p4 = p5 && p4 = p6) -> Joueur j
		  |_ -> match p7 with 
			|Joueur j when (p7 = p8 && p7 = p9) -> Joueur j
			|_ -> if aVide((p1,p2,p3),(p4,p5,p6),(p7,p8,p9)) then Vide else raise NoMoreMove 
;;

gagne (((Joueur Croix, Joueur Croix, Joueur Croix),(Joueur Rond, Joueur Croix, Joueur Rond),(Joueur Rond, Joueur Rond,Joueur Croix)));;

let tour g j = 
