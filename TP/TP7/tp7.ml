let readLine fic =
  try 
    Some(input_line fic)
  with 
    End_of_file -> None
;;

let rec readList fic = 
  match readLine fic with 
  |None -> []
  |Some(s) -> s :: readList fic
;;

let rec writeLine fic l = 
  match l with 
  |[s] -> output_string fic s
  |h :: t -> output_string fic h; writeLine fic t
;;

let triFic f1 f2 = 
  let fic1 = open_in f1 in
  let fic2 = open_out f2 in
  let liste = List.sort compare (readList fic1) in 
  writeLine fic2 liste;
  close_out fic2
;;


triFic "mots" "copy";;

let getListeMots ch = 
  let rec litLigne c l =
    let ligne = 
      try 
	Some(input_line ch)
      with
	End_of_file -> None
    in match ligne with 
       |None -> close_in c; l
       |Some s -> litLigne c (s :: l)
  in List.rev(litLigne ch []);;
let f = "mots";;
let listeMots = getListeMots(open_in f);;
let listeTriee = List.sort compare listeMots;;

type soundex = Soundex of char*int*int*int;;

let code_lettre c =
match c with
|' ' -> 0
|'B' | 'P' -> 1
|'C' | 'K' | 'Q' -> 2
|'D' |'T' -> 3
|'L' -> 4
|'M' | 'N' -> 5
|'R' -> 6
|'G' | 'J' -> 7
|'X' | 'Z' | 'S' -> 8
|'F' | 'V' -> 9
|_ -> -1
;;

let rec rem_double l =
match l with 
|[] -> []
|[a] -> [a]
|h :: n :: t -> if h = n then rem_double (n::t) else h :: rem_double (n::t)
;;

rem_double [1;1;2;2;2;-1;3;3];;

let rec string_to_list s = 
if String.length s = 0 then [] 
else String.get s 0 :: (string_to_list (String.sub s 1 ((String.length s) -1)))
;; 

string_to_list "banane";;

let rec rem_1 l = 
match l with 
|[] -> []
|[a] -> if a > 0 then [a] else []
|h :: t -> if h > 0 then h :: rem_1 t else rem_1 t
;;

rem_1 [0; 2 ;5;-1;-1;2;5;0];;

let code_soundex s = 
  let arg1 = String.get (String.uppercase s) 0 in 
  let list = rem_1 (List.map code_lettre (rem_double(string_to_list (String.uppercase (String.sub s 1 ((String.length s) -1)))))) in 
  match list with 
  |[] -> (arg1, 0, 0, 0)
  |a :: [] -> (arg1, a, 0, 0)
  |a :: b :: [] -> (arg1, a, b, 0)
  |a :: b :: c :: [] -> (arg1, a, b, c)
  |a :: b :: c :: d :: t -> (arg1, a, b, c)
;;
code_soundex "pagani";;

type soundexarray = string list array array array array;;

let arraysoundex = 
Array.init 26 (fun _ ->
	       Array.init 10 (fun _ ->
			       Array.make_matrix 10 10 []))
;;

let lireLignes (tab:soundexarray) s =
  let l = code_soundex s in
  match l with 
  |(a, b, c, d) -> tab.(Char.code a - Char.code 'A').(b).(c).(d) <- s :: tab.(Char.code a - Char.code 'A').(b).(c).(d)
  |_ -> failwith "erreur"
;;

let lireLignes (t:soundexarray) f = 
  let cin = open_in f in 
  let rec boucle y = 
    let w = 
      try Some(input_line cin)
      with End_of_file -> None
    in match w with 
       |None -> ()
       |Some x -> 
	 begin match (code_soundex x) with 
	       |(a, b, c, d) -> 
		 t.(Char.code a - Char.code 'A').(b).(c).(d) <- 
		   x :: t.(Char.code a - Char.code 'A').(b).(c).(d); 
		 boucle y
	 end
  in boucle 1;
     close_in cin
;;

exception Correct;;

let estCorrect (t:soundexarray) s =
  let mot = code_soundex s in
  match mot with 
  |(a,b,c,d) -> let rec check l= 
		  match l with 
		  |[] -> l
		  |h :: t  -> if s = h then raise Correct 
			      else check t  
		in t.(Char.code a - Char.code 'A').(b).(c).(d)
  |_ -> failwith "erreur"
;; 

let connu (t:soundexarray) mot = 
  match (code_soundex mot) with
  |(a,b,c,d) -> let l = t.(Char.code a - Char.code 'A').(b).(c).(d) in 
		if List.mem mot l then raise Correct else l
;;
  
let rec testmots (t:soundexarray) = 
  let s = read_line () in 
  if s = "QUIT" then raise Exit
  else let c = 
	 try 
	   Some(estCorrect t s) 
	 with 
	   Correct -> None
       in match c with 
       |Some(l) -> print_list l
       |None -> testmots t
;;

let rec testMots (t:soundexarray) = 
  print_string "Entrer un mot (ou QUIT)";
  let w = read_line() in 
  if w = "QUIT" then raise Exit
  else 
    try 
      let l = connu t w in 
      begin 
	print_string "Mot inconnu\n";
	print_list l;
	testmots t 
      end
    with 
      Correct -> print_string "OK\n"; testmots t
....
