(*
Etant donnée une liste d'éléments comparables, cette fonction en supprime les doublons.
*)
let ss_doublons l =
	let rec aux = function
		| a::b::q	-> if a=b
					then aux (b::q)
					else a::(aux (b::q))
		| l		-> l
	in aux (List.sort compare l);;

(*
Cette fonction prend en entrée l'emplacement du fichier contenant le puzzle
et renvoie une matrice correspondant à l'instance associée.
*)
let parse entree =
	let ic = open_in entree and l = ref [] and flag = ref true in
	while !flag do
		try (l := (input_line ic)::!l)
		with _ -> flag := false;
	done;
	Array.of_list (List.map (fun str -> Array.init (String.length str)
		      (fun i -> str.[i])) (List.rev !l));;
(*
Cette fonction est la réciproque de la précédente, ie elle prend en entrée un
fichier de sortie et une matrice, et imprime la-dite matrice sur la-dite
de sortie.
*)
let imprime_mat sortie = 
	Array.iter (fun i -> Array.iter (fun c -> Printf.fprintf sortie "%c" c) i;
	Printf.fprintf sortie "\n");;

(*
Plus tard dans le code, nous aurons besoin d'utiliser des variables
propositionnelles correspondant à des emplacements de la matrice du jeu.
Pour pouvoir transformer un emplacement en un variable propositionnelle 
et vice versa, il nous faut de quoi transformer un entier (i.e. une suite
de chiffres) en un mot (ie une suite de lettres).
Pour cela on utilise le morphisme 0 -> a 1-> b etc... codé par i_to_s
(pour integer to string), alors que s_to_i fait évidemment le travail inverse.
*)
let rec i_to_s n =
	if n=0
		then ""
		else (i_to_s (n/10)) ^ (Char.escaped (char_of_int (97 + n mod 10)));;

let s_to_i mot =
	let a = ref 0 in
	for i = 0 to String.length mot - 1 do
		a := (10 * !a) + (int_of_char (mot.[i]) - 97);
	done;
	!a;;

(*
Si on veut transformer un couple (i,j) en une variable représentant la case (i,j),
on peut donc simplement faire:
*)
let transforme_ent (i,j) =
	"V"^(i_to_s i)^"X"^(i_to_s j);;


(*
Cette fonction prend en entrée une matrice, et renvoie la formule associée sous 
forme de liste de clauses, une clause étant elle-même représentée par une liste 
de littéraux. Un littéral est quand à lui représenté comme un quadruplet (i,j,lettre,booleen).
(i,j,lettre) représente une case de la matrice (cf rapport), alors que le booleen représente 
le fait que ce soit un littéral positif ou au contraire négatif.
*)
let mat_to_formula mat =
	let l = ref [] in
	for i=0 to Array.length mat - 1 do
		for j=0 to Array.length (mat.(0)) - 1 do
			match (mat.(i).(j)) with
				| '%' -> l := [i,j,'U',false]::[i,j,'R',false]::[i,j,'L',false]::[i,j,'D',false]::!l
				| ' ' -> l := [i,j,'L',false;i,j+1,'L',true]::[i,j,'R',false;i,j-1,'R',true]::
					      [i,j,'U',false;i+1,j,'U',true]::[i,j,'D',false;i-1,j,'D',true]::!l
				| 'D' -> l := [i,j,'L',true;i,j,'R',true;i,j,'U',true;i,j,'D',true]::
					      [i,j,'L',false;i,j+1,'L',true]::[i,j,'R',false;i,j-1,'R',true]::
					      [i,j,'U',false;i+1,j,'U',true]::[i,j,'D',false;i-1,j,'D',true]::!l
				| 'M' -> l := 	[(i,j,'M',true);(i,j,'L',false);(i-1,j,'D',true);]::
						[(i,j,'M',true);(i,j-1,'R',true);(i,j,'U',false);]::
						[(i,j,'M',true);(i,j,'D',false);(i,j+1,'L',true);]::
						[(i,j,'M',true);(i+1,j,'U',true);(i,j,'R',false);]::
						[(i,j,'M',false);(i,j,'L',false);(i+1,j,'U',true);]::
						[(i,j,'M',false);(i,j,'D',false);(i,j-1,'R',true);]::
						[(i,j,'M',false);(i,j,'R',false);(i-1,j,'D',true);]::
						[(i,j,'M',false);(i,j,'U',false);(i,j+1,'L',true);]::
						[(i,j,'L',false);(i+1,j,'U',true);(i-1,j,'D',true);]::
						[(i,j,'L',false);(i+1,j,'U',true);(i,j-1,'R',true);(i,j,'U',false);]::
						[(i,j,'L',false);(i+1,j,'U',true);(i,j,'D',false);(i,j+1,'L',true);]::
						[(i,j,'L',false);(i+1,j,'U',true);(i,j,'R',false);]::
						[(i,j,'L',false);(i,j,'D',false);(i,j-1,'R',true);(i-1,j,'D',true);]::
						[(i,j,'L',false);(i,j,'R',false);(i-1,j,'D',true);]::
						[(i,j,'L',false);(i-1,j,'D',true);(i,j,'U',false);(i,j+1,'L',true);]::
						[(i,j,'D',false);(i,j-1,'R',true);(i,j,'U',false);]::
						[(i,j,'D',false);(i,j-1,'R',true);(i,j+1,'L',true);]::
						[(i+1,j,'U',true);(i,j,'D',false);(i,j-1,'R',true);(i,j,'R',false);]::
						[(i,j-1,'R',true);(i,j,'R',false);(i-1,j,'D',true);(i,j,'U',false);]::
						[(i,j-1,'R',true);(i,j,'U',false);(i,j+1,'L',true);]::
						[(i,j,'D',false);(i,j,'R',false);(i-1,j,'D',true);(i,j+1,'L',true);]::
						[(i+1,j,'U',true);(i,j,'R',false);(i-1,j,'D',true);]::
						[(i,j,'D',false);(i,j,'U',false);(i,j+1,'L',true);]::
						[(i+1,j,'U',true);(i,j,'R',false);(i,j,'U',false);(i,j+1,'L',true);]::!l
				| 'E' -> if j = 0
						then l := [i,j,'L',true;i,j,'R',true;i,j,'U',true;i,j,'D',true]::[i,j,'L',false;i,j+1,'L',true]::
					       	     [i,j,'U',false;i+1,j,'U',true]::[i,j,'D',false;i-1,j,'D',true]::[i,j,'R',false]::!l
						else l := [i,j,'L',true;i,j,'R',true;i,j,'U',true;i,j,'D',true]::[i,j,'R',false;i,j-1,'R',true]
						     ::[i,j,'U',false;i+1,j,'U',true]::[i,j,'D',false;i-1,j,'D',true]::[i,j,'L',false]::!l;
				| _ -> ()
		done;
	done;!l;;

(*
Si on veut transformer un littéral obtenu via la fonction précédente, ie de la
forme (i,j,lettre,booleen), un moyen de le coder sous forme d'une suite de lettre
peut être de le mettre sous la forme "lettre^(i_to_si i)^X^(i_to_si j)"
*)
let rec transforme l = function
	| (a,b,c,false)	-> "(not " ^ (transforme l (a,b,c,true)) ^ ")"
	| (i,j,lettre,_)-> let a = (Char.escaped lettre)^(i_to_s i)^"X"^(i_to_s j)
			   in l := a::!l; a;;

(*
Cette fonction prend en entrée l'emplacement d'une fichier contenant une instance
initiale, et la transforme en une formule.
*)
let formule_finale entree =
	let mat = parse entree and declarations = ref [] in
	let l = List.map (List.map (transforme declarations)) (mat_to_formula mat)
	in	(l,mat,!declarations);;

(*
Etant donnée la liste des variables de type Bool, cette fonction les déclare
*)
let rec declare1 oc = function
	| []	-> ()
	| t::q	-> (Printf.fprintf oc "%s\n" ("(declare-const " ^ t ^ " Bool)");
		   declare1 oc q);;

(*
Etant donnée la liste des variables de type Int, cette fonction les déclare
*)
let rec declare2 oc = function
	| []	-> ()
	| t::q	-> (Printf.fprintf oc "%s\n" ("(declare-const " ^ t ^ " Int)");
		   declare2 oc q);;

(*
Etant donnée une liste de clauses, cette fonction imprime la formule logique 
associée
*)
let rec ecrit1 oc = function
	| []	-> ()
	| t::q	-> if List.length t = 1
			then Printf.fprintf oc "(assert %s)\n" (List.hd t)
			else Printf.fprintf oc "(assert (or %s))\n" (String.concat " " t);
		   ecrit1 oc q;;

(*
Etant donnée une liste de triplets (i,j,k), cette fonction imprime la liste de
(non i) ou (j > k)
*)
let rec ecrit2 oc = function
	| []	-> ()
	| (a,b,c)::q	-> Printf.fprintf oc "(assert (or (not %s) (> %s %s)))\n" a b c;
			   ecrit2 oc q;;

(*
Une fois qu'on aura utilisé les quatre fonctions précédentes pour rentrer la formule
que l'on désire dans SMT, on va récupérer ce que renvoie SMT sous forme d'une
liste l. Ayant à notre disposition la matrice mat, on va modifier celle-ci pour
que les miroirs soient bien orientés. Pour cela on va parcourir la liste l, et
on va utiliser une méthode un peu complexe pour extraire de chaque élément de
cette liste le littéral correspondant. Si le-dit littéral correspond à un miroir,
on change la matrice en conséquent via la fonction change
*)

let change mat mot booleen =
	if mot.[0] = 'M'
		then let a = String.index mot 'X' in
			mat.(s_to_i (String.sub mot 1 (a-1))).
			(s_to_i (String.sub mot (a+1) (String.length mot - a - 1)))
			<- if booleen then '\\' else '/';;

let soccupe liste mat =
	let l = ref (List.tl (List.tl (List.rev (List.tl (List.tl liste))))) in
	while List.length (!l) > 1 do
		let a::b::q = !l in
			l := q;
			if a.[String.length a -1] = 'l'
				then change mat (String.sub a 14 (String.length a - 22)) 
						(b.[String.length b - 3] = 'u');
	done;;

(*
La fonction principale:
Tout d'abord, elle prend en entrée le fichier d'entrée sur lequel est situé 
l'instance que l'on souhaite résoudre, ainsi que le fichier de sortie sur 
lequel on veut imprimer la solution. Tout d'abord, on va chercher les variables
 à déclarer. Ensuite, on va créer les formules logiques correspondantes à
l'instance considéree.
On déclare les variables, puis on écrit les formules à écrire.
On récupère alors ce que renvoie SMT, et on le parse via la fonction soccupe. 
Enfin on imprime la matrice solution.
*)

let final () =
	let v = Sys.argv in
	let entree = v.(1) and sortie = v.(2) in
	let (formule1,mat,declarations_const) = formule_finale entree
	and (ic,oc) = Unix.open_process "z3 -smt2 -in" in
	let declarations_int = ref [] and formule2 = ref [] in
		for i=0 to Array.length mat - 1 do
			for j=0 to Array.length (mat.(0)) - 1 do
				declarations_int := (transforme_ent (i,j))::!declarations_int;
			done;
		done;
		for i=1 to Array.length mat - 2 do
			for j=1 to Array.length (mat.(0)) - 2 do
				formule2 := ("R" ^ (i_to_s i) ^ "X" ^ (i_to_s j),
				transforme_ent (i,j),transforme_ent (i,j+1))::!formule2;
				formule2 := ("U" ^ (i_to_s i) ^ "X" ^ (i_to_s j),
				transforme_ent (i,j),transforme_ent (i-1,j))::!formule2;
				formule2 := ("L" ^ (i_to_s i) ^ "X" ^ (i_to_s j),
				transforme_ent (i,j),transforme_ent (i,j-1))::!formule2;
				formule2 := ("D" ^ (i_to_s i) ^ "X" ^ (i_to_s j),
				transforme_ent (i,j),transforme_ent (i+1,j))::!formule2;
			done;
		done;
		declare1 oc (ss_doublons declarations_const);
		declare2 oc !declarations_int;
		ecrit1 oc formule1;
		ecrit2 oc !formule2;
		Printf.fprintf oc "(check-sat)\n";
		Printf.fprintf oc "(get-model)\n";
		close_out oc;
		let l = ref [] and flag = ref true in
		while !flag do
			try (
			l := input_line ic :: !l;
			) with _ -> flag := false;
		done;
		let sortie_c = open_out sortie in
		if List.hd (List.rev !l) = "sat"
			then (soccupe !l mat;imprime_mat sortie_c mat)
			else Printf.fprintf sortie_c "unsatisfiable";;


final ();; 

(* ./a.out "/import/gaubian/Documents/test_prolog.txt" "/import/gaubian/Documents/sortie_prolog.txt" *)
