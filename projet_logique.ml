(*
Prend l'emplacement d'un fichier d'entrée et parse la matrice qui y est située
*)
let parse entree =
	let ic = open_in entree and l = ref [] and flag = ref true in
	while !flag do
		try ( l := (input_line ic)::!l)
		with _ -> flag := false;
	done;
	Array.of_list (List.map (fun str -> Array.init (String.length str) (fun i -> str.[i])) (List.rev !l));;

(*
Permet d'interpréter la sortie qui sera renvoyée par z3
*)
let parse_mot mot =
	let l = ref [] and en_cours = ref "" in
	for i=0 to String.length mot - 1 do
		match (mot.[i]) with
			| ' ' -> l := (int_of_string !en_cours) :: !l; en_cours := ""
			| '\n' -> ()
			|  c  -> en_cours := !en_cours ^ (Char.escaped c);
	done;!l;;

(*
Cette fonction prend en entrée une matrice, et renvoie la formule associée sous 
forme de liste de clauses, une clause étant elle-même représentée par une liste 
de littéraux. Un littéral est quand à lui représenté comme un quadruplet 
(i,j,lettre,booleen).
(i,j,lettre) représente une case de la matrice (cf rapport), alors que le 
booleen représente le fait que ce soit un littéral positif ou au contraire négatif.
*)
let mat_to_formula mat =
	let l = ref [] in
	for i=0 to Array.length mat - 1 do
		for j=0 to Array.length (mat.(0)) - 1 do
			match (mat.(i).(j)) with
				| '%' -> l := [i,j,'U',false]::[i,j,'R',false]::[i,j,'L',false]::[i,j,'D',false]::!l
				| ' ' -> l := [i,j,'L',false;i,j+1,'L',true]::[i,j,'R',false;i,j-1,'R',true]::
					      [i,j,'U',false;i+1,j,'U',true]::[i,j,'D',false;i-1,j,'D',true]::!l
				| 'D' -> l := [i,j,'L',true;i,j,'R',true;i,j,'U',true;i,j,'D',true]::[i,j,'L',false;i,j+1,'L',true]::
					      [i,j,'R',false;i,j-1,'R',true]::[i,j,'U',false;i+1,j,'U',true]::[i,j,'D',false;i-1,j,'D',true]::!l
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
let rec transforme table n = function
	| (a,b,c,false)	-> - (transforme table n (a,b,c,true))
	| (i,j,'M',_)	-> (Hashtbl.add table ((n*i + j) * 5) (i,j) ;(n*i + j) * 5)
	| (i,j,'R',_)	-> (n*i + j) * 5 + 1
	| (i,j,'U',_)	-> (n*i + j) * 5 + 2
	| (i,j,'L',_)	-> (n*i + j) * 5 + 3
	| (i,j,'D',_)	-> (n*i + j) * 5 + 4
	| _		-> failwith "YOLOSWAG";;

(*
Cette fonction prend en entrée l'emplacement d'un fichier contenant une instance
initiale, et la transforme en une formule.
*)
let formule_finale table entree =
	let mat = parse entree in
		(List.map (List.map (transforme table (Array.length (mat.(0))))) (mat_to_formula mat),mat);;

(*
Permet de communiquer notre formule logique à z3
*)
let ecrire ic =
	List.iter (fun i -> List.iter (Printf.fprintf ic "%d ") i; Printf.fprintf ic "0\n");;

(*
Imprime le résultat à la fin
*)
let imprime_mat sortie = Array.iter (fun i -> Array.iter (fun c -> Printf.fprintf sortie "%c" c) i; Printf.fprintf sortie "\n");;

(* 
La fonction finale
*)
let final () =
	let v = Sys.argv in
	let entree = v.(1) and sortie_c = open_out v.(2) and table = Hashtbl.create 1 in
	let (formule,mat) = formule_finale table entree
	and (ic,oc) = Unix.open_process "z3 -dimacs -in" in
		ecrire oc formule;
		close_out oc;
	let a = input_line ic in
		if a.[0] = 'u'
			then Printf.fprintf sortie_c "unsatisfiable"
			else (let b = parse_mot (input_line ic) in 
				List.iter (fun x -> try (
						let (i,j) = Hashtbl.find table (abs x) in mat.(i).(j) <- if x > 0 then '\\' else '/') with _ -> ()
							) b;
	imprime_mat sortie_c mat);; 	

final ();;
