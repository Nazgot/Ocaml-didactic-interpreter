	(*Expressions Types*)

	type ide = string;;

	type operator = Plus | Minus | Mul | Div | And | Or | Eq;;

	type exp = Int of int
			 | Bool of bool
			 | Den of string
			 | Op of exp * operator * exp
			 | Let of ide * exp * exp
			 | Fun of ide * exp
			 | Apply of exp * exp
			 | Ifz of exp * exp * exp
			 | Etup of tuple
			 | Pipe of tuple * exp
			 | ManyTimes of int * exp * exp
				and tuple = Nil | Seq of exp * tuple;;
	    

	(*Environment definition*)
	type 't env = (string * 't) list;;
	exception WrongBindlist;;
	let emptyenv(x) = [("", x)];;
	let rec applyenv(x, y) = match x with
										| [(_, e)] -> e
										| (i1, e1) :: x1 -> if y = i1 then e1 else applyenv(x1, y)
										| [] -> failwith("wrong env");;
	let bind(r, l, e) = (l, e) :: r;;
	let rec bindlist(r, il, el) = match (il, el) with
										| ([], []) -> r
										| (i::il1, e::el1) -> bindlist (bind(r, i, e), il1, el1)
										| _ -> raise WrongBindlist
	;;

	(*Types*)
	type dexp = Dint of int | Dbool of bool | Unbound | Dtuple of dexp list | Funval of efun and efun = ide * exp * dexp env ;;
	
	(*Basic operations*)
	let apply_op e1 op e2 = match e1,op,e2 with
	| Dint i , Plus , Dint j -> Dint(i+j)
	| Dint i , Minus , Dint j -> Dint(i-j)
	| Dint i , Mul , Dint j -> Dint(i*j)
	| Dint i , Div , Dint j -> Dint(i/j)
	| Dbool i, And , Dbool j -> Dbool(i && j)
	| Dbool i, Or , Dbool j -> Dbool(i || j)
	| Dint i, Eq , Dint j -> Dbool(i=j)
	| Dbool i, Eq , Dbool j -> Dbool(i && j)
	| _,_,_ -> failwith("operazione non supportata");; 
	  
	  (*run-time*)

	let rec eval ((e: exp), (r:dexp env)) = match e with
		 Int i -> Dint i
	   | Bool i -> Dbool i
	   | Den s -> applyenv(r,s)
	   | Let(i, e1, e2) -> eval(e2, bind(r, i, eval(e1, r)))
	   | Op (e1,op,e2) ->  let v1 = (eval(e1,r)) in 
									let v2 = (eval(e2,r))
									in apply_op v1 op v2
	   | Ifz (e1,e2,e3) -> let cond = (eval(e1,r)) in (match cond with 
													   |Dbool true -> eval(e2,r)
													   |_ -> eval(e3,r))
	   | Etup e1 -> let v = (evalList e1 r) in Dtuple v 			
	   | Fun(i, a) -> Funval(i, a, r)
	   | Apply(e1, e2) -> (match eval(e1, r) with
				| Funval(i, a, r1) -> eval(a, bind(r1, i, eval(e2, r)))
				| _ -> failwith("no funct in apply"))
	   | ManyTimes(i,e,par) -> let rec createnest(a,n,p) = if n <= 0 then p else Apply(e,createnest(a,n-1,p))
									      in let nest = createnest(e,i,par) in eval(nest,r)									
	   | Pipe(f,par) -> let rec crealista a = match a with 
						  | Nil -> par
						  | Seq(e1,el1) -> Apply(e1,crealista(el1))
						  in let lista = crealista(f) in eval (lista,r)
		and evalList el r = match el with 
									  Nil -> []
									| Seq (e1,el1) -> (eval(e1,r))::(evalList el1 r)
	   
		;;
	 
		let dIntToInt de = match de with 
								Dint x -> x
								| _ -> failwith("wrong int");;  
	   
	    (*tests*)
	   print_int(dIntToInt(eval((Op(Int 3,Plus,Int 2)),emptyenv Unbound)));;  (*3+2*)
	   let p = eval(Let("x" , Op(Int 1, Plus, Int 0), Let("y", Ifz(Op(Den "x",Eq,Int 0),Op(Den "x",Minus, Int 1),Op(Den "x",Plus,Int 1)),Let("z",Op(Den "x",Plus,Den "y"),Den "z"))), emptyenv Unbound);;(*ifthenelse = 3*)
	   print_int(dIntToInt(p));;
	   let p2 = eval(Let("t",Etup(Seq(Int 3, Seq(Int 33, Seq(Bool true,Nil)))),Den "t"), emptyenv Unbound) in p2;; (*tuple int int bool*)
	   let p2 = eval(Let("t",Etup(Seq(Fun("k",Op(Den "k",Plus, Int 1)),Nil)),Den "t"), emptyenv Unbound) in p2;; (*tuple with a function inside*)
	   
	   
	   
        let p = Pipe(Seq(Fun("k",Op(Den "k",Plus,Int 1)),Seq(Fun("k",Op(Den "k",Plus,Int 2)),Nil)),Int 3) in  (*pipe on a tuple with 2 functions, = 6*)
        eval(p,emptyenv Unbound);;
		
		let p1 = ManyTimes(2,Fun("k",Op(Den "k",Plus,Int 1)),Int 1) in eval(p1,emptyenv Unbound);; (*manytimes of "k+1" with k = 1 on first call*)
	   
	   
	   
	   
