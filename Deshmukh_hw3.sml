datatype 'a inflist = NIL
                    | CONS of 'a * (unit -> 'a inflist);

fun HD (CONS(a,b)) = a
  | HD NIL = raise Subscript;

fun TL (CONS(a,b)) = b()
  | TL NIL = raise Subscript;

fun NULL NIL = true
  | NULL _ = false;

fun FILTER f l = if NULL l
                 then NIL
                 else if f (HD l)
                      then CONS(HD l, fn () => 
                                         (FILTER f (TL l)))
                      else FILTER f (TL l);

fun TAKE(xs, 0) = []
  | TAKE(NIL, n) = raise Subscript
  | TAKE(CONS(x,xf), n) = x::TAKE(xf(), n-1);

fun even num = 	if (num mod 2) = 0
		then true
		else false;

fun odd num = 	if (num mod 2) = 0
		then false
		else true;

fun fib a b = CONS(a,fn () => fib b (a+b)); 

val fibs = fib 0 1;

val evenFibs = FILTER even fibs;

val oddFibs = FILTER odd fibs;

fun printGenList f l =  if null l
			then (
				print("\n");
				()
			)
			else (
				f (hd l);
				printGenList f (tl l)
			);


fun printList l = 	let
				fun f n = print(Int.toString(n) ^ " ");
			in
				printGenList f l
			end;


fun printPairList l = 	let
				fun f (a, b) = print("(" ^ Int.toString(a) ^ ", " ^ Int.toString(b) ^ ") "); 
			in
				printGenList f l
			end;

fun zip (h1, h2) =  CONS( ((HD h1), (HD h2)),fn () => zip ((TL h1), (TL h2)) ); 

printList (TAKE(fibs, 20));
printList (TAKE(evenFibs, 10));
printList (TAKE(oddFibs, 10));
printPairList ( TAKE( (zip (evenFibs, oddFibs)), 10));
