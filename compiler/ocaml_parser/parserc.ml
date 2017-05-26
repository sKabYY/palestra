type 'a parser_t = char list -> ('a * char list) option

let satisfy pred = function
  | [] -> None
  | x::xs -> if pred x then Some(x, xs) else None;;
let range a b = satisfy (fun x -> x >= a && x <= b);;
let exactly x = satisfy ((=) x);;

let (<|>) p q = fun x -> match p x with Some _ as res -> res | None -> q x;;

let (>>=) m f = fun l -> match m l with
  | None -> None
  | Some(res, l1) -> f res l1;;
let return x = fun l -> Some(x, l);;

let (=>) p q = p >>= fun r -> return (q r);;
let (>>) p q = p >>= fun _ -> q;;
let (<~>) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs);;

(**)

let digit = range '0' '9';;
let lower = range 'a' 'z';;
let upper = range 'A' 'Z';;
let letter = lower <|> upper;;
