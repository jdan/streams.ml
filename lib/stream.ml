type 'a t = Stream of 'a * (unit -> 'a t)

let rec ones = Stream (1, fun _ -> ones)

let head = function
  | Stream (value, _) -> value

let%test _ = 1 = head ones

let tail = function
  | Stream (_, rest) -> (rest ())

let%test _ = 1 = head (tail ones)

let rec constant k =
  Stream (k, fun _ -> constant k)

let nums =
  let rec inner n =
    Stream (n, fun _ -> inner (n+1))
  in inner 0

let%test _ = 0 = head nums
let%test _ = 1 = head (tail nums)
let%test _ = 2 = head (tail (tail nums))

let rec take n s = match n with
  | 0 -> []
  | n -> head s :: take (n - 1) (tail s)

let%test _ = [0;1;2;3] = take 4 nums
let%test _ = [7;7;7;7;7] = take 5 (constant 7)

let rec drop n s = match n with
  | 0 -> s
  | n -> drop (n - 1) (tail s)

let%test _ = [4;5;6;7] = (nums |> drop 4 |> take 4)

let rec map f s =
  Stream (f (head s), fun _ -> map f (tail s))

let%test _ = [0;2;4;6;8] = (map (( * ) 2) nums |> take 5)

let fizzbuzz =
  let count n =
    if n mod 15 = 0
    then "FizzBuzz"
    else if n mod 3 = 0
    then "Fizz"
    else if n mod 5 = 0
    then "Buzz"
    else string_of_int n
  in let nats = map ((+) 1) nums
  in map count nats

let%test _ = ["1";"2";"Fizz";"4";"Buzz";"Fizz";"7"] = take 7 fizzbuzz

let rec filter f s =
  if f (head s)
  then Stream (head s, fun _ -> filter f (tail s))
  else filter f (tail s)

let%test _ =
  [1;3;5;7;9] = (filter (fun n -> n mod 2 = 1) nums |> take 5)

let rec reduce f init s =
  let acc = f init (head s)
  in
  Stream
    ( acc
    , fun _ -> reduce f acc (tail s)
    )

let%test _ =
  [0;1;3;6;10] = (reduce (+) 0 nums |> take 5)

let rec iterate f init =
  Stream (init, fun _ -> iterate f (f init))

let%test _ = take 10 ones = take 10 (iterate (fun x -> x) 1)
let%test _ = take 10 nums = take 10 (iterate ((+) 1) 0)

let fib =
  let pairs =
    iterate (fun (a, b) -> (b, a + b)) (0, 1)
  in map (fun (a, _) -> a) pairs

let%test _ = [0;1;1;2;3;5;8;13;21] = take 9 fib

let collatz n =
  iterate
    ( fun n ->
        if n mod 2 = 0
        then n / 2
        else 3 * n + 1
    )
    n

let%test _ =
  [7;22;11;34;17;52;26;13;40;20;10;5;16;8;4;2;1]
  = take 17 (collatz 7)

let cycle ls =
  let rec inner ls = function
    | [] -> inner ls ls
    | x::xs -> Stream (x, fun _ -> inner ls xs)
  in inner ls ls

let%test _ =
  [1;2;3;1;2;3;1;2;3;1]
  = take 10 (cycle [1;2;3])

let rec interleave a b =
  Stream
    ( head a
    , fun _ ->
      Stream
        ( head b
        , fun _ ->
          interleave (tail a) (tail b)
        )
    )

let%test _ =
  [ 0; 0
  ; 1; 1
  ; 2; 1
  ; 3; 2
  ; 4; 3
  ; 5; 5
  ; 6; 8
  ; 7; 13
  ] =
  (interleave nums fib |> take 16)