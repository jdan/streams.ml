type 'a t = Stream of 'a * (unit -> 'a t)

let rec ones = Stream (1, fun _ -> ones)

let head = function
  | Stream (value, _) -> value

let%test _ = 1 = head ones

let tail = function
  | Stream (_, rest) -> (rest ())

let%test _ = 1 = head (tail ones)

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

let rec generate f init =
  Stream (init, fun _ -> generate f (f init))

let%test _ = take 10 ones = take 10 (generate (fun x -> x) 1)
let%test _ = take 10 nums = take 10 (generate ((+) 1) 0)

let fib =
  let pairs =
    generate (fun (a, b) -> (b, a + b)) (0, 1)
  in map (fun (a, _) -> a) pairs

let%test _ = [0;1;1;2;3;5;8;13;21] = take 9 fib

let collatz n =
  generate 
    (
      fun n -> 
        if n mod 2 = 0 
        then n / 2
        else 3 * n + 1
    )
    n

let%test _ =
  [7;22;11;34;17;52;26;13;40;20;10;5;16;8;4;2;1]
  = take 17 (collatz 7)
