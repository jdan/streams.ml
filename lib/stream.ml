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