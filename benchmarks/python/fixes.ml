module P2 = Asp.Unstaged.Parse(Python_tokens_base.Tag)

open P2

let fix0 (type a b)
      (f : a P2.t)
    : a P2.t =
  f

let fix1 (type a b)
      (f : a P2.t -> a P2.t)
    : a P2.t =
  fix @@ fun b ->
  f b

let fix2 (type a b)
      (f : a P2.t * b P2.t -> a P2.t)
      (g : a P2.t * b P2.t -> b P2.t) : b P2.t =
  fix @@ fun b ->
  let a' = fix @@ fun a -> f (a, b) in
  g (a', b)

(* Pairs *)
module type PROD2 = sig
  type a and b and t
  val make : a -> b -> t
  val fst : t -> a
  val snd : t -> b
end

(* "Coinductive pairs", defined by observations *)
module CProd2 : PROD2 = struct
  type a and b
  type _ index = Fst : a index | Snd : b index
  type t = { resolve : 'c. 'c index -> 'c }
  let make (fst : a) (snd : b) =
    let resolve : type c. c index -> c = function Fst -> fst | Snd -> snd in
    {resolve}
  let fst { resolve } = resolve Fst
  let snd { resolve } = resolve Snd
end


let fix3 (type a b c)
      (f : a P2.t * b P2.t * c P2.t -> a P2.t)
      (g : a P2.t * b P2.t * c P2.t -> b P2.t)
      (h : a P2.t * b P2.t * c P2.t -> c P2.t)
    : c P2.t =
  fix @@ fun c ->
  let b' = fix @@ fun b ->
                  let a' = (fix @@ fun a -> f (a, b, c)) in
           g (a', b, c)
  in
  let a'' = fix @@ fun a -> f (a, b', c)
  in
  h (a'', b', c)

type (_,_) eql = Refl : ('a, 'a) eql
module type INDEX =
sig
  type _ t
  val equalp : 'a t -> 'b t -> ('a, 'b) eql option
end

module Fix(Index: INDEX) =
struct
  type entry = Entry : 'a Index.t * 'a P2.t -> entry
  type resolve = { resolve : 'a. 'a Index.t -> 'a P2.t;
                   mutable cache : entry list }

  let push i v r = r.cache <- Entry (i, v) :: r.cache

  let (!!) (type a) r (index : a Index.t) =
    let rec lookup = function
      | [] -> let v = r.resolve index in
              push index v r;
              v
      | Entry (index', v) :: entries ->
         match Index.equalp index index' with
         | None -> lookup entries
         | Some Refl -> v
    in lookup r.cache

  type r = { r : 'a. resolve -> 'a Index.t -> 'a P2.t }

  let fixn (r : r) index =
    let rec r' = { cache = []; resolve = fun i -> !! r' i } in
    fix @@ fun x -> push index x r'; r'.resolve index
end

let fix2 (type a b)
      (f : a P2.t * b P2.t -> a P2.t)
      (g : a P2.t * b P2.t -> b P2.t) : b P2.t =
  let module I2 = struct
      type _ t = Fst : a t | Snd : b t
      let equalp : type a b. a t -> b t -> (a, b) eql option =
        fun x y -> match x, y with
                   | Fst, Fst -> Some Refl
                   | Snd, Snd -> Some Refl
                   | _ -> None
    end in
  let module F2 = Fix(I2) in
  let r : type a. F2.resolve -> a I2.t -> a P2.t =
    fun rr -> function
    | I2.Fst -> f (rr.resolve I2.Fst, rr.resolve I2.Snd)
    | I2.Snd -> g (rr.resolve I2.Fst, rr.resolve I2.Snd)
  in F2.fixn {F2.r} I2.Snd 

(*
type (_,_) eql = Refl : ('a, 'a) eql

module FixN
         (Index:
            sig
              type _ t
              val equalp : 'a t -> 'b t -> ('a, 'b) eql option
            end) =
struct
  type resolve = { resolve : 'a. 'a Index.t -> 'a P2.t }
  type rhs = { rhs: 'a. resolve -> 'a Index.t -> 'a P2.t }

  type entry = Entry : 'a Index.t * 'a P2.t -> entry

  let rec lookup : type a. a Index.t -> entry list -> a P2.t option =
    fun index entries -> match entries with
     | [] -> None
     | Entry (index', v) :: entries ->
        match Index.equalp index index' with
        | None -> lookup index entries
        | Some Refl -> Some v

  let fixn (type a) {rhs} (index: a Index.t) : a P2.t =
    let rec go : type c. c Index.t -> entry list -> c P2.t =
      fun index entries ->
      match lookup index entries with
      | Some v -> v
      | None ->
         fix @@ fun (c : a P2.t) ->
         let resolve : resolve = 
           { resolve = fun (type b) (index : b Index.t) ->
                       assert false }
         in
         rhs resolve index
    in go index []
end
 *)
