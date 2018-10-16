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
