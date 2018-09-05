(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type ('a, 'b) cmp =
   Leq : ('a, 'b) cmp
 | Eql : ('a, 'a) cmp
 | Geq : ('a, 'b) cmp

(** Interface to the types of tags for tokens in the input stream *)
module type TAG = sig
  type 'a t
  val compare : 'a t -> 'b t -> ('a, 'b) cmp
  val print : Format.formatter -> 'a t -> unit
end


(** Interface to the types of elements (tokens) in the input stream *)
module type TOKEN =
sig
  type t
  type utag
  module Ord : Set.OrderedType with type t = utag

  val pp_utag : Format.formatter -> utag -> unit

  val to_int : utag -> int
  val all : Set.Make(Ord).t
  val within_ : utag -> utag -> t code -> bool code

  type _ tag
  val inj : _ tag -> utag
  val test_tag : complete:bool -> 'a tag list -> t code -> ('a code option -> 'b code) -> 'b code
end

(** An interface to parsers.  The {!Asp.Staged} and {!Asp.Unstaged} modules
    provide implementations *)
module type PARSE =
sig
  type 'a t
  (** The type of parsers *)

  type 'a v
  (** The type of values that parsers produce *)

  type 'a tag
  (** The type of tags that appear in a parser's input *)

  val eps : 'a v -> 'a t
  (** [eps v] succeeds without consuming input and returns [v] *)

  val ( >>> ) : 'a t -> 'b t -> ('a * 'b) t
  (** [p >>> q] parses successive prefixes of the input using [p] and then [q]
      and returns a pair of the result of the two parses *)

  val tok : 'a tag -> 'a t
  (** [tok t] parses a token whose tag is [t], or fails *)

  val bot : 'a t
  (** [bot] fails without consuming input *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** [p <|> q] parses using either [p] or [q], which must be disjoint
     --- that is, they must not both accept the empty string, and
     their first sets must not overlap *)

  val any : 'a t list -> 'a t
  (** [any [p1; p2; ... pn]] is [p1 <|> p2 <|> ... <|> pn]  *)

  val ( $ ) : 'a t -> ('a v -> 'b v) -> 'b t
  (** If [p] parses the input and returns [v] then
      [p $ f] parses the input and returns [f v] *)

  val fix : ('b t -> 'b t) -> 'b t
  (** [fix (fun x -> e)] creates a recursive parser [e].  In the body [e]
      the parser can be accessed using the bound variable [x]. *)

  val star : 'a t -> 'a list t
  (** [star p] repeatedly parses successive prefixes of the input using [p],
      and returns a list of the zero or more results *)

  val maybe : 'a t -> 'a option t
  (** [maybe p] either parses using [p], returning [Some v] when [p] returns [v],
      or fails and returns [None] *)

  type assoc = Left | Right
  val infixr : ('a v -> 'b v -> 'b v -> 'b v) -> 'b t -> 'a t -> 'b t
  val infixl : ('a v -> 'b v -> 'b v -> 'b v) -> 'b t -> 'a t -> 'b t
  val mkInfix : assoc -> ('a v -> 'b v -> 'b v -> 'b v) -> 'b t -> 'a t -> 'b t
  val infix : ('a v -> 'b v -> 'b v -> 'b v) -> 'b t -> (assoc * 'a t) list -> 'b t
end

(** Basic components used to generate stream-like code. *)
module type STREAMCODE =
sig
  type 'a stream   (** The stream type *)
  type element     (** The type of values in the stream *)
  type context     (** Additional information used during code generation *)
  type 'a return   (** Return type of generated functions *)

  (** Plumbing for generating function definitions and calls *)

  val init : (context -> 'a return code) -> 'a stream code
  (** Build the top-level function from streams to values *)

  type 'a mkcall = { mkcall : 'b. context -> (context -> 'a code -> 'b code) -> 'b code }
  val genfun : Genletrec.locus_t -> (context -> 'a mkcall -> 'a return code) -> unit
  (** Build a recursive definition from a function that generates the body:

          genfun locus (fun context self -> e)

      where [e] builds the body with context [context], calling [self] to generate
      recursive calls. *)

  val return : context -> 'a code -> 'a return code
  (** Incorporate context information to build the return expression for a function *)

  (** Code for generating stream operations *)

  val junk : context -> (context -> 'a code) -> 'a code
  (** build the code:

          junk stream; e
   *)

  val peek : context -> (context -> element code option -> 'a code) -> 'a code
  (** build the code:

          match peek stream with
           | None -> e1
           | Some x -> e2[x]
   *)
end
