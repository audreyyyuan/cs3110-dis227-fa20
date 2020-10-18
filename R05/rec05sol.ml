
(* Our favorite data structure from 2110: (hash) maps. 
 * (not actually a hashmap, just a map, pls forgive 


 * credot for this goes to Sitar Harel, spring 2018 :) *)

module type MyMap = sig

  (* [('k,'v) t] is the type of a map
   * containing bindings from keys of
   * type ['k] to values of type ['v]. 
   *)
  type ('k,'v) t
  
  (* [empty] is the map containing no 
   * bindings. *)
  val empty : ('k,'v) t
  
  (* [mem k m] is true if [k] is bound 
   * in [m] and false otherwise. *)
  val mem : 'k -> ('k,'v) t -> bool
  
  (* [find k m] is [v] iff [k] is 
   * bound to [v] in [m]. 
   * raises: [Not_found] if [k] is not 
   * bound in [m]. *)
  val find : 'k -> ('k,'v) t -> 'v
  
  (* [add k v m] is the map [m'] that
   * contains the same bindings
   * as [m], and additionally binds 
   * [k] to [v]. If [k] was
   * already bound in [m], its old 
   * binding is replaced by the new 
   * binding in [m']. *)
  val add : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
  
  (* [remove k m] is the map [m'] that 
   * contains the same bindings as 
   * [m], except that [k] is unbound 
   * in [m']. *)
  val remove : 'k -> ('k,'v) t -> ('k,'v) t

end 

(* Q1 *)

(* Alright, we have how we're supposed to represent a map. Let's write out 
   the abstraction function and representation invariant so we don't get 
   lost trying to keep track of everything. *)

(* Once you're done, try to implement map functions following the RI for both 
   representations. (forget the RI for now) *)


module ListMapDups = struct

  (* AF: 
   * RI: 
   *)
  type ('k,'v) t = ('k * 'v) list
  
  let empty = []
  
  let mem = List.mem_assoc
      
  let find = List.assoc
  
  let add k v m = (k, v)::m
    
  let remove k m = 
    if memm k m 
    then remove k (List.remove_assoc k m)
    else m

  let rep_ok m = m

end

module ListMapNoDups = struct

  (* AF: 
   * RI: 
   *)
  type ('k,'v) t = ('k * 'v) list
  
  let empty = []
  
  let mem = List.mem_assoc
      
  let find = List.assoc
  
  let add k v m = 
    let m' = if mem k then remove k m else m in 
    (k, v)::m'
    
  let remove k m = List.remove_assoc

  let rep_ok m = 
    let num_uniq_keys = List.(m |> map fst |> sort_uniq compare |> length) in 
    let num_keys = List.length m in 
    if num_keys = num_uniq_keys 
    then m 
    else failwith "RI"

end

(* Q2 *)

(* Well this is new! trying to store the data of a map in a function.... huh 
   We'll think of it this way: let [f] represent our map. if k' --> v' 
   in the map, then [f k] is v. Otherwise, it'll raise an error.
   Try to implement the FnMap module below. *)

module FnMap = struct

  (* AF: The function [f] represents a map that binds [k] to [v] 
   *   iff [f k] is [v]. If [k] is unbound in the map then [f k]
   *   raises [Not_found].
   * RI: <fill this in>
   *)
  type ('k,'v) t = 'k -> 'v
  
  let empty = fun _ -> raise Not_found
  
  let mem k m = 
    try 
      let _ = m k in true
    with 
      Not Found -> false
      
  let find k m = k |> m
  
  let add k v m = fun key -> if key = k then v else m key
    
  let remove k m = fun key -> if key = k then raise Not_found else m key

  let rep_ok m = m

end


(* Q3 *)

(* Yay, implementations are done?! Try write [rep_ok] for each implementation *)