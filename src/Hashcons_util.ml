open Containers

      
(* to allow deriving show, we must make this special redefinition *)
type +'a hash_consed = 'a Hashcons.hash_consed = private {
  hkey : int;
  tag : int;
  node : 'a 
} [@@deriving show]
