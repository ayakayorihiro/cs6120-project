(* This file contains all the builtin functions and variables in Brawn. *)

open Brawn_ast.Ast

let builtin_consts = [
    Number 1.;
    String "0";
    String ""
]

let builtin_functions = [
    ("atan2", 2);
    ("cos", 1);
    ("sin", 1);
    ("exp", 1);
    ("log", 1);
    ("sqrt", 1);
    ("int", 1);
    ("rand", 0);
    ("srand", 1);
    ("gsub", 3);
    ("index", 2);
    ("length", 1);
    ("match", 1);
    ("split", 3);
    ("sub", 3);
    ("substr", 3);
    ("tolower", 1);
    ("toupper", 1);
    ("system", 1)
]

let builtin_variables = [
    "ARGC";
    "ARGV";
    "ENVIRON";
    "FNR";
    "FS";
    "NF";
    "NR";
    "OFS";
    "ORS";
    "RLENGTH";
    "RS";
    "RSTART";
    "SUBSEP"
]
