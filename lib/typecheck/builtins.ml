(* This file contains all the builtin functions and variables in Brawn. *)

open Brawn_ast.Ast

let builtin_functions = [
    ("atan2", 2),
    ("cos", 1),
    ("sin", 1),
    ("exp", 1),
    ("log", 1),
    ("sqrt", 1),
    ("int", 1), 
    ("rand", 0),
    ("srand", 1),
    ("gsub", 3),
    ("index", 2),
    ("length", 1),
    ("match", 1),
    ("split", 3),
    ("sub", 3),
    ("substr", 3),
    ("tolower", 1),
    ("toupper", 1),
    ("system", 1)
]

let builtin_variables = [
    Identifier "ARGC",
    Identifier "ARGV",
    Identifier "CONVFMT",
    Identifier "ENVIRON",
    Identifier "FILENAME",
    Identifier "FNR",
    Identifier "FS",
    Identifier "NF",
    Identifier "NR",
    Identifier "OFMT",
    Identifier "OFS",
    Identifier "ORS",
    Identifier "RLENGTH",
    Identifier "RS",
    Identifier "RSTART",
    Identifier "SUBSEP",
]