(* This file contains all the builtin functions and variables in Brawn. *)

open Brawn_ast.Ast

let builtin_functions = [
    (Identifier "atan2", 2);
    (Identifier "cos", 1);
    (Identifier "sin", 1);
    (Identifier "exp", 1);
    (Identifier "log", 1);
    (Identifier "sqrt", 1);
    (Identifier "int", 1);
    (Identifier "rand", 0);
    (Identifier "srand", 1);
    (Identifier "gsub", 3);
    (Identifier "index", 2);
    (Identifier "length", 1);
    (Identifier "match", 1);
    (Identifier "split", 3);
    (Identifier "sub", 3);
    (Identifier "substr", 3);
    (Identifier "tolower", 1);
    (Identifier "toupper", 1);
    (Identifier "system", 1)
]

let builtin_variables = [
    Identifier "ARGC";
    Identifier "ARGV";
    Identifier "CONVFMT";
    Identifier "ENVIRON";
    Identifier "FILENAME";
    Identifier "FNR";
    Identifier "FS";
    Identifier "NF";
    Identifier "NR";
    Identifier "OFMT";
    Identifier "OFS";
    Identifier "ORS";
    Identifier "RLENGTH";
    Identifier "RS";
    Identifier "RSTART";
    Identifier "SUBSEP"
]
