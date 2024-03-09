(** Variable Declarations for IMITATOR.

    This module provides types and functions for defining and manipulating
    variable declarations in IMITATOR.

    @author Masaki Waga
*)

(** The type of variable declarations in IMITATOR. *)
type t

(** {1:variable_construction Variable Construction} *)

(** Constructs integer variable declarations from a list of names.
    @param names A list of strings representing the names of the integer variables.
    @return A variable declaration of type [t] encompassing all given integer variables. *)
val int_variables: string list -> t

(** Constructs boolean variable declarations from a list of names.
    @param names A list of strings representing the names of the boolean variables.
    @return A variable declaration of type [t] encompassing all given boolean variables. *)
val boolean_variables: string list -> t

(** Constructs clock variable declarations from a list of names.
    @param names A list of strings representing the names of the clock variables.
    @return A variable declaration of type [t] encompassing all given clock variables. *)
val clock_variables: string list -> t

(** Constructs parameter variable declarations from a list of names.
    @param names A list of strings representing the names of the parameter variables.
    @return A variable declaration of type [t] encompassing all given parameter variables. *)
val parameter_variables: string list -> t

(** {1:printing_functions Printing Functions} *)

(** Converts a variable declaration to a string representation.
    @param var A variable declaration of type [t].
    @return A string representation of the variable declaration. *)
val to_string: t -> string

(** Generates a string representing the definitions of a list of variable declarations.
    @param vars A list of variable declarations of type [t].
    @return A string containing the definitions of the given variable declarations. *)
val definition_string: t list -> string

(** {1:variable_duplication Variable Duplication for Self-Composition} *)

(** Duplicates a variable declaration a specified number of times for self-composition.
    @param times The number of times to duplicate the variable declaration.
    @param var A variable declaration of type [t].
    @return A new variable declaration of type [t] containing the duplicated variables. *)
val duplicate_variables: int -> t -> t

(** Duplicates a list of variable declarations a specified number of times for self-composition.
    @param times The number of times to duplicate the variable declarations.
    @param vars A list of variable declarations of type [t].
    @return A new list of variable declarations of type [t] containing the duplicated variables. *)
val duplicate_variables_list: int -> t list -> t list

(** {1:misc Other functions} *)

(** A list of variable declarations always used in our construction. *)
val internal_variables: t list

(** Extracts the names of parameters from a list of variable declarations.
    @param vars A list of variable declarations of type [t].
    @return A list of strings representing the names of the parameters. *)
val parameters: t list -> string list
