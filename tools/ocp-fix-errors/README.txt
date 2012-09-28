ocp-fix-errors
==============

Automatically fix errors in Emacs *compilation* buffer.

To install:
1. Modify ocp-fix-errors.el so that ocp-fix-errors-program matches
   the name of your installed version of ocp-fix-errors (I use a script
   for debugging purpose)
2. Load emacs/ocp-fix-errors.el from your .emacs
3. Bind the function "ocp-fix-errors" on some key:

  (define-key global-map  (kbd "C-c C-d") 'ocp-fix-errors)

4. Enjoy ! When you think the error that the compiler issues should
   trigger an automatic behavior, try ocp-fix-errors. If nothing
   happens, ocp-fix-errors could not solve your problem, you should
   send us a mail explaining what was the behavior that you were
   expecting, so that we can improve ocp-fix-errors

CURRENT AUTOMATIC CORRECTIONS
=============================

1. Fix non-exhaustive pattern matchings
---------------------------------------

Currently, find the following pattern in the compilation buffer:

<LOCATION LINE>
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
<PATTERN LINES>

and insert the pretty-formatted PATTERN at the end of the LOCATION.

For now, only patterns with constructors are supported.

2. Fix simple interface mismatches
----------------------------------

When the compiler complains because a VALUE or a TYPE does not have
the same type in the interface and in the implementation, and that
value/type is only defined once in the whole interface, the
ocp-fix-errors can copy the correct type from the implementation to
the interface.

3. Fix missing fields in records
--------------------------------

If you forget some labels in a record you are creating, the compiler
will list the missing labels. ocp-fix-errors can use that message
to add the fields in the record.

4. Fix missing semi-colon between two expressions
-------------------------------------------------

If the compiler complains that you are trying to apply an argument
to an expression that is not a function, you can use ocp-fix-errors
to automatically add a semi-colon (;) between the two expressions.

5. Fix confusion between string and array indexes
-------------------------------------------------

If you used .(_) to access a string char, or .[_] to access an
array element, ocp-fix-errors can automatically switch these calls.

6. Fix unused variables and patterns
------------------------------------

When the compiler complains about a unused variables, ocp-fix-errors
will prefix ALL the unused variables with an underscore.

When the compiler complains about unused patters, ocp-fix-errors will
remove ALL these patterns (beware it will not remove the corresponding
code if there is some, so it will only work if the unused pattern is
part of an or-pattern).

7. Fix floating point expressions
---------------------------------

If the compiler complains about mixing integers and floats in the
same expression, ocp-fix-errors can switch all the terms of the
expression to float (0 -> 0., + -> +., x -> (float x), etc.).


ROADMAP:
========

TODO:
- handle parens around pattern-matching
- fix integers and integer operators, used in a floating point context

- Give advices: for example:

File "./tools/ocp-fix-errors/fixRecord.ml", line 54, characters 12-43:
Error: This expression has type
         (Buffer.t -> unit) list -> string -> Buffer.t -> unit
       but an expression was expected of type string -> Buffer.t -> unit

it is obvious that the end of the second type is the end of the second type,
so you need to provide one more argument before (actually, it happened to
me when I put a ; before the last argument).

- When a field is in the interface and not in the implementation, we can automatically
remove it from the implementation.

Command stderr:
File "./tools/ocp-build/src/buildEngineGlobals.ml", line 1, characters 0-1:
Error: The implementation ./tools/ocp-build/src/buildEngineGlobals.ml
       does not match the interface ./_obuild/ocp-build-engine/buildEngineGlobals.cmi:
       The field `print_rule' is required but not provided
[934.1] ERROR in project ocp-build-engine

Command stderr:
File "./tools/ocp-test-suite/testPackage.ml", line 201, characters 2-10:
Error: This function is applied to too many arguments;
maybe you forgot a `;'
-------------------------------------------------
Difference in one file:

ocamlc -c test_actors1.ml
File "test_actors1.ml", line 9, characters 6-75:
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = unit
           val create : 'a -> unit
           val receive : 'a -> 'b -> unit
         end
       is not included in
         sig
           type t
           type arg = Actor of t | S of string
           val create : (t -> unit) -> t
           val receive : t -> ('a * arg list -> unit) -> unit
         end
       The field `arg' is required but not provided

Compilation exited abnormally with code 2 at Tue Oct 25 15:39:27
