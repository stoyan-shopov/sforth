\ facility extension words

0 [if]

10.6.2.0135 +FIELD “plus-field” FACILITY EXT
X:structures
( n 1 n 2 “hspacesiname” –– n 3 )
Skip leading space delimiters. Parse name delimited by a space. Create a definition for
name with the execution semantics defined below. Return n 3 = n 1 + n 2 where n 1 is the
offset in the data structure before +FIELD executes, and n 2 is the size of the data to be
added to the data structure. n 1 and n 2 are in address units.
name Execution: ( addr 1 –– addr 2 )
Add n 1 to addr 1 giving addr 2 .

[then]

0 [if]

10.6.2.0763 BEGIN-STRUCTURE FACILITY EXT
X:structures
( “hspacesiname” –– struct-sys 0 )
Skip leading space delimiters. Parse name delimited by a space. Create a definition for
name with the execution semantics defined below. Return a struct-sys (zero or more
implementation dependent items) that will be used by END-STRUCTURE and an initial
offset of 0.
108 ! “ # $ % & ’ ( ) * + , - . / digits : ; < = > ? @ ALPHA [ \ ] ? _ ‘ alpha { | } ~ facility
Forth 200x / 16.1 10. FACILITY Word Set
name Execution: ( –– +n )
+n is the size in memory expressed in address units of the data structure. An ambiguous
condition exists if name is executed prior to the associated END-STRUCTURE being
executed.
See: 10.6.2.0135 +FIELD, 10.6.2.1336 END-STRUCTURE,
A.10.6.2.0763 BEGIN-STRUCTURE.

[then]

0 [if]

10.6.2.0893 CFIELD: “c-field-colon” FACILITY EXT
X:structures
( n 1 “hspacesiname” –– n 2 )
Skip leading space delimiters. Parse name delimited by a space. Offset is the first char-
acter aligned value greater than or equal to n 1 . n 2 = offset + 1 character.
Create a definition for name with the execution semantics given below.
name Execution: ( addr 1 –– addr 2 )
Add the offset calculated during the compile-time action to addr 1 giving the address
addr 2

[then]


0 [if]

10.6.2.1336 END-STRUCTURE FACILITY EXT
X:structures
( struct-sys +n –– )
Terminate definition of a structure started by BEGIN-STRUCTURE.

[then]

0 [if]

10.6.2.1518 FIELD: “field-colon” FACILITY EXT
X:structures
( n 1 “hspacesiname” –– n 2 )
Skip leading space delimiters. Parse name delimited by a space. Offset is the first cell
aligned value greater than or equal to n 1 . n 2 = offset + 1 cell.
Create a definition for name with the execution semantics given below.
name Execution: ( addr 1 –– addr 2 )
Add the offset calculated during the compile-time action to addr 1 giving the address
addr 2

[then]

.( WARNING!!! THE WORDS PROVIDED HERE ARE NOT IDENTICAL TO THE STANDARD-PROPOSED WORDS!!!)CR
.( see the test-drive section below for word usage examples)cr cr

: begin-structure
	create here 0 , 1 cells does> create @ dup , allot does> ." using structure of size " dup @ . cr ;

: field:
	create dup , 1 cells + does> @ + ;

: +field
	create over , + does> @ + ;

: end-structure
	swap ! ;

: sizeof-structure ( name -- size)
	bl word find 0= abort" data structure variable not found" >body @ ;


[defined] test-drive [if]
.( 'structure' data structure test drive)cr

begin-structure 2d-vector
	field:	2dv.x
	field:	2dv.y
	2 cells +field .data-values
end-structure

2d-vector v
10 v 2dv.x !
20 v 2dv.y !

.( the sum of fields x and y: ) v 2dv.x @ v 2dv.y @ + . cr
.( size of structure 2d-vector: )sizeof-structure v . cr

[then]

