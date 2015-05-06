cobolg
======

COBOL grammar.

| Master                                              | Develop                                                |
| --------------------------------------------------- | ------------------------------------------------------ |
| [![Master status][master status]][master travis-ci] | [![Develop status][develop status]][develop travis-ci] |


Notes about cobolg parser
-------------------------

COBOL originally had to be written in a fixed width form, bearing 72 columns,
comprising fixed width zones (or areas), in order: sequence number area (6
chars), indicator area (1 char), area A (4 chars), area B (61 chars).
Everything from 73rd column and beyond (until end-of-line) is ignored.

Here's an example of fixed format COBOL source (here in a traditional 80
characters form):

```cobol
	0        1         2         3         4         5         6         7         8
	12345678901234567890123456789012345678901234567890123456789012345678901234567890
	--------------------------------------------------------------------------------
	000100 ID DIVISION.                                                     IGNORED-
	000200 PROGRAM-ID. PROGRAMNAME.                                         IGNORED-
	000300 AUTHOR. RSLEMOS.                                                 IGNORED-
	000400******************************************************************IGNORED-
	000500 ENVIRONMENT DIVISION.                                            IGNORED-
	000600 CONFIGURATION SECTION.                                           IGNORED-
	000700 SPECIAL-NAMES.                                                   IGNORED-
	000800     DECIMAL-POINT IS COMMA.                                      IGNORED-
	000900******************************************************************IGNORED-
	001000 DATA DIVISION.                                                   IGNORED-
	001100 WORKING-STORAGE SECTION.                                         IGNORED-
	001200 77  FILLER                          PIC X(50)   VALUE 'THIS STRINIGNORED-
	001300-    'G IS TOO LONG TO FIT IN THE LINE ABOVE'                     IGNORED-
	001400******************************************************************IGNORED-
	001500 PROCEDURE DIVISION.                                              IGNORED-
	001600     GOBACK.                                                      IGNORED-
```

More recently COBOL can be written in a more loose style, the free format, that
comprises just areas (also called margins) A and B.

Here's an example of free format COBOL source:

```cobol
	ID DIVISION.
	PROGRAM-ID. PROGRAMNAME.
	AUTHOR. RSLEMOS.
	*****************************************************************
	ENVIRONMENT DIVISION.
	CONFIGURATION SECTION.
	SPECIAL-NAMES.
	    DECIMAL-POINT IS COMMA.
	*****************************************************************
	DATA DIVISION.
	WORKING-STORAGE SECTION.
	77  FILLER                          PIC X(50)   VALUE 'THIS STRING IS TOO LONG TO FIT IN THE LINE ABOVE'
	*****************************************************************
	PROCEDURE DIVISION.
	    GOBACK.
```

In cobolg the same grammar is used for both formats. Only the lexers work a bit
differently. The lexer for free format is modeless and requires less effort to
understand (supposing you know how to read ANTLR lexers).


How does lexer for fixed format work?
-------------------------------------

ANTLR lexers work by reading characters, one at a time, and grouping them in
tokens. The output is one (or more) stream of tokens. There is no room for the
concept of fixed width areas. In cobolg the fixed width areas are artificially
introduced in the source code, by stuffing single chars in specific positions. 

Here's the same example as above of fixed format COBOL source, marked with |
(pipeline) characters (the characters actually used are in the range U+EBA0 to
U+EBA3, inside unicode private use range).

```cobol
	 0          1         2         3         4         5         6         7          8
	|123456|7|89012345678901234567890123456789012345678901234567890123456789012|34567890
	|------|-|-----------------------------------------------------------------|--------
	|000100| |ID DIVISION.                                                     |IGNORED-
	|000200| |PROGRAM-ID. PROGRAMNAME.                                         |IGNORED-
	|000300| |AUTHOR. RSLEMOS.                                                 |IGNORED-
	|000400|*|*****************************************************************|IGNORED-
	|000500| |ENVIRONMENT DIVISION.                                            |IGNORED-
	|000600| |CONFIGURATION SECTION.                                           |IGNORED-
	|000700| |SPECIAL-NAMES.                                                   |IGNORED-
	|000800| |    DECIMAL-POINT IS COMMA.                                      |IGNORED-
	|000900|*|*****************************************************************|IGNORED-
	|001000| |DATA DIVISION.                                                   |IGNORED-
	|001100| |WORKING-STORAGE SECTION.                                         |IGNORED-
	|001200| |77  FILLER                          PIC X(50)   VALUE 'THIS STRIN|IGNORED-
	|001300|-|    'G IS TOO LONG TO FIT IN THE LINE ABOVE'.                    |IGNORED-
	|001400|*|*****************************************************************|IGNORED-
	|001500| |PROCEDURE DIVISION.                                              |IGNORED-
	|001600| |    GOBACK.                                                      |IGNORED-
	 ───┬── ┬ ────────────────────────────────┬──────────────────────────────── ────┬───
        │   └ INDICATOR                    DEFAULT                        SKIPTOEOL ┘
        └ SEQUENCE
```

Lexer modes are suffixed with `_MODE`. They are 8:

| Mode name             | Areas in effect           | Notes                                     |
| --------------------- | ------------------------- | ----------------------------------------- |
| DEFAULT_MODE          | begin of line and DEFAULT | after newline anywhere, but before U+EBA0 |
| SEQUENCE_MODE         | SEQUENCE                  |                                           |
| INDICATOR_MODE        | INDICATOR                 |                                           |
| PRE_DEFAULT_MODE      | INDICATOR                 | after ' ', but before U+EBA2              |
| PRE_COMMENT_MODE      | INDICATOR                 | after '*', but before U+EBA2              |
| COMMENT_MODE          | DEFAULT                   | if INDICATOR is '*'                       |
| PRE_CONTINUATION_MODE | INDICATOR                 | after '-', but before U+EBA2              |
| CONTINUATION_MODE     | DEFAULT                   | if INDICATOR is '-'                       |
| SKIPTOEOL_MODE        | SKIPTOEOL                 |                                           |




Besides being used in the DEFAULT area, the `DEFAULT_MODE` also starts the
line. So the first U+EBA0 switches to `SEQUENCE_MODE`. Any newline switches
back to `DEFAULT_MODE`.

The DEFAULT area can be in two other modes:
* COMMENT_MODE, if INDICATOR area happens to have an '*';
* CONTINUATION_MODE, if INDICATOR area happens to have an '-'.

Since the complete switch to DEFAULT_MODE, COMMENT_MODE and CONTINUATION_MODE
happen only at U+EBA2, the ' ', '*' and '-' characters in INDICATOR_MODE
switches to PRE_ modes:
* PRE_DEFAULT_MODE;
* PRE_COMMENT_MODE;
* PRE_CONTINUATION_MODE.


The U+EBA0-U+EBA3 characters are used to switch lexer modes. They are assigned
to the MARK channel. As every other char is directed to either the DEFAULT or
the HIDDEN channel, then the input could be reconstructed by gluing all the
tokens in these channels together.


For the above source code, produced tokens are:

| MODE BEFORE           | TYPE                            | TEXT              | CHANNEL | FREE FORMAT |
| --------------------- | ------------------------------- | ----------------- | ------- | ----------- |
|                       | SWITCH_TO_SEQUENCEAREA_MODE     | U+EBA0            |  MARK   |             |
| SEQUENCEAREA_MODE     | SEQUENCE_NUMBER                 | 000100            | HIDDEN  |             |
| SEQUENCEAREA_MODE     | SWITCH_TO_INDICATORAREA_MODE    | U+EBA1            |  MARK   |             |
| INDICATORAREA_MODE    | INDICATOR_BLANK                 | ␢                 | HIDDEN  |             |
| PRE_DEFAULT_MODE      | SWITCH_TO_DEFAULT_MODE          | U+EBA2            |  MARK   |             |
|                       | IDENTIFICATION                  | ID                |         | ✔           |
|                       | WS                              | ␢                 | HIDDEN  | ✔           |
|                       | DIVISION                        | DIVISION          |         | ✔           |
|                       | PERIOD                          | .                 |         | ✔           |
|                       | WS                              | ␢ (×53)           | HIDDEN  | ✔           |
|                       | SWITCH_TO_SKIPTOEOLAREA_MODE    | U+EBA3            |  MARK   |             |
| SKIPTOEOLAREA_MODE    | SKIP_TO_EOL                     | IGNORED+\n        | HIDDEN  | (only \n)   |
|                                                                                                     |
|                       | SWITCH_TO_SEQUENCEAREA_MODE     | U+EBA0            |  MARK   |             |
| SEQUENCEAREA_MODE     | SEQUENCE_NUMBER                 | 000200            | HIDDEN  |             |
| SEQUENCEAREA_MODE     | SWITCH_TO_INDICATORAREA_MODE    | U+EBA1            |  MARK   |             |
| INDICATORAREA_MODE    | INDICATOR_BLANK                 | ␢                 | HIDDEN  |             |
| PRE_DEFAULT_MODE      | SWITCH_TO_DEFAULT_MODE          | U+EBA2            |  MARK   |             |
|                       | PROGRAM_ID                      | PROGRAM-ID        |         | ✔           |
|                       | PERIOD                          | .                 |         | ✔           |
|                       | WS                              | ␢                 | HIDDEN  | ✔           |
|                       | ID                              | PROGRAMNAME       |         | ✔           |
|                       | PERIOD                          | .                 |         | ✔           |
|                       | WS                              | ␢ (×41)           | HIDDEN  | ✔           |
| SKIPTOEOLAREA_MODE    | SKIP_TO_EOL                     | IGNORED+\n        | HIDDEN  | (only \n)   |
|                                                                                                     |
|                       | SWITCH_TO_SEQUENCEAREA_MODE     | U+EBA0            |  MARK   |             |
| SEQUENCEAREA_MODE     | SEQUENCE_NUMBER                 | 000300            | HIDDEN  |             |
| SEQUENCEAREA_MODE     | SWITCH_TO_INDICATORAREA_MODE    | U+EBA1            |  MARK   |             |
| INDICATORAREA_MODE    | INDICATOR_BLANK                 | ␢                 | HIDDEN  |             |
| PRE_DEFAULT_MODE      | SWITCH_TO_DEFAULT_MODE          | U+EBA2            |  MARK   |             |
|                       | AUTHOR                          | AUTHOR            |         | ✔           |
|                       | PERIOD                          | .                 |         | ✔           |
|                       | WS                              | ␢                 | HIDDEN  | ✔           |
|                       | ID                              | RSLEMOS           |         | ✔           |
|                       | PERIOD                          | .                 |         | ✔           |
|                       | WS                              | ␢ (×47)           | HIDDEN  | ✔           |
| SKIPTOEOLAREA_MODE    | SKIP_TO_EOL                     | IGNORED+\n        | HIDDEN  | (only \n)   |
|                                                                                                     |
|                       | SWITCH_TO_SEQUENCEAREA_MODE     | U+EBA0            |  MARK   |             |
| SEQUENCEAREA_MODE     | SEQUENCE_NUMBER                 | 000400            | HIDDEN  |             |
| SEQUENCEAREA_MODE     | SWITCH_TO_INDICATORAREA_MODE    | U+EBA1            |  MARK   |             |
| INDICATORAREA_MODE    | INDICATOR_COMMENT               | *                 | HIDDEN  |             |
| PRE_COMMENTAREA_MODE  | SWITCH_TO_COMMENTAREA_MODE      | U+EBA2            |  MARK   |             |
| COMMENTAREA_MODE      | COMMENTAREA                     | * (×65)           | HIDDEN  | (other)     |
| SKIPTOEOLAREA_MODE    | SKIP_TO_EOL                     | IGNORED+\n        | HIDDEN  | (only \n)   |
|                                                                                                     |
|                       | SWITCH_TO_SEQUENCEAREA_MODE     | U+EBA0            |  MARK   |             |
| SEQUENCEAREA_MODE     | SEQUENCE_NUMBER                 | 000500            | HIDDEN  |             |
| SEQUENCEAREA_MODE     | SWITCH_TO_INDICATORAREA_MODE    | U+EBA1            |  MARK   |             |
| INDICATORAREA_MODE    | INDICATOR_BLANK                 | ␢                 | HIDDEN  |             |
| PRE_DEFAULT_MODE      | SWITCH_TO_DEFAULT_MODE          | U+EBA2            |  MARK   |             |
|                       | ENVIRONMENT                     | ENVIRONMENT       |         | ✔           |
|                       | WS                              | ␢                 | HIDDEN  | ✔           |
|                       | DIVISION                        | DIVISION          |         | ✔           |
|                       | PERIOD                          | .                 |         | ✔           |
|                       | WS                              | ␢ (×44)           | HIDDEN  | ✔           |
|                       | SWITCH_TO_SKIPTOEOLAREA_MODE    | U+EBA3            |  MARK   |             |
| SKIPTOEOLAREA_MODE    | SKIP_TO_EOL                     | IGNORED+\n        | HIDDEN  | (only \n)   |
| ⋮                     | ⋮                               | ⋮                 | ⋮       | ⋮           |
|                       | SWITCH_TO_SEQUENCEAREA_MODE     | U+EBA0            |  MARK   |             |
| SEQUENCEAREA_MODE     | SEQUENCE_NUMBER                 | 001200            | HIDDEN  |             |
| SEQUENCEAREA_MODE     | SWITCH_TO_INDICATORAREA_MODE    | U+EBA1            |  MARK   |             |
| INDICATORAREA_MODE    | INDICATOR_BLANK                 | ␢                 | HIDDEN  |             |
| PRE_DEFAULT_MODE      | SWITCH_TO_DEFAULT_MODE          | U+EBA2            |  MARK   |             |
|                       | LEVEL                           | 77                |         | ✔           |
|                       | WS                              | ␢ (×2)            | HIDDEN  | ✔           |
|                       | FILLER                          | FILLER            |         | ✔           |
|                       | WS                              | ␢ (×26)           | HIDDEN  | ✔           |
|                       | PIC                             | PIC               |         | ✔           |
|                       | WS                              | ␢ (×1)            | HIDDEN  | ✔           |
|                       | PICSTRING                       | X(50)             |         | ✔           |
|                       | WS                              | ␢ (×3)            | HIDDEN  | ✔           |
|                       | VALUE                           | VALUE             |         | ✔           |
|                       | WS                              | ␢ (×44)           | HIDDEN  | ✔           |
|                       | SINGLEQUOTEDSTRING_START        | 'THIS STRIN       |         | (other)     |
|                       | SWITCH_TO_SKIPTOEOLAREA_MODE    | U+EBA3            |  MARK   |             |
| SKIPTOEOLAREA_MODE    | SKIP_TO_EOL                     | IGNORED+\n        | HIDDEN  | (only \n)   |
|                                                                                                     |
|                       | SWITCH_TO_SEQUENCEAREA_MODE     | U+EBA0            |  MARK   |             |
| SEQUENCEAREA_MODE     | SEQUENCE_NUMBER                 | 001300            | HIDDEN  |             |
| SEQUENCEAREA_MODE     | SWITCH_TO_INDICATORAREA_MODE    | U+EBA1            |  MARK   |             |
| INDICATORAREA_MODE    | INDICATOR_CONTINUATION          | -                 | HIDDEN  |             |
| PRE_CONTINUATION_MODE | SWITCH_TO_CONTINUATION_MODE     | U+EBA2            |  MARK   |             |
|                       | WS                              | ␢ (×4)            | HIDDEN  | ✔           |
|                       | SINGLEQUOTEDSTRING_CONTINUATION | 'G IS T... ABOVE' |         | (other)     |
|                       | PERIOD                          | .                 |         | ✔           |
|                       | WS                              | ␢ (×20)           | HIDDEN  | ✔           |
|                       | SWITCH_TO_SKIPTOEOLAREA_MODE    | U+EBA3            |  MARK   |             |
| SKIPTOEOLAREA_MODE    | SKIP_TO_EOL                     | IGNORED+\n        | HIDDEN  | (only \n)   |
| ⋮                     | ⋮                               | ⋮                 | ⋮       | ⋮           |
 

How does the compiler statements (like COPY) work?
--------------------------------------------------

This is one is tough.

Were this grammar intended for compilation, these statements would be dealt
with like they were directed to some sort of preprocessor: for the COPY
statement, for example, it would simple paste together the COPY LIB inside the
source code (not unlike the #include directive for a C preprocessor).

However this grammar has far broader applications: for example, it could be
used to syntax highlight COBOL source code in a home page, or to do static code
analysis, or gather metrics on code quality, and so on. So the compiler
statements cannot simply vanish (with their effects applied). They must end on
the parse tree.

To accomplish this, the lexer is prepared to throw these statements into a
separate channel (the COMPILE_CHANNEL). After the main source code is parsed
(perhaps with missing tokens insertion) a second pass is made over this
channel, to collect just these statements.

For each compiler statement, the (main) tokens immediately to the left and to
the right of it are searched for. As injected missing tokens have no position,
these are considered to lie to the right of our target if the token following
it lies to the right of the compiler statement and is a PERIOD.

To get to the rule to insert the compiler statement, the preprocess goes:
1. if there is no token either to the left or to the right (that is, the
compiler statement is at the very beginning or at the very end of the source),
then attach to the root;
2. if the left neighbor is an injected missing token, then attach to its
parent;
3. if the right neighbor is an injected missing PERIOD, then attach to its
parent (rationale: one expects that a COPY compiler statement will provide the
missing PERIOD);
4. else start with either neighbor's (both will reach the same place) parent
and travel to the root looking for the first rule that properly contains the
compiler statement; if the compiler.

After the correct rule to inject the compiler statement is found, a last effort
is made to find the correct position to inject it, which comprises basically
iterating over the rule tokens looking for the one immediately to the left.
Here, again there is a special provision for injected missing tokens.

See comments spread throughout br.eti.rslemos.cobolg.Compiler class.

--------------------------------------------------------------------------------

This project is permanently under development using this [successful branching
model](http://nvie.com/posts/a-successful-git-branching-model/).

The "master" branch contains only released versions (currently none).

All development occurs in the "develop" branch.

Features are developed on "feature/\*" branches. On these branches the ["Only
the Gods" kōan](http://stevelosh.com/blog/2013/04/git-koans/#only-the-gods)
maybe regularly violated, as pushed commits can be rebased at will (these
branches should be temporary anyway).

[master status]: https://travis-ci.org/rslemos/cobolg.png?branch=master
[master travis-ci]: https://travis-ci.org/rslemos/cobolg

[develop status]: https://travis-ci.org/rslemos/cobolg.png?branch=develop
[develop travis-ci]: https://travis-ci.org/rslemos/cobolg

--------------------------------------------------------------------------------
  BEGIN COPYRIGHT NOTICE
  
  This file is part of program "cobolg"
  Copyright 2013  Rodrigo Lemos
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  
  END COPYRIGHT NOTICE

--------------------------------------------------------------------------------
