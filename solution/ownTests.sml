(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";

val abs = fromFile "tests/9.plc";

val tv = teval abs [];
val v = eval abs [];

(* print("Type: " ^ type2string tv ^ "  ") *)

val r = run(abs);