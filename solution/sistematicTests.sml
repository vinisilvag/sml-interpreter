(* Infrastructure to run the Plc interpreter*)

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
use "testCases.sml";
(*use "ParserReference.sml";*)

val goodOutput = TextIO.openAppend "./tests/Plc-Output";
val badOutput = TextIO.openAppend "./tests/Bad-Plc-Output";

val caseIdx = ref 1

fun writeResult r file = 
	let
		val res = run r
		val idx = !caseIdx
	in
		TextIO.output(file, (Int.toString idx ^ ". " ^ res ^ "\n")); caseIdx := !caseIdx + 1
	end;

(* Test interpreter *)
map (fn x => writeResult (#2(x)) goodOutput) cases;

map (fn x => writeResult (#2(x)) badOutput) bad;


TextIO.closeOut goodOutput;
TextIO.closeOut badOutput;

OS.Process.exit(OS.Process.success);
