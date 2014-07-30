(* index.sml *)

structure Setup =
struct
  val stand_alone = false
  val first_page = "80" (* initial page number for stand-alone index doc *)
  val idxfile = "root.idx" (* file produced by makeidx *)
  val latexfile = "index.tex" (* file being produced *)
end (* structure Setup *)

structure Index =
struct
  open Setup
  val itemCount = ref 0 (* number of items in the index - 
                            one per item or subitem *)
  val refCount = ref 0  (* number of page references *)

  fun warning (s:string) = 
      print ("\nWARNING: " ^ s ^ "\n");

  fun build_conversion_table (): (string * string) list =
    (* returns a conversion table obtained from the idxfile by
       repeatedly parsing out lines of the form
         \indexentry{idxkey}{pageref} *)
      let val idxStream = TextIO.openIn idxfile

	  fun isbracket #"{" = true
	    | isbracket #"}" = true
	    | isbracket _ = false

	  exception ReadIdxFile
          fun readIdxFile(entries) =
	      case TextIO.inputLine idxStream
		of NONE => rev entries
		 | SOME line =>
		   (case String.tokens isbracket line
		      of ["\\indexentry", idxkey, pageref, _] =>
			  readIdxFile((idxkey, pageref)::entries)
		       | _ => raise ReadIdxFile)

       in readIdxFile nil before TextIO.closeIn idxStream
      end

  val outStream: TextIO.outstream = TextIO.openOut latexfile
  fun output (s: string) = TextIO.output(outStream, s)

  datatype inputItem
    = p of string (* single page reference *)
    | to of string * string (* interval *)
    
  infix to

  val table = build_conversion_table()

  exception Lookup of string
  fun lookup s =
      let fun find ((s',x)::rest) = if s' = s then x else find rest
            | find nil = raise Lookup s
       in find table
      end

  type interval = {frompage: int, topage: int}

  exception StringToInt
  fun stringToInt s =
      case Int.fromString s
       of SOME n => n
        | NONE => raise StringToInt

  fun convert(item: inputItem): interval =
      (case item
	 of p s => let val n = stringToInt s in {frompage=n,topage=n} end
          | s to s' => {frompage = stringToInt s, topage = stringToInt s'})

  fun lookupItem(i:inputItem):inputItem =
      (case i
	 of p(s) => p(lookup s)
          | s to s' => lookup s to lookup s')
      handle Lookup s =>
	       (warning ("the idxkey "^s^ "is not defined by the idxfile");
                p "0")
                       
  (* compresses list of ordered intervals, by merging overlapping intervals *)
  fun compress [] = []
    | compress [x] = [x]
    | compress ({frompage=f1, topage= t1}  ::  
		(rest as {frompage = f2, topage= t2}:: rest')) =
	if t1+1 >= f2 then compress({frompage=f1,topage=t2}::rest')
	else {frompage=f1,topage=t1}::compress rest

  (* print list of intervals, the entries separated by commas *)
  fun show_interval{frompage=f,topage=t}=
      if f = t   then Int.toString f else
      if t = f+1 then concat [Int.toString f, ", " , Int.toString t]
      else concat [Int.toString f, "--", Int.toString t ]

  and print_entries ([]:interval list):unit = ()
    | print_entries [e] =
      (refCount := !refCount + 1; 
       output(show_interval e))
    | print_entries (x::xs) =
      (refCount := !refCount + 1; 
       output(show_interval x ^ ", "); 
       print_entries xs)

  fun entry(kind: string, key: string, l: inputItem list) =
      (output("\n" ^ kind ^ key ^ (if l=[] then " " else ", ")); 
       itemCount := !itemCount+1;
       if (!itemCount mod 10) = 0 then print "\n." else print ".";
       print_entries (compress (map (convert o lookupItem) l)))

  fun item (key:string) (l:inputItem list) = 
      (print (key ^ "\n"); entry ("\\item ",key,l));

  fun subitem (key:string) (l:inputItem list) =
      entry ("\\subitem ",key,l);
 
  fun indexspace() = output "\n\\indexspace"

  fun --(s:string) =
      output("\n\\indexspace\n\\parbox{65mm}{\\hfil{\\large\\bf " 
               ^s^ "}\\hfil}\n\\indexspace")

  fun preamble () =  
      if stand_alone then
        (output("\\documentstyle[a4,12pt,twoside]{article}");
         output("\n\\include{mac}");
         output("\n\\pagestyle{headings}");
         output("\n\\begin{document}");
         output("\n\\setcounter{page}{" ^ first_page ^"}"))
      else
	(output("\\addcontentsline{toc}{section}{\\protect\\numberline{}{Index}}");
         output("\n\\label{index-sec}");
	 output("\n\\begin{theindex}"))

  fun finish () =
      (output("\n\\end{theindex}");
       if stand_alone then output("\n\\end{document}\n") else output "\n";
       TextIO.closeOut outStream;
       (!itemCount, !refCount))

  fun makeIndex () =
      (preamble();

(*-----------*)
item "\\verb+()+ (0-tuple)" [p"67.1",p"67.2",p"70",p"72.1"];
item "\\verb+(   )+" [p"6.1"];
subitem "in expression" [p"10.6",p"11",p"27.1",p"52.1",p"67.1",p"69.7",p"70"];
subitem "in pattern" [p"12.2",p"31.1",p"56.1",p"67.2",p"72.1"];
subitem "in sequence" [p"10.2",p"69.3"];
subitem "in type expression" [p"13.1",p"32.1",p"72.2"];
item "\\verb+[   ]+" [p"6.1",p"67.1",p"67.2",p"70",p"72.1"];
item "\\verb+{   }+" [p"6.1"];
subitem "in atomic expression" [p"11",p"27.0",p"52.1",p"70"]; 
subitem "in pattern" [p"12.2",p"31.1",p"56.0",p"72.1"];
subitem "in record type expression" [p"13.1",p"32.1",p"72.2"];
item "\\verb+(*  *)+ (comment brackets)" [p"7.1",p"8.4"];
item "\\verb+,+ (comma)" [p"6.1",p"10.2",p"67.1",p"69.3",p"70",p"72.1"];
item "\\verb+...+ (wildcard pattern row)" [p"6.1",p"12.2",p"31.2",p"32.2",p"56.2",p"72.1"];
item "\\verb+_+ (underbar)" [];
subitem "wildcard pattern" [p"6.1",p"30.4",p"55.3",p"72.1"];
subitem "in identifier" [p"7.2"];
item "\\verb+|+" [p"6.1",p"7.3",p"70",p"71"];
item "\\verb+=+ (reserved word)" [p"6.1"];
item "\\verb+=+ (identifier and basic value)" [p"7.3",p"49.1",p"76.1",p"78.1"];
item "\\verb+=>+" [p"6.1"];
subitem "in a match rule" [p"11",p"70"];
item "\\verb+->+" [p"6.1",p"13.1",p"32.1",p"72.2"];
item "\\verb+~+" [p"6.2",p"7.3",p"49.1",p"74",p"77.1"];
item "\\verb+.+ (period)" [];
subitem "in real constants" [p"6.2"];
subitem "in long identifiers" [p"7.2"];
item "\\verb+\"+" [p"6.2"];
item "\\verb+\\+" [p"6.2",p"7.3"];
item "\\verb+!+"  [p"7.3",p"74",p"77.1"];
item "\\verb+%+"  [p"7.3"];
item "\\verb+&+"  [p"7.3"];
item "\\verb+$+"  [p"7.3"];
item "\\verb+#+"  [p"6.1",p"7.3",p"67.1",p"70"];
item "\\verb(+("  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+-+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+/+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+:+ (see also type constraint)"  [p"7.3"];
item "\\verb+::+"  [p"73.1",p"74",p"75.2",p"76.1"];
item "\\verb+:=+ (assignment)" [p"48.1",p"52.3",p"74",p"76.1"];
item "\\verb+<+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+>+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+<=+" [p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+>=+" [p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+<>+" [p"49.1",p"74",p"76.1",p"78.1"];
item "\\verb+?+"  [p"7.3"];
item "\\verb+@+"  [p"7.3",p"74",p"76.1"];
item "\\verb+'+"  [p"7.3"];
item "\\verb+^+"  [p"6.2",p"7.3",p"74",p"76.1"];
item "\\verb+*+"  [p"7.3",p"7.5",p"49.1",p"67.2",p"72.2",p"74",p"76.1",p"78.1"];
item "$\\emptymap$ (empty map)" [p"21.1"];
item "$+$ (modification)" [p"21.1",p"22.1",p"51.1"];
item "$\\oplus$" [p"22.2",p"34.3"];
item "$\\Lambda$ (in type function)" [p"21.3",p"23.2",p"29.3"];
item "$\\forall$ (in type scheme)" [p"21.3",p"23.3"];
subitem "see also generalisation" [];
item "$\\alpha$ (see type variable)" [];
item "$\\varrho$ (see record type)" [];
item "$\\tau$ (see type)" [];
item "$\\tauk$ (type vector)" [p"21.2",p"23.1",p"23.2"];
item "$\\tych$ (type scheme)" [p"21.2",p"23.3","24.2"to"24.3",p"26.2",p"30.1",p"37.1",
                               p"43.3","74"to"75.1"];
item "$\\longtych$ (see type scheme)" [];
item "$\\rightarrow$ (function type)" [p"21.2",p"27.3",p"31.3",p"32.1"];
item "$\\downarrow$ (restriction)" [p"59.1"];
item "$\\typefcn$ (see type function)" (*theta*) [];
item "$(\\theta,\\CE)$ (see type structure)" [];
item "$\\typefcnk$ (see type function)" (* Lambda alphak.tau *) [];
item "$\\sig$ (see signature)" (*Sigma*) [];
item "$\\longsig{}$ (see signature)" [];
item "$\\funsig$ (see functor signature)" (*Phi*) [];
item "$\\longfunsig{}$ (see functor signature)" [];
item "$\\tyrea$ (type realisation)" [p"36.2"];
item "$\\strrea$ (structure realisation)" [p"36.3"];
item "$\\rea$ (realisation)" [p"36.3",p"37.2",p"37.2.5",p"46"];
item "$\\geq$ (see instance)" [];
item "$\\succ$ (see generalisation and enrichment)" [];
item "$\\ts$ (turnstile)" [p"5.1",p"26.1",p"27.3",p"39.1",p"50.2",p"59.2",p"64.1"];
item "$\\tsdyn$ (evaluation)" [p"64.1"];
item "$\\tsstat$ (elaboration)" [p"64.1"];
item "$\\ra$" [p"5.1",p"26.1",p"39.1",p"50.2",p"59.2",p"64.1"];
item "$\\langle\\ \\rangle$ (see options)" [];
item "$\\langle'\\rangle$" [p"41.1"];

--"A";
item "$a$ (see address)" (*a*) [];
item "$\\Abs$ (abstype operation)" [p"25.1",p"28.2"];
item "{\\tt abs}" [p"49.1",p"74",p"77.1"];
item "{\\tt Abs}" [p"49.2",p"77.1"];
item "$\\ABSTYPE$" [p"6.1",p"12.1",p"25.1",p"28.2",p"68.1",p"71"];
item "abstype declaration" [p"12.1",p"25.1",p"28.2",p"71"];
item "addition of numbers (\\ml{+})" [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
item "$\\Addr$ (addresses)" [p"47.2",p"48.1"];
item "address ($\\A$)" [p"47.2"];
subitem "fresh" [p"52.3"];
item "admissibility" [p"36.1",p"38.1"];
item "admit equality" [p"22.3",p"23.1",p"24.4",p"25.1",p"29.0",p"36.2",p"38.5",p"42.2",p"42.3",p"73.1",p"78.1"];
item "$\\AND$" [p"6.1","16.1"to"18.1",p"71"];
item "\\ANDALSO" [p"6.1",p"67.1",p"70"];
item "appending lists (\\verb+@+)" [p"7.3",p"74",p"76.1"];
item "$\\apexp$ (application expression)" (*appexp*) [p"69.1",p"70"];
item "application" [p"11",p"27.3"];
subitem "of basic value ($\\APPLY$)" [p"49.1",p"53.1",p"77.1"];
subitem "of (function) closure" [p"53.1"];
subitem "of value constructor" [p"52.3"];
subitem "of exception name" [p"52.3"];
subitem "of {\\tt ref}" [p"52.3"];
subitem "of {\\tt :=}" [p"52.3",p"74"];
subitem "infixed" [p"11"];
item "application of functor (see functor application)" [];
item "application of type function" [p"23.2",p"32.1"];
item "application expression" [p"69.1",p"70"];
item "applicative type variable (see type variable)" [];
item "$\\APPLY$ (see application)" [];
item "$\\AppTyVar$ (applicative type variables)" [p"8.1"];
item "$\\apptyvars$ (free applicative type variables)" [p"21.3"];
item "{\\tt arctan}" [p"49.1",p"74",p"77.1"];
item "arity" [];
subitem "of type name" [p"20.3"];
subitem "of type function" [p"23.2",p"43.2"];
item "arrow type (see function type expression)" [];
item "\\AS" [p"6.1",p"12.2",p"31.3",p"57.0",p"72.1"];
item "assignment (\\ml{:=})" [p"48.1",p"52.3",p"74",p"76.1"];
item "$\\atexp$ (atomic expression)" [p"10.1",p"11",p"26.2",p"51.2",p"67.1",p"70"];
item "atomic expression" [p"10.1",p"11",p"26.2",p"51.2",p"67.1",p"70"];
subitem "as expression" [p"11",p"27.3",p"52.3"];
item "atomic pattern" [p"10.1",p"12.2",p"30.4",p"55.3",p"67.2",p"72.1"];
subitem "as pattern" [p"12.2",p"31.3",p"56.3",p"72.1"];
item "$\\atpat$ (atomic pattern)" [p"10.1",p"12.2",p"30.4",p"55.3",p"67.2",p"72.1"];

--"B";

item "$b$ (see basic value)" [];
item "$\\B$ (see basis)" [];
item "$\\B_0$ (initial basis)" [];
subitem "static" [p"73.1"];
subitem "dynamic" [p"76.1"];
item "bare language" [p"4.1"];
item "$\\BasExc$ (basic exception names)" (*BasExName*) [p"49.2",p"76.1"];
item "basic value ($b$)" ["47.2"to"49.1","76.1"to"79.2"];
item "basis ($\\B$)" [p"4.3"];
subitem "static" [p"26.2",p"34.1",p"39.1",p"64.1",p"73.1"];
subitem "dynamic" [p"58.2",p"64.1",p"76.1"];
subitem "combined" [p"64.1"];
item "$\\Basis$ (bases)" [p"34.1",p"58.2",p"64.1"];
item "$\\BasVal$ (basic values)" ["47.2"to"49.1","76.1"to"79.2"];
item "$\\Bdyn$ (dynamic basis)" [p"64.1"];
item "{\\tt Bind} (exception)" [p"49.2",p"55.1"];
item "$\\BOOL$" [p"73.1",p"75.2"];
item "bound names" [p"34.2",p"35.2",p"36.3"];
item "$\\Bstat$ (static basis)" [p"64.1"];

--"C";

item "$\\C$ (context)" [p"21.2",p"22.1",p"26.1","26.2"to"33.2"];
item "``{\\tt Cannot open} $s$''" [p"79.1.1"];
item "\\CASE" [p"6.1",p"67.1",p"70"];
item "$\\CE$ (constructor environment)" [p"21.2","24.4"to"25.1",p"30.2",
p"44.1.5"];
item "{\\tt chr}" [p"49.1",p"74",p"77.1"];
item "{\\tt Chr}" [p"49.2",p"77.1"];
item "$\\cl{}{}$ (closure of types etc.)" ["24.2"to"24.3",p"28.2",p"30.1",p"42.2",p"43.3"];
item "\\verb+close_in+" [p"49.1",p"75.1",p"78.1",p"79.1.1"];
item "\\verb+close_out+" [p"49.1",p"75.1",p"79.1.1"];
item "$\\Closure$ (function closures)" [p"48.1"];
subitem "recursion" [p"50.1"];
item "closure rules (signatures and functors)" [p"18.2",p"41.3",p"44.2",p"45.1.5"];
item "coercion of numbers (\\ml{real})" [p"49.1",p"74",p"77.1"];
item "comments" [p"7.1",p"8.4"];
item "composition of functions (\\ml{o})" [p"74",p"76.1"];
item "$\\con$ (see value constructor)" [];
item "$\\Con$ (value constructors)" [p"7.2",p"48.1"];
item "$\\constrs$ (constructor binding)" (*conbind*) [p"10.1",p"12.1",p"30.1",p"71"];
item "$\\ConBind$ (constructor bindings)" (*ConBind*) [p"10.1",p"47.1"];
item "concatenating strings (\\verb+^+)" [p"7.3",p"74",p"76.1"];
item "$\\condesc$ (constructor description)" [p"15.1",p"16.2",p"17",p"43.35",
p"58.1"];
item "ConDesc (constructor descriptions)" [p"15.1",p"58.1"];
item "$\\ConEnv$ (constructor environments)" [p"21.2"];
item "``consing'' an element to a list (\\ml{::})" [p"73.1",p"74",p"75.2",p"76.1"];
item "consistency" [];
subitem "of type structures" [p"35.1",p"44.1.5"];
subitem "of semantic object" [p"35.1",p"36.1",p"44.1"];
item "constant (see also value constant and exception constant)" [];
subitem "special (see special constant)" [];
item "construction (see value construction and  exception construction)" [];
item "constructor binding ($\\constrs$)" [p"10.1",p"12.1",p"30.1",p"71"];
item "constructor description" [p"15.1",p"16.2",p"17",p"43.35",p"58.1"];
item "constructor environment ($\\CE$)" [p"21.2","24.4"to"25.1",p"30.2",
p"44.1.5"];
item "$\\ConsType$ (constructed types)" [p"21.2"];
item "contents of (see dereferencing)" [];
item "context ($\\C$)" [p"21.2",p"22.1",p"26.1","26.2"to"33.2"];
item "$\\Context$ (contexts)" [p"21.2"];
item "control character" [p"6.2"];
item "Core Language" [p"4.1"];
subitem "syntax" [p"6.1"];
subitem "static semantics" [p"20.1"];
subitem "dynamic semantics" [p"47.1"];
item "Core Language Programs" [p"65.4"];
item "{\\tt cos}" [p"49.1",p"74",p"77.1"];
item "cover" [p"38.1"];
item "cycle-freedom" [p"35.3",p"36.1"];

--"D";

item "\\DATATYPE" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"58.1",p"68.1",p"71"];
item "datatype binding" [p"10.1",p"12.1",p"30.1",p"71"];
item "datatype declaration" [p"12.1",p"28.2",p"71"];
item "datatype description" [p"15.1",p"17",p"43.3"];
item "datatype specification" [p"17",p"42.2",p"58.1"];
item "$\\datbind$ (datatype binding)" [p"10.1",p"12.1",p"30.1",p"71"];
item "DatBind (datatype bindings)" [p"10.1",p"47.1"];
item "$\\datdesc$ (datatype description)" [p"15.1",p"17",p"43.3"];
item "DatDesc (datatype descriptions)" [p"15.1",p"58.1"];
item "$\\dec$ (declaration)" [p"10.1",p"12.1",p"28.2",p"54.3",p"68.1",p"71"];
item "Dec (declarations)" [p"10.1"];
item "declaration (Core)" [p"10.1",p"12.1",p"28.2",p"54.3",p"68.1",p"71"];
subitem "as structure-level declaration" [p"16.1",p"40.2",p"60.2"];
item "dereferencing (\\ml{!})" [p"7.3",p"74",p"77.1"];
item "derived forms" [p"4.1",p"9.3",p"14.1","66.1"to"68.2"];
item "{\\tt Diff}" [p"49.2",p"78.1"];
item "digit"[];
subitem "in identifier" [p"7.2"];
subitem "in integers and reals" [p"6.2"];
item "$\\dir$ (fixity directive)" [p"9.2",p"12.1",p"14.1"];
item "directive" [p"12.1"];
(*item "disjoining bound names" [p"34.2",p"36.3"]; *)
item "{\\tt div}" [p"49.1",p"74",p"76.1",p"78.1"];
item "{\\tt Div}" [p"49.2",p"78.1"];
item "division of reals (\\ml{/})" [p"49.1",p"74",p"76.1",p"78.1"];
item "\\DO" [p"6.1",p"67.1",p"70"];
item "$\\Dom$ (domain)" [p"21.1"];
item "dynamic" [];
subitem "semantics (Core)" [p"47.1"];
subitem "semantics (Modules)" [p"58.1"];
subitem "basis (see basis)" [];

--"E";

item "$\\exval$ (exception value)" (*e*) [p"48.1"];
item "$[\\exval]$ (see packet)" [];
item "\\verb+E+\\ (exponent)" [p"6.2"];
item "$\\E$ (environment)" [];
subitem "static" [p"21.2",p"25.1",p"26.1","28.2"to"29.0"];
subitem "dynamic" [p"48.1","51.2"to"57.1",p"59.1",p"60.1"];
item "$\\EE$ (see exception constructor environment)" [];
item "elaboration" [p"4.1",p"4.1",p"5.1",p"26.1",p"39.1",p"64.1"];
item "\\ELSE" [p"6.1",p"67.1",p"70"];
item "empty" [];
subitem "declaration (Core)" [p"12.1",p"28.3",p"54.3",p"71"];
subitem "functor declaration" [p"18.1",p"45.1",p"63.1"];
subitem "functor specification" [p"18.1",p"44.2"];
subitem "signature declaration" [p"16.1",p"41.3",p"61.3"];
subitem "specification" [p"17",p"42.3",p"62.1"];
subitem "structure-level declaration" [p"16.1",p"40.2",p"60.2"];
item "$\\e$ (exception name)" (*en*)[p"47.2",p"55.2"];
item "\\END" [p"6.1","11"to"12.1","16.1"to"17","70"to"71"];
item "\\verb+end_of_stream+" [p"49.1",p"75.1",p"79.1.1"];
item "enrichment ($\\succ$)" [p"33.15",p"37.1",p"39.2",p"41.1",p"45.2"];
item "$\\excs$ (exception name set)" (*ens*)[p"48.1",p"55.2"];
item "environment (see $\\E$)"; 
item "$\\Env$ (environments)" [p"21.2",p"48.1"];
item "\\EQTYPE" [p"14.1",p"17",p"42.2",p"58.1"];
item "equality" [];
subitem "admit equality"  [p"22.3",p"23.1",p"24.4",p"25.1",p"29.0",
p"36.2",p"38.5",p"42.2",p"42.3",p"73.1",p"78.1"];

subitem "maximise equality" [p"24.4",p"28.2"];
subitem "on abstract types" ["24.4"to"25.1"];
subitem "of structures (sharing)" [p"44.1"];
subitem "of type functions (sharing)" [p"23.2","44.1"to"44.1.5"];
subitem "of type schemes" [p"23.3"];
subitem "of values" [p"22.3", p"74",p"76.1",p"78.1"];
subitem "-principal" [p"38.5",p"41.25",p"42.1",p"44.4",p"45.2"];
subitem "respect equality" [p"24.4", p"29.0", p "38.1",p"38.5"];
item "equality attribute" [];
subitem "of type name" [p"20.3",p"23.1","24.4"to"25.1",p"36.2",
p "38.5",p"42.2"];
subitem "of type variable" [p"7.4",p"20.3",p"22.3",p"23.2",p"23.3"];
item "equality type" ["22.3"to"23.1",p"74"];
item "equality type function" [p"23.2"];
item "equality type specification" [p"17",p"42.2",p"58.1"];
item "equality type variable" [p"7.4",p"20.3",p"22.3",p"23.1"];
item "escape sequence" [p"6.2"];
item "evaluation" [p"4.1",p"5.1",p"50.2",p"59.2",p"64.1"];
item "$\\exnbind$ (exception binding)" (*exbind*) [p"10.1",p"12.1",p"30.1",p"55.2",p"71"];
item "ExBind (exception bindings)" [p"10.1"];
item "\\EXCEPTION" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"54.3",p"61.5",p"71"];
item "exception binding" [p"10.1",p"12.1",p"30.1",p"55.2",p"71"];
item "exception constant ($\\exn$ or $\\longexn$)" [];
subitem "as atomic pattern" [p"12.2",p"30.4","55.3"to"56.0",p"72.1"];
item "exception construction" [];
subitem "as pattern" [p"12.2",p"31.3",p"56.3",p"72.1"];
subitem "infixed, as pattern" [p"9.2",p"12.2",p"72.1"];
item "exception constructor" [];
subitem "as atomic expression" [p"11",p"26.2",p"52.1",p"70"];
item "exception constructor environment ($\\EE$)" [];
subitem "static" [p"21.2",p"22.1",p"30.3",p"59.1"];
subitem "dynamic" [p"48.1",p"55.2",p"59.1"];
item "exception convention" [p"51.1",p"52.2",p"53.1",p"65.1"];
item "exception declaration" [p"12.1",p"28.2",p"54.3",p"71"];
item "exception description" [p"15.1",p"17",p"43.4",p"62.3"];
item "exception name ($\\e$)" (*en*) [p"47.2"];
subitem  "fresh" [p"55.2"];
item "exception name set ($\\excs$)" [p"48.1",p"55.2"];
item "exception packet (see packet)" [];
item "exception specification" [p"17",p"42.2",p"61.5"];
item "exception value ($\\exval$)" [p"48.1"];
item "$\\exn$ (see exception constant or constructor)" []; (*excon*)
item "$\\Exn$ (exception constructors)" [p"7.2"];(*ExCon*)
item "$\\ExnEnv$ (exception constructor environments)" (*ExConEnv*) [p"21.2",p"48.1"];
item "$\\exns$ (exeption constructor set)" [p"58.2",p"62.3"]; (*excons*) 
item "$\\exndesc$ (exception description)" (*exdesc*) [p"15.1",p"17",p"43.4",p"62.3"];
item "ExDesc (exception descriptions)" [p"15.1"];
item "execution" [p"4.3",p"64.1"];
item "exhaustive patterns" ["32.2"to"33.1",p"49.2"];
item "$\\EXCN$" (*the type exn*) [p"27.3",p"30.4",p"31.3",p"43.4",p"73.1",p"75.2"];
item "$\\Exc$ (exception names)" [p"47.2"]; (*ExName*)
item "$\\ExcSet$ (exception name sets)" [p"48.1"]; (*ExNameSet*)
item "$\\exp$ (expression)" [p"10.1",p"11",p"27.3",p"52.3",p"67.1",p"70"];
item "Exp (expressions)" [p"10.1"];
item "{\\tt exp} (exponential)" [p"49.1",p"74",p"77.1"];
item "{\\tt Exp}" [p"49.2",p"77.1"];
item "expansive expression" [p"23.4",p"24.3"];
item "{\\tt explode} (a string)" [p"49.1",p"74",p"78.0"];
item "expression" [p"10.1",p"11",p"27.3",p"52.3",p"67.1",p"70"];
item "expression row" [p"10.1",p"11",p"27.2",p"52.2",p"70"];
item "$\\labexps$ (expression row)" (*exprow*) [p"10.1",p"11",p"27.2",p"52.2",p"70"];
item "ExpRow (expression rows)" [p"10.1"];
item "$\\ExVal$ (exception values)" [p"48.1"];


--"F";
item "$\\F$ (functor environment)" [p"34.1",p"45.1",p"45.2",p"58.2",
p"62.5",p"63.1"];
item "$\\FAIL$ (failure in pattern matching)" [p"47.2",p"51.1",p"52.1",p"53.1",
p"54.1",p"55.3",p"56.0",p"56.3",p"57.0",p"57.1"];
item "\\FALSE" [p"73.1",p"74",p"75.2"];
item "$\\finfun{}{}$ (finite map)" [p"20.4"];
item "$\\Fin$ (finite subset)" [p"20.4"];
item "{\\tt floor}" [p"49.1",p"74",p"77.1"];
item "{\\tt Floor}" [p"49.2",p"77.1"];
item "\\FN" [p"6.1",p"11",p"13.2",p"27.3",p"53.1",p"70"];
item "formatting character" [p"6.3"];
item "\\FUN" [p"6.1",p"66.1",p"68.1",p"71"];
item "$\\funbind$ (functor binding)" [p"15.1",p"18.1",p"45.2",p"62.5",
p"66.1",p"68.2"];
item "FunBind (functor bindings)" [p"15.1"];
item "function ($\\fnexp$)" [p"11",p"27.3",p"53.1",p"70"];
item "function declaration (see $\\FUN$)" [];
item "function type ($\\rightarrow$)" [p"21.2",p"27.3",p"31.3",p"32.1"];
item "function type expression (\\verb+->+)" [p"13.1",p"32.1",p"72.2"];
item "function-value binding ($\\fvalbind$)" [p"33.1",p"66.1",p"68.1",p"71"];
item "\\FUNCTOR" [p"14.1",p"18.1",p"44.2",p"45.1",p"63.1"];
item "functor application" [p"16.1",p"39.2",p"60.1",p"68.2"];
item "functor binding" [p"15.1",p"18.1",p"45.2",p"62.5",p"66.1",p"68.2"];
item "functor closure" [p"58.2",p"60.1",p"62.5"];
item "functor declaration" [p"15.1",p"18.1",p"45.1",p"63.1"];
subitem "as top-level declaration" [p"18.1",p"46.0",p"63.2"];
item "functor description" [p"15.1",p"18.1",p"44.3"];
item "functor environment ($\\F$)" [p"34.1",p"45.1",p"45.2",p"58.2",
p"62.5",p"63.1"];
item "functor identifier ($\\funid$)" [p"14.1",p"16.1",p"18.1"];
item "functor signature ($\\funsig$)" [p"34.1",p"44.4",p"45.2",p"46"];
item "functor signature expression" [p"15.1",p"18.1",p"44.4",p"68.2"];
item "functor signature matching" [p"15.1",p"46"];
item "functor specification" [p"15.1",p"18.1",p"44.2"];
item "$\\FunctorClosure$ (functor closures)" [p"58.2"];
item "$\\fundec$ (functor declaration)" [p"15.1",p"18.1",p"45.1",p"63.1"];
item "FunDec (functor declarations)" [p"15.1"];
item "$\\fundesc$ (functor description)" [p"15.1",p"18.1",p"44.3"];
item "FunDesc (functor descriptions)" [p"15.1"];
item "$\\FunEnv$ (functor environments)" [p"34.1",p"58.2"];
item "$\\funid$ (functor identifier)" [p"14.1",p"16.1",p"18.1"];
item "$\\FunId$ (functor identifiers)" [p"14.1"];
item "$\\funsigexp$ (functor signature expression)" [p"15.1",p"18.1",p"44.4",p"68.2"];
item "FunSigExp (functor signature expressions)" [p"15.1"];
item "$\\funspec$ (functor specification)" [p"15.1",p"18.1",p"44.2"];
item "FunSpec (functor specifications)" [p"15.1"];
item "$\\FunType$ (function types)" [p"21.2"];
item "$\\fvalbind$ (function-value binding)" [p"66.1",p"68.1",p"71"];
subitem "exhaustive" [p"33.1"];

indexspace();
indexspace();
--"G";
item "$\\G$ (signature environment)" [p"34.1",p"41.3",p"58.2",p"61.3",p"61.4"];
item "generalisation ($\\succ$)" [p"23.3",p"26.2",p"30.4",p"31.3",
p"33.15",p"37.1"];
item "generative signature expression" [p"16.1",p"41.2",p"61.2"];
item "generative structure expression" [p"16.1",p"39.2",p"60.1"];
item "grammar" [p"4.1"];
subitem "for the Core" [p"9.3",p"69.1"];
subitem "for Modules" [p"14.2"];

--"H";

item "\\HANDLE" [p"6.1",p"11",p"27.3",p"53.1",p"70"];

--"I";

item "$\\I$ (interface)" [p"58.2",p"61.2","61.5"to"62.1"];
item "$\\IB$ (interface basis)" [p"58.2",p"59.1","61.2"to"62.1",p"62.4"];
item "identifier ($\\id$)" [p"7.2",p"14.1"];
subitem "alphanumeric" [p"7.2"];
subitem "long" [p"7.2",p"65.5"];
subitem "qualified" [p"7.2"];
subitem "symbolic" [p"7.2"];
item "$\\IE$ (interface environment)" [p"58.2",p"62.4"];
item "\\IF" [p"6.1",p"67.1",p"70"];
item "imperative attribute" [p"20.3",p"23.2",p"23.3"];
item "imperative type" [p"23.2",p"30.3",p"30.35"];
item "imperative type variable (see type variable)" [];
item "implementation" [p"4.1",p"64.1"];
item "{\\tt implode} (a string list)" [p"49.1",p"74",p"78.1"];
item "$\\ImpTyVar$ (imperative type variables)" [p"7.4"];
(* from version2: 
item "$\\imptyvars$ (free imperative type variables)" [p"21.3",p"40.2"]; *)
item "$\\imptyvars$ (free imperative type variables)" [p"21.3",
      "45.3"to"46.01"]; 
item "$\\In$ (injection)" [p"22.1"];
item "\\IN" [p"6.1",p"11",p"12.1",p"16.1",p"17",p"67.1",p"70",p"71"];
item "\\INCLUDE" [p"14.1",p"17",p"42.3",p"62.1"];
item "inference" [p"5.1"];
item "inference rules" [];
subitem "static semantics (Core)" [p"26.1"];
subitem "static semantics (Modules)" [p"39.1"];
subitem "dynamic semantics (Core)" [p"50.1"];
subitem "dynamic semantics (Modules)" [p"59.2"];
item "$\\inexp$ (infix expression)" [p"69.1",p"70"];
item "InfExp (infix expressions)" [p"69.1",p"70"];
item "\\INFIX" [p"6.1",p"8.5",p"9.1",p"12.1",p"71"];
item "infix expression" [p"9.1",p"11",p"69.1",p"70"];
item "infix pattern" [p"9.1",p"12.2",p"72.1"];
item "infixed identifiers" [p"8.5",p"9.1",p"11",p"12.1",p"14.1",
 p"70",p"71",p"72.1",p"74"];
item "\\INFIXR" [p"6.1",p"8.5",p"9.1",p"12.1",p"71"];
item "initial basis" [p"5.0",p"73.1",p"76.1"]; 
item "injection (\\In)" [p"22.1"];
item "{\\tt input}" [p"49.1",p"75.1",p"79.1.1"];
item "input/output" [p"75.1",p"78.1"];
item "instance ($\\geq$)" [];
subitem "of signature" [p"36.4",p"37.2",p"38.1",p"41.2",p"42.3"];
subitem "of functor signature" [p"36.5",p"39.2"];
subitem "in matching" ["37.2"to"37.2.5"];
item "$\\INSTREAM$" [p"73.1",p"75.2",p"78.1"];
item "$\\INT$" [p"73.1",p"75.2"];
item "$\\Int$ (interfaces)" [p"58.2"];
item "$\\IntBasis$ (interface bases)" [p"58.2"];
item "integer constant" [p"6.2",p"75.2"];
item "$\\IntEnv$ (interface environments)" [p"58.2"];
item "$\\Inter$" [p"58.2",p"61.1",p"62.5"];
item "interaction" [p"4.1",p"64.1"];
item "interface ($\\I$)" [p"58.2",p"61.2","61.5"to"62.1"];
item "interface basis ($\\IB$)" [p"58.2",p"59.1","61.2"to"62.1",p"62.4"];
item "interface environment ($\\IE$)" [p"58.2",p"62.4"];
item "{\\tt Interrupt}" [p"49.2",p"64.2"];
item "{\\tt Io}" [p"49.2",p"79.1.1",p"79.1.1"];
item "irredundant patterns" ["32.2"to"33.1",p"49.2"];
item "{\\tt it}" [p"68.1"];

--"L"; 

item "L (left associative)" [p"10.4",p"69.7"];
item "$\\lab$ (label)" [p"7.2",p"8.2"];
item "$\\Lab$ (labels)" [p"7.2",p"8.2"];
item "\\LET" [p"6.1"];
subitem "expression (Core)" [p"11",p"27.1",p"52.1",p"67.1",p"70"];
subitem "expression (Modules)" [p"16.1",p"39.2",p"60.1"];
item "letter in identifer" [p"7.2"];
item "lexical analysis" [p"8.4",p"9.3"];
item "$\\LIST$" [p"73.1",p"75.2"];
item "list reversal (\\ml{rev})" [p"74",p"77.1"];
item "{\\tt ln}" [p"49.1",p"74",p"77.1"];
item "{\\tt Ln}" [p"49.2",p"77.1"];
item "\\LOCAL" [p"6.1"];
subitem "declaration (Core)" [p"12.1",p"28.3",p"54.3",p"71"];
subitem "declaration (Modules)" [p"16.1",p"40.2",p"60.2"];
subitem "specification (Modules)" [p"17",p"42.3",p"62.1"];
item "long identifiers (e.g. $\\longexn$)" [p"7.2",p"65.5"];
item "{\\tt lookahead}" [p"49.1",p"75.1",p"79.1.1"];

--"M";

item "$\\m$ (structure name)" [p"20.2",p"21.2",p"22.1",p"34.2",p"35.1",p"35.3",
p"36.3",p"37.1",p"41.2",p"44.1",p"48.2"];
subitem "fresh" [p"39.2",p"40.1"];
item "$\\M$ (structure name set)" [p"34.1",p"39.2"];
item "\\ml{map}" [p"74",p"77.1"];
item "match ($\\match$)" [p"10.1",p"11",p"28.1",p"54.1"];
subitem "irredundant" [p"32.2",p"49.2"];
subitem "exhaustive" [p"32.2",p"49.2"];
subitem "in closure" [p"48.1",p"50.1"];
item "$\\Match$" [p"10.1"];
item "{\\tt Match} (exception)" [p"49.2",p"53.1"];
item "match rule" [p"10.1",p"11",p"28.1",p"54.2"];
item "matching" [];
subitem "signatures (see signature matching)" [];
subitem "functor signatures (see functor signature matching)" [];
item "maximise equality" [p"24.4",p"28.2"];
item "$\\mem$ (memory)" [p"48.1",p"52.3",p"57.0"];
item "$\\Mem$ (memories)" [p"48.1"];
item "memory ($\\mem$)" [p"48.1",p"52.3",p"57.0"];
item "{\\tt mod}" [p"49.1",p"74",p"76.1",p"78.1"];
item "{\\tt Mod}" [p"49.2",p"78.1"];
item "modification ($+$)" [];
subitem "of finite maps" [p"21.1"];
subitem "of environments" [p"22.1",p"51.1"];
item "module" [p"15.1"];
item "Modules" [p"4.1"];
item "$\\mrule$ (match rule)" [p"10.1",p"11",p"28.1",p"54.2"];
item "Mrule (match rules)" [p"10.1"];
item "multiplication of numbers (\\ml{*})" [p"49.1",p"74",p"76.1",p"78.1"];

--"N";

item "$\\n$ (name, see structure name, type name and exception name)" [];
item "$\\N$ (name set)" [p"34.1",p"39.1"];
item "$n$-tuple" [p"67.1",p"67.2",p"70",p"72.1"];
item "name" [];
subitem "of structure ($\\m$)" [p"20.2",p"21.2",p"22.1",p"34.2",
p"35.1",p"35.3",p"36.3",p"37.1",p"39.1",p"40.1",
p"41.2",p"44.1",p"48.2"];
item "name set ($\\N$)" [p"34.1",p"39.1"];
item "$\\NamesFcn$ (free names)" (*names*) [p"34.1",p"35.2",p"39.1",p"45.2"];
item "$\\NameSets$ (name sets)" [p"34.1"];
item "Natural Semantics" [p"5.1"];
item "{\\tt Neg}" [p"49.2",p"77.1"];
item "negation of booleans (\\ml{not})" [p"74",p"77.1"];
item "negation of numbers (\\verb+~+)" [p"6.2",p"49.1",p"74",p"77.1"];
item "\\NIL" [p"67.1",p"73.1",p"74",p"75.2"];
item "non-expansive expression" [p"23.4",p"24.3"];
item "\\NONFIX" [p"6.1",p"9.1",p"12.1",p"14.1",p"71",p"74"];
item "nonfix identifiers" [p"9.1",p"12.1",p"14.1",p"71",p"74"];
item "\\ml{not}" [p"74",p"77.1"];
item "\\NUM" [p"74"];

--"O";

item "\\ml{o} (function composition)" [p"74",p"76.1"]; 
item "occurrence" [];
subitem "substructure" [p"34.2"];
item "$\\of{}{}$ (projection)" [p"22.1",p"34.2"];
item "$\\OF$" [p"6.1"];
subitem "in $\\CASE$ expression" [p"67.1",p"70"];
subitem "in constructor binding" [p"12.1"];
subitem "in exception binding" [p"12.1",p"47.1"];
subitem "in exception description" [p"17",p"58.1"];
item "\\OP" [p"6.1",p"9.1"];
subitem "on variable or constructor" [p"11",p"12.2",p"70",p"71",p"72.1"];
subitem "in constructor binding" [p"12.1",p"71"];
item "\\OPEN" [p"6.1" (*version2:,p"7.2" *)
,p"12.1",p"17",p"28.3",p"42.3",p"54.3",p"58.2",p"62.1",p"65.5",p"68.2",p"71"];
item "\\verb+open_in+" [p"49.1",p"75.1",p"78.1",p"79.1.1"];
item "\\verb+open_out+" [p"49.1",p"75.1",p"78.1",p"79.1.1"];
item "opening structures in declarations" [p"12.1",p"28.3",p"54.3",p"71"];
item "opening structures in specifications" [p"17",p"18.2",p"42.3",p"62.1"];
item "options" [p"10.2"];
subitem "first ($\\langle\\ \\rangle$)" [p"26.1",p"41.1"];
subitem "second ($\\langle\\langle\\ \\rangle\\rangle$)" [p"26.1"];
item "{\\tt ord} (of string)" [p"49.1",p"74",p"78.0"];
item "{\\tt Ord}" [p"49.2",p"78.0"];
item "\\ORELSE" [p"6.1",p"67.1",p"70"];
item "{\\tt output}" [p"49.1",p"75.1",p"79.1.1"];
item "``{\\tt Output stream is closed}'' " [p"79.1.1"];
item "$\\OUTSTREAM$" [p"73.1",p"75.2",p"78.1"];

--"P";

item "$\\p$ (see packet)" [];
item "$\\Pack$ (packets)" [p"48.1"];
item "packet ($\\p$)" [p"48.1",p"51.1",p"53.1",p"59.2","64.1"to"65.2"];
item "parsing" [p"4.3",p"64.1"];
item "$\\pat$ (pattern)" [p"10.1",p"12.2",p"31.3",p"56.2",p"67.2",p"72.1"];
item "Pat (patterns)" [p"10.1"]; 
item "$\\labpats$ (pattern row)" (*patrow*) [p"10.1",p"12.2",p"31.2",p"56.2",p"67.2",p"72.1"];
item "PatRow (pattern rows)" [p"10.1"];
item "pattern" [p"10.1",p"12.2",p"31.3",p"56.2",p"67.2",p"72.1"];
subitem "layered" [p"12.2",p"31.3",p"57.0",p"72.1"];
item "pattern matching" ["32.2"to"33.1",p"47.2",p"49.2",p"56.2"];
subitem "with $\\REF$" [p"56.3",p"57.0"];
item "pattern row" [p"10.1",p"12.2",p"31.2",p"56.2",p"67.2",p"72.1"];
item "polymorphic" [];
subitem "functions" [p"26.2",p"28.2",p"30.4"];
subitem "references" [p"23.4",p"24.3",p"28.2","45.3"to"46.01",p"74"];
subitem "exceptions" [p"23.4",p"30.1",p"43.4","45.3"to"46.01"];
(* version2: subitem "references" [p"23.4",p"24.3",p"28.2",p"40.2",p"74"];
subitem "exceptions" [p"23.4",p"30.1",p"40.2",p"43.4"]; *)
item "precedence" [p"10.3",p"69.4"];
item "principal" [];
subitem "environment" [p"33.2",p"40.2"];
subitem "equality-" [p"38.5",p"41.25",p"42.1",p"44.4",p"45.2"];
subitem "signature" [p"38.1",p"38.5",p"41.25",p"42.1",p"44.4",p"45.2"];
item "printable character" [p"6.2"];
item "{\\tt Prod}" [p"49.2",p"78.1"];
item "product type (\\verb+*+)" [p"67.2",p"72.2"];
item "program ($\\program$)" [p"4.2","64.1"to"65.2"];
item "Program (programs)" [p"64.1"];
item "projection ($\\of{}{}$)" [p"22.1",p"34.2"];

--"Q"; 


item "qualified identifier" [p"7.2"];
item "{\\tt Quot}" [p"49.2",p"78.1"];


--"R";

item "$\\r$ (record)" [p"48.1",p"52.2",p"56.0",p"56.2"];
item "R (right associative)" [p"10.4",p"69.5"];
item "\\RAISE" [p"6.1",p"11",p"27.3",p"27.4",p"51.1",p"53.1",p"53.1",p"64.1",p"70"];
item "$\\Ran$ (range)" [p"21.1"];
item "$\\REAL$" [];
subitem "the type" [p"73.1",p"75.2"];
subitem "coercion" [p"49.1",p"74",p"77.1"];
item "real constant" [p"6.2",p"75.2"];
item "realisation ($\\rea$)" [p"36.3","37.2"to"37.2.5",p"46"];
item "$\\REC$" [p"6.1",p"12.1",p"13.2",p"29.1",p"50.1",p"55.1",p"71"];
item "$\\Rec$ (recursion operator)" [p"50.1",p"53.1",p"55.1"];
item "record " [];
subitem "$\\r$" [p"48.1",p"52.2",p"56.0",p"56.2"];
subitem "as atomic expression" [p"11",p"27.0",p"52.1",p"67.1",p"70"];
subitem "as atomic pattern" [p"12.2",p"31.1",p"56.0",p"67.2",p"72.1"];
subitem "selector (\\ml{\\#}\\ {\\it lab})" [p"6.1",p"67.1",p"70"];
subitem "type expression" [p"13.1",p"32.1",p"72.2"];
subitem "type ($\\varrho$)" [p"21.2",p"27.2",p"31.1",p"31.2",p"32.15"];
item "Record (records)" [p"48.1"];
item "$\\RecType$ (record types)" [p"21.2"];
item "recursion (see $\\REC$, $\\Rec$, and $\\FUN$)" [];
item "$\\REF$" [];
subitem "the type constructor" [p"73.1",p"75.2"];
subitem "the type name" [p"23.1",p"73.1",p"74",p"75.2"];
subitem "the value constructor" [p"47.2",p"52.3",p"56.3",p"57.0",p"74",p"75.2",p"77.1"];
item "reserved words" [p"6.1",p"14.1"];
item "respect equality (see equality)"[];
item "restrictions" [];
subitem "closure rules (see these)" [];
subitem "syntactic (Core)" [p"13.2","32.2"to"33.1"];
subitem "syntactic (Modules)" [p"16.1"];
item "\\ml{rev}" [p"74",p"77.1"];

--"S";

item "$\\s$ (state)" [p"48.1",p"50.2",p"52.2",p"57.0",p"59.2","64.1"to"65.2"];
item "$\\S$ (structure)" [p"21.2",p"34.2",p"35.1",p"37.1",p"39.2",
p"41.2",p"48.2"];
item "{\\SCon} (special constants)" [p"6.4"];
item "{\\scon} (see special constant)" [];
item "scope" [];
subitem "of constructor" [p"8.3",p"22.1"];
subitem "of value variable" [p"8.3",p"22.1"];
subitem "of fixity directive" [p"9.2",p"14.1"];
subitem "of explicit type variable" ["23.10"to"23.11",p"28.3",p"29.0"];
item "$\\SE$ (structure environment)" [];
subitem "static" [p"21.2",p"22.1",p"34.2",p"37.1",p"41.1",p"43.5",p"73.1"];
subitem "dynamic" [p"48.1",p"59.1",p"61.1",p"76.1"];
item "semantic object" [p"5.1"];
subitem "simple (Static)" [p"20.1"];
subitem "simple (Dynamic)" [p"47.2"];
subitem "compound (Core, Static)" [p"20.4",p"21.2"]; 
subitem "compound (Core, Dynamic)" [p"48.1"];
subitem "compound (Modules, Static)" [p"34.1"];
subitem "compound (Modules, Dynamic)" [p"58.2"];
item "sentence" [p"5.1",p"26.1",p"39.1",p"50.2",p"59.2",p"64.1"];
item "separate compilation" [p"15.1","18.2"to"19.1",p"46"];
item "sequential" [];
subitem "expression" [p"67.1",p"70"];
subitem "declaration (Core)" [p"12.1",p"28.3",p"54.3",p"71"];
subitem "functor declarations" [p"18.1",p"45.1.5",p"63.1"];
subitem "functor specification" [p"18.1",p"44.2"];
subitem "signature declaration" [p"16.1",p"41.3",p"61.3"];
subitem "specification" [p"17",p"42.3",p"62.1"];
subitem "structure-level declaration" [p"16.1",p"40.2",p"60.2"];
item "$\\shareq$ (sharing equation)" [p"15.1",p"17",p"44.1",p"58.1"];
item "SharEq (sharing equations)" [p"15.1",p"58.1"];
item "sharing" ["18.2"to"19.1",p"40.1",p"41.2",p"43.0",p"43.2",p"44.1",p"46"];
subitem "equations" [p"15.1",p"17",p"44.1",p"58.1"];
subitem "specification" [p"17",p"42.3"];
subitem "of structures" [p"17",p"44.1"];
subitem "of types" [p"17","44.1"to"44.1.5"];
subitem "multiple" [p"17",p"44.1"];
item "\\SHARING" [p"14.1",p"17",p"42.3"];
item "side-condition" [p"50.2",p"59.2"];
item "side-effect" [p"59.2",p"65.2"];
item "\\SIG" [p"14.1",p"16.1",p"41.2",p"61.2"];
item "$\\Sig$ (signatures)" [p"34.1"];
item "$\\sigbind$ (signature binding)" [p"15.1",p"16.1",p"42.1",p"61.4"];
item "SigBind (signature bindings)" [p"15.1"];
item "$\\sigdec$ (signature declaration)" [p"15.1",p"16.1",p"41.3",p"61.3"];
item "SigDec (signature declarations)" [p"15.1"];
item "$\\SigEnv$ (signature environments)" [p"34.1",p"58.2"];
item "$\\sigexp$ (signature expression)" [p"15.1",p"16.1",p"41.2",p"61.2"];
item "SigExp (signature expressions)" [p"15.1"];
item "$\\sigid$ (signature identifier)" [p"14.1",p"16.1",p"41.2",p"61.2"];
item "$\\SigId$ (signature identifiers)" [p"14.1"];
item "signature ($\\sig$)" [p"34.1",p"35.2",p"36.35",
     p"36.4","37.2"to"37.2.5",p"38.1",p"38.5",p"41.2", p"41.25",
     p"42.1",p"44.4",p"45.2",p"46",p"59.1"];
item "\\SIGNATURE" [p"14.1",p"16.1",p"41.3",p"61.3"];
item "signature binding" [p"15.1",p"16.1",p"42.1",p"61.4"];
item "signature declaration" [p"15.1",p"16.1",p"41.3",p"61.3"];
subitem "in top-level declaration" [p"18.1",p"46.0",p"63.1"];
item "signature environment ($\\G$)" [];
subitem "static" [p"34.1",p"41.3",p"42.1",p"46.0"];
subitem "dynamic" [p"58.2",p"59.1",p"61.3",p"61.4",p"63.2"];
item "signature expression" [p"15.1",p"16.1",p"41.2",p"61.2"];
item "signature identifier" [p"14.1",p"16.1",p"41.2",p"61.2"];
item "signature instantiation (see instance)"[];
item "signature matching" ["37.2"to"37.2.5","39.2"to"40.1",p"41.1",p"45.2"];
item "{\\tt sin}" [p"49.1",p"74",p"77.1"];
item "{\\tt size} (of strings)" [p"49.1",p"74",p"77.1"];
item "$\\spec$ (specification)" [p"15.1",p"17",p"42.2",p"61.5"];
item "Spec (specifications)" [p"15.1"];
item "special constant (\\scon)" [p"6.2",p"6.4",p"20.35"];
subitem "as atomic expression" [p"11",p"26.3",p"51.15",p"70"];
subitem "in pattern" [p"12.2", p"30.5",p"55.35",p"55.36",p"72.1"];
item "special value ($\\sv$)" [p"47.2"];
item "specification" [p"15.1",p"17",p"42.2",p"61.5"];
item "{\\tt sqrt} (square root)" [p"49.1",p"74",p"77.1"];
item "{\\tt Sqrt}" [p"49.2",p"77.1"];
item "state ($\\s$)" [p"48.1",p"50.2",p"52.2",p"57.0",p"59.2","64.1"to"65.2"];
item "$\\State$" [p"48.1"];
item "state convention" [p"51.1",p"52.2"];
item "static" [];
subitem "basis" [p"4.1",p"26.2",p"34.1",p"39.1",p"64.1",p"73.1"];
subitem "semantics (Core)" [p"20.1"];
subitem "semantics (Modules)" [p"34.1"];
item "\\verb+std_in+" [p"49.1",p"75.1",p"79.1.1"];
item "\\verb+std_out+" [p"49.1",p"75.1",p"79.1.1"];
item "$\\Str$ (structures)" [p"21.2"];
item "$\\strbind$ (structure binding)" [p"15.1",p"16.1",p"41.1",p"61.1"];
item "StrBind (structure bindings)" [p"15.1"];
item "$\\strdec$ (structure-level declaration)" [p"15.1",p"16.1",p"40.2",
     p"60.2",p"65.4"];
item "StrDec (structure-level declarations)" [p"15.1"];
item "$\\strdesc$ (structure description)" [p"15.1",p"17",p"43.5",p"62.4"];
item "StrDesc (structure descriptions)" [p"15.1"];
item "stream (input/output)" [p"78.1"];
item "$\\StrEnv$ (structure environments)" [p"21.2",p"48.1"];
item "$\\strexp$ (structure expression)" [p"15.1",p"16.1",p"39.2",p"60.1",p"68.2"]; 
item "$\\StrExp$ (structure expressions)" [p"15.1"];
item "$\\strid$ (structure identifier)" [p"7.2"];
subitem "as structure expression" [p"16.1",p"39.2",p"60.1"];
item "$\\StrId$ (structure identifiers)" [p"7.2"];
item "$\\STRING$" [p"73.1",p"75.2"];
item "string constant" [p"6.2",p"75.2"];
item "$\\StrNames$ (structure names)" [p"20.2"];
item "$\\StrNamesFcn$ (free structure names)" (*strnames*) [p"34.1"];
item "$\\StrNameSets$ (structure name sets)" [p"34.1"];
item "$\\STRUCT$" [p"14.1",p"16.1",p"39.2",p"60.1",p"68.2"];
item "structure ($\\S$ or $(\\m,\\E)$)" [p"21.2",p"34.2",p"35.1",p"37.1",
p"39.2",p"41.2",p"48.2"];
item "$\\STRUCTURE$" [p"14.1",p"16.1",p"17",p"40.2",p"42.2",p"60.2",p"62.1"];
item "structure binding ($\\strbind$)" [p"15.1",p"16.1",p"41.1",p"61.1"];
item "structure declaration" [p"16.1",p"40.2",p"60.2"];
item "structure description ($\\strdesc$)" [p"15.1",p"17",p"43.5",p"62.4"];
item "structure environment ($\\SE$)" [];
subitem "static" [p"21.2",p"22.1",p"34.2",p"37.1",p"41.1",p"43.5",p"73.1"];
subitem "dynamic" [p"48.1",p"59.1",p"61.1",p"76.1"];
item "structure expression ($\\strexp$)" [p"15.1",p"16.1",p"39.2",p"60.1",p"68.2"];
item "structure identifier ($\\strid$)" [p"7.2"];
subitem "as structure expression" [p"16.1",p"39.2",p"60.1"];
item "structure-level declaration ($\\strdec$)"
 [p"15.1",p"16.1",p"40.2",p"60.2",p"65.4"];
subitem "in top-level declaration" [p"18.1",p"45.3",p"63.1",p"65.4"];
item "structure name ($\\m$, see name)" [];
item "structure name set ($\\M$)" [p"34.1",p"39.1"];
item "structure realisation ($\\strrea$)" [p"36.3"];
item "structure specification" [p"17",p"42.2",p"62.1"];
item "substructure" [p"34.2"];
subitem "proper" [p"34.2",p"35.3"];
item "subtraction of numbers (\\ml{-})" [p"49.1",p"74",p"76.1",p"78.1"];
item "{\\tt Sum}" [p"49.2",p"78.1"];
item "{\\SVal} (special values)" [p"47.2"];
item "$\\Supp$ (support)" [p"36.2",p"36.3"];
item "$\\sv$ (special value)" [p"47.2"];
item "symbol" [p"7.3"];
item "syntax" [p"6.1",p"14.1",p"47.1",p"58.1",p"69.1"];

--"T";

item "$\\t$ (type name)" [p"20.2",p"23.2","24.4"to"25.1",p"28.2",p"30.1",
p"33.2",p"34.1",p"35.1",p"36.2",p"37.1",p"43.3",p"75.2"];
item "$\\T$ (type name set)" [p"21.2",p"34.1"];
item "$\\TE$ (type environment)" [p"21.2","24.4"to"25.1",p"29.3",p"30.1",
p"37.1",p"43.3",p"59.1"];
item "\\THEN" [p"6.1",p"67.1",p"70"];
item "$\\topdec$ (top-level declaration)" [p"15.1",p"18.1",p"45.3",p"63.1",p"65.4"];
subitem "in program" ["64.1"to"65.2"];
item "TopDec (top-level declarations)" [p"15.1"];
item "top-level declaration" [p"4.3",p"15.1",p"18.1",p"45.3",p"63.1",p"65.4"];
item "$\\TRUE$" [p"73.1",p"74",p"75.2"];
item "truncation of reals (\\ml{floor})" [p"49.1",p"74",p"77.1"];
item "tuple" [p"67.1",p"67.2",p"70",p"72.1"];
item "tuple type" [p"67.2",p"72.2"];
item "$\\ty$ (type expression)" [p"10.1",p"13.1",p"32.1",p"47.1",
p"67.2",p"72.2"];
item "Ty (type expressions)" [p"10.1",p"13.1",p"47.1"];
item "$\\tycon$ (type constructor)" [p"7.2",p"12.1",p"13.1",p"17",p"21.1",
"24.4"to"25.1",p"29.3",p"30.1",p"32.1",p"35.1",
p"37.1",p"43.3",p"44.1",p"75.2"];
item "$\\TyCon$ (type constructors)" [p"7.2"];
item "$\\TyEnv$ (type environments)" [p"21.2"];
item "$\\TyNames$ (type names)" (*TyName*) [p"20.2"];
item "$\\TyNamesFcn$ (free type names)" (*tynames*) [p"21.3",p"39.1"];
item "$\\TyNameSets$ (type name sets)" (*TyNameSet*) [p"21.2"];
item "$\\typbind$ (type binding)" [p"10.1",p"12.1",p"29.3",p"47.1",p"71"];
item "TypBind (type bindings)" [p"10.1",p"47.1"];
item "$\\typdesc$ (type description)" [p"15.1",p"17",p"43.2",p"58.1"];
item "TypDesc (type descriptions)" [p"15.1",p"58.1"];
item "type ($\\tau$)" [p"21.1",p"22.3",p"23.3",p"24.2","26.2"to"28.1",
p"30.4",p"31.3",p"32.1"];
item "$\\Type$ (types)" [p"21.2"];
item "$\\TYPE$" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"44.1",p"47.1",
p"58.1",p"71"];
item "$\\scontype$ (function on special constants)" [p"20.35",p"26.3",
     p"30.5"];
item "type binding" [p"10.1",p"12.1",p"29.3",p"47.1",p"71"];
item "type constraint (\\verb+:+)" [];
subitem "in expression" [p"11",p"27.3",p"47.1",p"70"];
subitem "in pattern" [p"12.2",p"31.3",p"47.1",p"72.1"];
item "type construction" [p"13.1",p"32.1"];
item "type constructor ($\\tycon$)" [p"7.2",p"12.1",p"13.1",p"17",p"21.1",
"24.4"to"25.1",p"29.3",p"30.1",p"32.1",p"35.1",
p"37.1",p"43.3",p"44.1",p"75.2"]; 
item "type constructor name (see type name)" [];
item "type declaration" [p"12.1",p"28.2",p"47.1",p"71"];
item "type description ($\\typdesc$)" [p"15.1",p"17",p"43.2",p"58.1"];
item "type environment ($\\TE$)" [p"21.2","24.4"to"25.1",p"29.3",p"30.1",
p"37.1",p"43.3",p"59.1"];
item "type explication" [p"36.35",p"36.4",p"37.2",p"40.1",p"41.25",p"45.2"];
item "type-explicit signature (see type explication)" [];
item "type expression" [p"10.1",p"13.1",p"32.1",p"47.1",p"67.2",p"72.2"];
item "type-expression row ($\\labtys$)" [p"10.1",p"13.1",p"32.15",p"47.1",p"72.2"];
item "type function ($\\typefcn$)" [p"21.2","23.1"to"23.2","24.4"to"25.1",
p"29.3",p"35.1",p"36.2",p"37.1",p"43.2","44.1"to"44.1.5",p"75.2"];
item "type name ($\\t$)" [p"20.2",p"23.2","24.4"to"25.1",p"28.2",p"30.1",
p"33.2",p"34.1",p"35.1",p"36.2",p"37.1",p"43.3",p"75.2"];
item "type name set" [p"21.2",p"34.1"];
item "type realisation ($\\tyrea$)" [p"36.2"];
item "type scheme ($\\tych$)" [p"21.2",p"23.3","24.2"to"24.3",p"26.2",
p"30.4",p"37.1",p"43.3","74"to"75.2"];
item "type specification" [p"17",p"42.2",p"58.1"];
item "type structure $(\\theta,\\CE)$" [p"21.2","24.4"to"25.1",
p"28.2",p"29.3",p"30.1",p"32.1",p"35.1",p"37.1",p"42.2",p"43.2",
"44.1"to"44.1.5","74"to"75.1"];
item "type variable ($\\tyvar$, $\\alpha$)" [p"7.4",p"13.1",p"20.2"];
subitem "in type expression" [p"13.1",p"32.1",p"72.2"];
subitem "equality" [p"7.4",p"20.3",p"22.3",p"23.2",p"23.3"];
subitem "imperative" [p"7.4",p"20.3",p"21.4",p"23.2",p"23.3",p"24.3",
p"28.2",p"30.3",p"30.35",p"33.2","45.3"to"46.01"];
subitem "applicative" [p"8.1",p"20.3",p"21.4",p"23.2",p"23.3",p"24.3",
p"28.2",p"30.3",p"30.35"]; 
subitem "explicit" ["23.10"to"23.11",p"27.4",p"28.2",p"28.3"];
item "type vector ($\\tauk$)" [p"21.2",p"23.1",p"23.2"];
item "$\\TypeFcn$ (type functions)" [p"21.2"];
item "$\\TypeScheme$ (type schemes)" [p"21.2"];
item "$\\labtys$ (type-expression row)" (*tyrow*) [p"10.1",p"13.1",p"32.15",
p"47.1",p"72.2"];
item "TyRow (type-expression rows)" [p"10.1",p"13.1",p"47.1"];
item "$\\TyStr$ (type structures)" [p"21.2"];
item "$\\tyvar$ (see type variable)" [];
item "$\\TyVar$ (type variables)" [p"7.2",p"20.2"];
item "$\\TyVarFcn$ (free type variables)" (*tyvars*) [p"21.3"];
item "$\\tyvarseq$ (type variable sequence)" [p"10.2"]; 
item "$\\TyVarSet$" [p"21.2"];

--"U";

item "$\\U$ (explicit type variables)" [p"21.2",p"22.2",p"23.11",p"28.2",p"28.3"];
item "$\\UNIT$" [p"75.2"];
item "unguarded type variable" [p"23.11"];

--"V";

item "$\\V$ (value)" (*v*) [p"48.1","51.2"to"54.2"];
item "$\\sconval$ (function on special constants)" [p"47.2",p"51.15",p"55.35",
     p"55.36"];
item "$\\Val$ (values)" [p"48.1"];
item "$\\VAL$" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"54.3",p"61.5",p"71"];
item "$\\valbind$ (value binding)" [p"10.1",p"12.1",p"23.11",p"24.3",p"28.2",
p"29.1",p"55.1",p"71"];
subitem "simple" [p"12.1",p"29.1",p"55.1",p"71"];
subitem "recursive" [p"12.1",p"29.1",p"29.2",p"55.1",p"71"];
item "Valbind (value bindings)" [p"10.1"];
item "$\\valdesc$ (value description)" [p"15.1",p"17",p"43.1",p"62.2"];
item "ValDesc (value descriptions)" [p"15.1"];
item "value binding ($\\valbind$)" [p"10.1",p"12.1",p"23.11",p"24.3",p"28.2",
p"29.1",p"55.1",p"71"];
subitem "simple" [p"12.1",p"29.1",p"55.1",p"71"];
subitem "recursive" [p"12.1",p"29.1",p"29.2",p"55.1",p"71"];
item "value constant ($\\con$)" [];
subitem "in pattern" [p"12.2",p"30.4",p"55.3",p"72.1"];
item "value constructor ($\\con$)" [p"7.2"];
subitem "as atomic expression" [p"11",p"26.2",p"52.1",p"70"];
subitem "scope" [p"8.3",p"22.1"];
item "value construction" []; 
subitem "in pattern" [p"12.2",p"31.3",p"56.3",p"72.1"];
subitem "infixed, in pattern" [p"12.2",p"72.1"];
item "value declaration" [p"12.1",p"23.11",p"28.2",p"54.3",p"71"];
item "value description ($\\valdesc$)" [p"15.1",p"17",p"43.1",p"62.2"];
item "value variable ($\\var$)" [p"7.2"];
subitem "as atomic expression" [p"11",p"26.2",p"51.2",p"70"];
subitem "in pattern" [p"12.2",p"30.4",p"55.3",p"72.1"];
item "value specification" [p"17",p"42.2",p"61.5"];
item "$\\var$ (see value variable)" [];
item "$\\Var$ (value variables)" [p"7.2"];
item "$\\VarEnv$ (variable environments)" [p"21.2",p"48.1"];
item "variable (see value variable)" [];
item "variable environment ($\\VE$)" [];
subitem "static" [p"21.2",p"22.1","24.2"to"24.3",p"28.2",p"29.1",p"30.1",
p"30.4",p"31.2",p"31.3",p"37.1",p"43.1",p"43.3",p"59.1","74"to"75.1"];
subitem "dynamic" [p"48.1",p"50.1",p"55.1",p"55.3",p"56.2",p"56.3",
p"59.1",p"76.1"];
item "$\\vars$ (set of value variables)" [p"58.2",p"62.2"];
item "$\\VE$ (see variable environment)" [];
item "via $\\rea$" [p"37.2",p"46"];
item "view of a structure" [p"41.1",p"44.1",p"58.2",p"60.1",p"61.1"];

--"W";

item "well-formed" [];
subitem "assembly" [p"35.2",p"36.1"];
subitem "functor signature" [p"35.2"];
subitem "signature" [p"35.2"];
subitem "type structure" [p"24.4"];
item "\\WHILE" [p"6.1",p"67.1",p"70"];
item "wildcard pattern (\\verb+_+)" [p"12.2",p"30.4",p"55.3",p"72.1"];
item "wildcard pattern row (\\verb+...+)" [p"6.1",p"12.2",p"31.2",
p"32.2",p"56.2",p"72.1"];
item "\\WITH" [p"6.1",p"12.1",p"71"];
item "\\WITHTYPE" [p"6.1",p"66.1",p"68.1",p"71"];

--"Y";

item "$\\Yield$"  [p"36.3"];

let val (items,pagerefs) = finish()
 in print ("done\nitems: "^ Int.toString items ^ "\npagerefs: "^ Int.toString pagerefs ^ "\n")
end) (* makeIndex *)

end; (* structure Index *)
