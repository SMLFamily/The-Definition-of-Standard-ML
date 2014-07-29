infix to;

(* structure to bring code written for NJ ML into Standard ML *)

structure correction=  
struct
  val input = input (* for NJ ML:  fn (is,n)=> input is n; *)
  val output= output (* for NJ ML: fn (os,s)=> output os s; *)
  exception Hd and Tl
  fun hd[] = raise Hd
    | hd(x::_) = x
  and tl[] = raise Tl
    | tl(_::x) = x
  fun length[] = 0
    | length(_::x) = 1 + length x
  fun print s = output(std_out, s)
  fun intToString i =  
      (if i<0 then "~" else "") ^ natToString (abs i)

  and natToString n =
      let val d = n div 10 in
        if d = 0 then chr(ord"0" + n)
        else natToString(d) ^ chr(ord"0" + (n mod 10))
      end
  val makestring = intToString
end;

structure setup =
struct
  val stand_alone=false;
  val first_page = "80";
  val idxfile = "root.idx";
  val latexfile = "index.tex";
end; (*setup*)

structure io_util=
struct
  open correction;
  exception Read of string;
  fun read{is=is:instream,s=s:string}:string=
  (* returns the string that is between the beginning of is
     and the first occurrence of s. If s does not occur on
     is, Read(s) is raised *)
    if s="" then ""
    else let val sl = explode s;
             val first_s = hd sl;
             val buf = ref ([]:string list)
             (* we always have |buf|<=|s| *)
             val result = ref ([]:string list)
             (* reads up to first_s, which after the call
                will be in the buffer *)
             fun findchr () =
             if !buf = [] then
                   let val  first_in = input(is,1)
                   in  if      first_in = "" then raise Read(s)
                       else if first_in = first_s then (*found*)
                         (buf:= [first_in]; mask())
                       else (* store up first_in*)
                            (result:=first_in::(!result);
                             findchr())
                   end
             else
                   let val first_in = hd (!buf)
                   in  if      first_in = first_s then (*found*) mask()
                       else (result:=first_in::(!result);
                             buf:=tl(!buf); findchr())
                   end
             (* given that the first letters match, check
                that the remaining match *)
             and mask() =
                if length(!buf)>size(s) then
                  output (std_out, ("\n error in mask: buf= "^ (implode(!buf))
                          ^ ", looking for: " ^ s))
                else 
                 (* expand buffer to be of the same length as sl *)
                 (if length(!buf)<size s then 
                     buf := !buf @ explode(input(is,(size s - length (!buf))))
                  else (*same size*) ();
                  if length(!buf)<size s (*eof*) then raise Read  (s)
                  else if !buf = sl then () (*finished*)
                       else (*failure, store up one char*) 
                         (result:=(hd(! buf))::(!result);
                          buf:= tl(!buf); findchr())
                 )
         in findchr(); implode(rev(!result)) end;               

  exception Skip of string;

  fun skip{is=is:instream,s=s:string}=
  (* skips everything on is up to and including the first
     occurrence of s. If s does not occur on is, Skip(s) is
     raised and eof will be true*)
      (read{is=is,s=s}; ()) handle Read  s=> raise Skip  s

end; (*io_util*)      


structure table=
struct
  type conversion_table = (string*string)list
  val empty = []
  exception Insert of {idxkey:string}
  fun insert{idxkey=idxkey:string,pageref=pageref:string,
             ct=[]:conversion_table}=
      [(idxkey,pageref)]
    | insert{idxkey,pageref,ct as((x,y)::rest)}=
      if idxkey=x then raise Insert  {idxkey=idxkey}
                  else (x,y):: insert{idxkey=idxkey,pageref=pageref,ct=rest};


  exception Lookup of string
  fun lookup{idxkey=idxkey:string,ct=[]:conversion_table}=       
      raise Lookup  idxkey
    | lookup{idxkey,ct=ct as (x,y)::rest}=       
      if x=idxkey then y else lookup{idxkey=idxkey,ct=rest}
end; (*table*)

functor Index(X : sig
  exception Skip of string;
  val skip: {is:instream,s:string}->unit
  (* skips everything on is up to and including the first
     occurrence of s. If s does not occur on is, Skip(s) is
     raised and eof will be true*)

  exception Read of string;
  val read: {is:instream,s:string}->string
  (* returns the string that is between the beginning of is
     and the first occurrence of s. If s does not occur on
     is, Read(s) is raised *)

  type conversion_table;
     (* conversion from the idx keys to page references *)
  val empty: conversion_table;
  exception Insert of {idxkey:string}
  val insert: {idxkey:string,pageref:string,ct:conversion_table}->
               conversion_table
     (* inserts pageref under the key idxkey in ct. If idxkey already
        is present, Insert is raised *)
  exception Lookup of string
  val lookup: {idxkey:string,ct:conversion_table}->string
     (* returns the pageref recorded in ct under the entry of
        idxkey, if such an entry is present. Otherwise, Lookup(idxkey)
        is raised *)
  val stand_alone : bool
     (* depending on whether the latex file is to be included in the
        root file or not *)

  val first_page: string
     (* string giving number of first page of index, 
        in case it is stand_alone *)

  val idxfile: string;
  val latexfile: string end
)=
struct
  open X;
  val itemCount = ref 0; (* number of items in the index - 
                            one per item or subitem *)
  val refCount = ref 0;  (* number of page references *)

  open correction;
  val  outbuf = ref "";
  fun warning(s:string)= outbuf:= !outbuf ^ ("\nWARNING: " ^ s);



  fun build_conversion_table():conversion_table =
    (* returns a conversion table obtained from the idxfile by
       repeatedly seeking out lines of the form
         \indexentry{idxkey}{pageref}
    *)
  let val idxfile=open_in idxfile;
      fun readRest(ct:conversion_table):conversion_table=
          if end_of_stream(idxfile) then ct
          else
          (skip{is=idxfile,s="\\indexentry{"};
           readRest(insert{idxkey = read{is=idxfile , s="}{"   },
                           pageref = read{is=idxfile, s= "}" },
                           ct     = ct
                          }
                    handle Insert  {idxkey}=> (warning("the idxkey " ^idxkey^  
                                                 " is defined more than once");
                                          ct)
                   )
           handle Read  s=> (warning ("eof while looking for " ^ s); ct)
          )handle Skip  _ => ct;
      val result = readRest(empty)
  in
      close_in idxfile; result
  end (*build_conversion_table*)     
  
      val latexfile:outstream = open_out latexfile;
      datatype inputItem = p of string (* single page reference *)
                         | op to of string*string (* interval *)
    
      val (buffer,buflength)=(ref"",ref 0);
      fun resetBuf() = (buffer:=""; buflength:=0);
      fun emptyBuf() = (output( latexfile, !buffer);resetBuf());
      fun addToBuf(s:string)=
        (buffer:= !buffer ^ s;
         buflength:= !buflength + size s;
         if !buflength>5000 then emptyBuf() else ()
        )
      infix to;
    

      val ct = build_conversion_table();
      type interval = {frompage: int, topage: int}
      exception StringListToInt;
      fun stringToInt s = stringListToInt(rev(explode s),1)
      and stringListToInt ([],ten_to_x) = 0
        | stringListToInt ((s::rest),ten_to_x) = 
          (ord s - ord "0")*ten_to_x + stringListToInt(rest,10*ten_to_x)

      fun convert(i:inputItem):interval=
        (case i of 
             p s=> let val n = stringToInt s in {frompage=n,topage=n} end
          | s to s' =>{frompage= stringToInt s, topage= stringToInt s'})
      fun lookupItem(i:inputItem):inputItem =
        (case i of
             p(s) => p(lookup{idxkey=s,ct=ct})
           | s to s' => lookup{idxkey=s,ct=ct}  to
                        lookup{idxkey=s',ct=ct}
       )handle Lookup  idxkey=> (warning ("the idxkey " ^ idxkey ^ 
                                          " is not defined by the idxfile");
                                 p "0")
                       
      (* compresses list of ordered intervals *)
      fun compress [] = []
        | compress [x] = [x]
        | compress ({frompage=f1, topage= t1}  ::  
                    (rest as {frompage = f2, topage= t2}:: rest')) =
            if t1+1 >= f2 then compress({frompage=f1,topage=t2}::rest')
            else {frompage=f1,topage=t1}::compress rest

     (*print list of intervals, the entries separated by commas*)
       
      fun show_interval{frompage=f,topage=t}=
          if f = t   then makestring f else
          if t = f+1 then implode[makestring f, ", " , makestring t]
          else            implode[makestring f, "--", makestring t ]
      and print_entries ([]:interval list):unit = ()
        | print_entries [e]= (refCount:= !refCount +1; 
                              addToBuf (show_interval e))
        | print_entries (hd::tl) = (refCount:= !refCount +1; 
                                    addToBuf(show_interval hd ^ ", "); 
                                    print_entries tl);


      fun entry(kind:string,key:string,l:inputItem list)=
       (addToBuf("\n" ^ kind ^ key ^ (if l=[] then " " else ", ")); 
        itemCount:= !itemCount+1;
        if (!itemCount mod 10) = 0 then output(std_out,"\n.")else 
                                        output(std_out,".");
        print_entries (compress (map (convert o lookupItem) l))); 

   
      fun item(key:string)(l:inputItem list)= 
       (print (key ^ "\n"); entry ("\\item ",key,l));
      fun subitem(key:string)(l:inputItem list)= entry ("\\subitem ",key,l);
 
      fun indexspace() = addToBuf "\n\\indexspace"

      fun --(s:string)=
      addToBuf("\n\\indexspace\n\\parbox{65mm}{\\hfil{\\large\\bf " 
               ^s^ "}\\hfil}\n\\indexspace");
  val _ =  
      if stand_alone then
        (addToBuf("\\documentstyle[a4,12pt,twoside]{article}");
         addToBuf("\n\\include{mac}");
         addToBuf("\n\\pagestyle{headings}");
         addToBuf("\n\\begin{document}");
         addToBuf("\n\\setcounter{page}{" ^ first_page ^"}")
        )
      else ((addToBuf("\\addcontentsline{toc}{section}{\\protect\\numberline{}{Index}}");
            addToBuf("\n\\label{index-sec}"));
      addToBuf("\n\\begin{theindex}"));



      fun terminate() = ((addToBuf("\n\\end{theindex}");
      if stand_alone then addToBuf("\n\\end{document}") else ();
      emptyBuf();
      close_out latexfile; output(std_out, !outbuf);
      {items = !itemCount, pagerefs= !refCount}))
  
  
end; (*Index*)

structure Run= Index(struct open setup io_util table end);
open Run;



infix to;
(*-----------*)
val it= item "\\verb+()+ (0-tuple)" [p"67.1",p"67.2",p"70",p"72.1"];
val it= item "\\verb+(   )+" [p"6.1"];
val it= subitem "in expression" [p"10.6",p"11",p"27.1",p"52.1",p"67.1",p"69.7",p"70"];
val it= subitem "in pattern" [p"12.2",p"31.1",p"56.1",p"67.2",p"72.1"];
val it= subitem "in sequence" [p"10.2",p"69.3"];
val it= subitem "in type expression" [p"13.1",p"32.1",p"72.2"];
val it= item "\\verb+[   ]+" [p"6.1",p"67.1",p"67.2",p"70",p"72.1"];
val it= item "\\verb+{   }+" [p"6.1"];
val it= subitem "in atomic expression" [p"11",p"27.0",p"52.1",p"70"]; 
val it= subitem "in pattern" [p"12.2",p"31.1",p"56.0",p"72.1"];
val it= subitem "in record type expression" [p"13.1",p"32.1",p"72.2"];
val it= item "\\verb+(*  *)+ (comment brackets)" [p"7.1",p"8.4"];
val it= item "\\verb+,+ (comma)" [p"6.1",p"10.2",p"67.1",p"69.3",p"70",p"72.1"];
val it= item "\\verb+...+ (wildcard pattern row)" [p"6.1",p"12.2",p"31.2",p"32.2",p"56.2",p"72.1"];
val it= item "\\verb+_+ (underbar)" [];
val it= subitem "wildcard pattern" [p"6.1",p"30.4",p"55.3",p"72.1"];
val it= subitem "in identifier" [p"7.2"];
val it= item "\\verb+|+" [p"6.1",p"7.3",p"70",p"71"];
val it= item "\\verb+=+ (reserved word)" [p"6.1"];
val it= item "\\verb+=+ (identifier and basic value)" [p"7.3",p"49.1",p"76.1",p"78.1"];
val it= item "\\verb+=>+" [p"6.1"];
val it= subitem "in a match rule" [p"11",p"70"];
val it= item "\\verb+->+" [p"6.1",p"13.1",p"32.1",p"72.2"];
val it= item "\\verb+~+" [p"6.2",p"7.3",p"49.1",p"74",p"77.1"];
val it= item "\\verb+.+ (period)" [];
val it= subitem "in real constants" [p"6.2"];
val it= subitem "in long identifiers" [p"7.2"];
val it= item "\\verb+\"+" [p"6.2"];
val it= item "\\verb+\\+" [p"6.2",p"7.3"];
val it= item "\\verb+!+"  [p"7.3",p"74",p"77.1"];
val it= item "\\verb+%+"  [p"7.3"];
val it= item "\\verb+&+"  [p"7.3"];
val it= item "\\verb+$+"  [p"7.3"];
val it= item "\\verb+#+"  [p"6.1",p"7.3",p"67.1",p"70"];
val it= item "\\verb(+("  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+-+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+/+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+:+ (see also type constraint)"  [p"7.3"];
val it= item "\\verb+::+"  [p"73.1",p"74",p"75.2",p"76.1"];
val it= item "\\verb+:=+ (assignment)" [p"48.1",p"52.3",p"74",p"76.1"];
val it= item "\\verb+<+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+>+"  [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+<=+" [p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+>=+" [p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+<>+" [p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\verb+?+"  [p"7.3"];
val it= item "\\verb+@+"  [p"7.3",p"74",p"76.1"];
val it= item "\\verb+'+"  [p"7.3"];
val it= item "\\verb+^+"  [p"6.2",p"7.3",p"74",p"76.1"];
val it= item "\\verb+*+"  [p"7.3",p"7.5",p"49.1",p"67.2",p"72.2",p"74",p"76.1",p"78.1"];
val it= item "$\\emptymap$ (empty map)" [p"21.1"];
val it= item "$+$ (modification)" [p"21.1",p"22.1",p"51.1"];
val it= item "$\\oplus$" [p"22.2",p"34.3"];
val it= item "$\\Lambda$ (in type function)" [p"21.3",p"23.2",p"29.3"];
val it= item "$\\forall$ (in type scheme)" [p"21.3",p"23.3"];
val it= subitem "see also generalisation" [];
val it= item "$\\alpha$ (see type variable)" [];
val it= item "$\\varrho$ (see record type)" [];
val it= item "$\\tau$ (see type)" [];
val it= item "$\\tauk$ (type vector)" [p"21.2",p"23.1",p"23.2"];
val it= item "$\\tych$ (type scheme)" [p"21.2",p"23.3","24.2"to"24.3",p"26.2",p"30.1",p"37.1",
                               p"43.3","74"to"75.1"];
val it= item "$\\longtych$ (see type scheme)" [];
val it= item "$\\rightarrow$ (function type)" [p"21.2",p"27.3",p"31.3",p"32.1"];
val it= item "$\\downarrow$ (restriction)" [p"59.1"];
val it= item "$\\typefcn$ (see type function)" (*theta*) [];
val it= item "$(\\theta,\\CE)$ (see type structure)" [];
val it= item "$\\typefcnk$ (see type function)" (* Lambda alphak.tau *) [];
val it= item "$\\sig$ (see signature)" (*Sigma*) [];
val it= item "$\\longsig{}$ (see signature)" [];
val it= item "$\\funsig$ (see functor signature)" (*Phi*) [];
val it= item "$\\longfunsig{}$ (see functor signature)" [];
val it= item "$\\tyrea$ (type realisation)" [p"36.2"];
val it= item "$\\strrea$ (structure realisation)" [p"36.3"];
val it= item "$\\rea$ (realisation)" [p"36.3",p"37.2",p"37.2.5",p"46"];
val it= item "$\\geq$ (see instance)" [];
val it= item "$\\succ$ (see generalisation and enrichment)" [];
val it= item "$\\ts$ (turnstile)" [p"5.1",p"26.1",p"27.3",p"39.1",p"50.2",p"59.2",p"64.1"];
val it= item "$\\tsdyn$ (evaluation)" [p"64.1"];
val it= item "$\\tsstat$ (elaboration)" [p"64.1"];
val it= item "$\\ra$" [p"5.1",p"26.1",p"39.1",p"50.2",p"59.2",p"64.1"];
val it= item "$\\langle\\ \\rangle$ (see options)" [];
val it= item "$\\langle'\\rangle$" [p"41.1"];

val it= --"A";
val it= item "$a$ (see address)" (*a*) [];
val it= item "$\\Abs$ (abstype operation)" [p"25.1",p"28.2"];
val it= item "{\\tt abs}" [p"49.1",p"74",p"77.1"];
val it= item "{\\tt Abs}" [p"49.2",p"77.1"];
val it= item "$\\ABSTYPE$" [p"6.1",p"12.1",p"25.1",p"28.2",p"68.1",p"71"];
val it= item "abstype declaration" [p"12.1",p"25.1",p"28.2",p"71"];
val it= item "addition of numbers (\\ml{+})" [p"7.3",p"49.1",p"74",p"76.1",p"78.1"];
val it= item "$\\Addr$ (addresses)" [p"47.2",p"48.1"];
val it= item "address ($\\A$)" [p"47.2"];
val it= subitem "fresh" [p"52.3"];
val it= item "admissibility" [p"36.1",p"38.1"];
val it= item "admit equality" [p"22.3",p"23.1",p"24.4",p"25.1",p"29.0",
p"36.2",p"38.5",p"42.2",p"42.3",p"73.1",p"78.1"]
val it= item "$\\AND$" [p"6.1","16.1"to"18.1",p"71"];
val it= item "\\ANDALSO" [p"6.1",p"67.1",p"70"];
val it= item "appending lists (\\verb+@+)" [p"7.3",p"74",p"76.1"];
val it= item "$\\apexp$ (application expression)" (*appexp*) [p"69.1",p"70"];
val it= item "application" [p"11",p"27.3"];
val it= subitem "of basic value ($\\APPLY$)" [p"49.1",p"53.1",p"77.1"];
val it= subitem "of (function) closure" [p"53.1"];
val it= subitem "of value constructor" [p"52.3"];
val it= subitem "of exception name" [p"52.3"];
val it= subitem "of {\\tt ref}" [p"52.3"];
val it= subitem "of {\\tt :=}" [p"52.3",p"74"];
val it= subitem "infixed" [p"11"];
val it= item "application of functor (see functor application)" [];
val it= item "application of type function" [p"23.2",p"32.1"];
val it= item "application expression" [p"69.1",p"70"];
val it= item "applicative type variable (see type variable)" [];
val it= item "$\\APPLY$ (see application)" [];
val it= item "$\\AppTyVar$ (applicative type variables)" [p"8.1"];
val it= item "$\\apptyvars$ (free applicative type variables)" [p"21.3"];
val it= item "{\\tt arctan}" [p"49.1",p"74",p"77.1"];
val it= item "arity" [];
val it= subitem "of type name" [p"20.3"];
val it= subitem "of type function" [p"23.2",p"43.2"];
val it= item "arrow type (see function type expression)" [];
val it= item "\\AS" [p"6.1",p"12.2",p"31.3",p"57.0",p"72.1"];
val it= item "assignment (\\ml{:=})" [p"48.1",p"52.3",p"74",p"76.1"];
val it= item "$\\atexp$ (atomic expression)" [p"10.1",p"11",p"26.2",p"51.2",p"67.1",p"70"];
val it= item "atomic expression" [p"10.1",p"11",p"26.2",p"51.2",p"67.1",p"70"];
val it= subitem "as expression" [p"11",p"27.3",p"52.3"];
val it= item "atomic pattern" [p"10.1",p"12.2",p"30.4",p"55.3",p"67.2",p"72.1"];
val it= subitem "as pattern" [p"12.2",p"31.3",p"56.3",p"72.1"];
val it= item "$\\atpat$ (atomic pattern)" [p"10.1",p"12.2",p"30.4",p"55.3",p"67.2",p"72.1"];

val it= --"B";

val it= item "$b$ (see basic value)" [];
val it= item "$\\B$ (see basis)" [];
val it= item "$\\B_0$ (initial basis)" [];
val it= subitem "static" [p"73.1"];
val it= subitem "dynamic" [p"76.1"];
val it= item "bare language" [p"4.1"];
val it= item "$\\BasExc$ (basic exception names)" (*BasExName*) [p"49.2",p"76.1"];
val it= item "basic value ($b$)" ["47.2"to"49.1","76.1"to"79.2"];
val it= item "basis ($\\B$)" [p"4.3"];
val it= subitem "static" [p"26.2",p"34.1",p"39.1",p"64.1",p"73.1"];
val it= subitem "dynamic" [p"58.2",p"64.1",p"76.1"];
val it= subitem "combined" [p"64.1"];
val it= item "$\\Basis$ (bases)" [p"34.1",p"58.2",p"64.1"];
val it= item "$\\BasVal$ (basic values)" ["47.2"to"49.1","76.1"to"79.2"];
val it= item "$\\Bdyn$ (dynamic basis)" [p"64.1"];
val it= item "{\\tt Bind} (exception)" [p"49.2",p"55.1"];
val it= item "$\\BOOL$" [p"73.1",p"75.2"];
val it= item "bound names" [p"34.2",p"35.2",p"36.3"];
val it= item "$\\Bstat$ (static basis)" [p"64.1"];

val it= --"C";

val it= item "$\\C$ (context)" [p"21.2",p"22.1",p"26.1","26.2"to"33.2"];
val it= item "``{\\tt Cannot open} $s$''" [p"79.1.1"];
val it= item "\\CASE" [p"6.1",p"67.1",p"70"];
val it= item "$\\CE$ (constructor environment)" [p"21.2","24.4"to"25.1",p"30.2",
p"44.1.5"];
val it= item "{\\tt chr}" [p"49.1",p"74",p"77.1"];
val it= item "{\\tt Chr}" [p"49.2",p"77.1"];
val it= item "$\\cl{}{}$ (closure of types etc.)" ["24.2"to"24.3",p"28.2",p"30.1",p"42.2",p"43.3"];
val it= item "\\verb+close_in+" [p"49.1",p"75.1",p"78.1",p"79.1.1"];
val it= item "\\verb+close_out+" [p"49.1",p"75.1",p"79.1.1"];
val it= item "$\\Closure$ (function closures)" [p"48.1"];
val it= subitem "recursion" [p"50.1"];
val it= item "closure rules (signatures and functors)" [p"18.2",p"41.3",p"44.2",p"45.1.5"];
val it= item "coercion of numbers (\\ml{real})" [p"49.1",p"74",p"77.1"];
val it= item "comments" [p"7.1",p"8.4"];
val it= item "composition of functions (\\ml{o})" [p"74",p"76.1"];
val it= item "$\\con$ (see value constructor)" [];
val it= item "$\\Con$ (value constructors)" [p"7.2",p"48.1"];
val it= item "$\\constrs$ (constructor binding)" (*conbind*) [p"10.1",p"12.1",p"30.1",p"71"];
val it= item "$\\ConBind$ (constructor bindings)" (*ConBind*) [p"10.1",p"47.1"];
val it= item "concatenating strings (\\verb+^+)" [p"7.3",p"74",p"76.1"];
val it= item "$\\condesc$ (constructor description)" [p"15.1",p"16.2",p"17",p"43.35",
p"58.1"];
val it= item "ConDesc (constructor descriptions)" [p"15.1",p"58.1"];
val it= item "$\\ConEnv$ (constructor environments)" [p"21.2"];
val it= item "``consing'' an element to a list (\\ml{::})" [p"73.1",p"74",p"75.2",p"76.1"];
val it= item "consistency" [];
val it= subitem "of type structures" [p"35.1",p"44.1.5"];
val it= subitem "of semantic object" [p"35.1",p"36.1",p"44.1"];
val it= item "constant (see also value constant and exception constant)" [];
val it= subitem "special (see special constant)" [];
val it= item "construction (see value construction and  exception construction)" [];
val it= item "constructor binding ($\\constrs$)" [p"10.1",p"12.1",p"30.1",p"71"];
val it= item "constructor description" [p"15.1",p"16.2",p"17",p"43.35",p"58.1"];
val it= item "constructor environment ($\\CE$)" [p"21.2","24.4"to"25.1",p"30.2",
p"44.1.5"];
val it= item "$\\ConsType$ (constructed types)" [p"21.2"];
val it= item "contents of (see dereferencing)" [];
val it= item "context ($\\C$)" [p"21.2",p"22.1",p"26.1","26.2"to"33.2"];
val it= item "$\\Context$ (contexts)" [p"21.2"];
val it= item "control character" [p"6.2"];
val it= item "Core Language" [p"4.1"];
val it= subitem "syntax" [p"6.1"];
val it= subitem "static semantics" [p"20.1"];
val it= subitem "dynamic semantics" [p"47.1"];
val it= item "Core Language Programs" [p"65.4"];
val it= item "{\\tt cos}" [p"49.1",p"74",p"77.1"];
val it= item "cover" [p"38.1"];
val it= item "cycle-freedom" [p"35.3",p"36.1"];

val it= --"D";

val it= item "\\DATATYPE" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"58.1",p"68.1",p"71"];
val it= item "datatype binding" [p"10.1",p"12.1",p"30.1",p"71"];
val it= item "datatype declaration" [p"12.1",p"28.2",p"71"];
val it= item "datatype description" [p"15.1",p"17",p"43.3"];
val it= item "datatype specification" [p"17",p"42.2",p"58.1"];
val it= item "$\\datbind$ (datatype binding)" [p"10.1",p"12.1",p"30.1",p"71"];
val it= item "DatBind (datatype bindings)" [p"10.1",p"47.1"];
val it= item "$\\datdesc$ (datatype description)" [p"15.1",p"17",p"43.3"];
val it= item "DatDesc (datatype descriptions)" [p"15.1",p"58.1"];
val it= item "$\\dec$ (declaration)" [p"10.1",p"12.1",p"28.2",p"54.3",p"68.1",p"71"];
val it= item "Dec (declarations)" [p"10.1"];
val it= item "declaration (Core)" [p"10.1",p"12.1",p"28.2",p"54.3",p"68.1",p"71"];
val it= subitem "as structure-level declaration" [p"16.1",p"40.2",p"60.2"];
val it= item "dereferencing (\\ml{!})" [p"7.3",p"74",p"77.1"];
val it= item "derived forms" [p"4.1",p"9.3",p"14.1","66.1"to"68.2"];
val it= item "{\\tt Diff}" [p"49.2",p"78.1"];
val it= item "digit"[];
val it= subitem "in identifier" [p"7.2"];
val it= subitem "in integers and reals" [p"6.2"];
val it= item "$\\dir$ (fixity directive)" [p"9.2",p"12.1",p"14.1"];
val it= item "directive" [p"12.1"];
(*item "disjoining bound names" [p"34.2",p"36.3"]; *)
val it= item "{\\tt div}" [p"49.1",p"74",p"76.1",p"78.1"];
val it= item "{\\tt Div}" [p"49.2",p"78.1"];
val it= item "division of reals (\\ml{/})" [p"49.1",p"74",p"76.1",p"78.1"];
val it= item "\\DO" [p"6.1",p"67.1",p"70"];
val it= item "$\\Dom$ (domain)" [p"21.1"];
val it= item "dynamic" [];
val it= subitem "semantics (Core)" [p"47.1"];
val it= subitem "semantics (Modules)" [p"58.1"];
val it= subitem "basis (see basis)" [];

val it= --"E";

val it= item "$\\exval$ (exception value)" (*e*) [p"48.1"];
val it= item "$[\\exval]$ (see packet)" [];
val it= item "\\verb+E+\\ (exponent)" [p"6.2"];
val it= item "$\\E$ (environment)" [];
val it= subitem "static" [p"21.2",p"25.1",p"26.1","28.2"to"29.0"];
val it= subitem "dynamic" [p"48.1","51.2"to"57.1",p"59.1",p"60.1"];
val it= item "$\\EE$ (see exception constructor environment)" [];
val it= item "elaboration" [p"4.1",p"4.1",p"5.1",p"26.1",p"39.1",p"64.1"];
val it= item "\\ELSE" [p"6.1",p"67.1",p"70"];
val it= item "empty" [];
val it= subitem "declaration (Core)" [p"12.1",p"28.3",p"54.3",p"71"];
val it= subitem "functor declaration" [p"18.1",p"45.1",p"63.1"];
val it= subitem "functor specification" [p"18.1",p"44.2"];
val it= subitem "signature declaration" [p"16.1",p"41.3",p"61.3"];
val it= subitem "specification" [p"17",p"42.3",p"62.1"];
val it= subitem "structure-level declaration" [p"16.1",p"40.2",p"60.2"];
val it= item "$\\e$ (exception name)" (*en*)[p"47.2",p"55.2"];
val it= item "\\END" [p"6.1","11"to"12.1","16.1"to"17","70"to"71"];
val it= item "\\verb+end_of_stream+" [p"49.1",p"75.1",p"79.1.1"];
val it= item "enrichment ($\\succ$)" [p"33.15",p"37.1",p"39.2",p"41.1",p"45.2"];
val it= item "$\\excs$ (exception name set)" (*ens*)[p"48.1",p"55.2"];
val it= item "environment (see $\\E$)"; 
val it= item "$\\Env$ (environments)" [p"21.2",p"48.1"];
val it= item "\\EQTYPE" [p"14.1",p"17",p"42.2",p"58.1"];
val it= item "equality" [];
val it= subitem "admit equality"  [p"22.3",p"23.1",p"24.4",p"25.1",p"29.0",
p"36.2",p"38.5",p"42.2",p"42.3",p"73.1",p"78.1"]

val it= subitem "maximise equality" [p"24.4",p"28.2"];
val it= subitem "on abstract types" ["24.4"to"25.1"];
val it= subitem "of structures (sharing)" [p"44.1"];
val it= subitem "of type functions (sharing)" [p"23.2","44.1"to"44.1.5"];
val it= subitem "of type schemes" [p"23.3"];
val it= subitem "of values" [p"22.3", p"74",p"76.1",p"78.1"];
val it= subitem "-principal" [p"38.5",p"41.25",p"42.1",p"44.4",p"45.2"];
val it= subitem "respect equality" [p"24.4", p"29.0", p "38.1",p"38.5"];
val it= item "equality attribute" [];
val it= subitem "of type name" [p"20.3",p"23.1","24.4"to"25.1",p"36.2",
p "38.5",p"42.2"];
val it= subitem "of type variable" [p"7.4",p"20.3",p"22.3",p"23.2",p"23.3"];
val it= item "equality type" ["22.3"to"23.1",p"74"];
val it= item "equality type function" [p"23.2"];
val it= item "equality type specification" [p"17",p"42.2",p"58.1"];
val it= item "equality type variable" [p"7.4",p"20.3",p"22.3",p"23.1"];
val it= item "escape sequence" [p"6.2"];
val it= item "evaluation" [p"4.1",p"5.1",p"50.2",p"59.2",p"64.1"];
val it= item "$\\exnbind$ (exception binding)" (*exbind*) [p"10.1",p"12.1",p"30.1",p"55.2",p"71"];
val it= item "ExBind (exception bindings)" [p"10.1"];
val it= item "\\EXCEPTION" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"54.3",p"61.5",p"71"];
val it= item "exception binding" [p"10.1",p"12.1",p"30.1",p"55.2",p"71"];
val it= item "exception constant ($\\exn$ or $\\longexn$)" [];
val it= subitem "as atomic pattern" [p"12.2",p"30.4","55.3"to"56.0",p"72.1"];
val it= item "exception construction" [];
val it= subitem "as pattern" [p"12.2",p"31.3",p"56.3",p"72.1"];
val it= subitem "infixed, as pattern" [p"9.2",p"12.2",p"72.1"];
val it= item "exception constructor" [];
val it= subitem "as atomic expression" [p"11",p"26.2",p"52.1",p"70"];
val it= item "exception constructor environment ($\\EE$)" [];
val it= subitem "static" [p"21.2",p"22.1",p"30.3",p"59.1"];
val it= subitem "dynamic" [p"48.1",p"55.2",p"59.1"];
val it= item "exception convention" [p"51.1",p"52.2",p"53.1",p"65.1"];
val it= item "exception declaration" [p"12.1",p"28.2",p"54.3",p"71"];
val it= item "exception description" [p"15.1",p"17",p"43.4",p"62.3"];
val it= item "exception name ($\\e$)" (*en*) [p"47.2"];
val it= subitem  "fresh" [p"55.2"];
val it= item "exception name set ($\\excs$)" [p"48.1",p"55.2"];
val it= item "exception packet (see packet)" [];
val it= item "exception specification" [p"17",p"42.2",p"61.5"];
val it= item "exception value ($\\exval$)" [p"48.1"];
val it= item "$\\exn$ (see exception constant or constructor)" []; (*excon*)
val it= item "$\\Exn$ (exception constructors)" [p"7.2"];(*ExCon*)
val it= item "$\\ExnEnv$ (exception constructor environments)" (*ExConEnv*) [p"21.2",p"48.1"];
val it= item "$\\exns$ (exeption constructor set)" [p"58.2",p"62.3"]; (*excons*) 
val it= item "$\\exndesc$ (exception description)" (*exdesc*) [p"15.1",p"17",p"43.4",p"62.3"];
val it= item "ExDesc (exception descriptions)" [p"15.1"];
val it= item "execution" [p"4.3",p"64.1"];
val it= item "exhaustive patterns" ["32.2"to"33.1",p"49.2"];
val it= item "$\\EXCN$" (*the type exn*) [p"27.3",p"30.4",p"31.3",p"43.4",p"73.1",p"75.2"];
val it= item "$\\Exc$ (exception names)" [p"47.2"]; (*ExName*)
val it= item "$\\ExcSet$ (exception name sets)" [p"48.1"]; (*ExNameSet*)
val it= item "$\\exp$ (expression)" [p"10.1",p"11",p"27.3",p"52.3",p"67.1",p"70"];
val it= item "Exp (expressions)" [p"10.1"];
val it= item "{\\tt exp} (exponential)" [p"49.1",p"74",p"77.1"];
val it= item "{\\tt Exp}" [p"49.2",p"77.1"];
val it= item "expansive expression" [p"23.4",p"24.3"];
val it= item "{\\tt explode} (a string)" [p"49.1",p"74",p"78.0"];
val it= item "expression" [p"10.1",p"11",p"27.3",p"52.3",p"67.1",p"70"];
val it= item "expression row" [p"10.1",p"11",p"27.2",p"52.2",p"70"];
val it= item "$\\labexps$ (expression row)" (*exprow*) [p"10.1",p"11",p"27.2",p"52.2",p"70"];
val it= item "ExpRow (expression rows)" [p"10.1"];
val it= item "$\\ExVal$ (exception values)" [p"48.1"];


val it= --"F";
val it= item "$\\F$ (functor environment)" [p"34.1",p"45.1",p"45.2",p"58.2",
p"62.5",p"63.1"];
val it= item "$\\FAIL$ (failure in pattern matching)" [p"47.2",p"51.1",p"52.1",p"53.1",
p"54.1",p"55.3",p"56.0",p"56.3",p"57.0",p"57.1"];
val it= item "\\FALSE" [p"73.1",p"74",p"75.2"];
val it= item "$\\finfun{}{}$ (finite map)" [p"20.4"];
val it= item "$\\Fin$ (finite subset)" [p"20.4"];
val it= item "{\\tt floor}" [p"49.1",p"74",p"77.1"];
val it= item "{\\tt Floor}" [p"49.2",p"77.1"];
val it= item "\\FN" [p"6.1",p"11",p"13.2",p"27.3",p"53.1",p"70"];
val it= item "formatting character" [p"6.3"];
val it= item "\\FUN" [p"6.1",p"66.1",p"68.1",p"71"];
val it= item "$\\funbind$ (functor binding)" [p"15.1",p"18.1",p"45.2",p"62.5",
p"66.1",p"68.2"];
val it= item "FunBind (functor bindings)" [p"15.1"];
val it= item "function ($\\fnexp$)" [p"11",p"27.3",p"53.1",p"70"];
val it= item "function declaration (see $\\FUN$)" [];
val it= item "function type ($\\rightarrow$)" [p"21.2",p"27.3",p"31.3",p"32.1"];
val it= item "function type expression (\\verb+->+)" [p"13.1",p"32.1",p"72.2"];
val it= item "function-value binding ($\\fvalbind$)" [p"33.1",p"66.1",p"68.1",p"71"];
val it= item "\\FUNCTOR" [p"14.1",p"18.1",p"44.2",p"45.1",p"63.1"];
val it= item "functor application" [p"16.1",p"39.2",p"60.1",p"68.2"];
val it= item "functor binding" [p"15.1",p"18.1",p"45.2",p"62.5",p"66.1",p"68.2"];
val it= item "functor closure" [p"58.2",p"60.1",p"62.5"];
val it= item "functor declaration" [p"15.1",p"18.1",p"45.1",p"63.1"];
val it= subitem "as top-level declaration" [p"18.1",p"46.0",p"63.2"];
val it= item "functor description" [p"15.1",p"18.1",p"44.3"];
val it= item "functor environment ($\\F$)" [p"34.1",p"45.1",p"45.2",p"58.2",
p"62.5",p"63.1"];
val it= item "functor identifier ($\\funid$)" [p"14.1",p"16.1",p"18.1"];
val it= item "functor signature ($\\funsig$)" [p"34.1",p"44.4",p"45.2",p"46"];
val it= item "functor signature expression" [p"15.1",p"18.1",p"44.4",p"68.2"];
val it= item "functor signature matching" [p"15.1",p"46"];
val it= item "functor specification" [p"15.1",p"18.1",p"44.2"];
val it= item "$\\FunctorClosure$ (functor closures)" [p"58.2"];
val it= item "$\\fundec$ (functor declaration)" [p"15.1",p"18.1",p"45.1",p"63.1"];
val it= item "FunDec (functor declarations)" [p"15.1"];
val it= item "$\\fundesc$ (functor description)" [p"15.1",p"18.1",p"44.3"];
val it= item "FunDesc (functor descriptions)" [p"15.1"];
val it= item "$\\FunEnv$ (functor environments)" [p"34.1",p"58.2"];
val it= item "$\\funid$ (functor identifier)" [p"14.1",p"16.1",p"18.1"];
val it= item "$\\FunId$ (functor identifiers)" [p"14.1"];
val it= item "$\\funsigexp$ (functor signature expression)" [p"15.1",p"18.1",p"44.4",p"68.2"];
val it= item "FunSigExp (functor signature expressions)" [p"15.1"];
val it= item "$\\funspec$ (functor specification)" [p"15.1",p"18.1",p"44.2"];
val it= item "FunSpec (functor specifications)" [p"15.1"];
val it= item "$\\FunType$ (function types)" [p"21.2"];
val it= item "$\\fvalbind$ (function-value binding)" [p"66.1",p"68.1",p"71"];
val it= subitem "exhaustive" [p"33.1"];

val it= indexspace();
val it= indexspace();
val it= --"G";
val it= item "$\\G$ (signature environment)" [p"34.1",p"41.3",p"58.2",p"61.3",p"61.4"];
val it= item "generalisation ($\\succ$)" [p"23.3",p"26.2",p"30.4",p"31.3",
p"33.15",p"37.1"];
val it= item "generative signature expression" [p"16.1",p"41.2",p"61.2"];
val it= item "generative structure expression" [p"16.1",p"39.2",p"60.1"];
val it= item "grammar" [p"4.1"];
val it= subitem "for the Core" [p"9.3",p"69.1"];
val it= subitem "for Modules" [p"14.2"];

val it= --"H";

val it= item "\\HANDLE" [p"6.1",p"11",p"27.3",p"53.1",p"70"];

val it= --"I";

val it= item "$\\I$ (interface)" [p"58.2",p"61.2","61.5"to"62.1"];
val it= item "$\\IB$ (interface basis)" [p"58.2",p"59.1","61.2"to"62.1",p"62.4"];
val it= item "identifier ($\\id$)" [p"7.2",p"14.1"];
val it= subitem "alphanumeric" [p"7.2"];
val it= subitem "long" [p"7.2",p"65.5"];
val it= subitem "qualified" [p"7.2"];
val it= subitem "symbolic" [p"7.2"];
val it= item "$\\IE$ (interface environment)" [p"58.2",p"62.4"];
val it= item "\\IF" [p"6.1",p"67.1",p"70"];
val it= item "imperative attribute" [p"20.3",p"23.2",p"23.3"];
val it= item "imperative type" [p"23.2",p"30.3",p"30.35"];
val it= item "imperative type variable (see type variable)" [];
val it= item "implementation" [p"4.1",p"64.1"];
val it= item "{\\tt implode} (a string list)" [p"49.1",p"74",p"78.1"];
val it= item "$\\ImpTyVar$ (imperative type variables)" [p"7.4"];
(* from version2: 
val it= item "$\\imptyvars$ (free imperative type variables)" [p"21.3",p"40.2"]; *)
val it= item "$\\imptyvars$ (free imperative type variables)" [p"21.3",
      "45.3"to"46.01"]; 
val it= item "$\\In$ (injection)" [p"22.1"];
val it= item "\\IN" [p"6.1",p"11",p"12.1",p"16.1",p"17",p"67.1",p"70",p"71"];
val it= item "\\INCLUDE" [p"14.1",p"17",p"42.3",p"62.1"];
val it= item "inference" [p"5.1"];
val it= item "inference rules" [];
val it= subitem "static semantics (Core)" [p"26.1"];
val it= subitem "static semantics (Modules)" [p"39.1"];
val it= subitem "dynamic semantics (Core)" [p"50.1"];
val it= subitem "dynamic semantics (Modules)" [p"59.2"];
val it= item "$\\inexp$ (infix expression)" [p"69.1",p"70"];
val it= item "InfExp (infix expressions)" [p"69.1",p"70"];
val it= item "\\INFIX" [p"6.1",p"8.5",p"9.1",p"12.1",p"71"];
val it= item "infix expression" [p"9.1",p"11",p"69.1",p"70"];
val it= item "infix pattern" [p"9.1",p"12.2",p"72.1"];
val it= item "infixed identifiers" [p"8.5",p"9.1",p"11",p"12.1",p"14.1",
 p"70",p"71",p"72.1",p"74"];
val it= item "\\INFIXR" [p"6.1",p"8.5",p"9.1",p"12.1",p"71"];
val it= item "initial basis" [p"5.0",p"73.1",p"76.1"]; 
val it= item "injection (\\In)" [p"22.1"];
val it= item "{\\tt input}" [p"49.1",p"75.1",p"79.1.1"];
val it= item "input/output" [p"75.1",p"78.1"];
val it= item "instance ($\\geq$)" [];
val it= subitem "of signature" [p"36.4",p"37.2",p"38.1",p"41.2",p"42.3"];
val it= subitem "of functor signature" [p"36.5",p"39.2"];
val it= subitem "in matching" ["37.2"to"37.2.5"];
val it= item "$\\INSTREAM$" [p"73.1",p"75.2",p"78.1"];
val it= item "$\\INT$" [p"73.1",p"75.2"];
val it= item "$\\Int$ (interfaces)" [p"58.2"];
val it= item "$\\IntBasis$ (interface bases)" [p"58.2"];
val it= item "integer constant" [p"6.2",p"75.2"];
val it= item "$\\IntEnv$ (interface environments)" [p"58.2"];
val it= item "$\\Inter$" [p"58.2",p"61.1",p"62.5"];
val it= item "interaction" [p"4.1",p"64.1"];
val it= item "interface ($\\I$)" [p"58.2",p"61.2","61.5"to"62.1"];
val it= item "interface basis ($\\IB$)" [p"58.2",p"59.1","61.2"to"62.1",p"62.4"];
val it= item "interface environment ($\\IE$)" [p"58.2",p"62.4"];
val it= item "{\\tt Interrupt}" [p"49.2",p"64.2"];
val it= item "{\\tt Io}" [p"49.2",p"79.1.1",p"79.1.1"];
val it= item "irredundant patterns" ["32.2"to"33.1",p"49.2"];
val it= item "{\\tt it}" [p"68.1"];

val it= --"L"; 

val it= item "L (left associative)" [p"10.4",p"69.7"];
val it= item "$\\lab$ (label)" [p"7.2",p"8.2"];
val it= item "$\\Lab$ (labels)" [p"7.2",p"8.2"];
val it= item "\\LET" [p"6.1"];
val it= subitem "expression (Core)" [p"11",p"27.1",p"52.1",p"67.1",p"70"];
val it= subitem "expression (Modules)" [p"16.1",p"39.2",p"60.1"];
val it= item "letter in identifer" [p"7.2"];
val it= item "lexical analysis" [p"8.4",p"9.3"];
val it= item "$\\LIST$" [p"73.1",p"75.2"];
val it= item "list reversal (\\ml{rev})" [p"74",p"77.1"];
val it= item "{\\tt ln}" [p"49.1",p"74",p"77.1"];
val it= item "{\\tt Ln}" [p"49.2",p"77.1"];
val it= item "\\LOCAL" [p"6.1"];
val it= subitem "declaration (Core)" [p"12.1",p"28.3",p"54.3",p"71"];
val it= subitem "declaration (Modules)" [p"16.1",p"40.2",p"60.2"];
val it= subitem "specification (Modules)" [p"17",p"42.3",p"62.1"];
val it= item "long identifiers (e.g. $\\longexn$)" [p"7.2",p"65.5"];
val it= item "{\\tt lookahead}" [p"49.1",p"75.1",p"79.1.1"];

val it= --"M";

val it= item "$\\m$ (structure name)" [p"20.2",p"21.2",p"22.1",p"34.2",p"35.1",p"35.3",
p"36.3",p"37.1",p"41.2",p"44.1",p"48.2"];
val it= subitem "fresh" [p"39.2",p"40.1"];
val it= item "$\\M$ (structure name set)" [p"34.1",p"39.2"];
val it= item "\\ml{map}" [p"74",p"77.1"];
val it= item "match ($\\match$)" [p"10.1",p"11",p"28.1",p"54.1"];
val it= subitem "irredundant" [p"32.2",p"49.2"];
val it= subitem "exhaustive" [p"32.2",p"49.2"];
val it= subitem "in closure" [p"48.1",p"50.1"];
val it= item "$\\Match$" [p"10.1"];
val it= item "{\\tt Match} (exception)" [p"49.2",p"53.1"];
val it= item "match rule" [p"10.1",p"11",p"28.1",p"54.2"];
val it= item "matching" [];
val it= subitem "signatures (see signature matching)" [];
val it= subitem "functor signatures (see functor signature matching)" [];
val it =item "maximise equality" [p"24.4",p"28.2"];
val it= item "$\\mem$ (memory)" [p"48.1",p"52.3",p"57.0"];
val it= item "$\\Mem$ (memories)" [p"48.1"];
val it= item "memory ($\\mem$)" [p"48.1",p"52.3",p"57.0"];
val it= item "{\\tt mod}" [p"49.1",p"74",p"76.1",p"78.1"];
val it= item "{\\tt Mod}" [p"49.2",p"78.1"];
val it= item "modification ($+$)" [];
val it= subitem "of finite maps" [p"21.1"];
val it= subitem "of environments" [p"22.1",p"51.1"];
val it= item "module" [p"15.1"];
val it= item "Modules" [p"4.1"];
val it= item "$\\mrule$ (match rule)" [p"10.1",p"11",p"28.1",p"54.2"];
val it= item "Mrule (match rules)" [p"10.1"];
val it= item "multiplication of numbers (\\ml{*})" [p"49.1",p"74",p"76.1",p"78.1"];

val it= --"N";

val it= item "$\\n$ (name, see structure name, type name and exception name)" [];
val it= item "$\\N$ (name set)" [p"34.1",p"39.1"];
val it= item "$n$-tuple" [p"67.1",p"67.2",p"70",p"72.1"];
val it= item "name" [];
val it= subitem "of structure ($\\m$)" [p"20.2",p"21.2",p"22.1",p"34.2",
p"35.1",p"35.3",p"36.3",p"37.1",p"39.1",p"40.1",
p"41.2",p"44.1",p"48.2"];
val it= item "name set ($\\N$)" [p"34.1",p"39.1"];
val it= item "$\\NamesFcn$ (free names)" (*names*) [p"34.1",p"35.2",p"39.1",p"45.2"];
val it= item "$\\NameSets$ (name sets)" [p"34.1"];
val it= item "Natural Semantics" [p"5.1"];
val it= item "{\\tt Neg}" [p"49.2",p"77.1"];
val it= item "negation of booleans (\\ml{not})" [p"74",p"77.1"];
val it= item "negation of numbers (\\verb+~+)" [p"6.2",p"49.1",p"74",p"77.1"];
val it= item "\\NIL" [p"67.1",p"73.1",p"74",p"75.2"];
val it= item "non-expansive expression" [p"23.4",p"24.3"];
val it= item "\\NONFIX" [p"6.1",p"9.1",p"12.1",p"14.1",p"71",p"74"];
val it= item "nonfix identifiers" [p"9.1",p"12.1",p"14.1",p"71",p"74"];
val it= item "\\ml{not}" [p"74",p"77.1"];
val it= item "\\NUM" [p"74"];

val it= --"O";

val it= item "\\ml{o} (function composition)" [p"74",p"76.1"]; 
val it= item "occurrence" [];
val it= subitem "substructure" [p"34.2"];
val it= item "$\\of{}{}$ (projection)" [p"22.1",p"34.2"];
val it= item "$\\OF$" [p"6.1"];
val it= subitem "in $\\CASE$ expression" [p"67.1",p"70"];
val it= subitem "in constructor binding" [p"12.1"];
val it= subitem "in exception binding" [p"12.1",p"47.1"];
val it= subitem "in exception description" [p"17",p"58.1"];
val it= item "\\OP" [p"6.1",p"9.1"];
val it= subitem "on variable or constructor" [p"11",p"12.2",p"70",p"71",p"72.1"];
val it= subitem "in constructor binding" [p"12.1",p"71"];
val it= item "\\OPEN" [p"6.1" (*version2:,p"7.2" *)
,p"12.1",p"17",p"28.3",p"42.3",p"54.3",p"58.2",p"62.1",p"65.5",p"68.2",p"71"];
val it= item "\\verb+open_in+" [p"49.1",p"75.1",p"78.1",p"79.1.1"];
val it= item "\\verb+open_out+" [p"49.1",p"75.1",p"78.1",p"79.1.1"];
val it= item "opening structures in declarations" [p"12.1",p"28.3",p"54.3",p"71"];
val it= item "opening structures in specifications" [p"17",p"18.2",p"42.3",p"62.1"];
val it= item "options" [p"10.2"];
val it= subitem "first ($\\langle\\ \\rangle$)" [p"26.1",p"41.1"];
val it= subitem "second ($\\langle\\langle\\ \\rangle\\rangle$)" [p"26.1"];
val it= item "{\\tt ord} (of string)" [p"49.1",p"74",p"78.0"];
val it= item "{\\tt Ord}" [p"49.2",p"78.0"];
val it= item "\\ORELSE" [p"6.1",p"67.1",p"70"];
val it= item "{\\tt output}" [p"49.1",p"75.1",p"79.1.1"];
val it= item "``{\\tt Output stream is closed}'' " [p"79.1.1"];
val it= item "$\\OUTSTREAM$" [p"73.1",p"75.2",p"78.1"];

val it= --"P";

val it= item "$\\p$ (see packet)" [];
val it= item "$\\Pack$ (packets)" [p"48.1"];
val it= item "packet ($\\p$)" [p"48.1",p"51.1",p"53.1",p"59.2","64.1"to"65.2"];
val it= item "parsing" [p"4.3",p"64.1"];
val it= item "$\\pat$ (pattern)" [p"10.1",p"12.2",p"31.3",p"56.2",p"67.2",p"72.1"];
val it= item "Pat (patterns)" [p"10.1"]; 
val it= item "$\\labpats$ (pattern row)" (*patrow*) [p"10.1",p"12.2",p"31.2",p"56.2",p"67.2",p"72.1"];
val it= item "PatRow (pattern rows)" [p"10.1"];
val it= item "pattern" [p"10.1",p"12.2",p"31.3",p"56.2",p"67.2",p"72.1"];
val it= subitem "layered" [p"12.2",p"31.3",p"57.0",p"72.1"];
val it= item "pattern matching" ["32.2"to"33.1",p"47.2",p"49.2",p"56.2"];
val it= subitem "with $\\REF$" [p"56.3",p"57.0"];
val it= item "pattern row" [p"10.1",p"12.2",p"31.2",p"56.2",p"67.2",p"72.1"];
val it= item "polymorphic" [];
val it= subitem "functions" [p"26.2",p"28.2",p"30.4"];
val it= subitem "references" [p"23.4",p"24.3",p"28.2","45.3"to"46.01",p"74"];
val it= subitem "exceptions" [p"23.4",p"30.1",p"43.4","45.3"to"46.01"];
(* version2: subitem "references" [p"23.4",p"24.3",p"28.2",p"40.2",p"74"];
val it= subitem "exceptions" [p"23.4",p"30.1",p"40.2",p"43.4"]; *)
val it= item "precedence" [p"10.3",p"69.4"];
val it= item "principal" [];
val it= subitem "environment" [p"33.2",p"40.2"];
val it= subitem "equality-" [p"38.5",p"41.25",p"42.1",p"44.4",p"45.2"];
val it= subitem "signature" [p"38.1",p"38.5",p"41.25",p"42.1",p"44.4",p"45.2"];
val it= item "printable character" [p"6.2"];
val it= item "{\\tt Prod}" [p"49.2",p"78.1"];
val it= item "product type (\\verb+*+)" [p"67.2",p"72.2"];
val it= item "program ($\\program$)" [p"4.2","64.1"to"65.2"];
val it= item "Program (programs)" [p"64.1"];
val it= item "projection ($\\of{}{}$)" [p"22.1",p"34.2"];

val it= --"Q"; 


val it= item "qualified identifier" [p"7.2"];
val it= item "{\\tt Quot}" [p"49.2",p"78.1"];


val it= --"R";

val it= item "$\\r$ (record)" [p"48.1",p"52.2",p"56.0",p"56.2"];
val it= item "R (right associative)" [p"10.4",p"69.5"];
val it= item "\\RAISE" [p"6.1",p"11",p"27.3",p"27.4",p"51.1",p"53.1",p"53.1",p"64.1",p"70"];
val it= item "$\\Ran$ (range)" [p"21.1"];
val it= item "$\\REAL$" [];
val it= subitem "the type" [p"73.1",p"75.2"];
val it= subitem "coercion" [p"49.1",p"74",p"77.1"];
val it= item "real constant" [p"6.2",p"75.2"];
val it= item "realisation ($\\rea$)" [p"36.3","37.2"to"37.2.5",p"46"];
val it= item "$\\REC$" [p"6.1",p"12.1",p"13.2",p"29.1",p"50.1",p"55.1",p"71"];
val it= item "$\\Rec$ (recursion operator)" [p"50.1",p"53.1",p"55.1"];
val it= item "record " [];
val it= subitem "$\\r$" [p"48.1",p"52.2",p"56.0",p"56.2"];
val it= subitem "as atomic expression" [p"11",p"27.0",p"52.1",p"67.1",p"70"];
val it= subitem "as atomic pattern" [p"12.2",p"31.1",p"56.0",p"67.2",p"72.1"];
val it= subitem "selector (\\ml{\\#}\\ {\\it lab})" [p"6.1",p"67.1",p"70"];
val it= subitem "type expression" [p"13.1",p"32.1",p"72.2"];
val it= subitem "type ($\\varrho$)" [p"21.2",p"27.2",p"31.1",p"31.2",p"32.15"];
val it= item "Record (records)" [p"48.1"];
val it= item "$\\RecType$ (record types)" [p"21.2"];
val it= item "recursion (see $\\REC$, $\\Rec$, and $\\FUN$)" [];
val it= item "$\\REF$" [];
val it= subitem "the type constructor" [p"73.1",p"75.2"];
val it= subitem "the type name" [p"23.1",p"73.1",p"74",p"75.2"];
val it= subitem "the value constructor" [p"47.2",p"52.3",p"56.3",p"57.0",p"74",p"75.2",p"77.1"];
val it= item "reserved words" [p"6.1",p"14.1"];
val it= item "respect equality (see equality)"[];
val it= item "restrictions" [];
val it= subitem "closure rules (see these)" [];
val it= subitem "syntactic (Core)" [p"13.2","32.2"to"33.1"];
val it= subitem "syntactic (Modules)" [p"16.1"];
val it= item "\\ml{rev}" [p"74",p"77.1"];

val it= --"S";

val it= item "$\\s$ (state)" [p"48.1",p"50.2",p"52.2",p"57.0",p"59.2","64.1"to"65.2"];
val it= item "$\\S$ (structure)" [p"21.2",p"34.2",p"35.1",p"37.1",p"39.2",
p"41.2",p"48.2"];
val it= item "{\\SCon} (special constants)" [p"6.4"];
val it= item "{\\scon} (see special constant)" [];
val it= item "scope" [];
val it= subitem "of constructor" [p"8.3",p"22.1"];
val it= subitem "of value variable" [p"8.3",p"22.1"];
val it= subitem "of fixity directive" [p"9.2",p"14.1"];
val it= subitem "of explicit type variable" ["23.10"to"23.11",p"28.3",p"29.0"];
val it= item "$\\SE$ (structure environment)" [];
val it= subitem "static" [p"21.2",p"22.1",p"34.2",p"37.1",p"41.1",p"43.5",p"73.1"];
val it= subitem "dynamic" [p"48.1",p"59.1",p"61.1",p"76.1"];
val it= item "semantic object" [p"5.1"];
val it= subitem "simple (Static)" [p"20.1"];
val it= subitem "simple (Dynamic)" [p"47.2"];
val it= subitem "compound (Core, Static)" [p"20.4",p"21.2"]; 
val it= subitem "compound (Core, Dynamic)" [p"48.1"];
val it= subitem "compound (Modules, Static)" [p"34.1"];
val it= subitem "compound (Modules, Dynamic)" [p"58.2"];
val it= item "sentence" [p"5.1",p"26.1",p"39.1",p"50.2",p"59.2",p"64.1"];
val it= item "separate compilation" [p"15.1","18.2"to"19.1",p"46"];
val it= item "sequential" [];
val it= subitem "expression" [p"67.1",p"70"];
val it= subitem "declaration (Core)" [p"12.1",p"28.3",p"54.3",p"71"];
val it= subitem "functor declarations" [p"18.1",p"45.1.5",p"63.1"];
val it= subitem "functor specification" [p"18.1",p"44.2"];
val it= subitem "signature declaration" [p"16.1",p"41.3",p"61.3"];
val it= subitem "specification" [p"17",p"42.3",p"62.1"];
val it= subitem "structure-level declaration" [p"16.1",p"40.2",p"60.2"];
val it= item "$\\shareq$ (sharing equation)" [p"15.1",p"17",p"44.1",p"58.1"];
val it= item "SharEq (sharing equations)" [p"15.1",p"58.1"];
val it= item "sharing" ["18.2"to"19.1",p"40.1",p"41.2",p"43.0",p"43.2",p"44.1",p"46"];
val it= subitem "equations" [p"15.1",p"17",p"44.1",p"58.1"];
val it= subitem "specification" [p"17",p"42.3"];
val it= subitem "of structures" [p"17",p"44.1"];
val it= subitem "of types" [p"17","44.1"to"44.1.5"];
val it= subitem "multiple" [p"17",p"44.1"];
val it= item "\\SHARING" [p"14.1",p"17",p"42.3"];
val it= item "side-condition" [p"50.2",p"59.2"];
val it= item "side-effect" [p"59.2",p"65.2"];
val it= item "\\SIG" [p"14.1",p"16.1",p"41.2",p"61.2"];
val it= item "$\\Sig$ (signatures)" [p"34.1"];
val it= item "$\\sigbind$ (signature binding)" [p"15.1",p"16.1",p"42.1",p"61.4"];
val it= item "SigBind (signature bindings)" [p"15.1"];
val it= item "$\\sigdec$ (signature declaration)" [p"15.1",p"16.1",p"41.3",p"61.3"];
val it= item "SigDec (signature declarations)" [p"15.1"];
val it= item "$\\SigEnv$ (signature environments)" [p"34.1",p"58.2"];
val it= item "$\\sigexp$ (signature expression)" [p"15.1",p"16.1",p"41.2",p"61.2"];
val it= item "SigExp (signature expressions)" [p"15.1"];
val it= item "$\\sigid$ (signature identifier)" [p"14.1",p"16.1",p"41.2",p"61.2"];
val it= item "$\\SigId$ (signature identifiers)" [p"14.1"];
val it= item "signature ($\\sig$)" [p"34.1",p"35.2",p"36.35",
     p"36.4","37.2"to"37.2.5",p"38.1",p"38.5",p"41.2", p"41.25",
     p"42.1",p"44.4",p"45.2",p"46",p"59.1"];
val it= item "\\SIGNATURE" [p"14.1",p"16.1",p"41.3",p"61.3"];
val it= item "signature binding" [p"15.1",p"16.1",p"42.1",p"61.4"];
val it= item "signature declaration" [p"15.1",p"16.1",p"41.3",p"61.3"];
val it= subitem "in top-level declaration" [p"18.1",p"46.0",p"63.1"];
val it= item "signature environment ($\\G$)" [];
val it= subitem "static" [p"34.1",p"41.3",p"42.1",p"46.0"];
val it= subitem "dynamic" [p"58.2",p"59.1",p"61.3",p"61.4",p"63.2"];
val it= item "signature expression" [p"15.1",p"16.1",p"41.2",p"61.2"];
val it= item "signature identifier" [p"14.1",p"16.1",p"41.2",p"61.2"];
val it= item "signature instantiation (see instance)"[];
val it= item "signature matching" ["37.2"to"37.2.5","39.2"to"40.1",p"41.1",p"45.2"];
val it= item "{\\tt sin}" [p"49.1",p"74",p"77.1"];
val it= item "{\\tt size} (of strings)" [p"49.1",p"74",p"77.1"];
val it= item "$\\spec$ (specification)" [p"15.1",p"17",p"42.2",p"61.5"];
val it= item "Spec (specifications)" [p"15.1"];
val it= item "special constant (\\scon)" [p"6.2",p"6.4",p"20.35"];
val it= subitem "as atomic expression" [p"11",p"26.3",p"51.15",p"70"];
val it= subitem "in pattern" [p"12.2", p"30.5",p"55.35",p"55.36",p"72.1"];
val it= item "special value ($\\sv$)" [p"47.2"];
val it= item "specification" [p"15.1",p"17",p"42.2",p"61.5"];
val it= item "{\\tt sqrt} (square root)" [p"49.1",p"74",p"77.1"];
val it= item "{\\tt Sqrt}" [p"49.2",p"77.1"];
val it= item "state ($\\s$)" [p"48.1",p"50.2",p"52.2",p"57.0",p"59.2","64.1"to"65.2"];
val it= item "$\\State$" [p"48.1"];
val it= item "state convention" [p"51.1",p"52.2"];
val it= item "static" [];
val it= subitem "basis" [p"4.1",p"26.2",p"34.1",p"39.1",p"64.1",p"73.1"];
val it= subitem "semantics (Core)" [p"20.1"];
val it= subitem "semantics (Modules)" [p"34.1"];
val it= item "\\verb+std_in+" [p"49.1",p"75.1",p"79.1.1"];
val it= item "\\verb+std_out+" [p"49.1",p"75.1",p"79.1.1"];
val it= item "$\\Str$ (structures)" [p"21.2"];
val it= item "$\\strbind$ (structure binding)" [p"15.1",p"16.1",p"41.1",p"61.1"];
val it= item "StrBind (structure bindings)" [p"15.1"];
val it= item "$\\strdec$ (structure-level declaration)" [p"15.1",p"16.1",p"40.2",
     p"60.2",p"65.4"];
val it= item "StrDec (structure-level declarations)" [p"15.1"];
val it= item "$\\strdesc$ (structure description)" [p"15.1",p"17",p"43.5",p"62.4"];
val it= item "StrDesc (structure descriptions)" [p"15.1"];
val it= item "stream (input/output)" [p"78.1"];
val it= item "$\\StrEnv$ (structure environments)" [p"21.2",p"48.1"];
val it= item "$\\strexp$ (structure expression)" [p"15.1",p"16.1",p"39.2",p"60.1",p"68.2"]; 
val it= item "$\\StrExp$ (structure expressions)" [p"15.1"];
val it= item "$\\strid$ (structure identifier)" [p"7.2"];
val it= subitem "as structure expression" [p"16.1",p"39.2",p"60.1"];
val it= item "$\\StrId$ (structure identifiers)" [p"7.2"];
val it= item "$\\STRING$" [p"73.1",p"75.2"];
val it= item "string constant" [p"6.2",p"75.2"];
val it= item "$\\StrNames$ (structure names)" [p"20.2"];
val it= item "$\\StrNamesFcn$ (free structure names)" (*strnames*) [p"34.1"];
val it= item "$\\StrNameSets$ (structure name sets)" [p"34.1"];
val it= item "$\\STRUCT$" [p"14.1",p"16.1",p"39.2",p"60.1",p"68.2"];
val it= item "structure ($\\S$ or $(\\m,\\E)$)" [p"21.2",p"34.2",p"35.1",p"37.1",
p"39.2",p"41.2",p"48.2"];
val it= item "$\\STRUCTURE$" [p"14.1",p"16.1",p"17",p"40.2",p"42.2",p"60.2",p"62.1"];
val it= item "structure binding ($\\strbind$)" [p"15.1",p"16.1",p"41.1",p"61.1"];
val it= item "structure declaration" [p"16.1",p"40.2",p"60.2"];
val it= item "structure description ($\\strdesc$)" [p"15.1",p"17",p"43.5",p"62.4"];
val it= item "structure environment ($\\SE$)" [];
val it= subitem "static" [p"21.2",p"22.1",p"34.2",p"37.1",p"41.1",p"43.5",p"73.1"];
val it= subitem "dynamic" [p"48.1",p"59.1",p"61.1",p"76.1"];
val it= item "structure expression ($\\strexp$)" [p"15.1",p"16.1",p"39.2",p"60.1",p"68.2"];
val it= item "structure identifier ($\\strid$)" [p"7.2"];
val it= subitem "as structure expression" [p"16.1",p"39.2",p"60.1"];
val it= item "structure-level declaration ($\\strdec$)"
 [p"15.1",p"16.1",p"40.2",p"60.2",p"65.4"];
val it= subitem "in top-level declaration" [p"18.1",p"45.3",p"63.1",p"65.4"];
val it= item "structure name ($\\m$, see name)" [];
val it= item "structure name set ($\\M$)" [p"34.1",p"39.1"];
val it= item "structure realisation ($\\strrea$)" [p"36.3"];
val it= item "structure specification" [p"17",p"42.2",p"62.1"];
val it= item "substructure" [p"34.2"];
val it= subitem "proper" [p"34.2",p"35.3"];
val it= item "subtraction of numbers (\\ml{-})" [p"49.1",p"74",p"76.1",p"78.1"];
val it= item "{\\tt Sum}" [p"49.2",p"78.1"];
val it= item "{\\SVal} (special values)" [p"47.2"];
val it= item "$\\Supp$ (support)" [p"36.2",p"36.3"];
val it= item "$\\sv$ (special value)" [p"47.2"];
val it= item "symbol" [p"7.3"];
val it= item "syntax" [p"6.1",p"14.1",p"47.1",p"58.1",p"69.1"];

val it= --"T";

val it= item "$\\t$ (type name)" [p"20.2",p"23.2","24.4"to"25.1",p"28.2",p"30.1",
p"33.2",p"34.1",p"35.1",p"36.2",p"37.1",p"43.3",p"75.2"];
val it= item "$\\T$ (type name set)" [p"21.2",p"34.1"];
val it= item "$\\TE$ (type environment)" [p"21.2","24.4"to"25.1",p"29.3",p"30.1",
p"37.1",p"43.3",p"59.1"];
val it= item "\\THEN" [p"6.1",p"67.1",p"70"];
val it= item "$\\topdec$ (top-level declaration)" [p"15.1",p"18.1",p"45.3",p"63.1",p"65.4"];
val it= subitem "in program" ["64.1"to"65.2"];
val it= item "TopDec (top-level declarations)" [p"15.1"];
val it= item "top-level declaration" [p"4.3",p"15.1",p"18.1",p"45.3",p"63.1",p"65.4"];
val it= item "$\\TRUE$" [p"73.1",p"74",p"75.2"];
val it= item "truncation of reals (\\ml{floor})" [p"49.1",p"74",p"77.1"];
val it= item "tuple" [p"67.1",p"67.2",p"70",p"72.1"];
val it= item "tuple type" [p"67.2",p"72.2"];
val it= item "$\\ty$ (type expression)" [p"10.1",p"13.1",p"32.1",p"47.1",
p"67.2",p"72.2"];
val it= item "Ty (type expressions)" [p"10.1",p"13.1",p"47.1"];
val it= item "$\\tycon$ (type constructor)" [p"7.2",p"12.1",p"13.1",p"17",p"21.1",
"24.4"to"25.1",p"29.3",p"30.1",p"32.1",p"35.1",
p"37.1",p"43.3",p"44.1",p"75.2"];
val it= item "$\\TyCon$ (type constructors)" [p"7.2"];
val it= item "$\\TyEnv$ (type environments)" [p"21.2"];
val it= item "$\\TyNames$ (type names)" (*TyName*) [p"20.2"];
val it= item "$\\TyNamesFcn$ (free type names)" (*tynames*) [p"21.3",p"39.1"];
val it= item "$\\TyNameSets$ (type name sets)" (*TyNameSet*) [p"21.2"];
val it= item "$\\typbind$ (type binding)" [p"10.1",p"12.1",p"29.3",p"47.1",p"71"];
val it= item "TypBind (type bindings)" [p"10.1",p"47.1"];
val it= item "$\\typdesc$ (type description)" [p"15.1",p"17",p"43.2",p"58.1"];
val it= item "TypDesc (type descriptions)" [p"15.1",p"58.1"];
val it= item "type ($\\tau$)" [p"21.1",p"22.3",p"23.3",p"24.2","26.2"to"28.1",
p"30.4",p"31.3",p"32.1"];
val it= item "$\\Type$ (types)" [p"21.2"];
val it= item "$\\TYPE$" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"44.1",p"47.1",
p"58.1",p"71"];
val it= item "$\\scontype$ (function on special constants)" [p"20.35",p"26.3",
     p"30.5"];
val it= item "type binding" [p"10.1",p"12.1",p"29.3",p"47.1",p"71"];
val it= item "type constraint (\\verb+:+)" [];
val it= subitem "in expression" [p"11",p"27.3",p"47.1",p"70"];
val it= subitem "in pattern" [p"12.2",p"31.3",p"47.1",p"72.1"];
val it= item "type construction" [p"13.1",p"32.1"];
val it= item "type constructor ($\\tycon$)" [p"7.2",p"12.1",p"13.1",p"17",p"21.1",
"24.4"to"25.1",p"29.3",p"30.1",p"32.1",p"35.1",
p"37.1",p"43.3",p"44.1",p"75.2"]; 
val it= item "type constructor name (see type name)" [];
val it= item "type declaration" [p"12.1",p"28.2",p"47.1",p"71"];
val it= item "type description ($\\typdesc$)" [p"15.1",p"17",p"43.2",p"58.1"];
val it= item "type environment ($\\TE$)" [p"21.2","24.4"to"25.1",p"29.3",p"30.1",
p"37.1",p"43.3",p"59.1"];
val it= item "type explication" [p"36.35",p"36.4",p"37.2",p"40.1",p"41.25",p"45.2"];
val it= item "type-explicit signature (see type explication)" [];
val it= item "type expression" [p"10.1",p"13.1",p"32.1",p"47.1",p"67.2",p"72.2"];
val it= item "type-expression row ($\\labtys$)" [p"10.1",p"13.1",p"32.15",p"47.1",p"72.2"];
val it= item "type function ($\\typefcn$)" [p"21.2","23.1"to"23.2","24.4"to"25.1",
p"29.3",p"35.1",p"36.2",p"37.1",p"43.2","44.1"to"44.1.5",p"75.2"];
val it= item "type name ($\\t$)" [p"20.2",p"23.2","24.4"to"25.1",p"28.2",p"30.1",
p"33.2",p"34.1",p"35.1",p"36.2",p"37.1",p"43.3",p"75.2"];
val it= item "type name set" [p"21.2",p"34.1"];
val it= item "type realisation ($\\tyrea$)" [p"36.2"];
val it= item "type scheme ($\\tych$)" [p"21.2",p"23.3","24.2"to"24.3",p"26.2",
p"30.4",p"37.1",p"43.3","74"to"75.2"];
val it= item "type specification" [p"17",p"42.2",p"58.1"];
val it= item "type structure $(\\theta,\\CE)$" [p"21.2","24.4"to"25.1",
p"28.2",p"29.3",p"30.1",p"32.1",p"35.1",p"37.1",p"42.2",p"43.2",
"44.1"to"44.1.5","74"to"75.1"];
val it= item "type variable ($\\tyvar$, $\\alpha$)" [p"7.4",p"13.1",p"20.2"];
val it= subitem "in type expression" [p"13.1",p"32.1",p"72.2"];
val it= subitem "equality" [p"7.4",p"20.3",p"22.3",p"23.2",p"23.3"];
val it= subitem "imperative" [p"7.4",p"20.3",p"21.4",p"23.2",p"23.3",p"24.3",
p"28.2",p"30.3",p"30.35",p"33.2","45.3"to"46.01"];
val it= subitem "applicative" [p"8.1",p"20.3",p"21.4",p"23.2",p"23.3",p"24.3",
p"28.2",p"30.3",p"30.35"]; 
val it= subitem "explicit" ["23.10"to"23.11",p"27.4",p"28.2",p"28.3"];
val it= item "type vector ($\\tauk$)" [p"21.2",p"23.1",p"23.2"];
val it= item "$\\TypeFcn$ (type functions)" [p"21.2"];
val it= item "$\\TypeScheme$ (type schemes)" [p"21.2"];
val it= item "$\\labtys$ (type-expression row)" (*tyrow*) [p"10.1",p"13.1",p"32.15",
p"47.1",p"72.2"];
val it= item "TyRow (type-expression rows)" [p"10.1",p"13.1",p"47.1"];
val it= item "$\\TyStr$ (type structures)" [p"21.2"];
val it= item "$\\tyvar$ (see type variable)" [];
val it= item "$\\TyVar$ (type variables)" [p"7.2",p"20.2"];
val it= item "$\\TyVarFcn$ (free type variables)" (*tyvars*) [p"21.3"];
val it= item "$\\tyvarseq$ (type variable sequence)" [p"10.2"]; 
val it= item "$\\TyVarSet$" [p"21.2"];

val it= --"U";

val it= item "$\\U$ (explicit type variables)" [p"21.2",p"22.2",p"23.11",p"28.2",p"28.3"];
val it= item "$\\UNIT$" [p"75.2"];
val it= item "unguarded type variable" [p"23.11"];

val it= --"V";

val it= item "$\\V$ (value)" (*v*) [p"48.1","51.2"to"54.2"];
val it= item "$\\sconval$ (function on special constants)" [p"47.2",p"51.15",p"55.35",
     p"55.36"];
val it= item "$\\Val$ (values)" [p"48.1"];
val it= item "$\\VAL$" [p"6.1",p"12.1",p"17",p"28.2",p"42.2",p"54.3",p"61.5",p"71"];
val it= item "$\\valbind$ (value binding)" [p"10.1",p"12.1",p"23.11",p"24.3",p"28.2",
p"29.1",p"55.1",p"71"];
val it= subitem "simple" [p"12.1",p"29.1",p"55.1",p"71"];
val it= subitem "recursive" [p"12.1",p"29.1",p"29.2",p"55.1",p"71"];
val it= item "Valbind (value bindings)" [p"10.1"];
val it= item "$\\valdesc$ (value description)" [p"15.1",p"17",p"43.1",p"62.2"];
val it= item "ValDesc (value descriptions)" [p"15.1"];
val it= item "value binding ($\\valbind$)" [p"10.1",p"12.1",p"23.11",p"24.3",p"28.2",
p"29.1",p"55.1",p"71"];
val it= subitem "simple" [p"12.1",p"29.1",p"55.1",p"71"];
val it= subitem "recursive" [p"12.1",p"29.1",p"29.2",p"55.1",p"71"];
val it= item "value constant ($\\con$)" [];
val it= subitem "in pattern" [p"12.2",p"30.4",p"55.3",p"72.1"];
val it= item "value constructor ($\\con$)" [p"7.2"];
val it= subitem "as atomic expression" [p"11",p"26.2",p"52.1",p"70"];
val it= subitem "scope" [p"8.3",p"22.1"];
val it= item "value construction" []; 
val it= subitem "in pattern" [p"12.2",p"31.3",p"56.3",p"72.1"];
val it= subitem "infixed, in pattern" [p"12.2",p"72.1"];
val it= item "value declaration" [p"12.1",p"23.11",p"28.2",p"54.3",p"71"];
val it= item "value description ($\\valdesc$)" [p"15.1",p"17",p"43.1",p"62.2"];
val it= item "value variable ($\\var$)" [p"7.2"];
val it= subitem "as atomic expression" [p"11",p"26.2",p"51.2",p"70"];
val it= subitem "in pattern" [p"12.2",p"30.4",p"55.3",p"72.1"];
val it= item "value specification" [p"17",p"42.2",p"61.5"];
val it= item "$\\var$ (see value variable)" [];
val it= item "$\\Var$ (value variables)" [p"7.2"];
val it= item "$\\VarEnv$ (variable environments)" [p"21.2",p"48.1"];
val it= item "variable (see value variable)" [];
val it= item "variable environment ($\\VE$)" [];
val it= subitem "static" [p"21.2",p"22.1","24.2"to"24.3",p"28.2",p"29.1",p"30.1",
p"30.4",p"31.2",p"31.3",p"37.1",p"43.1",p"43.3",p"59.1","74"to"75.1"];
val it= subitem "dynamic" [p"48.1",p"50.1",p"55.1",p"55.3",p"56.2",p"56.3",
p"59.1",p"76.1"];
val it= item "$\\vars$ (set of value variables)" [p"58.2",p"62.2"];
val it= item "$\\VE$ (see variable environment)" [];
val it= item "via $\\rea$" [p"37.2",p"46"];
val it= item "view of a structure" [p"41.1",p"44.1",p"58.2",p"60.1",p"61.1"];

val it= --"W";

val it= item "well-formed" [];
val it= subitem "assembly" [p"35.2",p"36.1"];
val it= subitem "functor signature" [p"35.2"];
val it= subitem "signature" [p"35.2"];
val it= subitem "type structure" [p"24.4"];
val it= item "\\WHILE" [p"6.1",p"67.1",p"70"];
val it= item "wildcard pattern (\\verb+_+)" [p"12.2",p"30.4",p"55.3",p"72.1"];
val it= item "wildcard pattern row (\\verb+...+)" [p"6.1",p"12.2",p"31.2",
p"32.2",p"56.2",p"72.1"];
val it= item "\\WITH" [p"6.1",p"12.1",p"71"];
val it= item "\\WITHTYPE" [p"6.1",p"66.1",p"68.1",p"71"];

val it= --"Y";

val it= item "$\\Yield$"  [p"36.3"];
val it= terminate();
