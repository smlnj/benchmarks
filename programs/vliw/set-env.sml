(* set-env.sml
 *)

structure SetEnv : SIMLABS=
struct

  open AbsMach;

  val array = Array.array
  val update = Array.update
  val sub = Array.sub
  infix 9 sub

  val codes : (opcode list ref)=ref nil;

  val RegN=ref 0 and LabN=ref 0 and memorysize=ref 10000;
  (*RegN = (pointer to) number of registers needed;
    LabN = (pointer to) number of labels;
    memorysize=(pointer to) memory space size.
   *)
  val IP: (opcode list) ref =ref nil;
  val inivalue=(INT 0);
  (*IP = Program Pointer;
    inivalue = zero- initial value of memory and registers.
   *)
  val Reg=ref (array(0,inivalue)) and Memory=ref (array(0,inivalue))
      and Lab_Array=ref (array(0, (0,IP) ));
  (*Reg = register array;
    Memory = memory cell array;
    Lab_Array = label-opcode list array.
   *)

  fun max(n1:int,n2:int)=if (n1>n2) then n1 else n2;

  (* hvnop tests whether the instruction is not a real machine instruction,
     but only useful in simulation.
   *)
  fun hvnop(LABEL{...})=true |
      hvnop(LABWORD{...})=true |
      hvnop(WORD{...})=true |
      hvnop(_)=false;

  (*count_number is used to take into account register references and label
    declarations, and change RegN or LabN.
   *)
  fun count_number(FETCH {ptr=(n1,_),dst=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(STORE {src=(n1,_),ptr=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(ARITHI {src1=(n1,_),dst=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(MOVE {src=(n1,_),dst=(n2,_)})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(BRANCH {src1=(n1,_),src2=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(GETLAB {dst=(n,_),...})=
      (RegN:=max((!RegN),n) )                      |
      count_number(GETREAL {dst=(n,_),...})=
      (RegN:=max((!RegN),n) )                      |
      count_number(ARITH{src1=(n1,_),src2=(n2,_),dst=(n3,_),...})=
      (RegN:=max((!RegN),max(n1,max(n2,n3)) )  )   |
      count_number(LABEL{...})=
      ( Ref.inc(LabN) )                            |
      count_number(_)=();

  (* scan is used to scan the opcode list for the first time, to determine
     the size of Reg and Lab_Array, i.e. number of registers and labels.
   *)
  fun scan(nil)=() |
      scan(h::t)=(count_number(h);scan(t));

  (* setlabels is used to set the label array, of which each item is a
     pair (label, codep), codep points to the codes containing the LABEL
     statement and afterwards codes.
   *)
  fun setlabels(nil,_)= () |
      setlabels(codel as ((LABEL {lab=(l,_),...})::t),k)=
      (update((!Lab_Array),k,(l,ref codel)); setlabels(t,k+1) ) |
      setlabels(h::t,k)=setlabels(t,k) ;

  (* initializing the enviroment of the simulation.
   *)
  fun init(l)=(RegN:=0; LabN:=0; IP:=l; codes:=l;
              scan(!IP); Ref.inc(RegN);
              Reg:=array( (!RegN), inivalue ) ;
              Memory:=array( (!memorysize), inivalue ) ;
              Lab_Array:=array( (!LabN), (0,IP));
              setlabels(!IP,0)
              );



  exception wrong_label;
  exception runtime_error_in_labwords;
  exception runtime_error_in_words_or_labwords;
  exception negative_label_offset;
  exception no_label_in_register;
  exception illegal_operator_or_operand;
  exception type_mismatch_in_comparison ;
  exception no_address_in_register;
  exception no_memory_address_in_register;

  (* bitwise operations on ints *)
  local
    fun wrap f (x, y) = Word.toIntX(f(Word.fromInt x, Word.fromInt y))
  in
  val int_orb  = wrap Word.orb
  val int_andb = wrap Word.andb
  val int_xorb = wrap Word.xorb
  val int_lshift = wrap Word.<<
  val int_rshift = wrap Word.>>
  end (* local *)

  exception NotAReal

  fun strToReal s = (case Real.fromString s
         of SOME r => r
          | _ => raise NotAReal
        (* end case *))

  (* getresult gives the results of arithmtic operations
   *)
  fun getresult(iadd,INT (n1:int),INT (n2:int))=INT (n1+n2) |
      getresult(isub,INT (n1:int),INT (n2:int))=INT (n1-n2) |
      getresult(imul,INT (n1:int),INT (n2:int))=INT (n1*n2) |
      getresult(idiv,INT (n1:int),INT (n2:int))=INT (n1 div n2) |
      getresult(fadd,REAL (r1:real),REAL (r2:real))=REAL (r1+r2) |
      getresult(fsub,REAL (r1:real),REAL (r2:real))=REAL (r1-r2) |
      getresult(fmul,REAL (r1:real),REAL (r2:real))=REAL (r1*r2) |
      getresult(fdiv,REAL (r1:real),REAL (r2:real))=REAL (r1/r2) |
      getresult(iadd,INT (n1:int),LABVAL (l,k))=LABVAL (l,k+n1)  |
      getresult(iadd,LABVAL (l,k),INT (n1:int))=LABVAL (l,k+n1)  |
      getresult(isub,LABVAL (l,k),INT (n1:int))=LABVAL (l,k-n1)  |
      getresult(orb,INT n1,INT n2)=INT (int_orb(n1,n2))         |
      getresult(andb,INT n1,INT n2)=INT (int_andb(n1,n2))       |
      getresult(xorb,INT n1,INT n2)=INT (int_xorb(n1,n2))       |
      getresult(rshift,INT n1,INT n2)=INT (int_rshift(n1,n2))   |
      getresult(lshift,INT n1,INT n2)=INT (int_lshift(n1,n2))   |
      getresult(real,INT n,_)=REAL (Real.fromInt n)                |
      getresult(floor,REAL r,_)=INT (Real.floor(r))                |
(*    getresult(logb,REAL r,_)=INT (System.Unsafe.Assembly.A.logb(r))| *)
      getresult(_)=raise illegal_operator_or_operand;

  (* compare gives the results of comparisons in BRANCH statement.
   *)
  fun compare(ilt,INT n1,INT n2)= (n1<n2) |
      compare(ieq,INT n1,INT n2)= (n1=n2) |
      compare(igt,INT n1,INT n2)= (n1>n2) |
      compare(ile,INT n1,INT n2)= (n1<=n2) |
      compare(ige,INT n1,INT n2)= (n1>=n2) |
      compare(ine,INT n1,INT n2)= (n1<>n2) |
      compare(flt,REAL r1,REAL r2)= (r1<r2) |
      compare(feq,REAL r1,REAL r2)= (Real.==(r1,r2)) |
      compare(fgt,REAL r1,REAL r2)= (r1>r2) |
      compare(fle,REAL r1,REAL r2)= (r1<=r2) |
      compare(fge,REAL r1,REAL r2)= (r1>=r2) |
      compare(fne,REAL r1,REAL r2)= (Real.!=(r1,r2)) |
      compare(inrange,INT a,INT b)= (a>=0) andalso (a<b) |
      compare(outofrange,INT a,INT b)=(a<0) orelse (a>b) |
      compare(inrange,REAL a,REAL b)= (a>=0.0) andalso (a<b) |
      compare(outofrange,REAL a,REAL b)=(a<0.0) orelse (a>b) |
      compare(_)=raise type_mismatch_in_comparison ;

  (* findjmp_place returns the pointer to the codes corresponding to the
     given label (the codes containing the LABEL statement itself).
   *)
  fun findjmp_place lab =
      let val ipp=ref (ref nil) and i=ref 0 and flag=ref true;
          val none=(while ( (!i < !LabN) andalso (!flag) ) do
                       (  let val (l,p)=((!Lab_Array) sub (!i)) in
                          if (l=lab) then (ipp:=p;flag:=false)
                                     else Ref.inc(i)
                          end
                          )
                     )
      in if (!flag) then raise wrong_label
                   else (!ipp)
      end;

  (* findjmp_word returns the content of the k th labword in a code stream.
   *)
  fun findjmp_word(k,ip)=if (k<0) then raise negative_label_offset
                         else let fun f2(1,LABWORD{lab=(herepos,_)}::t)
                                                                   =herepos |
                                      f2(k,LABWORD{...}::t)=f2(k-1,t)       |
                                      f2(_)=raise runtime_error_in_labwords ;
                              in  f2(k, (!ip) )
                              end;

  (* inst_word returns the content of the k'th word or labword in a code
     stream.
   *)
  fun inst_word(k,ip)=if (k<0) then raise negative_label_offset
                      else let fun f(1,LABWORD{lab=(herepos,_)}::t)
                                   =LABVAL (herepos,0)              |
                                   f(1,WORD{value=n}::t)=INT n      |
                                   f(k,LABWORD{...}::t)=f(k-1,t)    |
                                   f(k,WORD{...}::t)=f(k-1,t)       |
                                   f(_)=raise
                                          runtime_error_in_words_or_labwords
                           in f(k,(!ip))
                           end;


  (* execjmp changes IP, makes it point to the codes of the given label.
   *)
  fun execjmp(LABVAL (l,0))= (IP:= !(findjmp_place l) ) |
      execjmp(LABVAL (l,k))= (IP:=
                              ! (findjmp_place
                                   (findjmp_word(k,findjmp_place(l) ) ) )
                                 )                      |
      execjmp(_) = raise no_label_in_register;

  (* addrplus returns the result of address+offset.
   *)
  fun addrplus(INT n,ofst)= n+ofst |
      addrplus(_,_)=raise no_memory_address_in_register;

  (* content gives the content of the fetched word.
   *)
  fun content(INT n,ofst)= (!Memory) sub (n+ofst) |
      content(LABVAL (l,k),ofst)=inst_word(k+ofst,findjmp_place(l))    |
      content(_,_)=raise no_address_in_register;

  (* exec executes the given instruction.
   *)
  fun exec(FETCH{immutable=_,offset=ofst,ptr=(p,_),dst=(d,_)})=
        update((!Reg),d,content((!Reg) sub p,ofst) )                   |
      exec(STORE{offset=ofst,src=(s,_),ptr=(p,_)})=
        update((!Memory),addrplus((!Reg) sub p,ofst),(!Reg) sub s)     |
      exec(GETLAB {lab=(l,_),dst=(d,_)})=
        update((!Reg),d,(LABVAL (l,0)) )                       |
      exec(GETREAL {value=v,dst=(d,_)})=
        update((!Reg),d,(REAL (strToReal v)))                  |
      exec(MOVE{src=(s,_),dst=(d,_)})=
        update((!Reg),d, (!Reg) sub s )                        |
      exec(LABEL {...})=
        ()                                                       |
      exec(LABWORD {...}) =
        ()                                                       |
      exec(WORD{...})=
        ()                                                       |
      exec(JUMP {dst=(d,_),...})=
        execjmp((!Reg) sub d)                                  |
      exec(ARITH {oper=opn,src1=(s1,_),src2=(s2,_),dst=(d,_)})=
        update((!Reg),d,getresult(opn,(!Reg) sub s1,(!Reg) sub s2) )   |
      exec(ARITHI {oper=opn,src1=(s1,_),src2=n1,dst=(d,_)})=
        update((!Reg),d,getresult(opn,(!Reg) sub s1,(INT n1) ) )       |
      exec(BRANCH{test=comp,src1=(s1,_),src2=(s2,_),dst=(labnum,_),...})=
        if compare(comp,(!Reg) sub s1,(!Reg) sub s2)
        then (IP:= !(findjmp_place(labnum) ) )
        else ()                                                        |
      exec(NOP)= () |
      exec(BOGUS _)= raise Match

      ;



  exception End_of_Program;

  fun step () =let
                 val Instruction=(hd(!IP) handle Hd=> raise End_of_Program)
               in
               (IP:=tl(!IP) handle Tl=>raise End_of_Program;
                exec(Instruction) )
               end;
  fun run () =(step();run() )
              handle End_of_Program =>print "End of program\n";

  (* bms, ims, rms are simply abbreviations.
   *)
  val bms : bool -> string = Bool.toString
  and ims : int -> string = Int.toString
  and rms : real -> string = Real.toString

  (* dispv shows the content of a register, dispm shows the content of a
     memory word.
   *)
  fun dispv(n,INT k)=print("Register "^ims(n)^": "^ "INT "^ims(k)^"\n") |
      dispv(n,REAL r)=print("Register "^ims(n)^": "^ "REAL "^rms(r)^"\n") |
      dispv(n,LABVAL (l,0))=print("Register "^ims(n)^": "^ "LABEL "^ims(l)^"\n") |
      dispv(n,LABVAL (l,k))=print("Register "^ims(n)^": "^
                                  "LABWORD "^ims(k)^" after"^
                                  "LABEL "^ims(l)^"\n") ;

  fun dispm(n,INT k)=print("Memory "^ims(n)^": "^"INT "^ims(k)^"\n") |
      dispm(n,REAL r)=print("Memory "^ims(n)^": "^"REAL "^rms(r)^"\n") |
      dispm(n,LABVAL (l,0))=print("Memory "^ims(n)^": "^"LABEL "^ims(l)^"\n") |
      dispm(n,LABVAL (l,k))=print("Memory "^ims(n)^": "^
                                  "LABWORD "^ims(k)^" after"^
                                  "LABEL "^ims(l)^"\n") ;

  (* oms and cms give the strings of the functions and comparisions.
   *)
  fun oms(iadd)="iadd" | oms(isub)="isub" |
      oms(imul)="imul" | oms(idiv)="idiv" |
      oms(fadd)="fadd" | oms(fsub)="fsub" |
      oms(fmul)="fmul" | oms(fdiv)="fdiv" |
      oms(real)="real" | oms(floor)="floor" | oms(logb)="logb" |
      oms(orb)="orb" | oms(andb)="andb" | oms(xorb)="xorb" |
      oms(rshift)="rshift" | oms(lshift)="lshift" ;

  fun cms(ilt)="ilt" | cms(igt)="igt" | cms(ieq)="ieq" |
      cms(ile)="ile" | cms(ige)="ige" | cms(ine)="ine" |
      cms(flt)="flt" | cms(fgt)="fgt" | cms(feq)="feq" |
      cms(fle)="fle" | cms(fge)="fge" | cms(fne)="fne" |
      cms(outofrange)="outofrange" | cms(inrange)="inrange" ;

  (* lms gives the string of the live register list.
   *)
  fun lms(nil)="" |
      lms((h,s)::nil)="("^ims(h)^","^s^")" |
      lms((h,s)::t)="("^ims(h)^","^s^"),"^lms(t);

  (* disp gives the string for the instruction.
   *)
  fun disp(FETCH{immutable=b,offset=ofst,ptr=(p,s1),dst=(d,s2)}) =
      "FETCH{immutable="^bms(b)^",offset="^ims(ofst) ^",ptr=("^ims(p)^","^s1
      ^"),dst=("^ims(d)^","^s2^")}\n"                                      |

      disp(STORE{offset=ofst,src=(s,s1),ptr=(p,s2)}) =
      "STORE{offset="^ims(ofst)^",src=("^ims(s)^","^s1^"),ptr=("
      ^ims(p)^","^s2^")}\n"                                                |

      disp(GETLAB{lab=(l,ls),dst=(d,ds)}) =
      "GETLAB{lab=("^ims(l)^","^ls^"),dst=("^ims(d)^","^ds^")}\n"          |

      disp(GETREAL{value=r,dst=(d,ds)}) =
      "GETREAL{value="^r^",dst=("^ims(d)^","^ds^")}\n"                |

      disp(ARITH{oper=opn,src1=(s1,ss1),src2=(s2,ss2),dst=(d,ds)})=
      "ARITH{oper="^oms(opn)^",src1=("^ims(s1)^","^ss1^"),src2=("^ims(s2)
      ^","^ss2^"),dst=("^ims(d)^","^ds^")}\n"                              |

      disp(ARITHI{oper=opn,src1=(s1,ss1),src2=n,dst=(d,ds)})=
      "ARITH{oper="^oms(opn)^",src1=("^ims(s1)^","^ss1^"),src2="^ims(n)^
      ",dst=("^ims(d)^","^ds^")}\n"                                        |

      disp(MOVE{src=(s,ss),dst=(d,ds)})=
      "MOVE{src=("^ims(s)^","^ss^"),dst=("^ims(d)^","^ds^")}\n"            |

      disp(BRANCH{test=comp,src1=(s1,ss1),src2=(s2,ss2),dst=(labnum,ss3),
                  live=lt})=
      "BRANCH{test="^cms(comp)^",src1=("^ims(s1)^","^ss1^"),src2=("^ims(s2)
      ^","^ss2^"),dst=("^ims(labnum)^","^ss3^"),live=["^lms(lt)^"]}\n"     |

      disp(JUMP{dst=(d,ds),live=lt}) =
      "JUMP{dst=("^ims(d)^","^ds^"),live=["^lms(lt)^"]}\n"                 |

      disp(LABWORD{lab=(l,s)})="LABWORD{lab=("^ims(l)^","^s^")}\n"         |

      disp(LABEL{lab=(l,s),live=lt})=
      "LABEL{lab=("^ims(l)^","^s^"),live=["^lms(lt)^"]}\n"                 |

      disp(WORD{value=n})="WORD{value="^ims(n)^"}\n"                       |

      disp(NOP)="NOP" |
      disp(BOGUS _) = raise Match

      ;

  fun d_pc () =print(disp(hd(!IP)) handle Hd=>"No More Instruction\n");
  fun pc () = (!IP);
  fun pptr () =(List.length(!codes)-List.length(!IP))+1;
  fun breakptr k=let fun goon (LABEL {lab=(l,_),...})=(l<>k) |
                         goon (_)=true
                 in while goon(hd(!IP)) do step()
                 end;
  fun regc n=((!Reg) sub n);
  fun d_r () =let val i=ref 0 in
                (while ( !i < !RegN) do
                   (dispv((!i),(!Reg) sub (!i)); Ref.inc(i) )
                 )
                end;
  fun d_regs (nil)=() |
      d_regs (h::t)=(dispv(h,(!Reg) sub h);d_regs(t));

  fun mcell n=((!Memory) sub n);
  fun d_m (n,m)=let val i=ref n in
                while ( !i <=m) do (dispm(!i,(!Memory) sub !i); Ref.inc(i) )
                end;
  fun d_ms nil =() |
      d_ms (h::t)=(dispm(h,(!Memory) sub h); d_ms(t) );


(* This part for the VLIW mode execution.                                  *)


  val runcount=ref 0 and sizen=ref 0 and flag=ref true;
  exception Simulator_error_1;
  exception Simulator_error_2;
  exception Data_dependency_checked;

  (* member tests whether element a is in a list.
   *)
  fun member(a,nil)=false |
      member(a,h::t)=if (a=h) then true else member(a,t);
  (* hvcom tests whether the intersection of two list isnot nil.
   *)
  fun hvcom(nil,l)=false |
      hvcom(h::t,l)=member(h,l) orelse hvcom(t,l);

  (* gset returns the list of registers refered in a instruction.
     gwset returns the list of the register being written in a instruction.
   *)
  fun gset(FETCH{ptr=(p,_),dst=(d,_),...})=[p,d] |
      gset(STORE{src=(s,_),ptr=(p,_),...})=[s,p] |
      gset(GETLAB{dst=(d,_),...})=[d] |
      gset(GETREAL{dst=(d,_),...})=[d] |
      gset(ARITH{src1=(s1,_),src2=(s2,_),dst=(d,_),...})=[s1,s2,d] |
      gset(ARITHI{src1=(s1,_),dst=(d,_),...})=[s1,d] |
      gset(MOVE{src=(s,_),dst=(d,_)})=[s,d] |
      gset(BRANCH{src1=(s1,_),src2=(s2,_),...})=[s1,s2] |
      gset(JUMP{dst=(d,_),...})=[d] |
      gset(_)=nil ;
  fun gwset(FETCH{dst=(d,_),...})=[d] |
      gwset(GETLAB{dst=(d,_),...})=[d] |
      gwset(GETREAL{dst=(d,_),...})=[d] |
      gwset(ARITH{dst=(d,_),...})=[d] |
      gwset(ARITHI{dst=(d,_),...})=[d] |
      gwset(MOVE{dst=(d,_),...})=[d] |
      gwset(_)=nil ;

  (* fetchcode returns the instruction word which contains the next k
     instruction.  fetchcode3 is used in version 3 of VLIW mode, in which case
     labels within instruction words are OK.
   *)
  fun fetchcode(0)=nil |
      fetchcode(k)=let val h=hd(!IP) in
                     (IP:=tl(!IP);
                      if hvnop(h)
                      then (print "Warning: labels within the instruction word\n";
                            fetchcode(k)
                            )
                      else h::fetchcode(k-1) )
                   end handle Hd=>nil;
  fun fetchcode3(0)=nil |
      fetchcode3(k)=let val h=hd(!IP) in
                     (IP:=tl(!IP);
                      if hvnop(h) then fetchcode3(k)
                                  else h::fetchcode3(k-1) )
                   end handle Hd=>nil;

  (* allnop tests if all instructions left mean no operation.
   *)
  fun allnop(nil)=true |
      allnop(NOP::t)=allnop(t) |
      allnop(_)=false;

  (* nopcut cut the instruction stream in a way that the first half are all
     NOP instruction.
   *)
  fun nopcut(nil)=(nil,nil) |
      nopcut(NOP::t)=let val (l1,l2)=nopcut(t) in (NOP::l1,l2) end |
      nopcut(l)=(nil,l);

  (* cmdd tests the data dependency on memory cells and IP.
   *)
  fun cmdd(_,nil)=false |
      cmdd(wset,STORE{ptr=(p,_),offset=ofst,...}::t)=
        cmdd(addrplus((!Reg) sub p,ofst)::wset,t) |
      cmdd(wset,FETCH{ptr=(p,_),offset=ofst,...}::t)=
        member(addrplus((!Reg) sub p,ofst),wset) orelse cmdd(wset,t) |
      cmdd(wset,BRANCH{...}::t)=if allnop(t) then false else true    |
      cmdd(wset,JUMP{...}::t)=if allnop(t) then false else true      |
      cmdd(wset,h::t)=cmdd(wset,t);

  (* crdd test the data dependency on registers.
   *)
  fun crdd(_,nil)=false |
      crdd(wset,h::t)=if hvcom(gset(h),wset) then true
                      else crdd(gwset(h)@wset,t) ;

  (* check_dd checks whether there is data dependency in instruction stream l.
   *)
  fun check_dd(l)= crdd(nil,l) orelse cmdd(nil,l);

  (* rddcut seperate the longest part of the instruction stream that have no
     data dependency on registers , from the left.
   *)
  fun rddcut(_,nil)= (nil,nil)                                   |
      rddcut(wset,l as (h::t))=
        if hvcom(gset(h),wset) then (nil,l)
        else  let val (l1,l2)=rddcut(gwset(h)@wset,t)
              in (h::l1,l2) end
      ;
  (* mddcut seperate the longest part of the instruction stream that have no data
     dependency on memory cells and IP, from the left.
   *)
  fun mddcut(_,nil)= (nil,nil)                                   |
      mddcut(wset,(h as STORE{ptr=(p,_),offset=ofst,...})::t)=
        let val (l1,l2)=mddcut(addrplus((!Reg) sub p,ofst)::wset,t)
        in (h::l1,l2) end                                        |
      mddcut(wset,(h as FETCH{ptr=(p,_),offset=ofst,...})::t)=
        if member(addrplus((!Reg) sub p,ofst),wset)
        then (nil,h::t)
        else let val (l1,l2)=mddcut(wset,t) in (h::l1,l2) end    |
      mddcut(wset,(h as BRANCH{...})::t)=
        let val (l1,l2)=nopcut(t) in (h::l1,l2) end              |
      mddcut(wset,(h as JUMP{...})::t)=
        let val (l1,l2)=nopcut(t) in (h::l1,l2) end              |
      mddcut(wset,h::t)=
        let val (l1,l2)=mddcut(wset,t) in (h::l1,l2) end
      ;

  (* calcult returns the necessary value list corresponding to a instruction
     stream.  And change the IP when necessary.
   *)
  fun calcult(nil)=nil                                                    |
      calcult(FETCH{ptr=(p,_),offset=ofst,...}::t)=
        content((!Reg) sub p,ofst)::calcult(t)                            |
      calcult(STORE{src=(s,_),...}::t)=((!Reg) sub s )::calcult(t)        |
      calcult(MOVE{src=(s,_),...}::t)=((!Reg) sub s)::calcult(t)          |
      calcult(ARITH{oper=opn,src1=(s1,_),src2=(s2,_),...}::t)=
         getresult(opn,(!Reg) sub s1,(!Reg) sub s2)::calcult(t)           |
      calcult(ARITHI{oper=opn,src1=(s1,_),src2=n1,...}::t)=
         getresult(opn,(!Reg) sub s1,(INT n1))::calcult(t)                |
      calcult(JUMP{dst=(d,_),...}::t)=((!Reg) sub d)::calcult(t)          |
      calcult(h::t)=calcult(t);

  (* dowr does the actual writing operations.
   *)
  fun dowr(nil,nil)=() |
      dowr(nil,h::t)=raise Simulator_error_1                              |
      dowr(FETCH{...}::t,nil)=raise Simulator_error_2                     |
      dowr(STORE{...}::t,nil)=raise Simulator_error_2                     |
      dowr(MOVE{...}::t,nil)=raise Simulator_error_2                      |
      dowr(ARITH{...}::t,nil)=raise Simulator_error_2                     |
      dowr(ARITHI{...}::t,nil)=raise Simulator_error_2                    |
      dowr(JUMP{...}::t,nil)=raise Simulator_error_2                      |
      dowr(FETCH{dst=(d,_),...}::t,vh::vt)=(update((!Reg),d,vh);
                                            dowr(t,vt) )                  |
      dowr(STORE{ptr=(p,_),offset=ofst,...}::t,vh::vt)=
        (update((!Memory),addrplus((!Reg) sub p,ofst),vh); dowr(t,vt) )   |
      dowr(GETLAB{lab=(l,_),dst=(d,_)}::t,vt)=
        (update((!Reg),d,(LABVAL (l,0)) ); dowr(t,vt) )                   |
      dowr(GETREAL{value=v,dst=(d,_)}::t,vt)=
        (update((!Reg),d,(REAL (strToReal v)) ); dowr(t,vt) )                         |
      dowr(MOVE{dst=(d,_),...}::t,vh::vt)=
        (update((!Reg),d,vh); dowr(t,vt) )                                |
      dowr(ARITH{dst=(d,_),...}::t,vh::vt)=
        (update((!Reg),d,vh); dowr(t,vt) )                                |
      dowr(ARITHI{dst=(d,_),...}::t,vh::vt)=
        (update((!Reg),d,vh); dowr(t,vt) )                                |
      dowr(JUMP{...}::t,vh::vt)=
        (execjmp(vh); flag:=false; dowr(t,vt) )                           |
      dowr(BRANCH{test=comp,src1=(s1,_),src2=(s2,_),
                     dst=(labnum,_),...}::t,vt)=
        if compare(comp,(!Reg) sub s1,(!Reg) sub s2)
        then (IP:= !(findjmp_place(labnum)); flag:=false; dowr(t,vt) )
        else dowr(t,vt)                                                          |
      dowr(h::t,vt)=dowr(t,vt)
      ;

  (* vv3 executes an instruction word in version 3 mode.
   *)
  fun vv3(nil)= () |
      vv3(l)=let val (l1,l2)=rddcut(nil,l);
                 val (l3,l4)=mddcut(nil,l1)
             in (flag:=true; dowr(l3,calcult(l3)); Ref.inc(runcount);
                 if (!flag) then vv3(l4@l2) else () )
             end;

  fun vinit(k,l)=(init(l); sizen:=k; runcount:=0 ) ;

  fun vstep1()=let val f=(while hvnop(hd(!IP)) do IP:=tl(!IP))
                         handle Hd=>raise End_of_Program;
                   val codel=fetchcode(!sizen)
               in
                 (dowr(codel,calcult(codel)); Ref.inc(runcount) )
               end;

  fun vstep2()=let val f=(while hvnop(hd(!IP)) do IP:=tl(!IP))
                         handle Hd=>raise End_of_Program;
                   val codel=fetchcode(!sizen)
               in
                 if check_dd(codel)
                 then (print "Data dependency checked in:\n";
                       let fun f(nil)=() |
                               f(h::t)=(print(":"^disp(h)); f(t))
                       in f(codel) end;
                       raise Data_dependency_checked
                       )
                 else (dowr(codel,calcult(codel)); Ref.inc(runcount) )
               end;

  fun vstep3()=let val f=if (!IP)=nil then raise End_of_Program else ();
                   val codel=fetchcode3(!sizen)
               in vv3(codel) end;

  fun vrun1()=(vstep1();vrun1())
              handle End_of_Program =>
                     print("End of program.\nTotal runtime: "
                           ^ims(!runcount)^" steps.\n");
  fun vrun2()=(vstep2(); vrun2())
              handle End_of_Program =>
                     print("End of program.\nTotal runtime: "
                           ^ims(!runcount)^" steps.\n")|
                     Data_dependency_checked=>
                     print "Program halted.\n";
  fun vrun3()=(vstep3(); vrun3())
              handle End_of_Program =>
                     print("End of program.\nTotal runtime: "
                           ^ims(!runcount)^" substeps.\n");

  fun vpc()=let val codel=(!IP) ;
                fun f (_,nil)=() |
                    f (0,_)= () |
                    f (k,h::l)=if k<=0 then ()
                            else (print(disp(h) );
                                  if hvnop(h) then f(k,l)
                                  else f(k-1,l) )
            in f((!sizen),codel) end;


(*  This part for Pipeline mode                                 *)


  exception illegal_jump_within_branchdelay;
  exception illegal_branch_within_branchdelay;
  exception illegal_label_within_branchdelay;
  exception illegal_labword_within_branchdelay;
  exception illegal_word_within_branchdelay;
  (* Rdelay points to the timing array of registers.
   *)
  val Rdelay=ref ( array(0,0) );
  (* clock records run time.  withindelay is a flag used in BRANCH and JUMP delays.
   *)
  val clock=ref 0 and withindelay=ref false;
  val fdelay=ref 1 and ardelay: ((arithop->int) ref)=ref (fn k=>1)
      and jdelay=ref 1;

  (* pexec executes one instruction, increasing the clock when necessary, which
     corresponding to the holding down of instruction streams.
   *)
  fun pexec(FETCH{immutable=_,offset=ofst,ptr=(p,_),dst=(d,_)})=
        (let val t=(!Rdelay) sub p in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,content((!Reg) sub p,ofst) );
         update((!Rdelay),d,(!clock)+(!fdelay));
         Ref.inc(clock)
         )                                                              |
      pexec(STORE{offset=ofst,src=(s,_),ptr=(p,_)})=
        (let val t1=((!Rdelay) sub p) and t2=((!Rdelay) sub s) ;
             val t=Int.max(t1,t2)  in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Memory),addrplus((!Reg) sub p,ofst),(!Reg) sub s);
         Ref.inc(clock)
         )                                                             |
      pexec(GETLAB{lab=(l,_),dst=(d,_)})=
        (update((!Reg),d,(LABVAL (l,0)) );
         Ref.inc(clock)
         )                                                             |
      pexec(GETREAL{value=v,dst=(d,_)})=
        (update((!Reg),d,(REAL (strToReal v)) );
         Ref.inc(clock)
         )                                                             |
      pexec(MOVE{src=(s,_),dst=(d,_)})=
        (let val t=(!Rdelay) sub s in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,(!Reg) sub s);
         Ref.inc(clock)
         )                                                             |
      pexec(ARITH{oper=opn,src1=(s1,_),src2=(s2,_),dst=(d,_)})=
        (let val t1=((!Rdelay) sub s1) and t2=((!Rdelay) sub s2);
             val t=Int.max(t1,t2) in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,getresult(opn,(!Reg) sub s1,(!Reg) sub s2) );
         update((!Rdelay),d,((!ardelay) opn)+(!clock) );
         Ref.inc(clock)
         )                                                             |
      pexec(ARITHI{oper=opn,src1=(s1,_),src2=n1,dst=(d,_)})=
        (let val t=((!Rdelay) sub s1) in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,getresult(opn,(!Reg) sub s1,(INT n1) ) );
         update((!Rdelay),d,((!ardelay) opn)+(!clock) );
         Ref.inc(clock)
         )                                                             |
      pexec(JUMP {dst=(d,_),...})=
        if (!withindelay) then raise illegal_jump_within_branchdelay
        else
        (let val t=((!Rdelay) sub d) in
           if (!clock)<t then clock:=t else ()
         end;
         Ref.inc(clock); withindelay:=true;
         let val i=ref 0 in
           while ((!i)<(!jdelay)) do
             (let val h=hd(!IP) in
                ( pexec(h); Ref.inc(i) )
              end handle Hd=> (i:=(!jdelay) ) ;
              (IP:=tl(!IP)) handle Tl=>()
              )
         end;
         execjmp((!Reg) sub d)
         )                                                             |
      pexec(BRANCH{test=comp,src1=(s1,_),src2=(s2,_),dst=(labnum,_),...})=
        if (!withindelay) then raise illegal_branch_within_branchdelay
        else
        (let val t1=((!Rdelay) sub s1) and t2=((!Rdelay) sub s2);
             val t=Int.max(t1,t2) in
           if (!clock)<t then clock:=t else ()
         end;
         Ref.inc(clock); withindelay:=true;
         let val i=ref 0 in
           while ((!i)<(!jdelay)) do
             (let val h=hd(!IP) in
                ( pexec(h); Ref.inc(i) )
              end handle Hd=> (i:=(!jdelay) ) ;
              (IP:=tl(!IP)) handle Tl=>()
              )
         end;
         if compare(comp,(!Reg) sub s1,(!Reg) sub s2)
         then (IP:= !(findjmp_place(labnum) ) )
         else ()
         )                                                             |
      pexec(NOP)=Ref.inc(clock)                                        |
      pexec(LABEL{...})=if (!withindelay)
                        then raise illegal_label_within_branchdelay
                        else ()                                        |
      pexec(LABWORD{...})=if (!withindelay)
                          then raise illegal_labword_within_branchdelay
                          else ()                                      |
      pexec(WORD{...})=if (!withindelay)
                       then raise illegal_word_within_branchdelay
                       else ()
      ;

  fun pinit(fetchdelay,arithdelay,jumpdelay,l)=
       (init(l);
        Rdelay:=array((!RegN),0);
        clock:=0; fdelay:=fetchdelay;
        ardelay:=arithdelay; jdelay:=jumpdelay );

  fun pstep()=
    let
      val Instruction=(hd(!IP) handle Hd=>raise End_of_Program)
    in (IP:=tl(!IP) handle Tl=>raise End_of_Program;
        withindelay:=false; pexec(Instruction) )
    end;

  fun prun()=(pstep(); prun() ) handle End_of_Program=>
             (print "End of program.\n";
              print("Total time used: "^ims(!clock)^" cycles.\n") );

end
