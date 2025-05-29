(* THIS FILE HAS BEEN GENERATED *)

let external_call (n,args,env) =
  match n with
  | 0 -> caml_identity(args,env)
  | 1 -> caml_print_string(args,env)
  | 2 -> caml_print_int(args,env)
  | 3 -> caml_led_off(args,env)
  | 4 -> caml_led_on(args,env)
  | 5 -> caml_equal(args,env)
  | 6 -> caml_lessthan(args,env)
  | 7 -> caml_greaterthan(args,env)
  | 8 -> caml_greaterequal(args,env)
  | 9 -> caml_lessequal(args,env)
  | 10 -> caml_fresh_oo_id(args,env)
  | 11 -> caml_make_vect(args,env)
  | 12 -> caml_obj_dup(args,env)
  | 13 -> bytes_create(args,env)
  | 14 -> caml_array_get(args,env)
  | 15 -> caml_array_set(args,env)
  | 16 -> caml_array_unsafe_set(args,env)
  | 17 -> caml_array_unsafe_get(args,env)
  | 18 -> caml_array_get_addr(args,env)
  | 19 -> caml_array_set_addr(args,env)
  | 20 -> caml_array_unsafe_get_addr(args,env)
  | 21 -> caml_array_unsafe_set_addr(args,env)
  | 22 -> unsafe_chr(args,env)
  | 23 -> bytes_unsafe_set(args,env)
  | 24 -> unsafe_to_string(args,env)
  | 25 -> caml_ml_string_length(args,env)
  | 26 -> caml_create_bytes(args,env)
  | 27 -> caml_blit_string(args,env)
  | 28 -> caml_string_of_bytes(args,env)
  | 29 -> caml_compare(args,env)
  | 30 -> caml_array_sub(args,env)
  | 31 -> caml_string_get(args,env)
  | 32 -> caml_string_equal(args,env)
  | 33 -> caml_print_newline(args,env)
  | _ -> print_string "unknown primitive"; (val_unit,env)
  ;;


let static code = (GROUP3(STOP()):opcode)^4096 ;;
let static data_rom = ((0,false))^2048 ;;


 let load_code () =
  (code.(0) <- GROUP3(BRANCH(19)));
  (code.(1) <- GROUP4(RESTART()));
  (code.(2) <- GRAB(1));
  (code.(3) <- GROUP1(ACC(),0));
  (code.(4) <- GROUP3(BRANCHIF(false,17)));
  (code.(5) <- GROUP1(ACC(),1));
  (code.(6) <- GROUP1(PUSH(),0));
  (code.(7) <- GROUP1(ACC(),1));
  (code.(8) <- GROUP1(GETFIELD(),1));
  (code.(9) <- GROUP1(PUSH(),0));
  (code.(10) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(11) <- APPLY(true,2,0));
  (code.(12) <- GROUP1(PUSH(),0));
  (code.(13) <- GROUP1(ACC(),1));
  (code.(14) <- GROUP1(GETFIELD(),0));
  (code.(15) <- MAKEBLOCK(false,false,2,0,0));
  (code.(16) <- RETURN(2));
  (code.(17) <- GROUP1(ACC(),1));
  (code.(18) <- RETURN(2));
  (code.(19) <- CLOSUREREC(1,0,362,2));
  (code.(20) <- GROUP1(ACC(),0));
  (code.(21) <- GROUP1(SETGLOBAL(),15));
  (code.(22) <- GROUP3(BRANCH(108)));
  (code.(23) <- GROUP4(RESTART()));
  (code.(24) <- GRAB(1));
  (code.(25) <- GROUP1(ACC(),1));
  (code.(26) <- GROUP3(BRANCHIF(false,35)));
  (code.(27) <- GROUP1(ACC(),1));
  (code.(28) <- GROUP1(GETFIELD(),1));
  (code.(29) <- GROUP1(PUSH(),0));
  (code.(30) <- GROUP1(ACC(),1));
  (code.(31) <- GROUP1(OFFSET(1),0));
  (code.(32) <- GROUP1(PUSH(),0));
  (code.(33) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(34) <- APPTERM(2,4));
  (code.(35) <- GROUP1(ACC(),0));
  (code.(36) <- RETURN(2));
  (code.(37) <- GROUP4(RESTART()));
  (code.(38) <- GRAB(1));
  (code.(39) <- GROUP1(ACC(),0));
  (code.(40) <- GROUP3(BRANCHIF(false,52)));
  (code.(41) <- GROUP1(ACC(),1));
  (code.(42) <- GROUP1(PUSH(),0));
  (code.(43) <- GROUP1(ACC(),1));
  (code.(44) <- GROUP1(GETFIELD(),0));
  (code.(45) <- MAKEBLOCK(false,false,2,0,0));
  (code.(46) <- GROUP1(PUSH(),0));
  (code.(47) <- GROUP1(ACC(),1));
  (code.(48) <- GROUP1(GETFIELD(),1));
  (code.(49) <- GROUP1(PUSH(),0));
  (code.(50) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(51) <- APPTERM(2,4));
  (code.(52) <- GROUP1(ACC(),1));
  (code.(53) <- RETURN(2));
  (code.(54) <- GROUP4(RESTART()));
  (code.(55) <- GRAB(1));
  (code.(56) <- GROUP1(ACC(),1));
  (code.(57) <- GROUP3(BRANCHIF(false,84)));
  (code.(58) <- GROUP1(ACC(),1));
  (code.(59) <- GROUP1(GETFIELD(),1));
  (code.(60) <- GROUP1(PUSH(),0));
  (code.(61) <- GROUP1(ACC(),2));
  (code.(62) <- GROUP1(GETFIELD(),0));
  (code.(63) <- GROUP1(PUSH(),0));
  (code.(64) <- GROUP1(ACC(),0));
  (code.(65) <- GROUP1(PUSH(),0));
  (code.(66) <- GROUP1(ENVACC(),2));
  (code.(67) <- APPLY(true,1,0));
  (code.(68) <- GROUP3(BRANCHIF(false,78)));
  (code.(69) <- GROUP1(ACC(),1));
  (code.(70) <- GROUP1(PUSH(),0));
  (code.(71) <- GROUP1(ACC(),3));
  (code.(72) <- GROUP1(PUSH(),0));
  (code.(73) <- GROUP1(ACC(),2));
  (code.(74) <- MAKEBLOCK(false,false,2,0,0));
  (code.(75) <- GROUP1(PUSH(),0));
  (code.(76) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(77) <- APPTERM(2,6));
  (code.(78) <- GROUP1(ACC(),1));
  (code.(79) <- GROUP1(PUSH(),0));
  (code.(80) <- GROUP1(ACC(),3));
  (code.(81) <- GROUP1(PUSH(),0));
  (code.(82) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(83) <- APPTERM(2,6));
  (code.(84) <- GROUP1(ACC(),0));
  (code.(85) <- GROUP1(PUSH(),0));
  (code.(86) <- GROUP1(ENVACC(),1));
  (code.(87) <- APPTERM(1,3));
  (code.(88) <- GROUP1(ACC(),0));
  (code.(89) <- GROUP1(PUSH(),0));
  (code.(90) <- GROUP1(GETGLOBAL(),12));
  (code.(91) <- CLOSUREREC(1,2,363,55));
  (code.(92) <- GROUP1(CONST(0),0));
  (code.(93) <- GROUP1(PUSH(),0));
  (code.(94) <- GROUP1(ACC(),1));
  (code.(95) <- APPTERM(1,3));
  (code.(96) <- GROUP1(CONST(0),0));
  (code.(97) <- GROUP1(PUSH(),0));
  (code.(98) <- GROUP1(ACC(),1));
  (code.(99) <- GROUP1(PUSH(),0));
  (code.(100) <- GROUP1(GETGLOBAL(),13));
  (code.(101) <- APPTERM(2,3));
  (code.(102) <- GROUP1(ACC(),0));
  (code.(103) <- GROUP1(PUSH(),0));
  (code.(104) <- GROUP1(CONST(0),0));
  (code.(105) <- GROUP1(PUSH(),0));
  (code.(106) <- GROUP1(GETGLOBAL(),14));
  (code.(107) <- APPTERM(2,3));
  (code.(108) <- CLOSUREREC(1,0,364,24));
  (code.(109) <- GROUP1(ACC(),0));
  (code.(110) <- GROUP1(SETGLOBAL(),14));
  (code.(111) <- MAKEBLOCK(false,true,0,closure_tag,102));
  (code.(112) <- GROUP1(SETGLOBAL(),22));
  (code.(113) <- CLOSUREREC(1,0,365,38));
  (code.(114) <- GROUP1(ACC(),0));
  (code.(115) <- GROUP1(SETGLOBAL(),13));
  (code.(116) <- MAKEBLOCK(false,true,0,closure_tag,96));
  (code.(117) <- GROUP1(SETGLOBAL(),12));
  (code.(118) <- MAKEBLOCK(false,true,0,closure_tag,88));
  (code.(119) <- GROUP1(SETGLOBAL(),17));
  (code.(120) <- GROUP1(CONST(0),0));
  (code.(121) <- (CALL(10,false,false,false,false)));
  (code.(122) <- GROUP1(CONST(0),0));
  (code.(123) <- (CALL(10,false,false,false,false)));
  (code.(124) <- GROUP3(BRANCH(318)));
  (code.(125) <- GROUP4(RESTART()));
  (code.(126) <- GRAB(1));
  (code.(127) <- GROUP1(ACC(),1));
  (code.(128) <- GROUP1(PUSH(),0));
  (code.(129) <- GROUP1(ACC(),1));
  (code.(130) <- GROUP2(COMPARE(GT())));
  (code.(131) <- GROUP3(BRANCHIF(false,134)));
  (code.(132) <- GROUP1(CONST(0),0));
  (code.(133) <- RETURN(2));
  (code.(134) <- GROUP1(ACC(),1));
  (code.(135) <- GROUP1(PUSH(),0));
  (code.(136) <- GROUP1(ACC(),1));
  (code.(137) <- GROUP1(OFFSET(1),0));
  (code.(138) <- GROUP1(PUSH(),0));
  (code.(139) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(140) <- APPLY(true,2,0));
  (code.(141) <- GROUP1(PUSH(),0));
  (code.(142) <- GROUP1(ACC(),1));
  (code.(143) <- MAKEBLOCK(false,false,2,0,0));
  (code.(144) <- RETURN(2));
  (code.(145) <- GROUP4(RESTART()));
  (code.(146) <- GRAB(1));
  (code.(147) <- GROUP1(ACC(),1));
  (code.(148) <- GROUP3(BRANCHIF(false,165)));
  (code.(149) <- GROUP1(ACC(),1));
  (code.(150) <- GROUP1(GETFIELD(),1));
  (code.(151) <- GROUP1(PUSH(),0));
  (code.(152) <- GROUP1(ACC(),1));
  (code.(153) <- GROUP1(PUSH(),0));
  (code.(154) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(155) <- APPLY(true,2,0));
  (code.(156) <- GROUP1(PUSH(),0));
  (code.(157) <- GROUP1(ACC(),2));
  (code.(158) <- GROUP1(GETFIELD(),0));
  (code.(159) <- GROUP1(PUSH(),0));
  (code.(160) <- GROUP1(ACC(),2));
  (code.(161) <- APPLY(true,1,0));
  (code.(162) <- GROUP1(PUSH(),0));
  (code.(163) <- GROUP1(GETGLOBAL(),15));
  (code.(164) <- APPTERM(2,4));
  (code.(165) <- RETURN(2));
  (code.(166) <- GROUP4(RESTART()));
  (code.(167) <- GRAB(2));
  (code.(168) <- GROUP1(ACC(),2));
  (code.(169) <- GROUP3(BRANCHIF(false,208)));
  (code.(170) <- GROUP1(ACC(),2));
  (code.(171) <- GROUP1(GETFIELD(),0));
  (code.(172) <- GROUP1(PUSH(),0));
  (code.(173) <- GROUP1(ACC(),0));
  (code.(174) <- GROUP1(PUSH(),0));
  (code.(175) <- GROUP1(ACC(),3));
  (code.(176) <- GROUP2(COMPARE(EQ())));
  (code.(177) <- GROUP1(UNOP(NOT()),0));
  (code.(178) <- GROUP3(BRANCHIF(false,207)));
  (code.(179) <- GROUP1(ACC(),1));
  (code.(180) <- GROUP1(PUSH(),0));
  (code.(181) <- GROUP1(ACC(),1));
  (code.(182) <- GROUP2(ADD()));
  (code.(183) <- GROUP1(PUSH(),0));
  (code.(184) <- GROUP1(ACC(),3));
  (code.(185) <- GROUP2(COMPARE(EQ())));
  (code.(186) <- GROUP1(UNOP(NOT()),0));
  (code.(187) <- GROUP3(BRANCHIF(false,207)));
  (code.(188) <- GROUP1(ACC(),1));
  (code.(189) <- GROUP1(PUSH(),0));
  (code.(190) <- GROUP1(ACC(),1));
  (code.(191) <- GROUP2(SUB()));
  (code.(192) <- GROUP1(PUSH(),0));
  (code.(193) <- GROUP1(ACC(),3));
  (code.(194) <- GROUP2(COMPARE(EQ())));
  (code.(195) <- GROUP1(UNOP(NOT()),0));
  (code.(196) <- GROUP3(BRANCHIF(false,207)));
  (code.(197) <- GROUP1(ACC(),3));
  (code.(198) <- GROUP1(GETFIELD(),1));
  (code.(199) <- GROUP1(PUSH(),0));
  (code.(200) <- GROUP1(ACC(),3));
  (code.(201) <- GROUP1(PUSH(),0));
  (code.(202) <- GROUP1(ACC(),3));
  (code.(203) <- GROUP1(OFFSET(1),0));
  (code.(204) <- GROUP1(PUSH(),0));
  (code.(205) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(206) <- APPTERM(3,7));
  (code.(207) <- RETURN(4));
  (code.(208) <- GROUP1(CONST(1),0));
  (code.(209) <- RETURN(3));
  (code.(210) <- GROUP1(ACC(),0));
  (code.(211) <- GROUP3(BCOMPARE(EQ(),0,222)));
  (code.(212) <- GROUP1(ACC(),0));
  (code.(213) <- GROUP1(OFFSET(-1),0));
  (code.(214) <- GROUP1(PUSH(),0));
  (code.(215) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(216) <- APPLY(true,1,0));
  (code.(217) <- GROUP1(PUSH(),0));
  (code.(218) <- GROUP1(ENVACC(),2));
  (code.(219) <- GROUP1(PUSH(),0));
  (code.(220) <- GROUP1(ENVACC(),1));
  (code.(221) <- APPTERM(2,3));
  (code.(222) <- GROUP1(GETGLOBAL(),16));
  (code.(223) <- RETURN(1));
  (code.(224) <- GROUP1(ENVACC(),1));
  (code.(225) <- GROUP1(PUSH(),0));
  (code.(226) <- GROUP1(ACC(),1));
  (code.(227) <- MAKEBLOCK(false,false,2,0,0));
  (code.(228) <- RETURN(1));
  (code.(229) <- GROUP1(ENVACC(),3));
  (code.(230) <- GROUP1(PUSH(),0));
  (code.(231) <- GROUP1(ACC(),1));
  (code.(232) <- MAKEBLOCK(false,true,1,closure_tag,224));
  (code.(233) <- GROUP1(PUSH(),0));
  (code.(234) <- GROUP1(ENVACC(),1));
  (code.(235) <- APPLY(true,2,0));
  (code.(236) <- GROUP1(PUSH(),0));
  (code.(237) <- GROUP1(ENVACC(),2));
  (code.(238) <- GROUP1(PUSH(),0));
  (code.(239) <- GROUP1(GETGLOBAL(),17));
  (code.(240) <- APPTERM(2,3));
  (code.(241) <- GROUP1(ACC(),0));
  (code.(242) <- GROUP1(PUSH(),0));
  (code.(243) <- GROUP1(GETGLOBAL(),18));
  (code.(244) <- APPLY(true,1,0));
  (code.(245) <- GROUP1(PUSH(),0));
  (code.(246) <- GROUP1(ACC(),0));
  (code.(247) <- GROUP1(PUSH(),0));
  (code.(248) <- GROUP1(GETGLOBAL(),19));
  (code.(249) <- GROUP1(PUSH(),0));
  (code.(250) <- GROUP1(GETGLOBAL(),20));
  (code.(251) <- MAKEBLOCK(false,true,3,closure_tag,229));
  (code.(252) <- GROUP1(PUSH(),0));
  (code.(253) <- GROUP1(ACC(),0));
  (code.(254) <- GROUP1(PUSH(),0));
  (code.(255) <- GROUP1(GETGLOBAL(),21));
  (code.(256) <- CLOSUREREC(1,2,366,210));
  (code.(257) <- GROUP1(ACC(),3));
  (code.(258) <- GROUP1(PUSH(),0));
  (code.(259) <- GROUP1(ACC(),1));
  (code.(260) <- APPLY(true,1,0));
  (code.(261) <- GROUP1(PUSH(),0));
  (code.(262) <- GROUP1(GETGLOBAL(),22));
  (code.(263) <- APPTERM(1,5));
  (code.(264) <- GROUP1(ACC(),0));
  (code.(265) <- GROUP3(BRANCHIF(false,276)));
  (code.(266) <- GROUP1(ACC(),0));
  (code.(267) <- GROUP1(GETFIELD(),1));
  (code.(268) <- GROUP1(PUSH(),0));
  (code.(269) <- GROUP1(ACC(),1));
  (code.(270) <- GROUP1(GETFIELD(),0));
  (code.(271) <- GROUP1(PUSH(),0));
  (code.(272) <- GROUP1(CONST(1),0));
  (code.(273) <- GROUP1(PUSH(),0));
  (code.(274) <- GROUP1(GETGLOBAL(),23));
  (code.(275) <- APPTERM(3,4));
  (code.(276) <- GROUP1(CONST(1),0));
  (code.(277) <- RETURN(1));
  (code.(278) <- GROUP1(ACC(),0));
  (code.(279) <- GROUP1(GETFIELD(),1));
  (code.(280) <- GROUP1(PUSH(),0));
  (code.(281) <- GROUP1(ACC(),1));
  (code.(282) <- GROUP1(GETFIELD(),0));
  (code.(283) <- GROUP1(PUSH(),0));
  (code.(284) <- GROUP1(ACC(),0));
  (code.(285) <- GROUP3(BRANCHIF(false,303)));
  (code.(286) <- GROUP1(ACC(),0));
  (code.(287) <- GROUP1(GETFIELD(),0));
  (code.(288) <- GROUP1(PUSH(),0));
  (code.(289) <- GROUP1(ENVACC(),1));
  (code.(290) <- APPLY(true,1,0));
  (code.(291) <- GROUP1(PUSH(),0));
  (code.(292) <- GROUP1(ACC(),2));
  (code.(293) <- GROUP1(PUSH(),0));
  (code.(294) <- GROUP1(ACC(),1));
  (code.(295) <- MAKEBLOCK(false,false,2,0,0));
  (code.(296) <- GROUP1(PUSH(),0));
  (code.(297) <- GROUP1(ACC(),2));
  (code.(298) <- GROUP1(GETFIELD(),1));
  (code.(299) <- MAKEBLOCK(false,false,2,0,0));
  (code.(300) <- GROUP1(PUSH(),0));
  (code.(301) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(302) <- APPTERM(1,5));
  (code.(303) <- GROUP1(ACC(),1));
  (code.(304) <- GROUP1(PUSH(),0));
  (code.(305) <- GROUP1(GETGLOBAL(),12));
  (code.(306) <- APPTERM(1,4));
  (code.(307) <- GROUP4(RESTART()));
  (code.(308) <- GRAB(1));
  (code.(309) <- GROUP1(ACC(),0));
  (code.(310) <- CLOSUREREC(1,1,367,278));
  (code.(311) <- GROUP1(CONST(0),0));
  (code.(312) <- GROUP1(PUSH(),0));
  (code.(313) <- GROUP1(ACC(),3));
  (code.(314) <- MAKEBLOCK(false,false,2,0,0));
  (code.(315) <- GROUP1(PUSH(),0));
  (code.(316) <- GROUP1(ACC(),1));
  (code.(317) <- APPTERM(1,4));
  (code.(318) <- CLOSUREREC(1,0,368,126));
  (code.(319) <- CLOSUREREC(1,0,369,146));
  (code.(320) <- GROUP1(ACC(),0));
  (code.(321) <- GROUP1(SETGLOBAL(),21));
  (code.(322) <- GROUP1(CONST(1),0));
  (code.(323) <- GROUP1(PUSH(),0));
  (code.(324) <- GROUP1(ACC(),2));
  (code.(325) <- APPLY(true,1,0));
  (code.(326) <- GROUP1(SETGLOBAL(),18));
  (code.(327) <- MAKEBLOCK(false,true,0,closure_tag,308));
  (code.(328) <- GROUP1(SETGLOBAL(),20));
  (code.(329) <- CLOSUREREC(1,0,370,167));
  (code.(330) <- GROUP1(ACC(),0));
  (code.(331) <- GROUP1(SETGLOBAL(),23));
  (code.(332) <- MAKEBLOCK(false,true,0,closure_tag,264));
  (code.(333) <- GROUP1(SETGLOBAL(),19));
  (code.(334) <- MAKEBLOCK(false,true,0,closure_tag,241));
  (code.(335) <- GROUP1(PUSH(),0));
  (code.(336) <- GROUP1(CONST(1),0));
  (code.(337) <- GROUP1(PUSH(),0));
  (code.(338) <- GROUP1(CONST(1),0));
  (code.(339) <- GROUP1(PUSH(),0));
  (code.(340) <- GROUP1(ACC(),1));
  (code.(341) <- GROUP1(PUSH(),0));
  (code.(342) <- GROUP1(PUSH(),0));
  (code.(343) <- GROUP1(ACC(),2));
  (code.(344) <- GROUP2(COMPARE(GT())));
  (code.(345) <- GROUP3(BRANCHIF(true,359)));
  (code.(346) <- GROUP3(CHECK_SIGNALS()));
  (code.(347) <- GROUP1(CONST(7),0));
  (code.(348) <- GROUP1(PUSH(),0));
  (code.(349) <- GROUP1(ACC(),4));
  (code.(350) <- APPLY(true,1,0));
  (code.(351) <- (CALL(2,false,false,false,false)));
  (code.(352) <- GROUP1(ACC(),1));
  (code.(353) <- GROUP1(PUSH(),0));
  (code.(354) <- GROUP1(OFFSET(1),0));
  (code.(355) <- GROUP1(ASSIGN(),2));
  (code.(356) <- GROUP1(ACC(),1));
  (code.(357) <- GROUP2(COMPARE(NEQ())));
  (code.(358) <- GROUP3(BRANCHIF(true,346)));
  (code.(359) <- GROUP1(POP(),2));
  (code.(360) <- GROUP1(POP(),5));
  (code.(361) <- GROUP3(STOP()));
  (code.(362) <- LABEL(2));
  (code.(363) <- LABEL(55));
  (code.(364) <- LABEL(24));
  (code.(365) <- LABEL(38));
  (code.(366) <- LABEL(210));
  (code.(367) <- LABEL(278));
  (code.(368) <- LABEL(126));
  (code.(369) <- LABEL(146));
  (code.(370) <- LABEL(167));
  ()
 ;;

let init_data () = (
let x1 = global_start + 24 in
(* ADD GLOBAL 12 *)
data_rom.(global_start + 12) <- val_long(0);
(* ADD GLOBAL 13 *)
data_rom.(global_start + 13) <- val_long(0);
(* ADD GLOBAL 14 *)
data_rom.(global_start + 14) <- val_long(0);
(* ADD GLOBAL 15 *)
data_rom.(global_start + 15) <- val_long(0);
(* ADD GLOBAL 16 *)
(* ========= *)
data_rom.(x1) <- val_long(make_header(0,2));
let x2 = x1+3 in
data_rom.(x1+1) <- val_long(0);
data_rom.(x1+2) <- val_long(0);
data_rom.(global_start + 16) <- val_ptr(x1);
(* ADD GLOBAL 17 *)
data_rom.(global_start + 17) <- val_long(0);
(* ADD GLOBAL 18 *)
data_rom.(global_start + 18) <- val_long(0);
(* ADD GLOBAL 19 *)
data_rom.(global_start + 19) <- val_long(0);
(* ADD GLOBAL 20 *)
data_rom.(global_start + 20) <- val_long(0);
(* ADD GLOBAL 21 *)
data_rom.(global_start + 21) <- val_long(0);
(* ADD GLOBAL 22 *)
data_rom.(global_start + 22) <- val_long(0);
(* ADD GLOBAL 23 *)
data_rom.(global_start + 23) <- val_long(0);
  for i = 0 to 2047 do
    ram.(i) <- data_rom.(i)
  done ) ;;
let main_load () =
  load_code();
  init_data();
  global_end.(0) <- global_start + 24 ;;

