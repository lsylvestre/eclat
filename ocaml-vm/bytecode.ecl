(* THIS FILE HAS BEEN GENERATED *)

let init_data () =
let x1 = global_start in
(* ADD GLOBAL 12 *)
ram[global_start + 12] <- val_long(0);
(* ADD GLOBAL 13 *)
ram[global_start + 13] <- val_long(0);
(* ADD GLOBAL 14 *)
ram[global_start + 14] <- val_long(0);
(* ADD GLOBAL 15 *)
(* ========= *)
ram[x1] <- val_long(make_header(0,2));
let x2 = x1+3 in
ram[x1+1] <- val_long(0);
ram[x1+2] <- val_long(0);
ram[global_start + 15] <- val_ptr(x1);
(* ADD GLOBAL 16 *)
ram[global_start + 16] <- val_long(0);
(* ADD GLOBAL 17 *)
ram[global_start + 17] <- val_long(0);
(* ADD GLOBAL 18 *)
ram[global_start + 18] <- val_long(0);
(* ADD GLOBAL 19 *)
ram[global_start + 19] <- val_long(0);
(* ADD GLOBAL 20 *)
ram[global_start + 20] <- val_long(0);
(* ADD GLOBAL 21 *)
ram[global_start + 21] <- val_long(0);
(* ADD GLOBAL 22 *)
ram[global_start + 22] <- val_long(0);
  global_end[0] <- global_start + 23
 ;;

let external_call (n,args,env) =
  match n with
  | 0 -> caml_print_int(args,env)
  | _ -> print_string "unknown primitive"; (val_unit,env)
  end ;;


let static code = (0:long)^424 ;;

let load_code () =
  code[0] <- 84;
  code[1] <- 21;
  code[2] <- 41;
  code[3] <- 42;
  code[4] <- 1;
  code[5] <- 0;
  code[6] <- 86;
  code[7] <- 12;
  code[8] <- 1;
  code[9] <- 11;
  code[10] <- 68;
  code[11] <- 50;
  code[12] <- 34;
  code[13] <- 11;
  code[14] <- 67;
  code[15] <- 64;
  code[16] <- 0;
  code[17] <- 40;
  code[18] <- 2;
  code[19] <- 1;
  code[20] <- 40;
  code[21] <- 2;
  code[22] <- 44;
  code[23] <- 1;
  code[24] <- 0;
  code[25] <- -22;
  code[26] <- 0;
  code[27] <- 57;
  code[28] <- 14;
  code[29] <- 84;
  code[30] <- 99;
  code[31] <- 41;
  code[32] <- 42;
  code[33] <- 1;
  code[34] <- 0;
  code[35] <- 86;
  code[36] <- 11;
  code[37] <- 1;
  code[38] <- 11;
  code[39] <- 67;
  code[40] <- 64;
  code[41] <- 0;
  code[42] <- 11;
  code[43] <- 68;
  code[44] <- 50;
  code[45] <- 38;
  code[46] <- 4;
  code[47] <- 1;
  code[48] <- 40;
  code[49] <- 2;
  code[50] <- 41;
  code[51] <- 42;
  code[52] <- 1;
  code[53] <- 0;
  code[54] <- 86;
  code[55] <- 23;
  code[56] <- 0;
  code[57] <- 68;
  code[58] <- 11;
  code[59] <- 67;
  code[60] <- 10;
  code[61] <- 28;
  code[62] <- 33;
  code[63] <- 86;
  code[64] <- 9;
  code[65] <- 3;
  code[66] <- 11;
  code[67] <- 64;
  code[68] <- 0;
  code[69] <- 12;
  code[70] <- 50;
  code[71] <- 38;
  code[72] <- 6;
  code[73] <- 3;
  code[74] <- 12;
  code[75] <- 50;
  code[76] <- 38;
  code[77] <- 6;
  code[78] <- 1;
  code[79] <- 27;
  code[80] <- 37;
  code[81] <- 3;
  code[82] <- 41;
  code[83] <- 42;
  code[84] <- 1;
  code[85] <- 0;
  code[86] <- 54;
  code[87] <- 12;
  code[88] <- 44;
  code[89] <- 1;
  code[90] <- 2;
  code[91] <- -40;
  code[92] <- 99;
  code[93] <- 13;
  code[94] <- 12;
  code[95] <- 38;
  code[96] <- 5;
  code[97] <- 41;
  code[98] <- 42;
  code[99] <- 1;
  code[100] <- 0;
  code[101] <- 86;
  code[102] <- 9;
  code[103] <- 1;
  code[104] <- 127;
  code[105] <- 1;
  code[106] <- 11;
  code[107] <- 68;
  code[108] <- 50;
  code[109] <- 38;
  code[110] <- 4;
  code[111] <- 1;
  code[112] <- 40;
  code[113] <- 2;
  code[114] <- 44;
  code[115] <- 1;
  code[116] <- 0;
  code[117] <- -19;
  code[118] <- 99;
  code[119] <- 12;
  code[120] <- 12;
  code[121] <- 38;
  code[122] <- 4;
  code[123] <- 99;
  code[124] <- 11;
  code[125] <- 54;
  code[126] <- 13;
  code[127] <- 38;
  code[128] <- 3;
  code[129] <- 44;
  code[130] <- 1;
  code[131] <- 0;
  code[132] <- -100;
  code[133] <- 0;
  code[134] <- 57;
  code[135] <- 13;
  code[136] <- 43;
  code[137] <- 0;
  code[138] <- -15;
  code[139] <- 57;
  code[140] <- 12;
  code[141] <- 43;
  code[142] <- 0;
  code[143] <- -29;
  code[144] <- 57;
  code[145] <- 21;
  code[146] <- 43;
  code[147] <- 0;
  code[148] <- -65;
  code[149] <- 57;
  code[150] <- 16;
  code[151] <- 84;
  code[152] <- 206;
  code[153] <- 41;
  code[154] <- 42;
  code[155] <- 1;
  code[156] <- 1;
  code[157] <- 11;
  code[158] <- 125;
  code[159] <- 86;
  code[160] <- 4;
  code[161] <- 99;
  code[162] <- 40;
  code[163] <- 2;
  code[164] <- 1;
  code[165] <- 11;
  code[166] <- 127;
  code[167] <- 1;
  code[168] <- 50;
  code[169] <- 34;
  code[170] <- 11;
  code[171] <- 64;
  code[172] <- 0;
  code[173] <- 40;
  code[174] <- 2;
  code[175] <- 41;
  code[176] <- 42;
  code[177] <- 1;
  code[178] <- 1;
  code[179] <- 86;
  code[180] <- 14;
  code[181] <- 1;
  code[182] <- 68;
  code[183] <- 11;
  code[184] <- 50;
  code[185] <- 34;
  code[186] <- 12;
  code[187] <- 67;
  code[188] <- 12;
  code[189] <- 33;
  code[190] <- 54;
  code[191] <- 14;
  code[192] <- 38;
  code[193] <- 4;
  code[194] <- 40;
  code[195] <- 2;
  code[196] <- 41;
  code[197] <- 42;
  code[198] <- 2;
  code[199] <- 2;
  code[200] <- 86;
  code[201] <- 36;
  code[202] <- 2;
  code[203] <- 67;
  code[204] <- 10;
  code[205] <- 13;
  code[206] <- 121;
  code[207] <- 88;
  code[208] <- 86;
  code[209] <- 26;
  code[210] <- 1;
  code[211] <- 11;
  code[212] <- 110;
  code[213] <- 13;
  code[214] <- 121;
  code[215] <- 88;
  code[216] <- 86;
  code[217] <- 18;
  code[218] <- 1;
  code[219] <- 11;
  code[220] <- 111;
  code[221] <- 13;
  code[222] <- 121;
  code[223] <- 88;
  code[224] <- 86;
  code[225] <- 10;
  code[226] <- 3;
  code[227] <- 68;
  code[228] <- 13;
  code[229] <- 13;
  code[230] <- 127;
  code[231] <- 1;
  code[232] <- 50;
  code[233] <- 39;
  code[234] <- 7;
  code[235] <- 40;
  code[236] <- 4;
  code[237] <- 100;
  code[238] <- 40;
  code[239] <- 3;
  code[240] <- 0;
  code[241] <- 131;
  code[242] <- 0;
  code[243] <- 10;
  code[244] <- 0;
  code[245] <- 127;
  code[246] <- -1;
  code[247] <- 50;
  code[248] <- 33;
  code[249] <- 28;
  code[250] <- 27;
  code[251] <- 38;
  code[252] <- 3;
  code[253] <- 53;
  code[254] <- 15;
  code[255] <- 40;
  code[256] <- 1;
  code[257] <- 22;
  code[258] <- 11;
  code[259] <- 64;
  code[260] <- 0;
  code[261] <- 40;
  code[262] <- 1;
  code[263] <- 24;
  code[264] <- 11;
  code[265] <- 43;
  code[266] <- 1;
  code[267] <- -10;
  code[268] <- 27;
  code[269] <- 34;
  code[270] <- 28;
  code[271] <- 54;
  code[272] <- 16;
  code[273] <- 38;
  code[274] <- 3;
  code[275] <- 0;
  code[276] <- 54;
  code[277] <- 17;
  code[278] <- 33;
  code[279] <- 10;
  code[280] <- 54;
  code[281] <- 18;
  code[282] <- 54;
  code[283] <- 19;
  code[284] <- 43;
  code[285] <- 3;
  code[286] <- -23;
  code[287] <- 10;
  code[288] <- 54;
  code[289] <- 20;
  code[290] <- 44;
  code[291] <- 1;
  code[292] <- 2;
  code[293] <- -53;
  code[294] <- 3;
  code[295] <- 11;
  code[296] <- 33;
  code[297] <- 54;
  code[298] <- 21;
  code[299] <- 37;
  code[300] <- 5;
  code[301] <- 0;
  code[302] <- 86;
  code[303] <- 10;
  code[304] <- 0;
  code[305] <- 68;
  code[306] <- 11;
  code[307] <- 67;
  code[308] <- 105;
  code[309] <- 54;
  code[310] <- 22;
  code[311] <- 39;
  code[312] <- 4;
  code[313] <- 100;
  code[314] <- 40;
  code[315] <- 1;
  code[316] <- 0;
  code[317] <- 68;
  code[318] <- 11;
  code[319] <- 67;
  code[320] <- 10;
  code[321] <- 86;
  code[322] <- 16;
  code[323] <- 0;
  code[324] <- 67;
  code[325] <- 27;
  code[326] <- 33;
  code[327] <- 12;
  code[328] <- 11;
  code[329] <- 64;
  code[330] <- 0;
  code[331] <- 12;
  code[332] <- 68;
  code[333] <- 64;
  code[334] <- 0;
  code[335] <- 50;
  code[336] <- 37;
  code[337] <- 5;
  code[338] <- 1;
  code[339] <- 54;
  code[340] <- 12;
  code[341] <- 37;
  code[342] <- 4;
  code[343] <- 41;
  code[344] <- 42;
  code[345] <- 1;
  code[346] <- 0;
  code[347] <- 44;
  code[348] <- 1;
  code[349] <- 1;
  code[350] <- -34;
  code[351] <- 99;
  code[352] <- 13;
  code[353] <- 64;
  code[354] <- 0;
  code[355] <- 11;
  code[356] <- 37;
  code[357] <- 4;
  code[358] <- 44;
  code[359] <- 1;
  code[360] <- 0;
  code[361] <- -207;
  code[362] <- 44;
  code[363] <- 1;
  code[364] <- 0;
  code[365] <- -189;
  code[366] <- 0;
  code[367] <- 57;
  code[368] <- 20;
  code[369] <- 100;
  code[370] <- 12;
  code[371] <- 33;
  code[372] <- 57;
  code[373] <- 17;
  code[374] <- 43;
  code[375] <- 0;
  code[376] <- -32;
  code[377] <- 57;
  code[378] <- 19;
  code[379] <- 44;
  code[380] <- 1;
  code[381] <- 0;
  code[382] <- -185;
  code[383] <- 0;
  code[384] <- 57;
  code[385] <- 22;
  code[386] <- 43;
  code[387] <- 0;
  code[388] <- -87;
  code[389] <- 57;
  code[390] <- 18;
  code[391] <- 43;
  code[392] <- 0;
  code[393] <- -118;
  code[394] <- 105;
  code[395] <- 105;
  code[396] <- 11;
  code[397] <- 9;
  code[398] <- 12;
  code[399] <- 125;
  code[400] <- 85;
  code[401] <- 18;
  code[402] <- 92;
  code[403] <- 103;
  code[404] <- 7;
  code[405] <- 14;
  code[406] <- 33;
  code[407] <- 93;
  code[408] <- 0;
  code[409] <- 1;
  code[410] <- 9;
  code[411] <- 127;
  code[412] <- 1;
  code[413] <- 20;
  code[414] <- 2;
  code[415] <- 1;
  code[416] <- 122;
  code[417] <- 85;
  code[418] <- -16;
  code[419] <- 19;
  code[420] <- 2;
  code[421] <- 19;
  code[422] <- 5;
  code[423] <- 143;
  ()
 ;;

