Some {|
    mod_types :=
        Tf nil nil :: 
        Tf (T_num T_i32 :: T_num T_i32 :: nil)%list (T_num T_i32 :: nil)%list :: 
        Tf (T_num T_i32 :: T_num T_i32 :: T_num T_i32 :: nil)%list (T_num T_i32 :: nil)%list ::
        Tf (T_num T_i32 :: nil)%list (T_num T_i32 :: nil)%list ::
        Tf (T_num T_f64 :: T_num T_f64 :: nil)%list (T_num T_f64 :: nil)%list ::
        Tf (T_num T_i32 :: nil)%list (T_num T_f64 :: nil)%list :: 
        Tf (T_num T_f64 :: nil)%list (T_num T_i32 :: nil)%list :: 
        Tf (T_num T_i64 :: nil)%list (T_num T_i32 :: nil)%list :: 
        Tf (T_num T_i32 :: nil)%list (T_num T_i64 :: nil)%list :: 
        nil;
    mod_funcs :=
        {|
            modfunc_type := BinNums.N0;
            modfunc_locals := nil;
            modfunc_body := nil
        |} :: 
        {|
            modfunc_type := BinNums.Npos BinNums.xH;
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_i32 (Binop_i BOI_add) :: nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xO BinNums.xH);
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_i32 (Binop_i BOI_add) :: 
                BI_local_get (BinNums.Npos (BinNums.xO BinNums.xH)) :: 
                BI_binop T_i32 (Binop_i BOI_add) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos BinNums.xH;
            modfunc_locals := T_num T_i32 :: T_num T_i32 :: nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_local_get BinNums.N0 :: 
                BI_binop T_i32 (Binop_i BOI_mul) :: 
                BI_local_set (BinNums.Npos (BinNums.xO BinNums.xH)) :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_i32 (Binop_i BOI_mul) :: 
                BI_local_set (BinNums.Npos (BinNums.xI BinNums.xH)) :: 
                BI_local_get (BinNums.Npos (BinNums.xO BinNums.xH)) :: 
                BI_local_get (BinNums.Npos (BinNums.xI BinNums.xH)) :: 
                BI_binop T_i32 (Binop_i BOI_add) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos BinNums.xH;
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 ::
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_i32 (Binop_i (BOI_div SX_S)) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos BinNums.xH;
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_i32 (Binop_i (BOI_div SX_U)) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xI BinNums.xH);
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_const_num (VAL_int32 {|
                    Wasm_int.Int32.intval := BinNums.Z0;
                    Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
                |}) :: 
                BI_relop T_i32 (Relop_i (ROI_gt SX_S)) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xI BinNums.xH);
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_testop T_i32 TO_eqz :: nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xO (BinNums.xO BinNums.xH));
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_f64 (Binop_f BOF_add) :: 
                BI_const_num (VAL_float64
                    (Binary.B754_finite
                    (BinNums.Zpos
                    (BinNums.xI
                    (BinNums.xO
                    (BinNums.xI
                    (BinNums.xO
                    (BinNums.xI BinNums.xH)))))
                    )
                    (BinNums.Zpos
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO BinNums.xH))))))))))
                    )
                    false
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO BinNums.xH))))))))))))))))))))))))))))))))))))))))))))))))))))
                    (BinNums.Zneg
                    (BinNums.xI
                    (BinNums.xI
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xI BinNums.xH))))))
                    (Bits.binary_float_of_bits_aux_correct
                    (BinNums.Zpos
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xI
                    (BinNums.xO
                    (BinNums.xI BinNums.xH))))))
                    (BinNums.Zpos
                    (BinNums.xI
                    (BinNums.xI
                    (BinNums.xO BinNums.xH))))
                    eq_refl eq_refl eq_refl
                    (BinNums.Zpos
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO
                    (BinNums.xO BinNums.xH)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: 
                BI_binop T_f64 (Binop_f BOF_div) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xO (BinNums.xO BinNums.xH));
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_local_get BinNums.N0 :: 
                BI_binop T_f64 (Binop_f BOF_mul) :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_f64 (Binop_f BOF_mul) :: 
                BI_binop T_f64 (Binop_f BOF_add) :: 
                BI_unop T_f64 (Unop_f UOF_sqrt) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xI (BinNums.xO BinNums.xH));
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_cvtop T_f64 CVO_convert T_i32 (Some SX_S) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xO (BinNums.xI BinNums.xH));
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_cvtop T_i32 CVO_trunc T_f64 (Some SX_S) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xO (BinNums.xI BinNums.xH));
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_cvtop T_i32 CVO_trunc_sat T_f64 (Some SX_S) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xI (BinNums.xI BinNums.xH));
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_cvtop T_i32 CVO_wrap T_i64 None :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xO (BinNums.xO (BinNums.xO BinNums.xH)));
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_cvtop T_i64 CVO_extend T_i32 (Some SX_S) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xI BinNums.xH);
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_unop T_i32 (Unop_i UOI_popcnt) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xI BinNums.xH);
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_unop T_i32 (Unop_i UOI_clz) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos BinNums.xH;
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                BI_binop T_i32 (Binop_i BOI_rotl) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos BinNums.xH;
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_drop :: 
                BI_local_get (BinNums.Npos BinNums.xH) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.Npos (BinNums.xI BinNums.xH);
            modfunc_locals := nil;
            modfunc_body := (
                BI_local_get BinNums.N0 :: 
                BI_const_num (VAL_int32 {|
                    Wasm_int.Int32.intval := BinNums.Zpos (BinNums.xI BinNums.xH);
                    Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos (BinNums.xI BinNums.xH))
                |}) :: 
                BI_binop T_i32 (Binop_i BOI_mul) :: 
                nil
            )%list
        |} :: 
        {|
            modfunc_type := BinNums.N0;
            modfunc_locals := nil;
            modfunc_body := nil
        |} :: 
        nil;
    mod_tables := nil;
    mod_mems := nil;
    mod_globals := nil;
    mod_elems := nil;
    mod_datas := nil;
    mod_start := Some {|
            modstart_func := BinNums.Npos (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO BinNums.xH))))
    |};
    mod_imports := nil;
    mod_exports := 
        {|
            modexp_name := ("a" :: "d" :: "d" :: nil)%list;
            modexp_desc := MED_func (BinNums.Npos BinNums.xH)
        |} :: 
        {|
            modexp_name := ("s" :: "u" :: "m" :: "3" :: nil)%list;
            modexp_desc :=
            MED_func (BinNums.Npos (BinNums.xO BinNums.xH))
        |} :: 
        {|
            modexp_name := ("h" :: "y" :: "p" :: "o" :: "t" :: nil)%list;
            modexp_desc := MED_func (BinNums.Npos (BinNums.xI (BinNums.xO (BinNums.xO BinNums.xH))))
        |} :: 
        {|
            modexp_name := ("p" :: "o" :: "p" :: "c" :: "o" :: "u" :: "n" :: "t" :: nil)%list;
            modexp_desc := MED_func (BinNums.Npos (BinNums.xI (BinNums.xI (BinNums.xI BinNums.xH))))
        |} :: 
        {|
            modexp_name := ("t" :: "r" :: "i" :: "p" :: "l" :: "e" :: nil)%list;
            modexp_desc := MED_func (BinNums.Npos (BinNums.xI (BinNums.xI (BinNums.xO (BinNums.xO BinNums.xH)))))
        |} :: 
        nil
    |}
: option module