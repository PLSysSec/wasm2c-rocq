Some {|
  mod_types :=
    Tf (T_num T_i32 :: nil)%list (T_num T_i32 :: nil)%list :: 
    Tf (T_num T_i32 :: T_num T_i32 :: nil)%list (T_num T_i32 :: nil)%list :: 
    Tf nil (T_num T_i32 :: nil)%list :: 
    nil;
  mod_funcs := 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := nil;
    modfunc_body := (
      BI_local_get BinNums.N0 :: 
      BI_const_num (VAL_int32 {|
        Wasm_int.Int32.intval := BinNums.Z0;
        Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
      |}) :: 
      BI_relop T_i32 (Relop_i (ROI_lt SX_S)) :: 
      BI_if (BT_valtype (Some (T_num T_i32))) 
      (
        BI_const_num (VAL_int32 {|
          Wasm_int.Int32.intval := BinNums.Z0;
          Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
        |}) :: 
        BI_local_get BinNums.N0 :: 
        BI_binop T_i32 (Binop_i BOI_sub) :: 
        nil
      )
      (
        BI_local_get BinNums.N0 :: 
        nil
      ) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := T_num T_i32 :: nil;
    modfunc_body := (
      BI_local_get BinNums.N0 :: 
      BI_local_set (BinNums.Npos BinNums.xH) :: 
      BI_local_get BinNums.N0 :: 
      BI_const_num (VAL_int32 {|
        Wasm_int.Int32.intval := BinNums.Z0;
        Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
      |}) :: 
      BI_relop T_i32 (Relop_i (ROI_lt SX_S)) :: 
      BI_if (BT_valtype None)
      (
        BI_const_num (VAL_int32 {|
          Wasm_int.Int32.intval := BinNums.Z0;
          Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
        |}) :: 
        BI_local_set (BinNums.Npos BinNums.xH) :: 
        nil
      )
      nil :: 
      BI_local_get (BinNums.Npos BinNums.xH) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := nil;
    modfunc_body := (
      BI_local_get BinNums.N0 :: 
      BI_const_num (VAL_int32 {|
        Wasm_int.Int32.intval := BinNums.Z0;
        Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
      |}) :: 
      BI_relop T_i32 (Relop_i (ROI_lt SX_S)) :: 
      BI_if (BT_valtype (Some (T_num T_i32)))
      (
        BI_const_num (VAL_int32 {|
          Wasm_int.Int32.intval := BinNums.Z0;
          Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
        |}) ::
        BI_local_get BinNums.N0 :: 
        BI_binop T_i32 (Binop_i BOI_sub) :: 
        nil
      )
      (BI_local_get BinNums.N0 :: nil) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.Npos BinNums.xH;
    modfunc_locals := nil;
    modfunc_body := (
      BI_local_get BinNums.N0 :: 
      BI_local_get (BinNums.Npos BinNums.xH) :: 
      BI_local_get BinNums.N0 :: 
      BI_local_get (BinNums.Npos BinNums.xH) :: 
      BI_relop T_i32 (Relop_i (ROI_gt SX_S)) :: 
      BI_select None :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := nil;
    modfunc_body := (
      BI_block (BT_valtype (Some (T_num T_i32)))
      (
        BI_local_get BinNums.N0 :: 
        BI_const_num (VAL_int32 {|
          Wasm_int.Int32.intval := BinNums.Z0;
          Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
        |}) :: 
        BI_relop T_i32 (Relop_i (ROI_lt SX_S)) :: 
        BI_if (BT_valtype None)
        (
          BI_const_num (VAL_int32 {|
            Wasm_int.Int32.intval :=
              BinNums.Zpos
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI
              (BinNums.xI BinNums.xH)))))))))))))))))))))))))))))));
              Wasm_int.Int32.intrange :=
              Wasm_int.Int32.Z_mod_modulus_range'
              (BinNums.Zneg BinNums.xH)
          |}) :: 
          BI_br (BinNums.Npos BinNums.xH) :: 
          nil
        )
        nil :: 
        BI_local_get BinNums.N0 :: 
        BI_testop T_i32 TO_eqz :: 
        BI_if  (BT_valtype None)
        (
          BI_const_num (VAL_int32 {|
            Wasm_int.Int32.intval := BinNums.Z0;
            Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
          |}) :: 
          BI_br (BinNums.Npos BinNums.xH) :: 
          nil
        ) 
        nil :: 
        BI_const_num (VAL_int32 {|
          Wasm_int.Int32.intval := BinNums.Zpos BinNums.xH;
          Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos BinNums.xH)
        |}) :: 
        nil
      ) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := T_num T_i32 :: T_num T_i32 :: nil;
    modfunc_body := (
      BI_block (BT_valtype None)
      (
        BI_loop (BT_valtype None)
        (
          BI_local_get (BinNums.Npos BinNums.xH) :: 
          BI_local_get BinNums.N0 :: 
          BI_relop T_i32 (Relop_i (ROI_gt SX_S)) :: 
          BI_br_if (BinNums.Npos BinNums.xH) :: 
          BI_local_get (BinNums.Npos (BinNums.xO BinNums.xH)) :: 
          BI_local_get (BinNums.Npos BinNums.xH) :: 
          BI_binop T_i32 (Binop_i BOI_add) :: 
          BI_local_set (BinNums.Npos (BinNums.xO BinNums.xH)) :: 
          BI_local_get (BinNums.Npos BinNums.xH) :: 
          BI_const_num (VAL_int32 {|
            Wasm_int.Int32.intval := BinNums.Zpos BinNums.xH;
            Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos BinNums.xH)
          |}) :: 
          BI_binop T_i32 (Binop_i BOI_add) :: 
          BI_local_set (BinNums.Npos BinNums.xH) :: 
          BI_br BinNums.N0 :: 
          nil
        ) :: 
        nil
      ) :: 
      BI_local_get (BinNums.Npos (BinNums.xO BinNums.xH)) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := T_num T_i32 :: nil;
    modfunc_body := (
      BI_const_num (VAL_int32 {|
        Wasm_int.Int32.intval := BinNums.Zpos BinNums.xH;
        Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos BinNums.xH)
      |}) :: 
      BI_local_set (BinNums.Npos BinNums.xH) :: 
      BI_local_get BinNums.N0 :: 
      BI_const_num (VAL_int32 {|
        Wasm_int.Int32.intval := BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO BinNums.xH)));
        Wasm_int.Int32.intrange :=
          Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO BinNums.xH))))
      |}) :: 
      BI_binop T_i32 (Binop_i (BOI_div SX_U)) :: 
      BI_local_set BinNums.N0 :: 
      BI_loop (BT_valtype None)
      (
        BI_local_get BinNums.N0 :: 
        BI_if (BT_valtype None)
        (
          BI_local_get (BinNums.Npos BinNums.xH) :: 
          BI_const_num (VAL_int32 {|
            Wasm_int.Int32.intval := BinNums.Zpos BinNums.xH;
            Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos BinNums.xH)
          |}) :: 
          BI_binop T_i32 (Binop_i BOI_add) :: 
          BI_local_set (BinNums.Npos BinNums.xH) :: 
          BI_local_get BinNums.N0 :: 
          BI_const_num (VAL_int32 {|
            Wasm_int.Int32.intval := BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO BinNums.xH)));
            Wasm_int.Int32.intrange :=
              Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO BinNums.xH))))
          |}) :: 
          BI_binop T_i32 (Binop_i (BOI_div SX_U)) :: 
          BI_local_set BinNums.N0 :: 
          BI_br (BinNums.Npos BinNums.xH) :: 
          nil
        )
        nil :: 
        nil
      ) :: 
      BI_local_get (BinNums.Npos BinNums.xH) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.Npos (BinNums.xO BinNums.xH);
    modfunc_locals := T_num T_i32 :: nil;
    modfunc_body := (
      BI_block (BT_valtype None)
      (
        BI_loop (BT_valtype None)
        (
          BI_block (BT_valtype None)
          (
            BI_loop (BT_valtype None)
            (
              BI_local_get BinNums.N0 :: 
              BI_const_num (VAL_int32 {|
                Wasm_int.Int32.intval :=
                  BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH)))));
                Wasm_int.Int32.intrange :=
                  Wasm_int.Int32.Z_mod_modulus_range'
                  (BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH))))))
              |}) :: 
              BI_relop T_i32 (Relop_i (ROI_gt SX_S)) :: 
              BI_br_if (BinNums.Npos (BinNums.xI BinNums.xH)) :: 
              BI_local_get BinNums.N0 :: 
              BI_const_num (VAL_int32 {|
                Wasm_int.Int32.intval := BinNums.Zpos (BinNums.xI (BinNums.xI BinNums.xH));
                Wasm_int.Int32.intrange :=
                  Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos (BinNums.xI (BinNums.xI BinNums.xH)))
              |}) :: 
              BI_binop T_i32 (Binop_i BOI_add) :: 
              BI_local_set BinNums.N0 :: 
              BI_br BinNums.N0 :: 
              nil
            ) :: 
            nil
          ) :: 
          BI_br BinNums.N0 :: 
          nil
        ) :: 
        nil
      ) :: 
      BI_local_get BinNums.N0 :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.Npos (BinNums.xO BinNums.xH);
    modfunc_locals := T_num T_i32 :: nil;
    modfunc_body := (
      BI_block (BT_valtype None)
      (
        BI_loop (BT_valtype None)
        (
          BI_block (BT_valtype None)
          (
            BI_loop (BT_valtype None)
            (
              BI_local_get BinNums.N0 :: 
              BI_const_num (VAL_int32 {|
                Wasm_int.Int32.intval :=
                  BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH)))));
                Wasm_int.Int32.intrange :=
                  Wasm_int.Int32.Z_mod_modulus_range'
                  (BinNums.Zpos (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH))))))
              |}) :: 
              BI_relop T_i32 (Relop_i (ROI_gt SX_S)) :: 
              BI_br_if (BinNums.Npos (BinNums.xI BinNums.xH)) :: 
              BI_local_get BinNums.N0 :: 
              BI_const_num (VAL_int32 {|
                Wasm_int.Int32.intval := BinNums.Zpos (BinNums.xI (BinNums.xI BinNums.xH));
                Wasm_int.Int32.intrange :=
                  Wasm_int.Int32.Z_mod_modulus_range' (BinNums.Zpos (BinNums.xI (BinNums.xI BinNums.xH)))
              |}) :: 
              BI_binop T_i32 (Binop_i BOI_add) :: 
              BI_local_set BinNums.N0 :: 
              BI_br BinNums.N0 :: 
              nil
            ) :: 
            nil
          ) :: 
          BI_br BinNums.N0 :: 
          nil
        ) :: 
        nil
      ) :: 
      BI_local_get BinNums.N0 :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := nil;
    modfunc_body := (
      BI_block (BT_valtype None)
      (
        BI_block (BT_valtype None)
        (
          BI_block (BT_valtype None)
          (
            BI_block (BT_valtype None)
            (
              BI_block (BT_valtype None)
              (
                BI_local_get BinNums.N0 :: 
                BI_const_num (VAL_int32 {|
                  Wasm_int.Int32.intval :=
                    BinNums.Zpos (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH))))));
                  Wasm_int.Int32.intrange :=
                    Wasm_int.Int32.Z_mod_modulus_range'
                    (BinNums.Zpos (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH)))))))
                |}) :: 
                BI_binop T_i32 (Binop_i (BOI_div SX_U)) :: 
                BI_br_table (
                  BinNums.Npos (BinNums.xO (BinNums.xO BinNums.xH)) :: 
                  BinNums.Npos (BinNums.xO (BinNums.xO BinNums.xH)) :: 
                  BinNums.N0 :: 
                  BinNums.Npos BinNums.xH :: 
                  BinNums.Npos (BinNums.xO BinNums.xH) :: 
                  BinNums.Npos (BinNums.xI BinNums.xH) :: 
                  nil
                ) (BinNums.Npos (BinNums.xO (BinNums.xO BinNums.xH))) :: 
                nil
              ) :: 
              BI_const_num (VAL_int32 {|
                Wasm_int.Int32.intval :=
                  BinNums.Zpos
                  (BinNums.xO (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH)))))));
                Wasm_int.Int32.intrange :=
                  Wasm_int.Int32.Z_mod_modulus_range'
                  (BinNums.Zpos
                  (BinNums.xO (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH))))))))
              |}) :: 
              BI_return :: 
              nil
            ) :: 
            BI_const_num (VAL_int32 {|
              Wasm_int.Int32.intval :=
                BinNums.Zpos
                (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xI (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO BinNums.xH))))))));
              Wasm_int.Int32.intrange :=
                Wasm_int.Int32.Z_mod_modulus_range'
                (BinNums.Zpos
                (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xI (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO BinNums.xH)))))))))
            |}) :: 
            BI_return :: 
            nil
          ) :: 
          BI_const_num (VAL_int32 {|
            Wasm_int.Int32.intval :=
              BinNums.Zpos
              (BinNums.xO (BinNums.xO (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH))))))));
            Wasm_int.Int32.intrange :=
              Wasm_int.Int32.Z_mod_modulus_range'
              (BinNums.Zpos
              (BinNums.xO (BinNums.xO (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH)))))))))
          |}) :: 
          BI_return :: 
          nil
        ) :: 
        BI_const_num (VAL_int32 {|
          Wasm_int.Int32.intval :=
            BinNums.Zpos
            (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xI (BinNums.xI (BinNums.xI (BinNums.xI BinNums.xH))))))));
          Wasm_int.Int32.intrange :=
            Wasm_int.Int32.Z_mod_modulus_range'
            (BinNums.Zpos
            (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xI (BinNums.xI (BinNums.xI (BinNums.xI BinNums.xH)))))))))
        |}) :: 
        BI_return :: 
        nil
      ) :: 
      BI_const_num (VAL_int32 {|
        Wasm_int.Int32.intval := BinNums.Z0;
        Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
      |}) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.Npos BinNums.xH;
    modfunc_locals := nil;
    modfunc_body := (
      BI_local_get (BinNums.Npos BinNums.xH) :: 
      BI_testop T_i32 TO_eqz :: 
      BI_if (BT_valtype None)
      (
        BI_const_num (VAL_int32 {|
          Wasm_int.Int32.intval := BinNums.Z0;
          Wasm_int.Int32.intrange := Wasm_int.Int32.Z_mod_modulus_range' BinNums.Z0
        |}) :: 
        BI_return :: 
        nil
      )
      nil :: 
      BI_local_get BinNums.N0 :: 
      BI_local_get (BinNums.Npos BinNums.xH) :: 
      BI_binop T_i32 (Binop_i (BOI_div SX_S)) :: 
      nil
    )%list
  |} :: 
  {|
    modfunc_type := BinNums.N0;
    modfunc_locals := nil;
    modfunc_body := (
      BI_local_get BinNums.N0 :: 
      BI_const_num (VAL_int32 {|
        Wasm_int.Int32.intval :=
          BinNums.Zpos (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH))))));
        Wasm_int.Int32.intrange :=
          Wasm_int.Int32.Z_mod_modulus_range'
          (BinNums.Zpos (BinNums.xO (BinNums.xO (BinNums.xI (BinNums.xO (BinNums.xO (BinNums.xI BinNums.xH)))))))
      |}) :: 
      BI_relop T_i32 (Relop_i (ROI_gt SX_U)) :: 
      BI_if (BT_valtype None)
      (
        BI_unreachable :: 
        nil
      )
      nil :: 
      BI_local_get BinNums.N0 :: 
      nil
    )%list
  |} :: 
  nil;
  mod_tables := nil;
  mod_mems := nil;
  mod_globals := nil;
  mod_elems := nil;
  mod_datas := nil;
  mod_start := None;
  mod_imports := nil;
  mod_exports :=
    {|
      modexp_name := ("s" :: "u" :: "m" :: "_" :: "t" :: "o" :: nil)%list;
      modexp_desc := MED_func (BinNums.Npos (BinNums.xI (BinNums.xO BinNums.xH)))
    |} :: 
    {|
      modexp_name :=
        ("c" :: "o" :: "u" :: "n" :: "t" :: "_" :: "d" :: "i" :: "g" :: "i" :: "t" :: "s" :: nil)%list;
      modexp_desc := MED_func (BinNums.Npos (BinNums.xO (BinNums.xI BinNums.xH)))
    |} :: 
    {|
      modexp_name :=
        ("h" :: "t" :: "t" :: "p" :: "_" :: "c" :: "a" :: "t" :: "e" :: "g" :: "o" :: "r" :: "y" :: nil)%list;
      modexp_desc := MED_func (BinNums.Npos (BinNums.xI (BinNums.xO (BinNums.xO BinNums.xH))))
    |} :: 
    {|
      modexp_name := ("s" :: "a" :: "f" :: "e" :: "_" :: "d" :: "i" :: "v" :: nil)%list;
      modexp_desc := MED_func (BinNums.Npos (BinNums.xO (BinNums.xI (BinNums.xO BinNums.xH))))
    |} :: 
    {|
      modexp_name :=
        ("m" :: "u" :: "s" :: "t" :: "_" :: "b" :: "e" :: "_" :: "s" :: "m" :: "a" :: "l" :: "l" :: nil)%list;
      modexp_desc := MED_func (BinNums.Npos (BinNums.xI (BinNums.xI (BinNums.xO BinNums.xH))))
    |} :: 
    nil
|}
: option module
