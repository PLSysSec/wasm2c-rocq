(module
  (func $nothing)

  ;; $names are a convenience of the text format. The binary
  ;; format only has numeric indices; names are erased.
  (func $add (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add)

  (func $add3 (param i32 i32 i32) (result i32)
    (i32.add (i32.add (local.get 0) (local.get 1)) (local.get 2)))

  ;; All locals are declared up front, always zero-initialised,
  ;; and numbered continuing on from the parameters.
  (func $dist_squared (param $x i32) (param $y i32) (result i32)
    (local $xx i32)
    (local $yy i32)
    (local.set $xx (i32.mul (local.get $x) (local.get $x)))
    (local.set $yy (i32.mul (local.get $y) (local.get $y)))
    (i32.add (local.get $xx) (local.get $yy)))

  ;; i32 is 32 bits. Whether those bits mean a
  ;; signed or unsigned number is decided by which instruction
  ;; you pick. div_s and div_u read the same bits differently.
  (func $signed_div (param $a i32) (param $b i32) (result i32)
    (i32.div_s (local.get $a) (local.get $b)))
  (func $unsigned_div (param $a i32) (param $b i32) (result i32)
    (i32.div_u (local.get $a) (local.get $b)))

  ;; Comparisons push an i32 that is 0 or 1.
  (func $is_positive (param $n i32) (result i32)
    (i32.gt_s (local.get $n) (i32.const 0)))

  ;; eqz is the idiomatic "not":
  (func $is_zero (param $n i32) (result i32)
    (i32.eqz (local.get $n)))

  ;; --- FLOATS --------------------------------------------
  (func $average (param $a f64) (param $b f64) (result f64)
    (f64.div (f64.add (local.get $a) (local.get $b)) (f64.const 2)))

  ;; Floats get some ops integers do not: sqrt, abs, ceil,
  ;; floor, nearest, min, max, copysign.
  (func $hypot (param $x f64) (param $y f64) (result f64)
    (f64.sqrt
      (f64.add (f64.mul (local.get $x) (local.get $x))
               (f64.mul (local.get $y) (local.get $y)))))

  ;; conversions are always explicit
  (func $int_to_float (param $n i32) (result f64)
    (f64.convert_i32_s (local.get $n)))

  (func $float_to_int (param $x f64) (result i32)
    (i32.trunc_f64_s (local.get $x)))

  (func $float_to_int_safe (param $x f64) (result i32)
    (i32.trunc_sat_f64_s (local.get $x)))

  (func $narrow (param $n i64) (result i32)
    (i32.wrap_i64 (local.get $n)))

  (func $widen (param $n i32) (result i64)
    (i64.extend_i32_s (local.get $n)))

  ;; --- BIT TWIDDLING -------------------------------------
  (func $popcount (param $n i32) (result i32)
    (i32.popcnt (local.get $n)))
  (func $leading_zeros (param $n i32) (result i32)
    (i32.clz (local.get $n)))
  (func $rotate_left (param $n i32) (param $by i32) (result i32)
    (i32.rotl (local.get $n) (local.get $by)))

  ;; A function body must leave exactly its declared results on
  ;; the stack. drop throws away one value to make that true.
  (func $second_of_two (param $a i32) (param $b i32) (result i32)
    local.get $a
    drop                   ;; without this, two values remain: invalid
    local.get $b)

  ;; Nothing is visible to the host unless exported. The export
  ;; name is a string and need not match the internal $name.
  (export "add" (func $add))
  (export "sum3" (func $add3))
  (export "hypot" (func $hypot))
  (export "popcount" (func $popcount))

  ;; Inline export syntax, which is more common in practice:
  (func (export "triple") (param $n i32) (result i32)
    (i32.mul (local.get $n) (i32.const 3)))

  ;; Start function runs automatically at instantiation, 
  ;; before the host can call anything. Takes no params and 
  ;; returns nothing.
  (func $init)
  (start $init)
)