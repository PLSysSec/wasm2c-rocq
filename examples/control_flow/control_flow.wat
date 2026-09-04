(module
  ;; Condition is an i32. Zero is false, anything else true.
  (func $abs (param $n i32) (result i32)
    (if (result i32) (i32.lt_s (local.get $n) (i32.const 0))
      (then (i32.sub (i32.const 0) (local.get $n)))
      (else (local.get $n))))

  ;; An if with no result used purely for its side effect.
  (func $clamp_negative_to_zero (param $n i32) (result i32)
    (local $out i32)
    (local.set $out (local.get $n))
    (if (i32.lt_s (local.get $n) (i32.const 0))
      (then (local.set $out (i32.const 0))))
    (local.get $out))

  ;; The same abs written flat.
  (func $abs_flat (param $n i32) (result i32)
    local.get $n
    i32.const 0
    i32.lt_s
    if (result i32)
      i32.const 0
      local.get $n
      i32.sub
    else
      local.get $n
    end)

  ;; A branchless ternary: pops (a, b, cond), pushes a if cond
  ;; is non-zero else b. Both arms are evaluated first.
  (func $max (param $a i32) (param $b i32) (result i32)
    (select (local.get $a) (local.get $b)
            (i32.gt_s (local.get $a) (local.get $b))))

  ;; A block that produces a value: whatever is on the stack when
  ;; you branch out becomes the block's result, and so does
  ;; whatever is on the stack if you fall off its end.
  (func $classify (param $n i32) (result i32)
    (block $out (result i32)
      (if (i32.lt_s (local.get $n) (i32.const 0))
        (then (br $out (i32.const -1))))
      (if (i32.eqz (local.get $n))
        (then (br $out (i32.const 0))))
      (i32.const 1)))

  ;; The classic counted loop. Note the block/loop sandwich:
  ;; the outer block is the "break" target, the inner loop is
  ;; the "continue" target.
  (func (export "sum_to") (param $n i32) (result i32)
    (local $i i32)
    (local $acc i32)
    (block $done
      (loop $again
        (br_if $done (i32.gt_s (local.get $i) (local.get $n)))
        (local.set $acc (i32.add (local.get $acc) (local.get $i)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $again)))
    (local.get $acc))

  ;; A do-while, which needs no outer block at all because the
  ;; only exit is falling off the bottom of the loop.
  (func (export "count_digits") (param $n i32) (result i32)
    (local $count i32)
    (local.set $count (i32.const 1))
    (local.set $n (i32.div_u (local.get $n) (i32.const 10)))
    (loop $again
      (if (local.get $n)
        (then
          (local.set $count (i32.add (local.get $count) (i32.const 1)))
          (local.set $n (i32.div_u (local.get $n) (i32.const 10)))
          (br $again))))
    (local.get $count))

  ;; --- labels are sugar for relative depth ---------------
  ;; `br 0` targets the innermost enclosing construct, `br 1`
  ;; the next one out, and so on. These two are the same code.
  (func $break_out_of_two_loops_named (result i32)
    (local $i i32)
    (block $outer_done
      (loop $outer
        (block $inner_done
          (loop $inner
            (br_if $outer_done (i32.gt_s (local.get $i) (i32.const 50)))
            (local.set $i (i32.add (local.get $i) (i32.const 7)))
            (br $inner)))
        (br $outer)))
    (local.get $i))

  (func $break_out_of_two_loops_numbered (result i32)
    (local $i i32)
    (block
      (loop
        (block
          (loop
            ;; from here: 0 = inner loop, 1 = inner block,
            ;;            2 = outer loop, 3 = outer block
            (br_if 3 (i32.gt_s (local.get $i) (i32.const 50)))
            (local.set $i (i32.add (local.get $i) (i32.const 7)))
            (br 0)))
        (br 0)))
    (local.get $i))

  ;; --- br_table = switch ---------------------------------
  ;; Pops an index and branches to the label at that position in
  ;; the list. The LAST label is the default for out-of-range.
  ;; The nested-block staircase below is how every compiler
  ;; lowers a switch statement into wasm.
  (func (export "http_category") (param $code i32) (result i32)
    (block $default
      (block $server_error
        (block $client_error
          (block $redirect
            (block $success
              ;; index = code / 100
              (br_table $default $default $success $redirect
                        $client_error $server_error $default
                        (i32.div_u (local.get $code) (i32.const 100))))
            (return (i32.const 200)))
          (return (i32.const 300)))
        (return (i32.const 400)))
      (return (i32.const 500)))
    (i32.const 0))

  ;; `return` is just a branch to the function's outermost label.
  (func (export "safe_div") (param $a i32) (param $b i32) (result i32)
    (if (i32.eqz (local.get $b))
      (then (return (i32.const 0))))
    (i32.div_s (local.get $a) (local.get $b)))

  ;; `unreachable` traps immediately. It is also how you satisfy
  ;; the type checker on a path that cannot happen: after it,
  ;; the stack is considered to hold whatever is needed.
  (func (export "must_be_small") (param $n i32) (result i32)
    (if (i32.gt_u (local.get $n) (i32.const 100))
      (then unreachable))
    (local.get $n))
)