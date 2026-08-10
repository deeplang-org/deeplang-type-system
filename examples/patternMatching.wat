=== ANF for patternMatching.dp ===
fun main($1) -> #1 -- "patternMatching.dp":11,0-18,1 =
  block #2() =
    jump #1 () -- "patternMatching.dp":11,0-18,1
  in
  block #3() =
    jump #1 (0) -- "patternMatching.dp":13,18-13,20
  in
  block #4($2) =
    jump #1 (0) -- "patternMatching.dp":14,26-14,28
  in
  block #5() =
    jump #1 (0) -- "patternMatching.dp":15,24-15,26
  in
  block #6($3) =
    jump #1 (0) -- "patternMatching.dp":16,28-16,30
  in
  jump #3 () -- "patternMatching.dp":13,9-13,23

fun testTuple($4) -> #7 -- "patternMatching.dp":20,0-26,1 =
  block #8() =
    jump #7 () -- "patternMatching.dp":20,0-26,1
  in
  block #9($5, $6, $7) =
    jump #7 (0) -- "patternMatching.dp":22,26-22,28
  in
  block #10($8, $9, $10) =
    jump #7 (0) -- "patternMatching.dp":23,50-23,52
  in
  block #11() =
    jump #7 (0) -- "patternMatching.dp":24,18-24,20
  in
  $13 = $4.2 -- _
  $12 = $4.1 -- _
  $11 = $4.0 -- _
  jump #9 ($13, $12, $11) -- "patternMatching.dp":22,17-22,31

fun testStruct($14) -> #12 -- "patternMatching.dp":28,0-35,1 =
  block #13() =
    jump #12 () -- "patternMatching.dp":28,0-35,1
  in
  block #14() =
    jump #12 (0) -- "patternMatching.dp":30,39-30,41
  in
  block #15() =
    jump #12 (0) -- "patternMatching.dp":31,39-31,41
  in
  block #16($15) =
    jump #12 (0) -- "patternMatching.dp":32,52-32,54
  in
  block #17() =
    jump #12 (0) -- "patternMatching.dp":33,18-33,20
  in
  $17 = $14.1 -- _
  $16 = $14.0 -- _
  jump #14 () -- "patternMatching.dp":30,30-30,44

fun testLiteral($18) -> #18 -- "patternMatching.dp":37,0-42,1 =
  block #19() =
    jump #18 () -- "patternMatching.dp":37,0-42,1
  in
  block #20() =
    jump #18 (0) -- "patternMatching.dp":39,18-39,20
  in
  block #21() =
    jump #18 (0) -- "patternMatching.dp":40,18-40,20
  in
  $19 = (ParseTree.BinOpCompare ParseTree.BinOpEq)($18, 7) -- _
  match $19 -- _:
  1 =>
    jump #20 () -- "patternMatching.dp":39,9-39,23
  _ =>
    jump #21 () -- "patternMatching.dp":40,9-40,23

=== WAT for patternMatching.dp ===
(module
  (memory (export "memory") 1)
  (global $heap_ptr (mut i32) (i32.const 1024))
  (func $bump_init
    (param $size i32)
    
    
    ;; Initialize heap pointer
    global.set $heap_ptr)

  (func $bump_alloc
    (param $size i32)
    (result i32)
    (local $ptr i32)
    ;; Simple bump allocator
    local.get $ptr
    local.get $heap_ptr
    local.set $ptr
    global.get $heap_ptr
    local.get $size
    i32.add
    global.set $heap_ptr
    local.get $ptr)

  (func $bump_alloc_zero
    (param $size i32)
    (result i32)
    (local $ptr i32)
    (local $i i32)
    local.get $size
    call $bump_alloc
    local.set $ptr
    i32.const 0
    local.set $i
    (loop $zero_loop
      local.get $i
      local.get $size
      i32.ge_s
      (if
        (then
          br $zero_end)
        (else
          nop))
      local.get $ptr
      local.get $i
      i32.add
      i32.const 0
      i32.store8
      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br $zero_loop)
    (block $zero_end
      nop)
    local.get $ptr)

  (func $main
    (param $v1 i32)
    (result i32)
    
    (block $l2
      br $l1)
    (block $l3
      i32.const 0
      br $l1)
    (block $l4
      i32.const 0
      br $l1)
    (block $l5
      i32.const 0
      br $l1)
    (block $l6
      i32.const 0
      br $l1)
    br $l3
    (block $l1
      nop))

  (func $testTuple
    (param $v4 i32)
    (result i32)
    
    (block $l8
      br $l7)
    (block $l9
      i32.const 0
      br $l7)
    (block $l10
      i32.const 0
      br $l7)
    (block $l11
      i32.const 0
      br $l7)
    local.get $v4
    i32.const 8
    i32.add
    i32.load
    local.set $v13
    local.get $v4
    i32.const 4
    i32.add
    i32.load
    local.set $v12
    local.get $v4
    i32.const 0
    i32.add
    i32.load
    local.set $v11
    local.get $v13
    local.get $v12
    local.get $v11
    br $l9
    (block $l7
      nop))

  (func $testStruct
    (param $v14 i32)
    (result i32)
    
    (block $l13
      br $l12)
    (block $l14
      i32.const 0
      br $l12)
    (block $l15
      i32.const 0
      br $l12)
    (block $l16
      i32.const 0
      br $l12)
    (block $l17
      i32.const 0
      br $l12)
    local.get $v14
    i32.const 4
    i32.add
    i32.load
    local.set $v17
    local.get $v14
    i32.const 0
    i32.add
    i32.load
    local.set $v16
    br $l14
    (block $l12
      nop))

  (func $testLiteral
    (param $v18 i32)
    (result i32)
    
    (block $l19
      br $l18)
    (block $l20
      i32.const 0
      br $l18)
    (block $l21
      i32.const 0
      br $l18)
    local.get $v18
    i32.const 7
    i32.eq
    local.set $v19
    local.get $v19
    local.get $v19
    (if
      (then
        br $l20)
      (else
        br $l21))
    (block $l18
      nop))

)


