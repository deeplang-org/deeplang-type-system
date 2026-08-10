semantics error: [Error] varaible x Not Found

=== ANF for examples/patternMatching.dp ===
fun main() -> #1 -- "examples/patternMatching.dp":1,0-14,1 =
  block #2() =
    jump #1 () -- "examples/patternMatching.dp":1,0-14,1
  in
  block #3() =
    jump #1 (0) -- "examples/patternMatching.dp":3,18-3,19
  in
  block #4($1) =
    jump #1 (1) -- "examples/patternMatching.dp":4,25-4,26
  in
  block #5() =
    jump #1 (2) -- "examples/patternMatching.dp":5,26-5,27
  in
  block #6($2) =
    jump #1 (3) -- "examples/patternMatching.dp":6,29-6,30
  in
  block #7() =
    jump #1 (4) -- "examples/patternMatching.dp":7,19-7,20
  in
  block #8($3) =
    jump #1 (5) -- "examples/patternMatching.dp":8,22-8,23
  in
  block #9($4, $5, $6, $7, $8) =
    jump #1 (6) -- "examples/patternMatching.dp":9,77-9,78
  in
  block #10() =
    jump #1 (7) -- "examples/patternMatching.dp":10,18-10,19
  in
  block #11() =
    jump #1 (8) -- "examples/patternMatching.dp":11,60-11,61
  in
  block #12($9) =
    jump #1 (9) -- "examples/patternMatching.dp":12,66-12,67
  in
  jump #3 () -- "examples/patternMatching.dp":3,9-3,22

=== WAT for examples/patternMatching.dp ===
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
    
    (result i32)
    
    (block $l2
      br $l1)
    (block $l3
      i32.const 0
      br $l1)
    (block $l4
      i32.const 1
      br $l1)
    (block $l5
      i32.const 2
      br $l1)
    (block $l6
      i32.const 3
      br $l1)
    (block $l7
      i32.const 4
      br $l1)
    (block $l8
      i32.const 5
      br $l1)
    (block $l9
      i32.const 6
      br $l1)
    (block $l10
      i32.const 7
      br $l1)
    (block $l11
      i32.const 8
      br $l1)
    (block $l12
      i32.const 9
      br $l1)
    br $l3
    (block $l1
      nop))

)


