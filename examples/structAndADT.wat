semantics error: [Type Error] Type Named Point Not Found
in [file "examples/structAndADT.dp", row 10, col 18 to row 10, col 23]

semantics error: [Error]  types doesn't match with ADT label Circle

semantics error: [Error]  type ColoredPoint Not Found 

=== ANF for examples/structAndADT.dp ===
fun main($1) -> #1 -- "examples/structAndADT.dp":14,0-14,20 =
  jump #1 () -- "examples/structAndADT.dp":14,0-14,20

=== WAT for examples/structAndADT.dp ===
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
    
    br $l1
    (block $l1
      nop))

)


