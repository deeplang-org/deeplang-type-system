=== ANF for interface.dp ===
fun Duck.quack() -> #1 -- "interface.dp":11,2-13,3 =
  $1 = 1 -- "interface.dp":12,4-12,24
  jump #1 () -- "interface.dp":11,2-13,3

fun Bird.quack() -> #2 -- "interface.dp":22,2-24,3 =
  $2 = 2 -- "interface.dp":23,4-23,24
  jump #2 () -- "interface.dp":22,2-24,3

fun sound($3) -> #3 -- "interface.dp":27,0-29,1 =
  $4 = $3.quack -- "interface.dp":28,2-28,16
  $5 = $4($3) -- "interface.dp":28,2-28,16
  jump #3 () -- "interface.dp":27,0-29,1

fun main() -> #4 -- "interface.dp":31,0-37,1 =
  $6 = mk((ANF.ADT ("Duck", "Mallard")))() -- "interface.dp":32,19-32,26
  $7 = mk((ANF.ADT ("Bird", "Sparrow")))() -- "interface.dp":33,19-33,26
  $8 = sound -- "interface.dp":35,2-35,13
  $9 = $8($6) -- "interface.dp":35,2-35,13
  $10 = sound -- "interface.dp":36,2-36,13
  $11 = $10($7) -- "interface.dp":36,2-36,13
  jump #4 () -- "interface.dp":31,0-37,1

=== WAT for interface.dp ===
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

  (func $Duck.quack
    
    (result i32)
    
    i32.const 1
    local.set $v1
    br $l1
    (block $l1
      nop))

  (func $Bird.quack
    
    (result i32)
    
    i32.const 2
    local.set $v2
    br $l2
    (block $l2
      nop))

  (func $sound
    (param $v3 i32)
    (result i32)
    
    local.get $v3
    local.set $v4
    local.get $v3
    call $v4
    local.set $v5
    br $l3
    (block $l3
      nop))

  (func $main
    
    (result i32)
    
    i32.const 4
    call $bump_alloc
    local.set $v6
    i32.const 4
    call $bump_alloc
    local.set $v7
    i32.const 0
    local.set $v8
    local.get $v6
    call $v8
    local.set $v9
    i32.const 0
    local.set $v10
    local.get $v7
    call $v10
    local.set $v11
    br $l4
    (block $l4
      nop))

)


