semantics error: [Error]  function print not found 

=== ANF for examples/controlFlow.dp ===
fun main($1, $2) -> #1 -- "examples/controlFlow.dp":1,0-33,1 =
  $3 = 0 -- "examples/controlFlow.dp":2,2-2,19
  $4 = 99 -- "examples/controlFlow.dp":3,2-3,21
  $3 := 1 -- "examples/controlFlow.dp":4,2-4,11
  block #2() =
    $5 = mk((ANF.Tuple 2))(0, 3) -- "examples/controlFlow.dp":21,24-21,30
    $9 = $5.1 -- _
    $8 = $5.0 -- _
    loop #3():
      block #4() =
        loop #5():
          block #6() =
            loop #7():
              block #8() =
                jump #1 () -- "examples/controlFlow.dp":1,0-33,1
              in
              jump #7 () -- "examples/controlFlow.dp":30,2-32,3
          in
          jump #5 () -- "examples/controlFlow.dp":27,2-29,3
      in
      $10 = (ParseTree.BinOpCompare ParseTree.BinOpEq)($7, 3) -- "examples/controlFlow.dp":22,13-22,19
      $11 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)(1, $10) -- "examples/controlFlow.dp":22,9-22,19
      match $11 -- "examples/controlFlow.dp":22,2-24,3:
      1 =>
        $12 = mk((ANF.Tuple 2))(1.200000, 99) -- "examples/controlFlow.dp":23,15-23,25
        $13 = mk((ANF.Tuple 3))(0, $12, "Abc") -- "examples/controlFlow.dp":23,11-23,33
        jump #1 ($13) -- "examples/controlFlow.dp":23,11-23,33
      _ =>
        jump #4 () -- "examples/controlFlow.dp":22,2-24,3
  in
  match $3 -- "examples/controlFlow.dp":7,2-18,3:
  1 =>
    $18 = print -- "examples/controlFlow.dp":8,4-8,12
    $19 = $18(1) -- "examples/controlFlow.dp":8,4-8,12
    jump #2 () -- "examples/controlFlow.dp":7,2-18,3
  _ =>
    match 1 -- "examples/controlFlow.dp":9,9-18,3:
    1 =>
      $20 = print -- "examples/controlFlow.dp":10,4-10,12
      $21 = $20($3) -- "examples/controlFlow.dp":10,4-10,12
      block #9() =
        jump #2 () -- "examples/controlFlow.dp":7,2-18,3
      in
      match 0 -- "examples/controlFlow.dp":11,4-15,5:
      1 =>
        $22 = foo -- "examples/controlFlow.dp":12,6-12,11
        $23 = $22() -- "examples/controlFlow.dp":12,6-12,11
        jump #9 () -- "examples/controlFlow.dp":11,4-15,5
      _ =>
        match $24 -- "examples/controlFlow.dp":13,11-15,5:
        1 =>
          $25 = baz -- "examples/controlFlow.dp":14,6-14,11
          $26 = $25() -- "examples/controlFlow.dp":14,6-14,11
          jump #9 () -- "examples/controlFlow.dp":11,4-15,5
        _ =>
          jump #9 () -- "examples/controlFlow.dp":11,4-15,5
    _ =>
      $27 = fooBar -- "examples/controlFlow.dp":17,4-17,12
      $28 = $27() -- "examples/controlFlow.dp":17,4-17,12
      jump #2 () -- "examples/controlFlow.dp":7,2-18,3

=== WAT for examples/controlFlow.dp ===
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
    (param $v2 i32)
    (result i32)
    
    i32.const 0
    local.set $v3
    i32.const 99
    local.set $v4
    local.get $v3
    i32.const 1
    i32.store
    (block $l2
      i32.const 8
      call $bump_alloc
      i32.const 0
      i32.const 3
      local.set $v5
      local.get $v5
      i32.const 4
      i32.add
      i32.load
      local.set $v9
      local.get $v5
      i32.const 0
      i32.add
      i32.load
      local.set $v8
      (loop $l3
        (block $l4
          (loop $l5
            (block $l6
              (loop $l7
                (block $l8
                  br $l1)
                br $l7))
            br $l5))
        local.get $v7
        i32.const 3
        i32.eq
        local.set $v10
        i32.const 1
        local.get $v10
        i32.add
        local.set $v11
        local.get $v11
        local.get $v11
        (if
          (then
            i32.const 8
            call $bump_alloc
            f32.const 1.2
            i32.const 99
            local.set $v12
            i32.const 12
            call $bump_alloc
            i32.const 0
            local.get $v12
            i32.const 0
            local.set $v13
            local.get $v13
            br $l1)
          (else
            br $l4))))
    local.get $v3
    local.get $v3
    (if
      (then
        i32.const 0
        local.set $v18
        i32.const 1
        call $v18
        local.set $v19
        br $l2)
      (else
        i32.const 1
        i32.const 1
        (if
          (then
            i32.const 0
            local.set $v20
            local.get $v3
            call $v20
            local.set $v21
            (block $l9
              br $l2)
            i32.const 0
            i32.const 0
            (if
              (then
                i32.const 0
                local.set $v22
                call $v22
                local.set $v23
                br $l9)
              (else
                local.get $v24
                local.get $v24
                (if
                  (then
                    i32.const 0
                    local.set $v25
                    call $v25
                    local.set $v26
                    br $l9)
                  (else
                    br $l9)))))
          (else
            i32.const 0
            local.set $v27
            call $v27
            local.set $v28
            br $l2))))
    (block $l1
      nop))

)


