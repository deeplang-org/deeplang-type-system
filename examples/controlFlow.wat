=== ANF for controlFlow.dp ===
fun main() -> #1 -- "controlFlow.dp":1,0-40,1 =
  $1 = 0 -- "controlFlow.dp":2,2-2,19
  $1 := 1 -- "controlFlow.dp":3,2-3,11
  block #2() =
    $2 = 0 -- "controlFlow.dp":20,2-20,22
    loop #3():
      block #4() =
        $5 = 0 -- "controlFlow.dp":26,7-26,26
        loop #5():
          block #6() =
            $9 = 0 -- "controlFlow.dp":31,2-31,28
            loop #7():
              block #8() =
                jump #1 () -- "controlFlow.dp":1,0-40,1
              in
              match 1 -- "controlFlow.dp":32,2-39,3:
              1 =>
                $10 = (ParseTree.BinOpCompare ParseTree.BinOpGt)($9, 5) -- "controlFlow.dp":33,8-33,19
                block #9() =
                  jump #7 () -- "controlFlow.dp":32,2-39,3
                in
                match $10 -- "controlFlow.dp":33,4-38,5:
                1 =>
                  jump #8 () -- "controlFlow.dp":34,6-34,12
                _ =>
                  $11 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)($9, 1) -- "controlFlow.dp":36,16-36,27
                  $9 := $11 -- "controlFlow.dp":36,6-36,28
                  jump #7 () -- "controlFlow.dp":37,6-37,15
              _ =>
                jump #8 () -- "controlFlow.dp":32,2-39,3
          in
          $8 = (ParseTree.BinOpCompare ParseTree.BinOpLt)($5, 5) -- "controlFlow.dp":26,28-26,33
          match $8 -- "controlFlow.dp":26,2-28,3:
          1 =>
            $6 = $5 -- "controlFlow.dp":26,35-26,38
            $7 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)($6, 1) -- "controlFlow.dp":26,35-26,38
            $5 := $7 -- "controlFlow.dp":26,35-26,38
            jump #5 () -- "controlFlow.dp":26,2-28,3
          _ =>
            jump #6 () -- "controlFlow.dp":26,2-28,3
      in
      $3 = (ParseTree.BinOpCompare ParseTree.BinOpLt)($2, 3) -- "controlFlow.dp":21,9-21,14
      match $3 -- "controlFlow.dp":21,2-23,3:
      1 =>
        $4 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)($2, 1) -- "controlFlow.dp":22,8-22,13
        $2 := $4 -- "controlFlow.dp":22,4-22,14
        jump #3 () -- "controlFlow.dp":21,2-23,3
      _ =>
        jump #4 () -- "controlFlow.dp":21,2-23,3
  in
  match $1 -- "controlFlow.dp":6,2-17,3:
  1 =>
    $12 = 1 -- "controlFlow.dp":7,4-7,25
    jump #2 () -- "controlFlow.dp":6,2-17,3
  _ =>
    match 1 -- "controlFlow.dp":8,9-17,3:
    1 =>
      $13 = 2 -- "controlFlow.dp":9,4-9,25
      block #10() =
        jump #2 () -- "controlFlow.dp":6,2-17,3
      in
      match 0 -- "controlFlow.dp":10,4-14,5:
      1 =>
        $14 = 3 -- "controlFlow.dp":11,6-11,27
        jump #10 () -- "controlFlow.dp":10,4-14,5
      _ =>
        match $1 -- "controlFlow.dp":12,11-14,5:
        1 =>
          $15 = 4 -- "controlFlow.dp":13,6-13,27
          jump #10 () -- "controlFlow.dp":10,4-14,5
        _ =>
          jump #10 () -- "controlFlow.dp":10,4-14,5
    _ =>
      $16 = 5 -- "controlFlow.dp":16,4-16,25
      jump #2 () -- "controlFlow.dp":6,2-17,3

fun testForIn($17) -> #11 -- "controlFlow.dp":43,0-47,1 =
  loop #12():
    block #13() =
      jump #11 () -- "controlFlow.dp":43,0-47,1
    in
    jump #12 () -- "controlFlow.dp":44,2-46,3

fun testForInTuple($19) -> #14 -- "controlFlow.dp":50,0-55,1 =
  loop #15():
    block #16() =
      jump #14 () -- "controlFlow.dp":50,0-55,1
    in
    jump #15 () -- "controlFlow.dp":51,2-54,3

=== WAT for controlFlow.dp ===
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
    
    i32.const 0
    local.set $v1
    local.get $v1
    i32.const 1
    i32.store
    (block $l2
      i32.const 0
      local.set $v2
      (loop $l3
        (block $l4
          i32.const 0
          local.set $v5
          (loop $l5
            (block $l6
              i32.const 0
              local.set $v9
              (loop $l7
                (block $l8
                  br $l1)
                i32.const 1
                i32.const 1
                (if
                  (then
                    local.get $v9
                    i32.const 5
                    i32.gt_s
                    local.set $v10
                    (block $l9
                      br $l7)
                    local.get $v10
                    local.get $v10
                    (if
                      (then
                        br $l8)
                      (else
                        local.get $v9
                        i32.const 1
                        i32.add
                        local.set $v11
                        local.get $v9
                        local.get $v11
                        i32.store
                        br $l7)))
                  (else
                    br $l8))))
            local.get $v5
            i32.const 5
            i32.lt_s
            local.set $v8
            local.get $v8
            local.get $v8
            (if
              (then
                local.get $v5
                local.set $v6
                local.get $v6
                i32.const 1
                i32.add
                local.set $v7
                local.get $v5
                local.get $v7
                i32.store
                br $l5)
              (else
                br $l6))))
        local.get $v2
        i32.const 3
        i32.lt_s
        local.set $v3
        local.get $v3
        local.get $v3
        (if
          (then
            local.get $v2
            i32.const 1
            i32.add
            local.set $v4
            local.get $v2
            local.get $v4
            i32.store
            br $l3)
          (else
            br $l4))))
    local.get $v1
    local.get $v1
    (if
      (then
        i32.const 1
        local.set $v12
        br $l2)
      (else
        i32.const 1
        i32.const 1
        (if
          (then
            i32.const 2
            local.set $v13
            (block $l10
              br $l2)
            i32.const 0
            i32.const 0
            (if
              (then
                i32.const 3
                local.set $v14
                br $l10)
              (else
                local.get $v1
                local.get $v1
                (if
                  (then
                    i32.const 4
                    local.set $v15
                    br $l10)
                  (else
                    br $l10)))))
          (else
            i32.const 5
            local.set $v16
            br $l2))))
    (block $l1
      nop))

  (func $testForIn
    (param $v17 i32)
    (result i32)
    
    (loop $l12
      (block $l13
        br $l11)
      br $l12)
    (block $l11
      nop))

  (func $testForInTuple
    (param $v19 i32)
    (result i32)
    
    (loop $l15
      (block $l16
        br $l14)
      br $l15)
    (block $l14
      nop))

)


