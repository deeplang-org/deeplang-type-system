=== ANF for example.dp ===
fun noop() -> #1 -- "example.dp":14,0-14,14 =
  jump #1 () -- "example.dp":14,0-14,14

fun add($1, $2) -> #2 -- "example.dp":16,0-18,1 =
  $3 = (ParseTree.BinOpCalculate ParseTree.BinOpMul)($2, 2) -- "example.dp":17,13-17,18
  $4 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)($1, $3) -- "example.dp":17,9-17,18
  jump #2 ($4) -- "example.dp":17,9-17,18

fun Greeter.show() -> #3 -- "example.dp":67,2-70,3 =
  $5 = 42 -- "example.dp":68,4-68,26
  jump #3 ($5) -- "example.dp":69,11-69,17

fun controlFlow() -> #4 -- "example.dp":74,0-98,1 =
  $6 = 0 -- "example.dp":75,2-75,22
  block #5() =
    $7 = 0 -- "example.dp":83,2-83,28
    loop #6():
      block #7() =
        $11 = 0 -- "example.dp":93,7-93,26
        loop #9():
          block #10() =
            jump #4 ($6) -- "example.dp":97,9-97,10
          in
          $15 = (ParseTree.BinOpCompare ParseTree.BinOpLt)($11, 5) -- "example.dp":93,28-93,33
          match $15 -- "example.dp":93,2-95,3:
          1 =>
            $12 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)($6, $11) -- "example.dp":94,8-94,13
            $6 := $12 -- "example.dp":94,4-94,14
            $13 = $11 -- "example.dp":93,35-93,38
            $14 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)($13, 1) -- "example.dp":93,35-93,38
            $11 := $14 -- "example.dp":93,35-93,38
            jump #9 () -- "example.dp":93,2-95,3
          _ =>
            jump #10 () -- "example.dp":93,2-95,3
      in
      $8 = (ParseTree.BinOpCompare ParseTree.BinOpLt)($7, 10) -- "example.dp":84,9-84,21
      match $8 -- "example.dp":84,2-91,3:
      1 =>
        $9 = (ParseTree.BinOpCalculate ParseTree.BinOpAdd)($7, 1) -- "example.dp":85,14-85,25
        $7 := $9 -- "example.dp":85,4-85,26
        $10 = (ParseTree.BinOpCompare ParseTree.BinOpGt)($7, 5) -- "example.dp":86,8-86,19
        block #8() =
          jump #6 () -- "example.dp":84,2-91,3
        in
        match $10 -- "example.dp":86,4-90,5:
        1 =>
          jump #7 () -- "example.dp":87,6-87,12
        _ =>
          jump #6 () -- "example.dp":89,6-89,15
      _ =>
        jump #7 () -- "example.dp":84,2-91,3
  in
  match 1 -- "example.dp":77,2-81,3:
  1 =>
    $6 := 1 -- "example.dp":78,4-78,10
    jump #5 () -- "example.dp":77,2-81,3
  _ =>
    $6 := 2 -- "example.dp":80,4-80,10
    jump #5 () -- "example.dp":77,2-81,3

fun testForIn($16) -> #11 -- "example.dp":101,0-105,1 =
  loop #12():
    block #13() =
      jump #11 () -- "example.dp":101,0-105,1
    in
    jump #12 () -- "example.dp":102,2-104,3

fun testMatch($18) -> #14 -- "example.dp":108,0-117,1 =
  block #15() =
    jump #14 () -- "example.dp":108,0-117,1
  in
  block #16($19, $20) =
    jump #14 ($20) -- "example.dp":111,13-111,18
  in
  block #17($21) =
    jump #14 ($21) -- "example.dp":114,13-114,19
  in
  match $18.tag -- _:
  0 =>
    $23 = $18.as(0).1 -- _
    $22 = $18.as(0).0 -- _
    jump #16 ($23, $22) -- "example.dp":110,32-112,5
  1 =>
    $24 = $18.as(1).0 -- _
    jump #17 ($24) -- "example.dp":113,22-115,5

fun testStructMatch($25) -> #18 -- "example.dp":120,0-126,1 =
  block #19() =
    jump #18 () -- "example.dp":120,0-126,1
  in
  block #20() =
    jump #18 (0) -- "example.dp":122,39-122,41
  in
  block #21() =
    jump #18 (0) -- "example.dp":123,39-123,41
  in
  block #22() =
    jump #18 (0) -- "example.dp":124,18-124,20
  in
  $27 = $25.1 -- _
  $26 = $25.0 -- _
  jump #20 () -- "example.dp":122,30-122,44

fun main() -> #23 -- "example.dp":129,0-142,1 =
  $28 = mk((ANF.ADT ("Shape", "Rectangle")))(3, 4) -- "example.dp":130,21-130,36
  $29 = testMatch -- "example.dp":131,19-131,35
  $30 = $29($28) -- "example.dp":131,19-131,35
  $31 = mk((ANF.Struct "Point"))(1, 2) -- "example.dp":133,18-133,40
  $32 = testStructMatch -- "example.dp":134,2-134,21
  $33 = $32($31) -- "example.dp":134,2-134,21
  $34 = mk((ANF.ADT ("Greeter", "Hello")))() -- "example.dp":136,23-136,28
  $35 = $34.show -- "example.dp":137,18-137,30
  $36 = $35($34) -- "example.dp":137,18-137,30
  $37 = controlFlow -- "example.dp":139,21-139,34
  $38 = $37() -- "example.dp":139,21-139,34
  $39 = add -- "example.dp":141,16-141,25
  $40 = $39(1, 2) -- "example.dp":141,16-141,25
  jump #23 () -- "example.dp":129,0-142,1

=== WAT for example.dp ===
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

  (func $noop
    
    (result i32)
    
    br $l1
    (block $l1
      nop))

  (func $add
    (param $v1 i32)
    (param $v2 i32)
    (result i32)
    
    local.get $v2
    i32.const 2
    i32.mul
    local.set $v3
    local.get $v1
    local.get $v3
    i32.add
    local.set $v4
    local.get $v4
    br $l2
    (block $l2
      nop))

  (func $Greeter.show
    
    (result i32)
    
    i32.const 42
    local.set $v5
    local.get $v5
    br $l3
    (block $l3
      nop))

  (func $controlFlow
    
    (result i32)
    
    i32.const 0
    local.set $v6
    (block $l5
      i32.const 0
      local.set $v7
      (loop $l6
        (block $l7
          i32.const 0
          local.set $v11
          (loop $l9
            (block $l10
              local.get $v6
              br $l4)
            local.get $v11
            i32.const 5
            i32.lt_s
            local.set $v15
            local.get $v15
            local.get $v15
            (if
              (then
                local.get $v6
                local.get $v11
                i32.add
                local.set $v12
                local.get $v6
                local.get $v12
                i32.store
                local.get $v11
                local.set $v13
                local.get $v13
                i32.const 1
                i32.add
                local.set $v14
                local.get $v11
                local.get $v14
                i32.store
                br $l9)
              (else
                br $l10))))
        local.get $v7
        i32.const 10
        i32.lt_s
        local.set $v8
        local.get $v8
        local.get $v8
        (if
          (then
            local.get $v7
            i32.const 1
            i32.add
            local.set $v9
            local.get $v7
            local.get $v9
            i32.store
            local.get $v7
            i32.const 5
            i32.gt_s
            local.set $v10
            (block $l8
              br $l6)
            local.get $v10
            local.get $v10
            (if
              (then
                br $l7)
              (else
                br $l6)))
          (else
            br $l7))))
    i32.const 1
    i32.const 1
    (if
      (then
        local.get $v6
        i32.const 1
        i32.store
        br $l5)
      (else
        local.get $v6
        i32.const 2
        i32.store
        br $l5))
    (block $l4
      nop))

  (func $testForIn
    (param $v16 i32)
    (result i32)
    
    (loop $l12
      (block $l13
        br $l11)
      br $l12)
    (block $l11
      nop))

  (func $testMatch
    (param $v18 i32)
    (result i32)
    
    (block $l15
      br $l14)
    (block $l16
      local.get $v20
      br $l14)
    (block $l17
      local.get $v21
      br $l14)
    local.get $v18
    i32.load
    (if
      (then
        local.get $v18
        i32.const 4
        i32.add
        i32.load
        i32.const 4
        i32.add
        i32.load
        local.set $v23
        local.get $v18
        i32.const 0
        i32.add
        i32.load
        i32.const 4
        i32.add
        i32.load
        local.set $v22
        local.get $v23
        local.get $v22
        br $l16)
      (else
        local.get $v18
        i32.const 0
        i32.add
        i32.load
        i32.const 8
        i32.add
        i32.load
        local.set $v24
        local.get $v24
        br $l17))
    (block $l14
      nop))

  (func $testStructMatch
    (param $v25 i32)
    (result i32)
    
    (block $l19
      br $l18)
    (block $l20
      i32.const 0
      br $l18)
    (block $l21
      i32.const 0
      br $l18)
    (block $l22
      i32.const 0
      br $l18)
    local.get $v25
    i32.const 4
    i32.add
    i32.load
    local.set $v27
    local.get $v25
    i32.const 0
    i32.add
    i32.load
    local.set $v26
    br $l20
    (block $l18
      nop))

  (func $main
    
    (result i32)
    
    i32.const 12
    call $bump_alloc
    i32.const 3
    i32.const 4
    local.set $v28
    i32.const 0
    local.set $v29
    local.get $v28
    call $v29
    local.set $v30
    i32.const 8
    call $bump_alloc
    i32.const 1
    i32.const 2
    local.set $v31
    i32.const 0
    local.set $v32
    local.get $v31
    call $v32
    local.set $v33
    i32.const 4
    call $bump_alloc
    local.set $v34
    local.get $v34
    local.set $v35
    local.get $v34
    call $v35
    local.set $v36
    i32.const 0
    local.set $v37
    call $v37
    local.set $v38
    i32.const 0
    local.set $v39
    i32.const 1
    i32.const 2
    call $v39
    local.set $v40
    br $l23
    (block $l23
      nop))

)


