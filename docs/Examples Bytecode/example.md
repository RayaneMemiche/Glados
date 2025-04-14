## Functions

```lua
local function f(x)
    return x + 2
end
```

Translates to:

```text
PUSH 2
CALLP [ADD]
RET
```

## Variables

```lua
local x = 2
local y = 3
local z = 42

local function test()
    return x + y - z
end
```

Translates to:

```text
PUSH 2
STORE 0x00
PUSH 3
STORE 0x01
PUSH 42
STORE 0x02
CALL 0x42

0x42 LOAD 0x00
     LOAD 0x01
     CALLP [ADD]
     LOAD 0x02
     CALLP [SUB]
     RET
```
