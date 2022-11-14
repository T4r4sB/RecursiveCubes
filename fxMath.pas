unit fxMath;

interface

  const k16 = $10000;
  function fxMul(a, b: integer): integer;
  function fxDiv(a, b: integer): integer;  
  function mulDiv(a, b, c: integer): integer;
  function bsf(a: cardinal): integer;
  function bsr(a: cardinal): integer;

implementation

  function fxMul(a, b: integer): integer;
  asm
    imul edx
    test edx, edx
    jns @
    add eax, $FFFF
    jnb @
    inc edx
    @:
    shrd eax, edx, 16
  end;

  function fxDiv(a, b: integer): integer;
  asm
    mov ecx, edx
    mov edx, eax
    sar edx, 16
    sal eax, 16
    idiv ecx
  end;

  function mulDiv(a, b, c: integer): integer;
  asm
    imul edx
    idiv ecx
  end;

  function bsf(a: cardinal): integer;
  asm
    bsf eax, eax
  end;

  function bsr(a: cardinal): integer;
  asm
    bsr eax, eax
  end;

end.
