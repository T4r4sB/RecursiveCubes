unit Render;

{$IFOPT Q+}
{$DEFINE HASQ}
{$ENDIF}

interface

uses Windows, Messages, WinAPI, Points, Geometry;

const
  STENCIL_ORD = 4;
  STENCIL_SIZE = 1 shl STENCIL_ORD;
  STENCIL_FULL = cardinal(1 shl ((1 shl STENCIL_ORD) - 1)) - cardinal(1) + cardinal(1 shl ((1 shl STENCIL_ORD) - 1));

type
  T32Colors = array [0..STENCIL_SIZE-1] of TColor;
  P32Colors = ^T32Colors;

  StencilCell = record
    count: integer;
    bits: array [0..STENCIL_SIZE-1] of cardinal;
    pixels: array [0..STENCIL_SIZE-1] of P32Colors;
  end;

  PStencilCell = ^StencilCell;

  StencilBuffer = record
    sizeX, sizeY: integer;
    cells: array of StencilCell;
  end;

  RenderContext = record
    buffer: TBitmap;
    stencil: StencilBuffer;
    viewC: Point;
    view: Matrix;
    viewSX, viewSY: float;
    polyCounter: integer;
  end;

procedure CreateRenderContext(var context: RenderContext; sizeX, sizeY: integer);
procedure SetRenderContext(var context: RenderContext; const viewC: Point; viewAX, viewAZ: float; viewSX, viewSY: float);
procedure DeleteRenderContext(var context: RenderContext);
procedure DrawConvex(var context: RenderContext; const v: Convex);
function FastCheckCube(var context: RenderContext; const pmin, pmax: Point): boolean;

procedure Counts(var context: RenderContext);

var
  dt: TLargeInteger = 0;
  counter1: TLargeInteger = 0;
  counter2: TLargeInteger = 0;
  counter3: TLargeInteger = 0;      
  counter4: TLargeInteger = 0;      
  counter5: TLargeInteger = 0;
  counter6: TLargeInteger = 0;
  counter7: TLargeInteger = 0;
  counter8: TLargeInteger = 0;

  texture, texture2: array [0..$30000] of integer;
  palette: array [0..15] of TColor;
  basePalette: array [0..15] of TColor;

implementation

uses fxMath;

function GetBitCount(c: cardinal): integer;
begin
  c := (c and $AAAAAAAA) shr 1  + (c and $55555555);
  c := (c and $CCCCCCCC) shr 2  + (c and $33333333);
  c := (c and $F0F0F0F0) shr 4  + (c and $0F0F0F0F);
  c := (c and $FF00FF00) shr 8  + (c and $00FF00FF);
  c := (c and $FFFF0000) shr 16 + (c and $0000FFFF);
  result := integer(c);
end;

var  bitCount: array [0..$FFFF] of integer;

procedure CreateRenderContext(var context: RenderContext; sizeX, sizeY: integer);
var
  i,j,k: integer;
  //r,g,b: integer;
  pc: PColor;
begin
  CreateBitmap(context.buffer, sizeX, sizeY, 32);
  context.stencil.sizeX := (sizeX+STENCIL_SIZE-1) div STENCIL_SIZE;
  context.stencil.sizeY := (sizeY+STENCIL_SIZE-1) div STENCIL_SIZE;
  SetLength(context.stencil.cells, context.stencil.sizeX*context.stencil.sizeY);
  for j := 0 to context.stencil.sizeY-1 do for i := 0 to context.stencil.sizeX-1 do begin
    for k := 0 to STENCIL_SIZE-1 do begin
      pc := context.buffer.Mem;
      Inc(pc, (j*STENCIL_SIZE+k)*context.buffer.sizeX+(i*STENCIL_SIZE));
      context.stencil.cells[j*context.stencil.sizeX+i].pixels[k] := P32Colors(pc);
    end;
  end;

  for i := Low(bitCount) to High(bitCount) do bitCount[i] := getBitCount(i);

  for i := Low(texture) to High(texture) do begin
    texture[i] := random(16);
  end;

  for i := 0 to 5 do basePalette[i] := (random(8)) * $20100 + $402000;
  for i := 6 to 9 do basePalette[i] := (random(64)) * $1 + $303030;
  for i := 10 to 15 do basePalette[i] := $204000 + (random(8)) * $10200;

  for i := 0 to 255 do for j := 0 to 255 do begin
    texture[i shl 8 + j] := random(6);
    texture[(i+256) shl 8 + j] := random(6)+10;
    texture[(i+512) shl 8 + j] := random(4)+6;
  end;

  for k := 0 to 0 do begin
    for i := 0 to 255 do for j := 0 to 255 do begin
      texture2[i shl 8 + j] := (
        texture[(i and $F0 + $05) and $FF shl 8 + (j and $F0 + $05) and $FF] * (16 - i and $F) * (16 - j and $F) +
        texture[(i and $F0 + $15) and $FF shl 8 + (j and $F0 + $05) and $FF] * (     i and $F) * (16 - j and $F) +
        texture[(i and $F0 + $05) and $FF shl 8 + (j and $F0 + $15) and $FF] * (16 - i and $F) * (     j and $F) +
        texture[(i and $F0 + $15) and $FF shl 8 + (j and $F0 + $15) and $FF] * (     i and $F) * (     j and $F) + 128) div (16*16);
    end;

    for i := 0 to 255 do for j := 0 to 255 do begin
      texture2[i shl 8 + j + $10000] := (
        texture[(i and $F0 + $05) and $FF shl 8 + (j and $F0 + $05) and $FF + $10000] * (16 - i and $F) * (16 - j and $F) +
        texture[(i and $F0 + $15) and $FF shl 8 + (j and $F0 + $05) and $FF + $10000] * (     i and $F) * (16 - j and $F) +
        texture[(i and $F0 + $05) and $FF shl 8 + (j and $F0 + $15) and $FF + $10000] * (16 - i and $F) * (     j and $F) +
        texture[(i and $F0 + $15) and $FF shl 8 + (j and $F0 + $15) and $FF + $10000] * (     i and $F) * (     j and $F) + 128) div (16*16);
    end;    

    for i := 0 to 255 do for j := 0 to 255 do begin
      texture2[i shl 8 + j + $20000] := (
        texture[(i and $F0 + $05) and $FF shl 8 + (j and $F0 + $05) and $FF + $20000] * (16 - i and $F) * (16 - j and $F) +
        texture[(i and $F0 + $15) and $FF shl 8 + (j and $F0 + $05) and $FF + $20000] * (     i and $F) * (16 - j and $F) +
        texture[(i and $F0 + $05) and $FF shl 8 + (j and $F0 + $15) and $FF + $20000] * (16 - i and $F) * (     j and $F) +
        texture[(i and $F0 + $15) and $FF shl 8 + (j and $F0 + $15) and $FF + $20000] * (     i and $F) * (     j and $F) + 128) div (16*16);
    end;

    texture := texture2;
  end;
end;

type
  AInteger = array [0 .. MaxInt div sizeof(integer)-1] of integer;
  PAInteger = ^AInteger;

  ProjPoint = record
    p: Point;
    planeDist: array [0..3] of float;
    hasProj: integer; // -1 hz, 0 out, 1 in
    sx, sy: float;
  end;

  PProjPoint = ^ProjPoint;
  AProjPoint = array [0 .. MaxInt div sizeof(ProjPoint)-1] of ProjPoint;
  PAProjPoint = ^AProjPoint;

  CutedLine = record
    processed: boolean;
    cuted: boolean;
    failed: boolean;
    reversed: boolean;
    w1, w2: float;
    zeroDist: float;
    x1, x2: integer;
    y1, y2: integer;
    i1, i2: integer; // indexes of frustum border planes
    x: PAInteger;
  end;

  PCutedLine = ^CutedLine;
  ACutedLine = array [0 .. MaxInt div sizeof(CutedLine)-1] of CutedLine;
  PACutedLine = ^ACutedLine;

  Zone = record           
    //x1, x2: integer;
    y1, y2: integer;
    lx, rx: PAInteger;
  end;

  PZone = ^Zone;
  AZone = array [0 .. MaxInt div sizeof(Zone)-1] of Zone;
  PAZone = ^AZone;

  FaceTexturingInfo = record
    processed: boolean;
    w0,wdx,wdy,tx0,txdx,txdy,ty0,tydx,tydy: float;
  end;

var
  intBuffer:     array [0..$FFFFF] of integer;
  intTop: integer = 0;
  projPointBuffer: array [0..$FFFF] of ProjPoint;
  projPointTop: integer = 0;
  cutedLineBuffer: array [0..$FFFF] of CutedLine;
  cutedLineTop: integer = 0;
  zoneBuffer: array [0..$FFFF] of Zone;
  zoneTop: integer = 0;

procedure SetRenderContext(var context: RenderContext; const viewC: Point; viewAX, viewAZ: float; viewSX, viewSY: float);
var
  i, j, k: integer;
  Px, Py: PColor;
  pc: PStencilCell;
  cx, cy: integer;
begin
  SetId(context.view);
  Translate(context.view, viewC);
  Rotate(context.view, 1, viewAX);
  Rotate(context.view, 0, viewAZ);
  
  context.viewC  := viewC;
  context.viewSX := viewSX;
  context.viewSY := viewSY;
  context.polyCounter := 0;

  Py := context.buffer.Mem;
  for j := 0 to context.buffer.SizeY-1 do begin
    Px := Py;
    for i := 0 to context.buffer.SizeX-1 do begin
      Px^ := $002244;
      Inc(Px);
    end;
    Inc(Py, context.buffer.SizeX);
  end;

  for j := 0 to context.stencil.sizeY-1 do for i := 0 to context.stencil.sizeX - 1 do begin
    pc := @context.stencil.cells[j*context.stencil.sizeX+i];
    if j=context.stencil.sizeY-1 then cy := context.buffer.SizeY-(j*STENCIL_SIZE) else cy := STENCIL_SIZE;
    if i=context.stencil.sizeX-1 then cx := context.buffer.SizeX-(i*STENCIL_SIZE) else cx := STENCIL_SIZE;
    pc.count := cx*cy;
    for k := 0 to STENCIL_SIZE-1 do pc.bits[k] := 0;
  end;
end;

procedure DeleteRenderContext(var context: RenderContext);
begin
  DeleteBitmap(context.buffer);
end;

procedure DrawConvex(var context: RenderContext; const v: Convex);
var
  i,j: integer;
  proj: PAProjPoint;
  pp: PProjPoint;
  fc1, fc2: float;
  lc: PACutedLine;
  zones: PAZone;
  vnormal: Point;
  vshift: float;
  f: float;
  
  procedure ProjectPoint(pindex: integer);
  begin    
    pp := @proj[pindex];

    pp.hasProj := 0;
    pp.p := rotateP(context.view, v.vertexes[pindex].p);
    pp.planeDist[0] := +pp.p.x*context.viewSx*0.999 + pp.p.z;
    pp.planeDist[1] := +pp.p.y*context.viewSy*0.999 + pp.p.z;
    pp.planeDist[2] := -pp.p.x*context.viewSx*0.999 + pp.p.z;
    pp.planeDist[3] := -pp.p.y*context.viewSy*0.999 + pp.p.z;

    if pp.p.z>0.0001 then begin
      pp.sx := (pp.p.x/pp.p.z*context.viewSX+1.0) * context.buffer.SizeX * 0.5;
      pp.sy := (pp.p.y/pp.p.z*context.viewSY+1.0) * context.buffer.SizeY * 0.5;
      pp.hasProj := 1;
    end;
  end;

  procedure PrerenderLine(index: integer);
  var
    pl: PLine;
    pp1, pp2: PProjPoint;
    x1,y1,x2,y2: float;
    ix1,iy1,ix2,iy2: integer;
    mp: Point;
    i, j: integer;
    x,cx,dx: integer;


  begin
    lc[index].processed := TRUE;
    lc[index].cuted := FALSE;
    lc[index].failed := FALSE;
    lc[index].w1 := 0.0;
    lc[index].w2 := 1.0;
    lc[index].i1 := -1;
    lc[index].i2 := -1;
                           
    pl := @v.lines[index];
    pp1 := @proj[pl.P1];
    pp2 := @proj[pl.P2];

    if pp1.hasProj<0 then ProjectPoint(pl.P1);
    if pp2.hasProj<0 then ProjectPoint(pl.P2);

    lc[index].zeroDist := pp1.p.x*pp2.p.y - pp1.p.y*pp2.p.x;

    for j := 0 to 3 do begin
      pl := @v.lines[index];
      fc1 := proj[pl.P1].planeDist[j];
      fc2 := proj[pl.P2].planeDist[j];

      if (fc1>=0.0) then begin
        if (fc2>=0.0) then begin
          //nothing, line is fully correct
        end else begin
          f := fc1/(fc1-fc2);
          if f<lc[index].w2 then begin
            lc[index].w2 := f;
            lc[index].i2 := j;
          end;
        end;
      end else begin
        if (fc2>=0.0) then begin
          f := fc1/(fc1-fc2);
          if f>lc[index].w1 then begin
            lc[index].w1 := f;
            lc[index].i1 := j;
          end;
        end else begin
          // totally incorrect line
          lc[index].cuted := TRUE;
        end;
      end;
    end;

    if lc[index].w2<=lc[index].w1 then lc[index].cuted := TRUE;

    if not lc[index].cuted then begin
      x1 := 0;
      y1 := 0;
      x2 := 0;
      y2 := 0;
      pl := @v.lines[index];
      pp1 := @proj[pl.P1];
      pp2 := @proj[pl.P2];

      if lc[index].w1=0.0 then begin
        if pp1.hasProj>0 then begin
          x1 := pp1.sx;
          y1 := pp1.sy;
        end else begin
          lc[index].failed := TRUE;
        end;
      end else begin
        Inc(projPointTop);

        mp := Add(pp1.p, Scale(Sub(pp2.p, pp1.p), lc[index].w1));
        x1 := (mp.x/mp.z*context.viewSX+1.0) * context.buffer.SizeX * 0.5;
        y1 := (mp.y/mp.z*context.viewSY+1.0) * context.buffer.SizeY * 0.5;
      end;

      if lc[index].w2=1.0 then begin
        if pp2.hasProj>0 then begin
          x2 := pp2.sx;
          y2 := pp2.sy;
        end else begin
          lc[index].failed := TRUE;
        end;
      end else begin
        Inc(projPointTop);
        mp := Add(pp1.p, Scale(Sub(pp2.p, pp1.p), lc[index].w2));
        x2 := (mp.x/mp.z*context.viewSX+1.0) * context.buffer.SizeX * 0.5;
        y2 := (mp.y/mp.z*context.viewSY+1.0) * context.buffer.SizeY * 0.5;
      end;

      if not lc[index].failed then begin
        if y1>y2 then begin
          ix1 := round(x2*$10000);
          iy1 := round(y2*$10000);
          ix2 := round(x1*$10000);
          iy2 := round(y1*$10000);
          lc[index].reversed := TRUE;
        end else begin
          ix1 := round(x1*$10000);
          iy1 := round(y1*$10000);
          ix2 := round(x2*$10000);
          iy2 := round(y2*$10000);
          lc[index].reversed := FALSE;
        end;
        lc[index].y1 := smallint(iy1 shr 16);
        lc[index].y2 := smallint(iy2 shr 16);
        lc[index].x1 := context.buffer.sizeX;
        lc[index].x2 := 0;

        if lc[index].y1=lc[index].y2 then begin   
          if lc[index].y1<0 then lc[index].y1 := 0;
          if lc[index].y2>=context.buffer.SizeY then lc[index].y2 := context.buffer.SizeY;
        end else if lc[index].y1+1=lc[index].y2 then begin
          if lc[index].y1<0 then lc[index].y1 := 0;
          if lc[index].y2>=context.buffer.SizeY then lc[index].y2 := context.buffer.SizeY;
          if lc[index].y1<lc[index].y2 then begin
            x := ix1+MulDiv((lc[index].y1+1) shl 16 -iy1, ix2-ix1, iy2-iy1);
            cx := smallint(x shr 16);
            lc[index].x := PAInteger(@intBuffer[intTop]);
            Inc(intTop, 1);
            if cx<0 then lc[index].x[0] := 0 else if cx>=context.buffer.SizeX then lc[index].x[0] := context.buffer.SizeX else lc[index].x[0] := cx;
            lc[index].x1 := lc[index].x[0];
            lc[index].x2 := lc[index].x[0];
          end;
        end else begin
          x := ix1+MulDiv((lc[index].y1+1) shl 16-iy1, ix2-ix1, iy2-iy1);
          dx := fxDiv(ix2-ix1, iy2-iy1);
          if lc[index].y1<0 then begin
            Inc(x, -dx*lc[index].y1);
            lc[index].y1 := 0;
          end;

          if lc[index].y2 >= context.buffer.SizeY then begin
            lc[index].y2 := context.buffer.SizeY;
          end;

          if lc[index].y2>lc[index].y1 then begin
            lc[index].x := PAInteger(@intBuffer[intTop]);
            Inc(intTop, lc[index].y2-lc[index].y1);

            for i := 0 to lc[index].y2-lc[index].y1-1 do begin
              cx := smallint(x shr 16);
              if cx<0 then lc[index].x[i] := 0 else if cx >= context.buffer.SizeX then lc[index].x[i] := context.buffer.SizeX else lc[index].x[i] := cx;
              Inc(x, dx);
            end;

            lc[index].x1 := lc[index].x[0];
            lc[index].x2 := lc[index].x[lc[index].y2-lc[index].y1-1];
            if lc[index].x1>lc[index].x2 then begin
              cx := lc[index].x1;
              lc[index].x1 := lc[index].x2;
              lc[index].x2 := cx;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure FillPixel(x,y: integer);
  var
    pc: PColor;
  begin
    pc := context.buffer.Mem;
    Inc(pc, y*context.buffer.SizeX + x);
    pc^ := $FFFFFF;
  end;

  {$Q-}
  procedure FillLine(y, x1, x2: integer; var fti: FaceTexturingInfo; const currentFace: Face);
  var
    celly: integer;
    incell: integer;
    inx1, inx2: integer;
    bc: integer;
    cellx1, cellx2: integer;
    ps, lastps: PStencilCell;
    pci: PCardinal;

    w1,w2,dw1,dw2,
    tx1,tx2,dtx,
    ty1,ty2,dty: integer;

    textureShift: integer;
    ltx1,ltx2,lty1,lty2: integer;

    procedure CountDeltas;
    begin
      if inx2>inx1 then begin
        dtx := (tx2-tx1) div (inx2+1-inx1);
        dty := (ty2-ty1) div (inx2+1-inx1);
      end else begin
        dtx := (tx2-tx1);
        dty := (ty2-ty1);
      end;
    end;

    procedure CountBordersAndIterate;
    begin
      tx1 := ltx1 + muldiv(ltx2-ltx1, w2, w1+w2);
      ty1 := lty1 + muldiv(lty2-lty1, w2, w1+w2);
      w1 := w1 + dw1*(inx2+1-inx1);
      w2 := w2 + dw2*(inx2+1-inx1);
      tx2 := ltx1 + muldiv(ltx2-ltx1, w2, w1+w2);
      ty2 := lty1 + muldiv(lty2-lty1, w2, w1+w2);
      CountDeltas;
    end;  {}

    procedure Loop(pixelLine: P32Colors; tx1,dtx,ty1,dty: integer);
    var
      j: integer;
    begin
      if pci^ = 0 then begin
        for j := inx1 to inx2 do begin
          {$I loop.inc}
          tx1 := tx1 + dtx;
          ty1 := ty1 + dty;
        end;
      end else if pci^ and (pci^+1) = 0 then begin      // last free
        bc := bsr(pci^)+1;
        if bc<inx1 then bc := inx1;
        if bc<=inx2 then begin
          tx1 := tx1 + dtx*(bc-inx1);
          ty1 := ty1 + dty*(bc-inx1);
          for j := bc to inx2 do begin
            {$I loop.inc}
            tx1 := tx1 + dtx;
            ty1 := ty1 + dty;
          end;
        end;
      end else if pci^ or (pci^-1) = STENCIL_FULL then begin   // first free
        bc := bsf(pci^)-1;
        if bc>inx2 then bc := inx2;
        if inx1<=bc then begin
          for j := inx1 to bc do begin
            {$I loop.inc}
            tx1 := tx1 + dtx;
            ty1 := ty1 + dty;
          end;
        end;
      end else begin
        for j := inx1 to inx2 do begin
          if ((pci^ shr j) and 1) = 0 then begin
            {$I loop.inc}
          end;
          tx1 := tx1 + dtx;
          ty1 := ty1 + dty;
        end;
      end;
    end;

    function truncFactored (f : single; factor: integer): integer;
    // for floor(f*2**factor)
    // FUCKING HACK
    var
      d : integer absolute f;
      e,m : integer;
    begin
      e := (d shr 23) and $FF;
      if e<>0 then
        m := (d and $7FFFFF) or $800000
      else
        m := (d and $7FFFFF) shr 1;
      e := 150-factor-e;    // 127 + 23 - 16 - e
      if e>=32 then result := 0
      else if e>0 then result := m shr e else result := m shl (-e);
      if d<0 then result := -1-result;
    end;   {}

    procedure CountFTI(var fti: FaceTexturingInfo; const currentFace: Face);
    var
      MN, MNx, MNy, corner: Point;
      den: float;
      colorFactor: TColor;
      lw1, lw2: float;
      i,r,g,b: integer;
    begin
      with fti do begin
        if not processed then begin
          processed := TRUE;
          MN  := RotateP(context.view, currentFace.normal, true);
          MNx := RotateP(context.view, currentFace.txn, true);
          MNy := RotateP(context.view, currentFace.tyn, true);

          den := 1.0 / (dot(MN, context.view[3]) - currentFace.nshift);
          corner.x := -1.0/context.viewSX;
          corner.y := -1.0/context.viewSY;
          corner.z := 1.0;
          w0 := dot(MN, corner)*den;
          tx0 := dot(MNx, corner) + w0*(currentFace.txc-dot(MNx, context.view[3]));
          ty0 := dot(MNy, corner) + w0*(currentFace.tyc-dot(MNy, context.view[3]));

          corner.x := +1.0/context.viewSX;
          corner.y := -1.0/context.viewSY;
          corner.z := 1.0;
          wdx := dot(MN, corner)*den;
          txdx := dot(MNx, corner) + wdx*(currentFace.txc-dot(MNx, context.view[3]));
          tydx := dot(MNy, corner) + wdx*(currentFace.tyc-dot(MNy, context.view[3]));
          wdx := (wdx-w0) / (context.buffer.SizeX);
          txdx := (txdx-tx0) / (context.buffer.SizeX);
          tydx := (tydx-ty0) / (context.buffer.SizeX);

          corner.x := -1.0/context.viewSX;
          corner.y := +1.0/context.viewSY;
          corner.z := 1.0;
          wdy := dot(MN, corner)*den;
          textureShift := currentFace.tn * 65536;
          txdy := dot(MNx, corner) + wdy*(currentFace.txc-dot(MNx, context.view[3]));
          tydy := dot(MNy, corner) + wdy*(currentFace.tyc-dot(MNy, context.view[3]));
          wdy := (wdy-w0) / (context.buffer.SizeY);
          txdy := (txdy-tx0) / (context.buffer.SizeY);
          tydy := (tydy-ty0) / (context.buffer.SizeY);   
          Inc(context.polyCounter);
          
          den := SqrLen(Add(context.viewC, currentFace.pinside));
          colorFactor := round(255/(den*0.3+1)) * $10101;

          for i := 0 to 15 do begin
            r := (basePalette[i] shr 16) * (colorFactor shr 16) shr 7;
            if r>255 then r:=255;     
            g := (basePalette[i] shr 8 and $FF) * (colorFactor shr 8 and $FF) shr 7;
            if g>255 then g:=255;
            b := (basePalette[i] and $FF) * (colorFactor and $FF) shr 7;
            if b>255 then b:=255;
            palette[i] := r shl 16 + g shl 8 + b;
          end;
        end;

        lw1 := w0 + wdx*(x1+1.0) + wdy*(y+1.0);
        lw2 := w0 + wdx*(x2    ) + wdy*(y+1.0);

        dw1  := truncFactored(lw1, 24) and $3fffffff;
        dw2  := truncFactored(lw2, 24) and $3fffffff;
        ltx1 := truncFactored((tx0+txdx*(x1+1)+txdy*(y+1))/lw1, 16);
        ltx2 := truncFactored((tx0+txdx*(x2  )+txdy*(y+1))/lw2, 16);
        lty1 := truncFactored((ty0+tydx*(x1+1)+tydy*(y+1))/lw1, 16);
        lty2 := truncFactored((ty0+tydx*(x2  )+tydy*(y+1))/lw2, 16);
        
        w1 := dw1+1;
        w2 := 1;
        dw1 := -dw1;
        dw2 := dw2;

        if x2>x1+2 then begin
          dw1 := dw1 div (x2-x1-1);
          dw2 := dw2 div (x2-x1-1);
        end;
      end;
    end;

  begin
    inc(counter2, GetTimer);
             { {}
    if x2>x1 then begin
      celly := y shr STENCIL_ORD;
      incell := y and (STENCIL_SIZE-1);
      cellx1 := x1 shr STENCIL_ORD;
      cellx2 := (x2-1) shr STENCIL_ORD;

      ps     := @context.stencil.cells[celly*context.stencil.sizeX + cellx1];
      lastps := @context.stencil.cells[celly*context.stencil.sizeX + cellx2];
      pci := @ps.bits[incell];

      if (cellx1=cellx2) then begin
        if pci^ <> STENCIL_FULL then begin
          inx1 := x1 and (STENCIL_SIZE-1);
          inx2 := (x2-1) and (STENCIL_SIZE-1)-1;
          CountFTI(fti, currentFace);
          CountBordersAndIterate;
          Inc(inx2);
          Loop(ps.pixels[incell],tx1,dtx,ty1,dty);
        end;
      end else begin
        inx1 := x1 and (STENCIL_SIZE-1);
        inx2 := STENCIL_SIZE-1;
        if pci^ <> STENCIL_FULL then begin
          CountFTI(fti, currentFace);
          CountBordersAndIterate;
          Loop(ps.pixels[incell],tx1,dtx,ty1,dty);
          Inc(ps);
          pci := @ps.bits[incell];
          tx1 := tx2;
          ty1 := ty2;
          // here we know, we has borders
        end else begin
          while true do begin
            Inc(ps);
            pci := @ps.bits[incell];   
            x1 := x1 and not (STENCIL_SIZE - 1) + STENCIL_SIZE;
            if pci^ <> STENCIL_FULL then begin
              CountFTI(fti, currentFace);
              break;
            end;
            if ps=lastps then exit;
          end;
          tx1 := ltx1 + muldiv(ltx2-ltx1, w2, w1+w2);
          ty1 := lty1 + muldiv(lty2-lty1, w2, w1+w2);
          // here we know, ps now good
        end;
  
        while true do begin     
          // here we has border1
          if ps=lastps then begin
            if pci^ <> STENCIL_FULL then begin
              inx1 := 0;
              inx2 := (x2-1) and (STENCIL_SIZE-1)-1;
              w1 := w1 + dw1*(inx2+1-inx1);
              w2 := w2 + dw2*(inx2+1-inx1);
              tx2 := ltx1 + muldiv(ltx2-ltx1, w2, w1+w2);
              ty2 := lty1 + muldiv(lty2-lty1, w2, w1+w2);
              CountDeltas;
              Inc(inx2);
              Loop(ps.pixels[incell],tx1,dtx,ty1,dty);
            end;

            exit;
          end;
                      
          inx1 := 0;
          inx2 := STENCIL_SIZE-1;
          if pci^ <> STENCIL_FULL then begin
            w1 := w1 + dw1*STENCIL_SIZE;
            w2 := w2 + dw2*STENCIL_SIZE;
            tx2 := ltx1 + muldiv(ltx2-ltx1, w2, w1+w2);
            ty2 := lty1 + muldiv(lty2-lty1, w2, w1+w2);
            dtx := (tx2-tx1) div STENCIL_SIZE;
            dty := (ty2-ty1) div STENCIL_SIZE;
            Loop(ps.pixels[incell],tx1,dtx,ty1,dty);
            Inc(ps);
            pci := @ps.bits[incell];
            tx1 := tx2;
            ty1 := ty2;
          end else begin
            while true do begin
              w1 := w1 + dw1*STENCIL_SIZE;
              w2 := w2 + dw2*STENCIL_SIZE;
              Inc(ps);
              pci := @ps.bits[incell];
              if pci^ <> STENCIL_FULL then break;
              if ps=lastps then exit;
            end;
            tx1 := ltx1 + muldiv(ltx2-ltx1, w2, w1+w2);
            ty1 := lty1 + muldiv(lty2-lty1, w2, w1+w2);
            // here we know, ps now good
          end;
        end;
      end;
    end;
  end;    
  {$IFDEF HASQ} {$Q+} {$ENDIF}

  procedure AdjustLineInfo(y, x1, x2: integer);
  var
    i: integer;
    celly: integer;
    incell: integer;
    cellx1, cellx2: integer;
    ps: PStencilCell;
    pci: PCardinal;
    newpci: Cardinal;
  begin
    if x2>x1 then begin
      celly := y shr STENCIL_ORD;
      incell := y and (STENCIL_SIZE-1);
      cellx1 := x1 shr STENCIL_ORD;
      cellx2 := (x2-1) shr STENCIL_ORD;

      ps := @context.stencil.cells[celly*context.stencil.sizeX + cellx1];

      for i := cellx1 to cellx2 do begin
        newpci := STENCIL_FULL;
        if i=cellx1 then newpci :=  newpci - cardinal(1 shl ( x1    and (STENCIL_SIZE-1)) - 1);
        if i=cellx2 then newpci := (newpci + cardinal(1 shl ((x2-1) and (STENCIL_SIZE-1) + 1))) and STENCIL_FULL;

        pci := @ps.bits[incell];    
        newpci := newpci and not pci^;
        ps.count := ps.count - bitcount[newpci and $FFFF] - bitcount[newpci shr 16];

        pci^ := pci^ or newpci;
        Inc(ps);
      end;
    end;
  end;

  procedure DrawFace(var currentFace: Face; var zone: Zone);
  var
    i, j: integer;
    pl: PLine;
    pcl: PCutedLine;
    failed: boolean;
    zinside: boolean;
    hl,lin,lout,rin,rout: boolean;
    //eleft, eright: boolean;
    center: Point;

    fti: FaceTexturingInfo;
  begin
    with currentFace do begin
      failed := FALSE;
      for i := Low(lines) to High(lines) do begin
        pcl := @lc[lines[i]];
        if not pcl.processed then PrerenderLine(lines[i]);
        if pcl.failed then failed := true;
      end;

      center := toPoint(0.0, 0.0, 0.0);
      for i := Low(vertexes) to High(vertexes) do
        center := Add(center, proj[vertexes[i]].p);
      center := Scale(center, 1/Length(vertexes));

      zone.rx := nil;
      zone.lx := nil;   
      zone.y1 := context.buffer.SizeY;
      zone.y2 := 0;

      if not failed then begin

        lin  := FALSE;
        lout := FALSE;
        rin  := FALSE;
        rout := FALSE;
        hl   := FALSE;
        zinside := TRUE;

        for j := Low(lines) to High(lines) do begin
          pl := @v.lines[lines[j]];
          pcl := @lc[lines[j]];
          if ((pl.P2 = vertexes[j]) and (pcl.zeroDist>0)) or ((pl.P1 = vertexes[j]) and (pcl.zeroDist<0)) then begin
          end else begin
            zinside := FALSE;
          end;

          if not pcl.cuted then begin

            if pl.P2 = vertexes[j] then begin
              if pcl.i1=0 then lout := TRUE;
              if pcl.i2=0 then lin  := TRUE;
              if pcl.i1=2 then rout := TRUE;
              if pcl.i2=2 then rin  := TRUE;
            end else begin
              if pcl.i2=0 then lout := TRUE;
              if pcl.i1=0 then lin  := TRUE;
              if pcl.i2=2 then rout := TRUE;
              if pcl.i1=2 then rin  := TRUE;
            end;

            hl := TRUE;
            if pcl.y1<zone.y1 then zone.y1:=pcl.y1;
            if pcl.y2>zone.y2 then zone.y2:=pcl.y2;
          end;
        end;


        if hl then begin
          //zone.x1 := context.buffer.sizeX;
          //zone.x2 := 0;

          if lin and not lout then
            zone.y1 := 0;
          if rout and not rin then
            zone.y1 := 0;
          if rin and not rout then
            zone.y2 := context.buffer.SizeY;
          if lout and not lin then
            zone.y2 := context.buffer.SizeY;

          zone.rx := PAInteger(@intBuffer[intTop]);
          Inc(intTop, zone.y2-zone.y1);
          zone.lx := PAInteger(@intBuffer[intTop]);
          Inc(intTop, zone.y2-zone.y1);
          for i := 0 to zone.y2-zone.y1-1 do begin
            zone.rx[i] := context.buffer.sizeX;
            zone.lx[i] := 0;
          end;

          //eleft := TRUE;
          //eright := TRUE;

          for j := Low(lines) to High(lines) do begin
            pl := @v.lines[lines[j]];
            pcl := @lc[lines[j]];
            if not pcl.cuted then begin
              if (pl.P2 = vertexes[j]) xor (pcl.reversed) then begin
                for i := pcl.y1 to pcl.y2-1 do zone.rx[i-zone.y1] := pcl.x[i-pcl.y1];
                //eright := FALSE;
              end else begin
                for i := pcl.y1 to pcl.y2-1 do zone.lx[i-zone.y1] := pcl.x[i-pcl.y1];
                //eleft := FALSE;
              end;
            end;

            //if zone.x1 > pcl.x1 then zone.x1 := pcl.x1;
            //if zone.x2 < pcl.x2 then zone.x2 := pcl.x2;
          end;

          //if lin or lout or eleft  then zone.x1 := 0;
          //if rin or rout or eright then zone.x2 := context.buffer.sizeX -1;
        end else if zinside then begin
          zone.y1 := 0;
          zone.y2 := context.buffer.SizeY;
          
          //zone.x1 := 0;
          //zone.x2 := context.buffer.sizeX-1;

          zone.rx := PAInteger(@intBuffer[intTop]);
          Inc(intTop, zone.y2-zone.y1);
          zone.lx := PAInteger(@intBuffer[intTop]);
          Inc(intTop, zone.y2-zone.y1);
          for i := 0 to zone.y2-zone.y1-1 do begin
            zone.rx[i] := context.buffer.sizeX;
            zone.lx[i] := 0;
          end;
        end else begin
          zone.y1 := context.buffer.sizeY;
          zone.y2 := 0;
        end;

        if (abs(currentFace.normal.x)>abs(currentFace.normal.y)) and (abs(currentFace.normal.x)>abs(currentFace.normal.z)) then begin
          txn.x := 0.0;
          txn.y := 600.0;
          txn.z := 0.0;
          txc := 0;

          tyn.x := 0.0;
          tyn.y := 0.0;
          tyn.z := -600.0;
          tyc := 0;
        end else if (abs(currentFace.normal.y)>abs(currentFace.normal.z)) then begin
          txn.x := -600.0;
          txn.y := 0.0;
          txn.z := 0.0;
          txc := 0;

          tyn.x := 0.0;
          tyn.y := 0.0;
          tyn.z := 600.0;
          tyc := 0;
        end else begin      
          txn.x := 600.0;
          txn.y := 0.0;
          txn.z := 0.0;
          txc := 0;

          tyn.x := 0.0;
          tyn.y := -600.0;
          tyn.z := 0.0;
          tyc := 0;
        end;
                                          {
        txn := toPoint(0.0, 0.0, 0.0);
        txc := currentFace.id;
        tyn := toPoint(0.0, 0.0, 0.0); 
        tyc := 0;                 {}

        fti.processed := FALSE;

        for i := 0 to zone.y2-zone.y1-1 do if zone.lx[i]<zone.rx[i] then begin
          FillLine(i+zone.y1, zone.lx[i], zone.rx[i], fti, currentFace);    
        end;
      end;
    end;
  end;

begin
  intTop       := 0;
  projPointTop := 0;
  cutedLineTop := 0;
  zoneTop      := 0;

  proj := PAProjPoint(@projPointBuffer[projPointTop]);
  Inc(projPointTop, Length(v.vertexes));

  lc := PACutedLine(@cutedLineBuffer[cutedLineTop]);
  Inc(cutedLineTop, Length(v.lines));

  zones := PAZone(@zoneBuffer[zoneTop]);
  Inc(zoneTop, Length(v.faces));

  for i := Low(v.vertexes) to High(v.vertexes) do proj[i].hasProj := -1;
  for i := Low(v.lines) to High(v.lines) do lc[i].processed := FALSE;

  for i := Low(v.faces) to High(v.faces) do with v.faces[i] do begin
    vnormal := RotateP(context.view, v.faces[i].normal, TRUE);
    vshift := v.faces[i].nshift - dot(context.view[3], vnormal);
    if vshift > 0.0 then begin
      DrawFace(v.faces[i], zones[i]);
    end else begin
      zones[i].y1 := 0;
      zones[i].y2 := -1;
    end;
  end;

  for j := Low(v.faces) to High(v.faces) do begin
    for i := 0 to zones[j].y2-zones[j].y1-1 do begin
      AdjustLineInfo(i+zones[j].y1, zones[j].lx[i], zones[j].rx[i]);
    end;
  end;

  for i := 0 to Length(v.vertexes)-1 do begin
    v.vertexes[i].icounter := 0;
    v.vertexes[i].fcounter := 0.0;
  end;
end;

function FastCheckCube(var context: RenderContext; const pmin, pmax: Point): boolean;
var
  middle: Point;
  rp: array [0..7] of Point;
  x, y: integer;
  x1,y1,x2,y2: integer;
  i,j: integer;
  dx,dy,dz: Point;
begin
  //if SqrLen(context.view[3]) < SqrLen(Sub(pmin, pmax)) * 20.0 then begin
  //if true then begin
  //  result := true;
  //end else begin
    middle := RotateP(context.view, Scale(Add(pmin, pmax), 0.5));
    dx := Scale(context.view[0], pmax.x-pmin.x);
    dy := Scale(context.view[1], pmax.y-pmin.y);
    dz := Scale(context.view[2], pmax.z-pmin.z);

    rp[0] := Sub(middle, Scale(Add(dx, Add(dy, dz)), 0.5));
    rp[1] := Add(rp[0], dx);
    rp[2] := Add(rp[0], dy);
    rp[3] := Add(rp[0], dz);
    rp[4] := Add(rp[1], dy);
    rp[5] := Add(rp[2], dz);
    rp[6] := Add(rp[3], dx);
    rp[7] := Add(rp[4], dz);
    x1 := 0;
    y1 := 0;
    x2 := 0;
    y2 := 0;

    result := FALSE;
    for i := 0 to 7 do if rp[i].z>rp[i].x*context.viewSX then result := TRUE;
    if not result then exit;

    result := FALSE;
    for i := 0 to 7 do if rp[i].z>-rp[i].x*context.viewSX then result := TRUE;
    if not result then exit;

    result := FALSE;
    for i := 0 to 7 do if rp[i].z>rp[i].y*context.viewSY then result := TRUE;
    if not result then exit;
    result := FALSE;
    for i := 0 to 7 do if rp[i].z>-rp[i].y*context.viewSY then result := TRUE;
    if not result then exit;

    result := FALSE;
    for i := 0 to 7 do if rp[i].z<=0.5*abs(rp[i].x)*context.viewSX+0.0001 then result := TRUE;
    if not result then for i := 0 to 7 do if rp[i].z<=0.5*abs(rp[i].y)*context.viewSY+0.0001 then result := TRUE;

    if not result then begin
      for i := 0 to 7 do begin
        x := trunc((rp[i].x/rp[i].z*context.viewSX+1.0) * context.buffer.SizeX * 0.5);
        y := trunc((rp[i].y/rp[i].z*context.viewSY+1.0) * context.buffer.SizeY * 0.5);

        x := (x + ($10000 shl STENCIL_ORD)) shr STENCIL_ORD - $10000;
        y := (y + ($10000 shl STENCIL_ORD)) shr STENCIL_ORD - $10000;

        if i=0 then begin
          x1 := x; y1 := y; x2 := x; y2 := y;
        end else begin
          if x<x1 then x1:=x;
          if y<y1 then y1:=y;
          if x>x2 then x2:=x;
          if y>y2 then y2:=y;
        end;
      end;

      if x1<0 then x1:=0;
      if y1<0 then y1:=0;
      if x2>=context.stencil.sizeX then x2:=context.stencil.sizeX-1;
      if y2>=context.stencil.sizeY then y2:=context.stencil.sizeY-1;

      for j := y1 to y2 do begin
        for i := x1 to x2 do begin
          if context.stencil.cells[i+j*context.stencil.sizeX].count>0 then begin
            result := true;
            break;
          end;
        end;
      end;
    end;
  //end;
end;

procedure Counts(var context: RenderContext);
var
  i,j: integer;
  pc: PColor;
begin
  for j := 0 to context.stencil.sizeY-1 do for i := 0 to context.stencil.sizeX - 1 do begin
    pc := context.buffer.Mem;
    Inc(pc, i+j*context.buffer.SizeX);
    pc^ := context.stencil.cells[j*context.stencil.sizeX+i].count*49;
  end;
end;

end.
