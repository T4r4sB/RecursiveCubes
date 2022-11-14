unit Geometry;

interface

uses Points;

type
  Vertex = record
    p: Point;
    icounter: integer;
    fcounter: float;
  end;

  PVertex = ^Vertex;

  PVertexArray = array of PVertex;

  Line = record
    P1, P2: integer; // vertexIndex
    icounter: integer;
    ucounter: cardinal;
  end;

  PLine = ^Line;

  IntegerArray = array of integer;

  Face = record
    id: integer;
    lines: IntegerArray;    // lineIndex
    vertexes: IntegerArray; // vertexIndex
    normal: Point;
    nshift: float;
    pinside: Point;
    sign: integer;

    tn: integer;
    txn: Point;
    txc: float;
    tyn: Point;
    tyc: float;
  end;

  PFace = ^Face;

  Convex = record
    lines: array of Line;
    faces: array of Face;
    vertexes: array of Vertex;
    pmin, pmax: Point;
  end;

  PConvex = ^Convex;

function GetConvex(var vertexes: array of PPoint): Convex;
procedure CutConvex(var v: Convex; const cutNormal: Point; cutNShift: float; closed: boolean);
procedure ReverseConvex(var v: Convex);
procedure AddSubConvex(var outv, inv: Convex);
procedure TranslateConvex(var v: Convex; const b: Point);
procedure ScaleConvex(var v: Convex; s: float);

implementation

function GetSign(s: float): integer;
begin
  if s > 0.000001 then result := 1
  else if s > -0.000001 then result := 0
  else result := -1;
end;

function GetSignWithEps(s: float; eps: float): integer;
begin
  if s > eps then result := 1
  else if s > -eps then result := 0
  else result := -1;
end;

procedure DeleteUnusedLines(var v: Convex);
var
  i, j: integer;
  lineni: array of integer;
begin
  for i := Low(v.faces) to High(v.faces) do with v.faces[i] do begin
    for j := Low(lines) to High(lines) do begin
      v.lines[lines[j]].icounter := 1;
    end;
  end;

  SetLength(lineni, Length(v.lines));
  j := 0;
  for i := Low(v.lines) to High(v.lines) do begin
    lineni[i] := j;
    if v.lines[i].icounter>0 then begin
      if i>j then v.lines[j] := v.lines[i];
      v.lines[j].icounter := 0;
      Inc(j);
    end;
  end;
  SetLength(v.lines, j);

  for i := Low(v.faces) to High(v.faces) do with v.faces[i] do begin
    for j := Low(lines) to High(lines) do begin
      lines[j] := lineni[lines[j]];
    end;
  end;
end;

procedure DeleteUnusedVertexes(var v: Convex);
var
  i, j: integer;
  vertexesi: array of integer;
begin
  for i := Low(v.vertexes) to High(v.vertexes) do v.vertexes[i].icounter := 0;

  for i := Low(v.lines) to High(v.lines) do with v.lines[i] do begin
    v.vertexes[P1].icounter := 1;                                      
    v.vertexes[P2].icounter := 1;
  end;

  SetLength(vertexesi, Length(v.vertexes));
  j := 0;
  for i := Low(v.vertexes) to High(v.vertexes) do begin
    vertexesi[i] := j;
    if v.vertexes[i].icounter>0 then begin
      if i>j then v.vertexes[j] := v.vertexes[i];
      v.vertexes[j].icounter := 0;
      Inc(j);
    end;
  end;
  SetLength(v.vertexes, j);

  for i := Low(v.lines) to High(v.lines) do with v.lines[i] do begin
    P1 := vertexesi[P1];
    P2 := vertexesi[P2];
  end;

  for i := Low(v.faces) to High(v.faces) do with v.faces[i] do begin
    for j := Low(vertexes) to High(vertexes) do begin
      vertexes[j] := vertexesi[vertexes[j]];
    end;
  end;
end;

procedure DeleteUnusedFromConvex(var v: Convex);
begin
  DeleteUnusedLines(v);
  DeleteUnusedVertexes(v);
end;

function GetConvex(var vertexes: array of PPoint): Convex;
var
  center: Point;

  type T = PPoint;
  function LESS(p1,p2: PPoint): boolean;
  begin
    result := sqrLen(Sub(p1^, center)) > sqrLen(Sub(p2^,center));
  end;    
  {$I qsort.inc}

  procedure MoveFirst4Points;
    procedure FindPoint1;
    var
      i: integer;
      s: float;
      tmp: PPoint;
    begin
      for i := Low(vertexes)+1 to High(vertexes) do begin
        s := sqrlen(sub(vertexes[i]^, vertexes[0]^));
        if s > 0.0001 then begin
          if i>1 then begin
            tmp := vertexes[1];
            vertexes[1] := vertexes[i];
            vertexes[i] := tmp;
          end;
          break;
        end;
      end;
    end;

    procedure FindPoint2;
    var
      i: integer;
      s: float;
      tmp: PPoint;
    begin
      for i := Low(vertexes)+2 to High(vertexes) do begin
        s := sqrlen(cross(sub(vertexes[0]^,vertexes[i]^), sub(vertexes[1]^,vertexes[i]^)));

        if s > 0.0000000001 then begin
          if i>2 then begin
            tmp := vertexes[2];
            vertexes[2] := vertexes[i];
            vertexes[i] := tmp;
          end;
          break;
        end;
      end;
    end;

    procedure FindPoint3;
    var
      i: integer;
      s: float;
      tmp: PPoint;
    begin
      for i := Low(vertexes)+3 to High(vertexes) do begin
        s := volume(sub(vertexes[0]^,vertexes[i]^), sub(vertexes[1]^,vertexes[i]^), sub(vertexes[2]^,vertexes[i]^));

        if abs(s) > 0.00000001 then begin
          if i>3 then begin
            tmp := vertexes[3];
            vertexes[3] := vertexes[i];
            vertexes[i] := tmp;
          end;
          if s<0 then begin
            tmp := vertexes[3];
            vertexes[3] := vertexes[2];
            vertexes[2] := tmp;
          end;
          break;
        end;
      end;
    end;

  begin
    FindPoint1;
    FindPoint2;
    FindPoint3;
  end;

  function CreateFirstTetraedr: Convex;
  var
    i: integer;
  begin
    SetLength(result.vertexes, 4);
    result.vertexes[0].p := vertexes[0]^;     
    result.vertexes[1].p := vertexes[1]^;
    result.vertexes[2].p := vertexes[2]^;
    result.vertexes[3].p := vertexes[3]^;

    SetLength(result.lines, 6);
    result.lines[0].P1 := 0;
    result.lines[0].P2 := 1;
    result.lines[1].P1 := 0;
    result.lines[1].P2 := 2;
    result.lines[2].P1 := 1;
    result.lines[2].P2 := 2;
    result.lines[3].P1 := 0;
    result.lines[3].P2 := 3;
    result.lines[4].P1 := 1;
    result.lines[4].P2 := 3;
    result.lines[5].P1 := 2;
    result.lines[5].P2 := 3;
    for i := Low(result.lines) to High(result.lines) do begin
      result.lines[i].icounter := 0;
      result.lines[i].ucounter := 0;
    end;
    SetLength(result.faces, 4);

    SetLength(result.faces[0].lines, 3);
    result.faces[0].lines[0] := 2;
    result.faces[0].lines[1] := 4;
    result.faces[0].lines[2] := 5;
    result.faces[0].normal := norm(cross(sub(vertexes[1]^,vertexes[2]^), sub(vertexes[3]^,vertexes[2]^)));
    result.faces[0].nshift := -dot(vertexes[1]^, result.faces[0].normal);
    result.faces[0].pinside := Scale(add(add(vertexes[1]^, vertexes[2]^), vertexes[3]^), 1.0/3.0);

    SetLength(result.faces[1].lines, 3);
    result.faces[1].lines[0] := 1;
    result.faces[1].lines[1] := 3;
    result.faces[1].lines[2] := 5;
    result.faces[1].normal := norm(cross(sub(vertexes[0]^,vertexes[3]^), sub(vertexes[2]^,vertexes[3]^)));
    result.faces[1].nshift := -dot(vertexes[2]^, result.faces[1].normal);
    result.faces[1].pinside := scale(add(add(vertexes[2]^, vertexes[3]^), vertexes[0]^), 1.0/3.0);

    SetLength(result.faces[2].lines, 3);
    result.faces[2].lines[0] := 0;
    result.faces[2].lines[1] := 3;
    result.faces[2].lines[2] := 4;
    result.faces[2].normal := norm(cross(sub(vertexes[3]^,vertexes[0]^), sub(vertexes[1]^,vertexes[0]^)));
    result.faces[2].nshift := -dot(vertexes[3]^, result.faces[2].normal);
    result.faces[2].pinside := scale(add(add(vertexes[3]^, vertexes[0]^), vertexes[1]^), 1.0/3.0);

    SetLength(result.faces[3].lines, 3);
    result.faces[3].lines[0] := 0;
    result.faces[3].lines[1] := 1;
    result.faces[3].lines[2] := 2;
    result.faces[3].normal := norm(cross(sub(vertexes[2]^,vertexes[1]^), sub(vertexes[0]^,vertexes[1]^)));
    result.faces[3].nshift := -dot(vertexes[0]^, result.faces[3].normal);
    result.faces[3].pinside := scale(add(add(vertexes[0]^, vertexes[1]^), vertexes[2]^), 1.0/3.0);
    for i := Low(result.faces) to High(result.faces) do result.faces[i].id := i;
  end;

  procedure AddPoint(var v: Convex; P: PPoint);
  var
    newP: integer;
    i, j, k: integer;
    f, df: integer;
    pl: PLine;
    pv1, pv2: PVertex;

    pfaces: array of integer;
    zfaces: array of integer;
    blines: array of integer;

    procedure ProcessBorderFace(facei: integer; ormask: integer);
    var
      j,k: integer;
      pv1, pv2: PVertex;
    begin
      with v.faces[facei] do for j := Low(lines) to High(lines) do begin
        pl := @v.lines[lines[j]];
        if pl.icounter=1 then begin
          k := length(blines);
          SetLength(blines, k+1);
          blines[k] := lines[j];
          pv1 := @v.vertexes[pl.P1];
          pv2 := @v.vertexes[pl.P2];
          pv1.icounter := pv1.icounter xor id or ormask;
          pv2.icounter := pv2.icounter xor id or ormask;
        end;
      end;
    end;

    function CreateLine(P1, P2: integer): integer;
    begin
      result := length(v.lines);
      SetLength(v.lines, result+1);
      v.lines[result].P1 := P1;
      v.lines[result].P2 := P2;
      v.lines[result].icounter := 0;
      v.lines[result].ucounter := 0;
      // result cannot be zero we already have lines
    end;

  begin
    for i := Low(v.faces) to High(v.faces) do with v.faces[i] do begin
      sign := GetSign(dot(normal, P^) + nshift);

      if sign=0 then begin
        j := Length(zfaces);
        SetLength(zfaces, j+1);
        zfaces[j] := i;
      end else if sign=1 then begin
        j := Length(pfaces);
        SetLength(pfaces, j+1);
        pfaces[j] := i;
      end;

      if sign>=0 then for j := Low(lines) to High(lines) do begin
        pl := @v.lines[lines[j]];
        pl.icounter := 1 - pl.icounter;
      end;
    end;

    if Length(pfaces)=0 then begin
      for i := Low(zfaces) to High(zfaces) do with v.faces[zfaces[i]] do begin
        for j := Low(lines) to High(lines) do begin
          pl := @v.lines[lines[j]];
          pl.icounter := 0;
        end;
      end;
    end else begin
      newP := Length(v.vertexes);
      SetLength(v.vertexes, newP+1);
      v.vertexes[newP].p := P^;
      v.vertexes[newP].icounter := 0;
      v.vertexes[newP].fcounter := 0;

      for i := Low(zfaces) to High(zfaces) do ProcessBorderFace(zfaces[i], 0);
      for i := Low(pfaces) to High(pfaces) do ProcessBorderFace(pfaces[i], -$7FFFFFFF-1); // 0x80 00 00 00

      for i := Low(blines) to High(blines) do begin
        pl := @v.lines[blines[i]];
        pv1 := @v.vertexes[pl.P1];
        pv2 := @v.vertexes[pl.P2];
        if pv1.icounter<>0 then pv1.icounter := -1;
        if pv2.icounter<>0 then pv2.icounter := -1;
      end;

      for i := Low(blines) to High(blines) do begin
        pl := @v.lines[blines[i]];
        pv1 := @v.vertexes[pl.P1];    
        pv2 := @v.vertexes[pl.P2];

        if pv1.icounter<0 then begin
          k := createLine(newP, pl.P1);
          pl := @v.lines[blines[i]];
          pv1.icounter := k;
        end;

        if pv2.icounter<0 then begin
          k := createLine(newP, pl.P2);
          pl := @v.lines[blines[i]];
          pv2.icounter := k;
        end;
      end;

      for i := Low(zfaces) to High(zfaces) do with v.faces[zfaces[i]] do begin
        k := Length(lines);
        SetLength(lines, k+2);
        for j := Low(lines) to High(lines)-2 do begin
          pl := @v.lines[lines[j]];
          if pl.icounter=1 then begin
            pv1 := @v.vertexes[pl.P1];
            pv2 := @v.vertexes[pl.P2];
            if pv1.icounter<>0 then begin
              lines[k] := pv1.icounter;
              Inc(k);
            end;
            if pv2.icounter<>0 then begin
              lines[k] := pv2.icounter;
              Inc(k);
            end;
          end;
        end;
         
        k := 0;
        for j := Low(lines) to High(Lines) do begin
          if (v.lines[lines[j]].icounter=1) or (j>High(lines)-2) then begin
            lines[k] := lines[j];
            Inc(k);
          end;
        end;
        SetLength(lines, k);
      end;

      df := 0;
      for i := Low(pfaces) to High(pfaces) do with v.faces[pfaces[i]] do begin
        for j := Low(lines) to High(lines) do begin
          pl := @v.lines[lines[j]];
          if pl.icounter=1 then Inc(df);
        end;
      end;

      f := Length(v.faces);
      SetLength(v.faces, f+df);

      for i := Low(pfaces) to High(pfaces) do with v.faces[pfaces[i]] do begin
        for j := Low(lines) to High(lines) do begin
          pl := @v.lines[lines[j]];
          if pl.icounter=1 then begin
            pv1 := @v.vertexes[pl.P1];
            pv2 := @v.vertexes[pl.P2];

            SetLength(v.faces[f].lines, 3);
            v.faces[f].id       := f;
            v.faces[f].lines[0] := pv1.icounter;
            v.faces[f].lines[1] := pv2.icounter;
            v.faces[f].lines[2] := lines[j];
            v.faces[f].normal   := norm(cross(sub(pv1.p,P^), sub(pv2.p,P^)));
            v.faces[f].nshift   := -dot(P^, v.faces[f].normal);
            if dot(pinside, v.faces[f].normal)+v.faces[f].nshift>0 then begin
              v.faces[f].normal := scale(v.faces[f].normal, -1.0);
              v.faces[f].nshift := -v.faces[f].nshift;
            end;
            v.faces[f].pinside := scale(add(add(pv1.p, pv2.p), P^), 1.0/3.0);
            v.faces[f].sign := 0;
            Inc(f);
          end;
        end;
      end;

      for i := Low(blines) to High(blines) do begin
        pl := @v.lines[blines[i]];
        pv1 := @v.vertexes[pl.P1];
        pv2 := @v.vertexes[pl.P2];
        pv1.icounter := 0;
        pv2.icounter := 0;
        pl.icounter := 0;
      end;

      j := 0;
      for i := Low(v.faces) to High(v.faces) do begin
        if v.faces[i].sign<=0 then begin
          if j<i then v.faces[j] := v.faces[i];
          v.faces[j].id := j;
          Inc(j);
        end;
      end;

      SetLength(v.faces, j);
    end;
  end;

  procedure AdjustFacePoints(var v: Convex);
  var
    i, j: integer;
    s: float;
    pl: PLine;
    pv1, pv2: PVertex;
    orderedLines: IntegerArray;
  begin
    for i := Low(v.faces) to High(v.faces) do with v.faces[i] do begin
      for j := Low(lines) to High(lines) do begin
        pl := @v.lines[lines[j]];
        pv1 := @v.vertexes[pl.P1];
        pv2 := @v.vertexes[pl.P2];
        s := dot(cross(sub(pv1.p, pinside), sub(pv2.p, pinside)), normal);
        if s>0 then pv1.icounter := lines[j] else pv2.icounter := lines[j];
      end;

      SetLength(vertexes, Length(lines));
      SetLength(orderedLines, Length(lines));
      vertexes[0] := v.lines[lines[0]].P1;
      orderedLines[0] := v.vertexes[vertexes[0]].icounter;
      for j := Low(vertexes) to High(vertexes)-1 do begin
        pl := @v.lines[v.vertexes[vertexes[j]].icounter];
        if vertexes[j] = pl.P1 then vertexes[j+1] := pl.P2 else vertexes[j+1] := pl.P1;
        orderedLines[j+1] := v.vertexes[vertexes[j+1]].icounter;
      end;

      for j := Low(vertexes) to High(vertexes) do v.vertexes[vertexes[j]].icounter := 0;
      lines := orderedLines;
    end;
  end;

  procedure CountMinMax(var v: Convex);
  var
    i: integer;
  begin
    v.pmin := v.vertexes[0].p;
    v.pmax := v.vertexes[0].p;
    for i := 1 to Length(v.vertexes)-1 do begin
      if v.vertexes[i].p.x<v.pmin.x then v.pmin.x:=v.vertexes[i].p.x;
      if v.vertexes[i].p.y<v.pmin.y then v.pmin.y:=v.vertexes[i].p.y;
      if v.vertexes[i].p.z<v.pmin.z then v.pmin.z:=v.vertexes[i].p.z;
      if v.vertexes[i].p.x>v.pmax.x then v.pmax.x:=v.vertexes[i].p.x;
      if v.vertexes[i].p.y>v.pmax.y then v.pmax.y:=v.vertexes[i].p.y;
      if v.vertexes[i].p.z>v.pmax.z then v.pmax.z:=v.vertexes[i].p.z;
    end;
  end;

var
  i: integer;
begin  // GetConvex
  center.x := 0.0;
  center.y := 0.0;
  center.z := 0.0;
  for i := Low(vertexes) to High(vertexes) do begin
    center.x := center.x + vertexes[i].x;
    center.y := center.y + vertexes[i].y;
    center.z := center.z + vertexes[i].z;
  end;

  center := scale(center, 1.0/Length(vertexes));

  Sort(vertexes, Low(vertexes), High(vertexes));
  MoveFirst4Points;
  result := CreateFirstTetraedr;

  for i := 4 to High(vertexes) do begin
    AddPoint(result, vertexes[i]);
    if i mod 10000=0 then
      WriteLn('processed ', i, ' of ', Length(vertexes));
  end;
  DeleteUnusedFromConvex(result);
  AdjustFacePoints(result);
  CountMinMax(result);
end;

function Complete(const v: Convex; vIndex,lIndex: IntegerArray): boolean;
var
  i,pi: integer;
begin
  result := FALSE;
  for i := Low(lIndex) to High(lIndex) do for pi := i+1 to High(lIndex) do
    if vIndex[i]=vIndex[pi] then exit;

  pi := High(lIndex);
  for i := Low(lIndex) to High(lIndex) do begin
    if (v.lines[lIndex[pi]].P1=vIndex[pi]) and (v.lines[lIndex[pi]].P2=vIndex[i]) then
    else if (v.lines[lIndex[pi]].P1=vIndex[i]) and (v.lines[lIndex[pi]].P2=vIndex[pi]) then
    else exit;
    pi := i;
  end;

  result := TRUE;
end;

procedure CutConvex(var v: Convex; const cutNormal: Point; cutNShift: float; closed: boolean);
var
  i, j: integer;
  f, l, fl, rl, al: integer;
  plc: integer;
  hg, hp, fb: boolean;
  b1, b2: cardinal;
  t: float;
  pl: PLine;
  pv1, pv2: PVertex;
  pc: array [0..1] of integer;
  alines: IntegerArray;

  newLines: IntegerArray;
  newVertexes: IntegerArray;
begin
  hp := false;

  for i := Low(v.vertexes) to High(v.vertexes) do begin
    v.vertexes[i].fcounter := dot(v.vertexes[i].p, cutNormal) + cutNshift;
    case GetSign(v.vertexes[i].fcounter) of
      -1: v.vertexes[i].icounter := 0;      
      0: v.vertexes[i].icounter := 1;
      1: v.vertexes[i].icounter := 3;
    end;
  end;

  for i := Low(v.lines) to High(v.lines) do begin
    pl  := @v.lines[i];
    pv1 := @v.vertexes[pl.P1];
    pv2 := @v.vertexes[pl.P2];

    b1 := cardinal(pv1.icounter);
    b2 := cardinal(pv2.icounter);

    if (b1 and b2) > 0 then begin
      if b1=1 then begin
        v.lines[i].ucounter := b1 or (b2 shl 2);
        j := pl.P1;
        pl.P1 := pl.P2;
        pl.P2 := j;
      end else
        v.lines[i].ucounter := b2 or (b1 shl 2);
    end else if (b1 xor b2) = 3 then begin
      plc := length(v.vertexes);
      SetLength(v.vertexes, plc+1);  
      pv1 := @v.vertexes[pl.P1];
      pv2 := @v.vertexes[pl.P2];

      t := pv1.fcounter / (pv1.fcounter-pv2.fcounter);               
      v.vertexes[plc].p := add(scale(pv1.p, 1.0-t), scale(pv2.p, t));
      v.vertexes[plc].icounter := 0;
      v.vertexes[plc].fcounter := 0.0;

      if b1=0 then begin
        v.lines[i].P1 := v.lines[i].P2;
        v.lines[i].ucounter := b1 or (b2 shl 2);
      end else begin
        v.lines[i].ucounter := b2 or (b1 shl 2);
      end;
      v.lines[i].P2 := plc;
    end;
  end;

  al := 0;

  for i := Low(v.faces) to High(v.faces) do with v.faces[i] do begin
    hg := false;
    fl := -1;
    fb := closed;
    for j := Low(lines) to High(lines) do begin
      pl := @v.lines[lines[j]];

      if pl.ucounter and $F<>$5 then fb := false;  // fullBorder
      if pl.ucounter and $A<>0 then hg := true;    // hasGood
      if pl.P1=vertexes[j] then begin
        if pl.ucounter and $C=0 then fl := j;      // first line for process
      end else begin
        if pl.ucounter and $3=0 then fl := j;
      end;
    end;

    if fb then begin
      hg := true;
      assert(fl<0);
    end;

    if hg then begin
      if fl<0 then begin     // full in good side
        for j := Low(lines) to High(lines) do begin
          pl := @v.lines[lines[j]];

          if closed and not fb then begin
            if pl.ucounter=$5 then begin
              SetLength(alines, al+1);
              alines[al] := lines[j];
              Inc(al);
            end;
          end;
        end;
      end else begin
        hp := true;
        l := 0;
        rl := Length(v.lines);
        for j := Low(lines) to High(lines) do begin
          pl := @v.lines[lines[fl]];
          plc := pl.ucounter;

          if pl.P1<>vertexes[fl] then plc := plc shr 2 + (plc and $03) shl 2;
          case plc of
            $0,$1,$4,$5:;
            $3,$7: begin
              if plc=3 then pc[0] := pl.P2 else pc[0] := vertexes[fl];
              SetLength(newLines, l+1);
              SetLength(newVertexes, l+1);
              newLines[l] := lines[fl];
              newVertexes[l] := pc[0];
              Inc(l);
            end;
            $C, $D: begin           
              if plc=$C then pc[1] := pl.P2 else if fl=High(lines) then pc[1] := vertexes[Low(lines)] else pc[1] := vertexes[fl+1];

              SetLength(v.lines, rl+1);
              v.lines[rl].P1 := pc[1];
              v.lines[rl].P2 := pc[0];
              v.lines[rl].icounter := 0;

              SetLength(newLines, l+2);
              SetLength(newVertexes, l+2);
              newLines[l] := lines[fl];
              newLines[l+1] := rl;
              newVertexes[l] := vertexes[fl];
              newVertexes[l+1] := pc[1];

              if closed then begin
                SetLength(alines, al+1);
                alines[al] := rl;
                Inc(al);
              end;
              break;
            end;         
            $F: begin
              SetLength(newLines, l+1);
              SetLength(newVertexes, l+1);
              newLines[l] := lines[fl];
              newVertexes[l] := vertexes[fl];
              Inc(l);
            end;       
            else begin
              assert(False);
            end;
          end;

          if fl=High(lines) then fl := Low(lines) else Inc(fl);
        end; 
        lines := newLines;
        vertexes := newVertexes;
      end;

      sign := 1;
    end else begin
      sign := -1;
    end;
  end;

  if closed and hp then begin
    assert(Length(alines)>=3);

    f := Length(v.faces);
    SetLength(v.faces, f+1);
    with v.faces[f] do begin
      id := f;
      sign := 1;
      pinside.x := 0.0;
      pinside.y := 0.0;
      pinside.z := 0.0;
      for i := Low(alines) to High(alines) do begin
        pl  := @v.lines[alines[i]];
        pv1 := @v.vertexes[pl.P1];
        pv2 := @v.vertexes[pl.P2];
        pinside := add(pinside, add(pv1.p, pv2.p));
      end;
      pinside := scale(pinside, 0.5/Length(alines));
      normal := scale(cutNormal, -1.0);
      nshift := -cutNshift;

      for i := Low(alines) to High(alines) do begin
        pl := @v.lines[alines[i]];
        pv1 := @v.vertexes[pl.P1];
        pv2 := @v.vertexes[pl.P2];
        if dot(cross(sub(pv1.p, pinside), sub(pv2.p, pinside)), normal)>0 then
          pv1.icounter := alines[i]
        else
          pv2.icounter := alines[i];
      end;

      SetLength(vertexes, Length(alines));
      SetLength(lines, Length(alines));
      vertexes[0] := v.lines[alines[0]].P1;
      lines[0] := v.vertexes[vertexes[0]].icounter;
      for j := Low(vertexes) to High(vertexes)-1 do begin
        pl := @v.lines[v.vertexes[vertexes[j]].icounter];
        if vertexes[j] = pl.P1 then vertexes[j+1] := pl.P2 else vertexes[j+1] := pl.P1;
        lines[j+1] := v.vertexes[vertexes[j+1]].icounter;
      end;

      for j := Low(vertexes) to High(vertexes) do v.vertexes[vertexes[j]].icounter := 0;

      assert(Complete(v, vertexes, lines));
    end;
  end;

  j := 0;
  for i := Low(v.faces) to High(v.faces) do begin
    if v.faces[i].sign>0 then begin
      if j<i then v.faces[j] := v.faces[i];
      v.faces[j].id := j;
      Inc(j);
    end;
  end;
  if j=1 then j := 0;
  SetLength(v.faces,j);


  for i := Low(v.vertexes) to High(v.vertexes) do begin
    v.vertexes[i].icounter := 0;
    v.vertexes[i].fcounter := 0.0;
  end;

  for i := Low(v.lines) to High(v.lines) do begin
    v.lines[i].icounter := 0;
    v.lines[i].ucounter := 0;
  end;

  DeleteUnusedFromConvex(v);

  for i := Low(v.vertexes) to High(v.vertexes) do begin
    v.vertexes[i].icounter := 0;
    v.vertexes[i].fcounter := 0.0;
  end;

  for i := Low(v.lines) to High(v.lines) do begin
    v.lines[i].icounter := 0;
    v.lines[i].ucounter := 0;
  end;
end;          {}

procedure ReverseConvex(var v: Convex);
var
  f: integer;
  p1,p2: integer;
  tmpi: integer;
begin
  for f := Low(v.faces) to High(v.faces) do with v.faces[f] do begin
    p1 := Low(vertexes);
    p2 := High(vertexes);
    while (p1<p2) do begin
      tmpi := vertexes[p1];
      vertexes[p1] := vertexes[p2];
      vertexes[p2] := tmpi;
      Inc(p1);
      Dec(p2);
    end;
    p1 := Low(lines);
    p2 := High(lines)-1;
    while (p1<p2) do begin
      tmpi := lines[p1];
      lines[p1] := lines[p2];
      lines[p2] := tmpi;
      Inc(p1);
      Dec(p2);
    end;

    normal := Scale(normal, -1.0);
    nshift := -nshift;
  end;
end;

type CutInfo = record
  pIndex: integer;
  lIndex: integer;
  w: float;
end;

type LineCutInfo = record
  cutIn, cutOut: CutInfo;
  swaped: boolean;
end;

type LineCutInfoArray = array of LineCutInfo;

function AddSubFace(var outv, inv: Convex; outfIndex, infIndex: integer; var vCutInfo: LineCutInfoArray): boolean;
var
  outf,inf: PFace;

  function MidPoint(indexes: array of integer): Point;
  var
    i: integer;
  begin
    result := toPoint(0.0, 0.0, 0.0);
    for i := Low(indexes) to High(indexes) do begin
      result := add(result, outv.vertexes[indexes[i]].p);
    end;
    result := scale(result, 1.0/Length(indexes));
  end;

  procedure AddFace(vIndex, lIndex: IntegerArray);
  var
    fc: integer;
  begin
    fc := Length(outv.faces);
    SetLength(outv.faces, fc+1);   
    outf := @outv.faces[outfIndex];
    outv.faces[fc].id := fc;
    outv.faces[fc].lines := lIndex;
    outv.faces[fc].vertexes := vIndex;
    outv.faces[fc].normal := outf.normal;
    outv.faces[fc].nshift := outf.nshift;
    outv.faces[fc].pinside := MidPoint(vIndex);
    outv.faces[fc].sign := 0;  
    outv.faces[fc].tn  := outf.tn;
    outv.faces[fc].txn := outf.txn;
    outv.faces[fc].txc := outf.txc;
    outv.faces[fc].tyn := outf.tyn;
    outv.faces[fc].tyc := outf.tyc;  {}
  end;

  function NewVertex(const p: Point): integer;
  begin
    result := Length(outv.vertexes);
    SetLength(outv.vertexes, result+1);
    outv.vertexes[result].icounter := 0;
    outv.vertexes[result].fcounter := 0.0;
    outv.vertexes[result].p := p;
  end;

  function NewLine(p1,p2: integer): integer;
  begin
    result := Length(outv.lines);
    SetLength(outv.lines, result+1);
    outv.lines[result].icounter := 0;
    outv.lines[result].ucounter := 0;
    outv.lines[result].P1 := p1;
    outv.lines[result].P2 := p2;
  end;

  type LineConnections = record
    o1: integer;
    w1: float;
    ow1: float;
    ip1: integer;
    op1: integer;
    il1: integer;

    o2: integer;
    w2: float;
    ow2: float;
    ip2: integer;    
    op2: integer;
    il2: integer;

    li: integer;
    hasTriangle: boolean;
  end;

var
  lc: array of LineConnections;

  function InitLineConnection: boolean;
  var
    pi,i,o: integer;
    d,nd,fd: float;
    s,ns,fs: integer;      
    vn: Point;
    vc: float;
    allPositive: boolean;
    hasPositive: boolean;

  begin
    result := FALSE;
    hasPositive := FALSE;
    SetLength(lc, Length(inf.vertexes));
    pi := High(inf.vertexes);
    for i := Low(inf.vertexes) to High(inf.vertexes) do begin
      vn := Cross(Sub(inv.vertexes[inf.vertexes[i]].p, inv.vertexes[inf.vertexes[pi]].p), inf.normal);
      vc := Dot(inv.vertexes[inf.vertexes[i]].p, vn);

      fd := Dot(vn, outv.vertexes[outf.vertexes[0]].p) - vc;
      fs := GetSign(fd);

      d := fd;
      s := fs;

      lc[i].o1  := -1;
      lc[i].w1  := -1.0;
      lc[i].ow1 := -1.0;
      lc[i].o2  := -1;
      lc[i].w2  := -1.0;
      lc[i].ow2 := -1.0;

      allPositive := TRUE;
      for o := Low(outf.vertexes) to High(outf.vertexes) do begin
        if o=High(outf.vertexes) then begin
          nd := fd;
          ns := fs;
        end else begin
          nd := Dot(vn, outv.vertexes[outf.vertexes[o+1]].p) - vc;
          ns := GetSign(nd);
        end;

        if s<0 then allPositive := FALSE;
        if s>0 then hasPositive := TRUE;

        if (s>0) and (ns<=0) then begin
          lc[i].o1  := o;
          lc[i].w1  := d/(d-nd);
          if lc[i].w1<0.0 then lc[i].w1:=0.0 else if lc[i].w1>1.0 then lc[i].w1:=1.0;
          lc[i].ow1 := lc[i].o1+lc[i].w1;
        end else if (s<=0) and (ns>0) then begin
          lc[i].o2  := o;
          lc[i].w2  := d/(d-nd);    
          if lc[i].w2<0.0 then lc[i].w2:=0.0 else if lc[i].w2>1.0 then lc[i].w2:=1.0;
          lc[i].ow2 := lc[i].o2+lc[i].w2;
        end;

        d := nd;
        s := ns;
      end;

      assert ((lc[i].o1<0) = (lc[i].o2<0));
      if allPositive then exit;

      pi := i;
    end;

    if not hasPositive then SetLength(lc,0);

    result := TRUE;
  end;

  function CountLineConnection: boolean;
  var
    i,pi,ni: integer;
    o,no: integer;
    d1,d2: float;
    s1,s2: integer;

    function SeqOk: boolean;
    var
      i,pi: integer;
    begin
      result := FALSE;
      pi := High(inf.vertexes);
      for i := Low(inf.vertexes) to High(inf.vertexes) do begin
        if lc[i].o1>=0 then begin
          if lc[i].ip1<>-2 then begin
            if lc[pi].o1<0 then Exit;
            if lc[pi].ip2=-2 then Exit;
          end;
        end else if lc[pi].o1>=0 then begin
          if lc[pi].ip2<>-2 then begin
            if lc[i].o1<0 then Exit;
            if lc[i].ip1=-2 then Exit;
          end;
        end;
        pi := i;
      end;
      result := TRUE;
    end;

  begin          
    result := FALSE;
    for i := Low(inf.vertexes) to High(inf.vertexes) do if lc[i].o1>=0 then lc[i].li := 0 else lc[i].li := -1;

    for i := Low(inf.vertexes) to High(inf.vertexes) do begin
      if lc[i].o1>=0 then begin
        if i=0 then pi := High(inf.vertexes) else pi := i-1;
        if i=High(inf.vertexes) then ni := 0 else ni := i+1;

        o := lc[i].o1;
        if o=High(outf.vertexes) then no:=0 else no:=o+1;
        d1 := Dot(Cross(Sub(inv.vertexes[inf.vertexes[pi]].p, outv.vertexes[outf.vertexes[o]].p), Sub(inv.vertexes[inf.vertexes[pi]].p, outv.vertexes[outf.vertexes[no]].p)), outf.normal);
        s1 := GetSign(d1);
        o := lc[i].o2;
        if o=High(outf.vertexes) then no:=0 else no:=o+1;
        d2 := Dot(Cross(Sub(inv.vertexes[inf.vertexes[pi]].p, outv.vertexes[outf.vertexes[o]].p), Sub(inv.vertexes[inf.vertexes[pi]].p, outv.vertexes[outf.vertexes[no]].p)), outf.normal);
        s2 := GetSign(d2);
        assert((s1>=0) or (s2>=0));
        if lc[pi].o1<0 then s1 := -1;
        if s1<=0 then lc[i].ip1 := -2
        else if s2<=0 then lc[i].ip1 := -1
        else lc[i].ip1 := 0;

        o := lc[i].o1;
        if o=High(outf.vertexes) then no:=0 else no:=o+1;
        d1 := Dot(Cross(Sub(inv.vertexes[inf.vertexes[i]].p, outv.vertexes[outf.vertexes[o]].p), Sub(inv.vertexes[inf.vertexes[i]].p, outv.vertexes[outf.vertexes[no]].p)), outf.normal);
        s1 := GetSign(d1);
        o := lc[i].o2;
        if o=High(outf.vertexes) then no:=0 else no:=o+1;
        d2 := Dot(Cross(Sub(inv.vertexes[inf.vertexes[i]].p, outv.vertexes[outf.vertexes[o]].p), Sub(inv.vertexes[inf.vertexes[i]].p, outv.vertexes[outf.vertexes[no]].p)), outf.normal);
        s2 := GetSign(d2);
        assert((s1>=0) or (s2>=0));
        if lc[ni].o1<0 then s2 := -1;
        if s2<=0 then lc[i].ip2 := -2
        else if s1<=0 then lc[i].ip2 := -1
        else lc[i].ip2 := 0;
      end;
    end;

    pi := High(inf.vertexes);
    for i := Low(inf.vertexes) to High(inf.vertexes) do begin
      if (lc[i].o1>=0) and (lc[pi].o1>=0) then begin
        if lc[pi].ip2=0 then lc[i].ip1 := 0;
        if lc[i].ip1=0 then lc[pi].ip2 := 0;
      end;
      pi := i;
    end;    
    
    pi := High(inf.vertexes);
    for i := Low(inf.vertexes) to High(inf.vertexes) do begin
      if (lc[i].o1>=0) and (lc[pi].o1>=0) then begin
        if (lc[pi].ip2=-1) and (lc[i].ip1=-1) then Exit;
      end;
      pi := i;
    end;

    for i := Low(inf.vertexes) to High(inf.vertexes) do begin
      if (lc[i].o1>=0) then begin
        if (lc[i].ip1=-1) and (lc[i].ip2=-2) then lc[i].li := -1;
        if (lc[i].ip1=-2) and (lc[i].ip2=-1) then lc[i].li := -1;
      end;
    end;

    for i := Low(inf.vertexes) to High(inf.vertexes) do begin
      if (lc[i].o1>=0) and (lc[i].li=-1) then begin
        lc[i].o1 := -1;
      end;
    end;

    for i := Low(inf.vertexes) to High(inf.vertexes) do begin
      if (lc[i].o1>=0) then begin
        if lc[i].ip1<>-2 then lc[i].ip1 := -1;
        if lc[i].ip2<>-2 then lc[i].ip2 := -1;
      end;
    end;

    Assert(seqOk); // corrupted order of inv
    result := TRUE;
  end;

  procedure CreateLciIndexes;
  var
    i,ni,pi: integer;
    lci: ^LineCutInfo;
    o,no,nno,po: integer;

    vci: array of integer;
    tmp: CutInfo;

  const eps=1.0e-5;

    procedure NewVCI(i: integer);
    var
      cvci: integer;
    begin
      cvci := Length(vci);
      SetLengtH(vci, cvci+1);
      vci[cvci] := i;
    end;

  begin
    for i := Low(inf.vertexes) to High(inf.vertexes) do if lc[i].o1>=0 then begin
      lc[i].hasTriangle := FALSE;
      if lc[i].ip1=-2 then begin
        o := lc[i].o1;
        if o = High(outf.vertexes) then no := 0 else no := o+1;

        if lc[i].w1<eps then begin
          if o=0 then po := High(outf.vertexes) else po := o-1;
          lc[i].ip1 := outf.vertexes[o];
          lc[i].il1 := outf.lines[po];
          lc[i].op1 := po;
        end else if lc[i].w1>1.0-eps then begin
          lc[i].ip1 := outf.vertexes[no];
          lc[i].il1 := outf.lines[o];
          lc[i].op1 := o;
        end else begin
          lci := @vCutInfo[outf.lines[o]];
          if lci.cutIn.pIndex < 0 then begin
            lci.cutIn.pIndex := NewVertex(
              Add(outv.vertexes[outf.vertexes[o]].p, Scale(Sub(outv.vertexes[outf.vertexes[no]].p, outv.vertexes[outf.vertexes[o]].p), lc[i].w1))
            );
            lci.cutIn.lIndex := NewLine(lci.cutIn.pIndex, outf.vertexes[o]);
            lc[i].ip1 := lci.cutIn.pIndex;
            lc[i].il1 := lci.cutIn.lIndex;
            NewVCI(outf.lines[o]);
          end else begin
            Assert(outv.lines[lci.cutIn.lIndex].p2=outf.vertexes[o]);
            lc[i].ip1 := lci.cutIn.pIndex;
            lc[i].il1 := lci.cutIn.lIndex;
          end;
          lc[i].op1 := o;
        end;
      end else begin        
        if i=0 then pi:=High(inf.vertexes) else pi := i-1;
        if lc[i].ip1<0 then lc[i].ip1 := NewVertex(inv.vertexes[inf.vertexes[pi]].p);
      end;

      if lc[i].ip2=-2 then begin
        o := lc[i].o2;
        if o = High(outf.vertexes) then no := 0 else no := o+1;

        if lc[i].w2<eps then begin       
          //if o=0 then po:=High(outf.vertexes) else po := o-1;
          lc[i].ip2 := outf.vertexes[o];
          lc[i].il2 := outf.lines[o];
          lc[i].op2 := no;
        end else if lc[i].w2>1.0-eps then begin  
          if no = High(outf.vertexes) then nno := 0 else nno := no+1;
          lc[i].ip2 := outf.vertexes[no];
          lc[i].il2 := outf.lines[no];
          lc[i].op2 := nno;
        end else begin
          lci := @vCutInfo[outf.lines[o]];
          if lci.cutOut.pIndex < 0 then begin
            lci.cutOut.pIndex := NewVertex(
              Add(outv.vertexes[outf.vertexes[o]].p, Scale(Sub(outv.vertexes[outf.vertexes[no]].p, outv.vertexes[outf.vertexes[o]].p), lc[i].w2))
            );
            lci.cutOut.lIndex := NewLine(lci.cutOut.pIndex, outf.vertexes[no]);
            lc[i].ip2 := lci.cutOut.pIndex;
            lc[i].il2 := lci.cutOut.lIndex;  
            NewVCI(outf.lines[o]);
          end else begin
            Assert(outv.lines[lci.cutOut.lIndex].p2=outf.vertexes[no]);
            lc[i].ip2 := lci.cutOut.pIndex;
            lc[i].il2 := lci.cutOut.lIndex;
          end;   
          lc[i].op2 := no;
        end;
      end else begin
        // add corner triangles if we need it
        if i=High(inf.vertexes) then ni := 0 else ni := i+1;
        if lc[i].ip2<0 then begin
          if lc[ni].ip1<0 then lc[ni].ip1 := NewVertex(inv.vertexes[inf.vertexes[i]].p);
          lc[i].ip2 := lc[ni].ip1;
        end;

        if lc[i].o2 = lc[ni].o1 then begin  
          if lc[ni].w1>1-eps then begin
            // single
            if lc[ni].o1 = High(outf.vertexes) then lc[i].op2 := 0 else lc[i].op2 := lc[ni].o1+1;
            lc[i].il2 := NewLine(lc[i].ip2, outf.vertexes[lc[i].op2]);
            lc[ni].il1 := lc[i].il2;
            lc[ni].op1 := lc[i].op2;
          end else if (lc[i].w2<eps) then begin
            // single
            lc[i].op2 := lc[ni].o1;
            lc[i].il2 := NewLine(lc[i].ip2, outf.vertexes[lc[i].op2]);
            lc[ni].il1 := lc[i].il2;
            lc[ni].op1 := lc[i].op2;
          end else begin    
            // double
            if lc[i].o2 = High(outf.vertexes) then lc[i].op2 := 0 else lc[i].op2 := lc[i].o2+1;
            lc[i].il2 := NewLine(lc[i].ip2, outf.vertexes[lc[i].op2]);
            lc[ni].op1 := lc[ni].o1;
            lc[ni].il1 := NewLine(lc[i].ip2, outf.vertexes[lc[ni].op1]);
            lc[i].hasTriangle := TRUE;
          end;
        end else begin
          // single
          lc[i].op2 := lc[ni].o1;
          lc[i].il2 := NewLine(lc[i].ip2, outf.vertexes[lc[i].op2]); 
          lc[ni].il1 := lc[i].il2;
          lc[ni].op1 := lc[i].op2;
        end;
      end;

      lc[i].li := NewLine(lc[i].ip1, lc[i].ip2);
    end;

    for i := Low(vci) to High(vci) do begin
      lci := @vCutInfo[vci[i]];
      if not lci.swaped then begin
        tmp := lci.cutIn;
        lci.cutIn := lci.cutOut;
        lci.cutOut := tmp;
        lci.swaped := TRUE;
      end;
    end;

    for i := Low(lc) to High(lc) do begin
      if (lc[i].o1>=0) and ((lc[i].ip1=outf.vertexes[lc[i].op2]) or (lc[i].ip2=outf.vertexes[lc[i].op1]) or (lc[i].ip1=lc[i].ip2)) then
        lc[i].li := -1;
    end;
  end;

  procedure UseLciIndexes;
  var
    o,i,ni: integer;
    vIndex, lIndex: IntegerArray;

    procedure AddVertex(v: integer);
    var
      vc: integer;
    begin
      vc := Length(vIndex);
      SetLength(vIndex, vc+1);
      vIndex[vc] := v;
    end;

    procedure AddLine(l: integer);
    var
      lc: integer;
    begin
      lc := Length(lIndex);
      SetLength(lIndex, lc+1);
      lIndex[lc] := l;
    end;

  begin
    for i := Low(inf.vertexes) to High(inf.vertexes) do if lc[i].li>=0 then begin
      if i=High(inf.vertexes) then ni:=0 else ni:=i+1;
      if lc[i].hasTriangle then begin
        assert((lc[i].o1>=0) and (lc[ni].ip1=lc[i].ip2) and (lc[ni].op1<>lc[i].op2));
        SetLength(vIndex,0);
        SetLength(lIndex,0);

        AddVertex(lc[i].ip2);
        AddVertex(outf.vertexes[lc[ni].op1]);
        AddVertex(outf.vertexes[lc[i].op2]);

        AddLine(lc[ni].il1);
        AddLine(outf.lines[lc[ni].op1]);
        AddLine(lc[i].il2);

        Assert(Complete(outv, vIndex, lIndex));
        AddFace(vIndex, lIndex);
      end;

      SetLength(vIndex,0);
      SetLength(lIndex,0);
      AddVertex(lc[i].ip1);
      AddVertex(lc[i].ip2);
      AddLine(lc[i].li);
      AddLine(lc[i].il2);
      o := lc[i].op2;
      while o<>lc[i].op1 do begin
        AddVertex(outf.vertexes[o]);
        AddLine(outf.lines[o]);
        if o=High(outf.vertexes) then o:=0 else Inc(o);
      end;
      AddVertex(outf.vertexes[o]);
      AddLine(lc[i].il1);


      Assert(Complete(outv, vIndex, lIndex));
      AddFace(vIndex, lIndex);
    end;
  end;

begin

  outf := @outv.faces[outfIndex];
  inf := @inv.faces[infIndex];

  result := FALSE;

  if not InitLineConnection then Exit;
  if length(lc)>0 then begin
    if not CountLineConnection then Exit;
    CreateLciIndexes;
    UseLciIndexes;
  end;
  result := TRUE;
end;

procedure AddSubConvex(var outv, inv: Convex);
var
  o,i,j: integer;
  initialOfc, initialIfc: integer;
  goodFaceIn, goodFaceOut: array of integer;
  oldSize,g: integer;
  vCutInfoIn, vCutInfoOut: LineCutInfoArray;
begin
  initialIfc := Length(inv.faces);
  initialOfc := Length(outv.faces);
  SetLength(goodFaceIn, initialIfc);
  SetLength(goodFaceOut, initialOfc);
  for i := Low(inv.faces) to High(inv.faces) do goodFaceIn[i] := 0;
  for i := Low(outv.faces) to High(outv.faces) do goodFaceOut[i] := 0;

  SetLength(vCutInfoIn, Length(inv.lines));
  SetLength(vCutInfoOut, Length(outv.lines));

  for i := Low(vCutInfoIn) to High(vCutInfoIn) do begin
    vCutInfoIn[i].cutIn.pIndex := -1;
    vCutInfoIn[i].cutOut.pIndex := -1;
    vCutInfoIn[i].swaped := FALSE;
  end;

  for i := Low(vCutInfoOut) to High(vCutInfoOut) do begin
    vCutInfoOut[i].cutIn.pIndex := -1;
    vCutInfoOut[i].cutOut.pIndex := -1;
    vCutInfoOut[i].swaped := FALSE;
  end;

  o := 0;
  while o<Length(outv.faces) do begin
    for i := goodFaceOut[o] to initialIfc-1 do begin
      Assert(Length(vCutInfoOut) = Length(outv.lines));
      if SqrLen(Add(outv.faces[o].normal, inv.faces[i].normal)) + sqr(outv.faces[o].nshift+inv.faces[i].nshift) < 0.00001 then begin
        if AddSubFace(outv, inv, o, i, vCutInfoOut) then begin
          oldSize := Length(goodFaceOut);
          SetLength(goodFaceOut, Length(outv.faces));
          for g := oldSize to High(goodFaceOut) do goodFaceOut[g] := i+1;
          oldSize := Length(vCutInfoOut);
          SetLength(vCutInfoOut, Length(outv.lines));
          for g := oldSize to High(vCutInfoOut) do begin
            vCutInfoOut[g].cutIn.pIndex := -1;
            vCutInfoOut[g].cutOut.pIndex := -1;
            vCutInfoOut[g].swaped := FALSE;
          end;

          goodFaceOut[o] := -1;
          break;
        end;
      end;
    end;
    Inc(o);
  end;
  
  i := 0;
  while i<Length(inv.faces) do begin
    for o := goodFaceIn[i] to initialOfc-1 do begin   
      Assert(Length(vCutInfoIn) = Length(inv.lines));
      if SqrLen(Add(outv.faces[o].normal, inv.faces[i].normal)) + sqr(outv.faces[o].nshift+inv.faces[i].nshift) < 0.00001 then begin
        if AddSubFace(inv, outv, i, o, vCutInfoIn) then begin
          oldSize := Length(goodFaceIn);
          SetLength(goodFaceIn, Length(inv.faces));
          for g := oldSize to High(goodFaceIn) do goodFaceIn[g] := o+1;    
          oldSize := Length(vCutInfoIn);
          SetLength(vCutInfoIn, Length(inv.lines));
          for g := oldSize to High(vCutInfoIn) do begin
            vCutInfoIn[g].cutIn.pIndex := -1;
            vCutInfoIn[g].cutOut.pIndex := -1;
            vCutInfoIn[g].swaped := FALSE;
          end;

          goodFaceIn[i] := -1;
          break;
        end;
      end;
    end;
    Inc(i);
  end;

  j := 0;
  for i := Low(inv.faces) to High(inv.faces) do if goodFaceIn[i]>=0 then begin
    if i>j then inv.faces[j]:=inv.faces[i];
    Inc(j);
  end;
  SetLength(inv.faces, j);

  j := 0;
  for i := Low(outv.faces) to High(outv.faces) do if goodFaceOut[i]>=0 then begin
    if i>j then outv.faces[j]:=outv.faces[i];
    Inc(j);
  end;        
  SetLength(outv.faces, j);
end;

procedure TranslateConvex(var v: Convex; const b: Point);
var
  i: integer;
begin
  for i := Low(v.vertexes) to High(v.vertexes) do begin
    v.vertexes[i].p := Add(v.vertexes[i].p, b);
  end;

  for i := Low(v.faces) to High(v.faces) do begin
    v.faces[i].nshift := v.faces[i].nshift - Dot(v.faces[i].normal, b);
    v.faces[i].pinside := Add(v.faces[i].pinside, b);
  end;

  v.pmin := Add(v.pmin, b);
  v.pmax := Add(v.pmax, b);
end;

procedure ScaleConvex(var v: Convex; s: float);
var
  i: integer;
begin
  for i := Low(v.vertexes) to High(v.vertexes) do begin
    v.vertexes[i].p := Scale(v.vertexes[i].p, s);
  end;

  for i := Low(v.faces) to High(v.faces) do begin
    v.faces[i].nshift := v.faces[i].nshift * s;
    v.faces[i].pinside := Scale(v.faces[i].pinside, s);
  end;

  v.pmin := Scale(v.pmin, s);
  v.pmax := Scale(v.pmax, s);
end;

end.

