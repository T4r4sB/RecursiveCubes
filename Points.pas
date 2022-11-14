unit Points;

interface

type
  float = single;
  Point = record case boolean of true: (x,y,z:float); false: (c: array [0..2] of float) end;
  PPoint = ^Point;
  
  function ToPoint (x,y,z: float): Point;   
  function Dot     (const p1,p2: Point): float;
  function Cross   (const p1,p2: Point): Point;
  function Volume  (const p1,p2,p3: Point): float;
  function SqrLen  (const p: Point): float;
  function LengthP (const p: Point): float;
  function Norm    (const p: Point): Point;
  function Add     (const p1,p2: Point): Point;
  function Sub     (const p1,p2: Point): Point;
  function Scale   (const p: Point; s: float): Point;
  function Mid     (const p1: Point; f1: float; const p2: Point; f2: float) : Point;
  function Atan2   (y,x:float):float;

type
  Matrix = array [0..3] of Point;
  PMatrix = ^Matrix;

  procedure SetID       (var M: Matrix);
  procedure Translate   (var M: Matrix; const P: Point);
  procedure Rotate      (var M: Matrix; coord: integer; a: float);
  procedure RotateToNew (const M: Matrix; var result: Matrix; coord: integer; a: float);
  procedure ScaleM      (var M: Matrix; S : Point);
  function  RotateP     (const M: Matrix; const P: Point; OnlyRot: boolean = false): Point;

implementation

  uses Math;

  function ToPoint (x,y,z: float): Point;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
  end;

  function Dot (const p1,p2: Point): float;
  begin
    Result := p1.x*p2.x + p1.y*p2.y + p1.z*p2.z;
  end;

  function Cross (const p1,p2: Point): Point;
  begin
    Result.x := p1.y*p2.z - p1.z*p2.y;
    Result.y := p1.z*p2.x - p1.x*p2.z;
    Result.z := p1.x*p2.y - p1.y*p2.x;
  end;

  function Volume (const p1,p2,p3: Point): float;
  begin
    Result := Dot(Cross(p1,p2),p3);
  end;

  function SqrLen(const p: Point): float;
  begin                
    Result := sqr(p.x)+sqr(p.y)+sqr(p.z);
  end;

  function LengthP (const p: Point): float;
  begin
    Result := sqrt(sqr(p.x)+sqr(p.y)+sqr(p.z));
  end;
  
  function Norm (const p: Point): Point;
  var
    l: float;
  begin
    l := 1.0/sqrt(sqr(p.x)+sqr(p.y)+sqr(p.z));
    Result.x := p.x*l;                                 
    Result.y := p.y*l;
    Result.z := p.z*l;
  end;

  function Add (const p1,p2: Point): Point;
  begin
    Result.x := p1.x+p2.x;
    Result.y := p1.y+p2.y;
    Result.z := p1.z+p2.z;
  end;

  function Sub (const p1,p2: Point): Point;
  begin
    Result.x := p1.x-p2.x;
    Result.y := p1.y-p2.y;
    Result.z := p1.z-p2.z;
  end;

  function Scale (const p: Point; s: float): Point;
  begin
    Result.x := p.x*s;
    Result.y := p.y*s;
    Result.z := p.z*s;
  end;

  function Mid (const p1: Point; f1: float; const p2: Point; f2: float) : Point;
  begin
    Result := Add(p1, Scale(Sub(p2,p1), f1/(f1+f2)));
  end;
  
  procedure SetID (var M: Matrix);
  begin
    M[0] := ToPoint(1,0,0);  
    M[1] := ToPoint(0,1,0);
    M[2] := ToPoint(0,0,1);
    M[3] := ToPoint(0,0,0);
  end;

  procedure Translate (var M: Matrix; const P: Point);
  begin
    M[3] := Add(M[3], P);
  end;

  procedure Rotate(var M: Matrix; coord: integer; a: float);
  // умножение справа
  var
    c,s,t: float;
    i : integer;
    c1,c2 : integer;
  begin
    c := cos(a);
    s := sin(a);
    c1 := (coord+1) mod 3;
    c2 := (coord+2) mod 3;
    for i := 0 to 3 do begin
      t          := M[i].c[c1]*c - M[i].c[c2]*s;
      M[i].c[c2] := M[i].c[c2]*c + M[i].c[c1]*s;
      M[i].c[c1] := t;
    end;
  end;
  
  procedure RotateToNew(const M: Matrix; var result: Matrix; coord: integer; a: float);
  var
    c,s: float;
    i : integer;
    c1,c2 : integer;
  begin
    c := cos(a);
    s := sin(a);
    c1 := (coord+1) mod 3;
    c2 := (coord+2) mod 3;
    for i := 0 to 3 do begin
      result[i].c[c1] := M[i].c[c1]*c - M[i].c[c2]*s;
      result[i].c[c2] := M[i].c[c2]*c + M[i].c[c1]*s;
    end;
  end;
  
  procedure ScaleM  (var M: Matrix; S : Point);
  var
    j, i : integer;
  begin
    for j := 0 to 2 do
      for i := 0 to 3 do
        M[i].c[j] := M[i].c[j] * S.c[j];
  end;

  function RotateP (const M: Matrix; const P: Point; OnlyRot : boolean = false) : Point;
  var
    i : integer;
  begin
    if OnlyRot then begin              
      for i := 0 to 2 do
        Result.c[i] := M[0].c[i]*P.c[0] + M[1].c[i]*P.c[1] + M[2].c[i]*P.c[2];
    end else begin
      for i := 0 to 2 do
        Result.c[i] := M[0].c[i]*P.c[0] + M[1].c[i]*P.c[1] + M[2].c[i]*P.c[2] + M[3].c[i];
    end;
  end;

  function Atan2(y,x:float):float;
  asm
    FLD     Y
    FLD     X
    FPATAN
    FWAIT
  end;

end.


