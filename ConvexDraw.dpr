program ConvexDraw;
{$APPTYPE CONSOLE}

uses
  Windows,
  Messages,
  WinAPI,
  Points,
  Geometry in 'Geometry.pas',
  fxMath in 'fxMath.pas',
  Render in 'Render.pas',
  ConvexTree in 'ConvexTree.pas',
  SectorGrid in 'SectorGrid.pas';

var
  fMain: TWindow;
  context1: RenderContext;

  rect: TRect;
  p1: array of Point;

  viewAX, viewAZ: float;
  viewC: Point;

  tree: array of HighNode;
  v: array of Convex;

  nc: integer = 0;    
  na, nb: Point;
  fps, fpsc: integer;
  currentTime, prevTime: cardinal;

  keypressed: array [0..65535] of boolean;

const
  sz=8;

function Perp(const v: Point): Point;
var
  l: float;
begin
  if (abs(v.x)<abs(v.y)) and (abs(v.x)<abs(v.z)) then begin
    l := 1/sqrt(sqr(v.y)+sqr(v.z));
    result.x := 0.0;
    result.y := -v.z * l;
    result.z := v.y * l;
  end else if  (abs(v.y)<abs(v.z)) then begin
    l := 1/sqrt(sqr(v.x)+sqr(v.z));
    result.x := v.z * l;
    result.y := 0.0;
    result.z := -v.x * l;
  end else begin
    l := 1/sqrt(sqr(v.x)+sqr(v.y));
    result.x := -v.y * l;
    result.y := v.x * l;
    result.z := 0.0;
  end;
end;

function max(a,b: float): float;
begin
  if a<b then result := b else result := a;
end;

procedure ProcessCut(var v: Convex; coord: integer);
var
  normal: Point;
  nshift: float;
  a1,a2,a3: float;
begin
  if length(v.faces)>0 then begin
    while nc<$400 do begin
      //if nc=0 then begin
        a1 := random;
        a2 := random*2*pi;
        a3 := sqrt(1.0-sqr(a1));
        na.x := a1;
        na.y := cos(a2)*a3;
        na.z := sin(a2)*a3;
        nb := perp(na);
        na := cross(nb,na);      {}         {
        na := toPoint(0.0, 0.0, 0.0);
        nb := toPoint(0.0, 0.0, 0.0);
        na.c[coord] := 1.0;
        nb.c[(coord+1) mod 3] := 1.0;    {}
      //end;
      //WriteLn('lap=', length(ap));


      normal := add(scale(na, cos(nc*2.0*pi/$400)), scale(nb, sin(nc*2.0*pi/$400)));
      //normal := ToPoint(0.0,-0.6,0.8);
      nshift := 1.3+0.2*normal.y;
      //nshift := 0.2;
      CutConvex(v, normal, nshift, true);
      Writeln('cut');
      break;
    end;          {}
  end;
end;

procedure StartNew(context: pointer);
var
  PP: array of PPoint;
  i: integer;
  vi: integer;
const detalisation=9;

  function CreateCave(x,y,z: float; size: float; cave: boolean; box: boolean): integer;
  var
    vc: integer;
    i: integer;
    dst,cx,cy,cz,r: float;
    hasFloor: boolean;

    function max3(a,b,c: float): float;
    begin
      if (a>b) and (a>c) then result := a
      else if b>c then result := b
      else result := c;
    end;

    function max2(a,b: float): float;
    begin
      if a>b then result := a
      else result := b;
    end;

  begin
    vc := Length(v);
    SetLength(v, vc+1);
    SetLength(PP, 0);
    cx := random;
    cy := random*0.4;
    cz := random;
    r := max2(
      abs((x*2.0+size)/sz-1.0),
      abs((z*2.0+size)/sz-1.0)
    );

    hasFloor := (y=0) or (random(5)=0);

    if box then  for i := Low(P1) to High(P1) do begin
      dst := sqr(P1[i].x-0.5) + sqr(P1[i].y-0.5) + sqr(P1[i].z-0.5);
      if (dst<sqr(0.8)) and (random(5)=0) then begin
        SetLength(PP, Length(PP)+1);
        PP[Length(PP)-1] := @P1[i];
      end;
    end else if cave then for i := Low(P1) to High(P1) do begin
      dst := sqr(P1[i].x-cx) + sqr(P1[i].y-cy) + sqr(P1[i].z-cz);
      if (dst<sqr(0.89-r*0.3)) and (not hasFloor or (P1[i].y>0.2)) {and (random(15)=0)} then begin
        SetLength(PP, Length(PP)+1);
        PP[Length(PP)-1] := @P1[i];
      end;
    end else for i := Low(P1) to High(P1) do begin 
      dst := sqr(P1[i].y-0.5) + sqr(max(abs(P1[i].x-0.5), abs(P1[i].z-0.5)));
      if (dst<sqr(990.69)) and (P1[i].y<1.94) then begin
        SetLength(PP, Length(PP)+1);
        PP[Length(PP)-1] := @P1[i];
      end;
    end;
    assert(Length(PP)>3);
    v[vc] := GetConvex(PP);
    for i := Low(v[vc].faces) to High(v[vc].faces) do
      if box then v[vc].faces[i].tn := 2
      else if v[vc].faces[i].normal.y=-1.0 then v[vc].faces[i].tn := 1
      else v[vc].faces[i].tn := 0;

    if cave then ReverseConvex(v[vc]);
    ScaleConvex(v[vc], size);
    TranslateConvex(v[vc], toPoint(x,y,z));
    result := vc;
  end;   

  function CreateBoxes(x1,y1,z1,x2,y2,z2: integer; const bound: integer): integer;
  var
    cx,cy,cz: integer;
    i: integer;
  begin
    result := Length(tree);
    SetLength(tree, result+1);

    tree[result].pmin := toPoint(x1*0.125, y1*0.125, z1*0.125);
    tree[result].pmax := toPoint(x2*0.125, y2*0.125, z2*0.125);

    if (x2-x1=1) or (random(3)=0) then begin
      tree[result].leaf := TRUE;
      if {(x1 and 7 in [2..5]) and (z1 and 7 in [2..5]) and} (random(5)=0) then begin
        tree[result].walls := CreateCave(x1*0.125,y1*0.125,z1*0.125, (x2-x1)*0.125, FALSE, TRUE);
        for i := low(v[bound].faces) to High(v[bound].faces) do begin
          CutConvex(v[tree[result].walls], Scale(v[bound].faces[i].normal, 1.0), v[bound].faces[i].nshift, TRUE);
        end;
      end else
        tree[result].walls := -1;
      tree[result].subWalls := -1;
    end else begin
      tree[result].leaf := FALSE;
      cx := (x1+x2) div 2;
      cy := (y1+y2) div 2;
      cz := (z1+z2) div 2;
      tree[result].center := toPoint(cx*0.125, cy*0.125, cz*0.125);
      tree[result].childs[0] := CreateBoxes(x1,y1,z1,cx,cy,cz, bound);
      tree[result].childs[1] := CreateBoxes(cx,y1,z1,x2,cy,cz, bound);
      tree[result].childs[2] := CreateBoxes(x1,cy,z1,cx,y2,cz, bound);
      tree[result].childs[3] := CreateBoxes(cx,cy,z1,x2,y2,cz, bound);
      tree[result].childs[4] := CreateBoxes(x1,y1,cz,cx,cy,z2, bound);
      tree[result].childs[5] := CreateBoxes(cx,y1,cz,x2,cy,z2, bound);
      tree[result].childs[6] := CreateBoxes(x1,cy,cz,cx,y2,z2, bound);
      tree[result].childs[7] := CreateBoxes(cx,cy,cz,x2,y2,z2, bound);
    end;
  end;

  function CreateTree(x1,y1,z1,x2,y2,z2: integer; sub: boolean): integer;
  var
    cx,cy,cz: integer;
  begin
    result := Length(tree);
    SetLength(tree, result+1);

    tree[result].pmin := toPoint(x1*1.0, y1*1.0, z1*1.0);
    tree[result].pmax := toPoint(x2*1.0, y2*1.0, z2*1.0);

    if (x2-x1=sz) and not sub then begin
      tree[result].leaf := TRUE;
      tree[result].subtracting := FALSE;
      tree[result].walls := CreateCave(x1,y1,z1, x2-x1, FALSE, FALSE);
      tree[result].subWalls := CreateTree(x1,y1,z1,x2,y2,z2, TRUE);
    end else begin
      if sub and ((x2-x1=1) or ((x2-x1<=8) and (random(6)=0))) then begin
        tree[result].leaf := TRUE;       
        tree[result].subtracting := TRUE;
        if (y1>=0) then begin
          tree[result].walls := CreateCave(x1,y1,z1, x2-x1, TRUE, FALSE);
          if x2-x1>1 then
            tree[result].subWalls := CreateBoxes(x1*8,y1*8,z1*8, x2*8,y2*8,z2*8, tree[result].walls)
          else
            tree[result].subWalls := -1;
        end else begin
          tree[result].walls := -1;  
          tree[result].subWalls := -1;
        end;
      end else begin
        tree[result].leaf := FALSE;
        cx := (x1+x2) div 2;
        cy := (y1+y2) div 2;
        cz := (z1+z2) div 2;
        tree[result].center := toPoint(cx*1.0, cy*1.0, cz*1.0);
        tree[result].childs[0] := CreateTree(x1,y1,z1,cx,cy,cz,sub);
        tree[result].childs[1] := CreateTree(cx,y1,z1,x2,cy,cz,sub);
        tree[result].childs[2] := CreateTree(x1,cy,z1,cx,y2,cz,sub);
        tree[result].childs[3] := CreateTree(cx,cy,z1,x2,y2,cz,sub);
        tree[result].childs[4] := CreateTree(x1,y1,cz,cx,cy,z2,sub);
        tree[result].childs[5] := CreateTree(cx,y1,cz,x2,cy,z2,sub);
        tree[result].childs[6] := CreateTree(x1,cy,cz,cx,y2,z2,sub);
        tree[result].childs[7] := CreateTree(cx,cy,cz,x2,y2,z2,sub);
      end;
    end;
  end;

  procedure AddSubConvexesBetween(tree1, tree2: integer; dim: integer);

    function GetChildOrSelf(t: integer; chi: integer): integer;
    begin
      if tree[t].leaf then result := t else result := tree[t].childs[chi];
    end;

  begin
    if tree[tree1].leaf and tree[tree2].leaf then begin
      if (tree[tree1].walls>=0) and (tree[tree2].walls>=0) then begin
        AddSubConvex(v[tree[tree1].walls], v[tree[tree2].walls]);
      end;
    end else case dim of
      0: begin
        AddSubConvexesBetween(GetChildOrSelf(tree1,1), GetChildOrSelf(tree2,0), 0);
        AddSubConvexesBetween(GetChildOrSelf(tree1,3), GetChildOrSelf(tree2,2), 0);
        AddSubConvexesBetween(GetChildOrSelf(tree1,5), GetChildOrSelf(tree2,4), 0);
        AddSubConvexesBetween(GetChildOrSelf(tree1,7), GetChildOrSelf(tree2,6), 0);
      end;
      1: begin
        AddSubConvexesBetween(GetChildOrSelf(tree1,2), GetChildOrSelf(tree2,0), 1);
        AddSubConvexesBetween(GetChildOrSelf(tree1,3), GetChildOrSelf(tree2,1), 1);
        AddSubConvexesBetween(GetChildOrSelf(tree1,6), GetChildOrSelf(tree2,4), 1);
        AddSubConvexesBetween(GetChildOrSelf(tree1,7), GetChildOrSelf(tree2,5), 1);
      end;
      2: begin
        AddSubConvexesBetween(GetChildOrSelf(tree1,4), GetChildOrSelf(tree2,0), 2);
        AddSubConvexesBetween(GetChildOrSelf(tree1,5), GetChildOrSelf(tree2,1), 2);
        AddSubConvexesBetween(GetChildOrSelf(tree1,6), GetChildOrSelf(tree2,2), 2);
        AddSubConvexesBetween(GetChildOrSelf(tree1,7), GetChildOrSelf(tree2,3), 2);
      end;
    end;
  end;

  procedure AddSubConvexes(t: integer);
  begin
    if not tree[t].leaf then begin
      AddSubConvexes(tree[t].childs[0]);
      AddSubConvexes(tree[t].childs[1]);
      AddSubConvexes(tree[t].childs[2]);
      AddSubConvexes(tree[t].childs[3]);
      AddSubConvexes(tree[t].childs[4]);
      AddSubConvexes(tree[t].childs[5]);
      AddSubConvexes(tree[t].childs[6]);
      AddSubConvexes(tree[t].childs[7]);
      AddSubConvexesBetween(tree[t].childs[0], tree[t].childs[1], 0);
      AddSubConvexesBetween(tree[t].childs[2], tree[t].childs[3], 0);
      AddSubConvexesBetween(tree[t].childs[4], tree[t].childs[5], 0);
      AddSubConvexesBetween(tree[t].childs[6], tree[t].childs[7], 0);
      AddSubConvexesBetween(tree[t].childs[0], tree[t].childs[2], 1);
      AddSubConvexesBetween(tree[t].childs[1], tree[t].childs[3], 1);
      AddSubConvexesBetween(tree[t].childs[4], tree[t].childs[6], 1);
      AddSubConvexesBetween(tree[t].childs[5], tree[t].childs[7], 1);
      AddSubConvexesBetween(tree[t].childs[0], tree[t].childs[4], 2);
      AddSubConvexesBetween(tree[t].childs[1], tree[t].childs[5], 2);
      AddSubConvexesBetween(tree[t].childs[2], tree[t].childs[6], 2);
      AddSubConvexesBetween(tree[t].childs[3], tree[t].childs[7], 2);
    end;
  end;

begin
  SetLength(P1, detalisation*detalisation*detalisation+0);
  for i := 0 to detalisation*detalisation*detalisation-1 do begin
    P1[i] := toPoint(
      (i div (detalisation*detalisation))/(detalisation-1),    
      (i div detalisation mod detalisation)/(detalisation-1),
      (i mod detalisation)/(detalisation-1)
     );
  end;


  setlength(tree, 0);
  CreateTree(0,0,0,sz,sz,sz,FALSE);

 // for i := low(v[0].faces) to High(v[0].faces) do
 // CutConvex(v[762], Scale(v[0].faces[i].normal, -1.0), -v[0].faces[i].nshift, TRUE);


  for vi := 1 to High(v) do begin
    for i := low(v[0].faces) to High(v[0].faces) do begin
      CutConvex(v[vi], Scale(v[0].faces[i].normal, -1.0), -v[0].faces[i].nshift, TRUE);
    end;
  end;       {}    

  for vi := 1 to High(v) do begin
    AddSubConvex(v[0],v[vi]);
  end;
  AddSubConvexes(tree[0].subWalls);   {}

  SetFocus(fMain.Handle);
end;

procedure Close(context: pointer);
begin
  SendMessage(fMain.Handle, WM_CLOSE, 0, 0)
end;

var firstDrawenConvex: integer = -1;

procedure DrawTree(var context: RenderContext; const node: HighNode; depth: integer=0);
var
  cubeIsVisible: boolean;
  ox,oy,oz: integer;
  mask: integer;
  i: integer;
begin
  cubeIsVisible := True;

  if cubeIsVisible then begin
    if node.leaf then begin
      Inc(counter8);

      if node.walls>=0 then begin
        cubeIsVisible := FastCheckCube(context, node.pmin, node.pmax);
        if cubeIsVisible then begin
          if node.subtracting then begin
            if node.subWalls>=0 then
              DrawTree(context, tree[node.subWalls], depth)
            else if firstDrawenConvex<0 then
              firstDrawenConvex := node.walls; 
            DrawConvex(context, v[node.walls]);
          end else begin          
            DrawConvex(context, v[node.walls]);
            if node.subWalls>=0 then
              DrawTree(context, tree[node.subWalls], depth)
            else if firstDrawenConvex<0 then
              firstDrawenConvex := node.walls;
          end;
        end;
      end;
    end else begin
      if -viewC.x<node.center.x then ox:=0 else ox:=1;
      if -viewC.y<node.center.y then oy:=0 else oy:=1;
      if -viewC.z<node.center.z then oz:=0 else oz:=1;
      mask := ox + (oy shl 1) + (oz shl 2);
      for i := 0 to 7 do DrawTree(context, tree[node.childs[i xor mask]], depth+1);
    end;
  end;
end;

procedure IdleProc;
var
  Handle: HWND;           
  P, CP: Windows.TPoint;
  CR, WR: TRect;      
  c,s: float;
begin
  Handle := fMain.Handle;
  if not WaitingMessageBox then begin
    currentTime := GetTickCount;

    GetWindowRect(Handle, WR);
    GetClientRect(Handle, CR);

    CP.X := (WR.Right+WR.Left) div 2;
    CP.Y := (WR.Bottom+WR.Top) div 2;
    GetCursorPos(P);
    viewAX := viewAX - (P.X - CP.X) * 0.001;
    viewAZ := viewAZ - (P.Y - CP.Y) * 0.001;
    SetCursorPos(CP.X, CP.Y);      {}

    if keyPressed[byte('W')] or keyPressed[byte('A')] or keyPressed[byte('S')] or keyPressed[byte('D')] then begin
      c := cos(viewAX);
      s := sin(viewAX);
      if keyPressed[byte('W')] then begin
        viewC.x := viewC.x + s*(currentTime-prevTime)*0.0005;
        viewC.z := viewC.z - c*(currentTime-prevTime)*0.0005;
      end;
      if keyPressed[byte('A')] then begin
        viewC.x := viewC.x + c*(currentTime-prevTime)*0.0005;
        viewC.z := viewC.z + s*(currentTime-prevTime)*0.0005;
      end;
      if keyPressed[byte('S')] then begin
        viewC.x := viewC.x - s*(currentTime-prevTime)*0.0005;
        viewC.z := viewC.z + c*(currentTime-prevTime)*0.0005;
      end;
      if keyPressed[byte('D')] then begin
        viewC.x := viewC.x - c*(currentTime-prevTime)*0.0005;
        viewC.z := viewC.z - s*(currentTime-prevTime)*0.0005;
      end;
    end;
    if keyPressed[VK_SPACE] then begin
      viewC.y := viewC.y - (currentTime-prevTime)*0.0005;
    end;
    if keyPressed[VK_CONTROL] then begin
      viewC.y := viewC.y + (currentTime-prevTime)*0.0005;
    end;

    SetRenderContext(context1, viewC, viewAX, viewAZ, 1.0, 1.0*context1.buffer.SizeX/context1.buffer.SizeY);

    if Length(tree)>0 then begin
      counter8 := 0;
      firstDrawenConvex := -1;
      dec(counter1, GetTimer); 
      DrawTree(context1, tree[0]);    
      inc(counter1, GetTimer);
      //Counts(context);

      //ProcessCut(v1,0);
      //ProcessCut(v2,1);
      //ProcessCut(v3,2);
      Inc(nc,1);
    end;   {}

    Inc(fpsc);

    if currentTime div 1000 <> prevTime div 1000 then begin
      fps := fpsc;
      fpsc := 0;
      WriteLn('fps=', fps,
        ' in ', firstDrawenConvex,
        ' polyCount=', context1.polyCounter,
        ' counter1(FULL)=', counter1,
        ' counter2=', counter2/(counter1+1):0:3,
        ' counter3=', counter3/(counter1+1):0:3,
        ' counter4=', counter4/(counter1+1):0:3,
        ' counter5=', counter5/(counter1+1):0:3,
        ' counter6=', counter6/(counter1+1):0:3,
        ' counter7=', counter7/(counter1+1):0:3,     
        ' counter8=', counter8);
    end;

    InvalidateRect(Handle, nil, FALSE);
    prevTime := currentTime;
  end;
end;

function MainWndProc(Handle: HWND; Message: UINT; WP: WParam; LP: LParam): longint; stdcall;
var
  dc: HDC;
  CR: TRect;
begin
  case Message of

    WM_COMMAND: begin
      if (HiWord(WP) = 1) and (LP = 0) then CheckControl(Handle, LoWord(WP), HiWord(DWORD(WP)))
      else Click(Handle, LoWord(WP), HiWord(DWORD(WP)));
    end;
    WM_ERASEBKGND: begin
      Result := 0;
      Exit;
    end;
    WM_PAINT: begin
      dc := GetDC(Handle);
      GetClientRect(Handle, CR);
      //BitBlt(dc, 0, 0, context.buffer.SizeX, context.buffer.SizeY, context.buffer.DC, 0, 0, SRCCOPY);
      SetStretchBltMode(dc, COLORONCOLOR);
      //SetStretchBltMode(dc, HALFTONE);
      StretchBlt(dc, 0, 0,
        CR.Right-CR.Left, CR.Bottom-CR.Top, context1.buffer.DC, 0, 0, context1.buffer.SizeX, context1.buffer.SizeY, SRCCOPY);
      ReleaseDC(Handle, dc);
    end;
    WM_KEYDOWN: begin
      if LoWord(WP) = byte('N') then StartNew(nil);
      keypressed[LoWord(WP)] := TRUE;
    end;     
    WM_KEYUP: begin
      keypressed[LoWord(WP)] := FALSE;
    end;
    WM_TIMER: begin
    end;
    WM_DESTROY: begin
      PostQuitMessage(0);
      Result := 0;
      Exit;
    end;
  end;
  Result := DefWindowProc(Handle, Message, WP, LP);
end;

begin
  randseed := 2;
  ChgFont('Courier New', 11);
  WindowProc := MainWndProc;
  CreateForm(fMain, 'ConvexDraw', 0, sFormFixedSize or WS_POPUP);

  //SetTimer(fMain.Handle, 0, 0, nil);
  WinAPI.IdleProc := IdleProc;
  viewC := toPoint(+0.1, -sz/2+0.1, +0.1);
  ShowForm(fMain);
  ShowWindow(fMain.Handle, SW_SHOWMAXIMIZED);
  GetClientRect(fMain.Handle, rect);
  CreateRenderContext(context1, (rect.Right-rect.Left) * 1, (rect.Bottom-rect.Top) * 1);
  GetMessages;
  DeleteRenderContext(context1);

end.
