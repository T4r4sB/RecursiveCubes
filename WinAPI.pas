unit WinAPI;

interface

{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

uses
  Windows, Messages;

type
  TColor = cardinal;

  PColor = ^TColor;

  TKind = (kForm, kGroup, kDefault);

  TBitmap = record
    Handle: HWND;
    DC: HDC;
    Mem: PColor;           
    SizeX, SizeY: integer;
  end;

  PFormStack = ^TFormStack;

  TFormStack = record
    Handle: hWnd;
    Accels: THandle;
    Next: PFormStack;
  end;

  TFileOfByte = file of byte;

  TWndProc = function(Handle: HWND; Message: UINT; WP: WParam; LP: LParam): longint; stdcall;
  THWNDFunction = function: HWND; stdcall;

  TPProc = procedure (d : pointer = nil);

  TEvent = record
    p : TPProc;
    d : pointer;
  end;

  PWindow = ^TWindow;
  TWindow = record
    Handle: HWND;
    //DC: HDC;
    ID: Word;
    Tag, TabOrder: integer;
    Event: TEvent;
    case TKind of
      kForm: (
        Accels: THandle;
      );
      kGroup: (
        MinID, MaxID: integer;
      );
  end;              

  TRadioGroup = array of TWindow;

const                         
  sChild = WS_CHILD or WS_VISIBLE or WS_CLIPSIBLINGS;
  sFormDef = WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN;
  sFormModal = WS_CLIPCHILDREN;
  sFormFixedSize = WS_SYSMENU or WS_MINIMIZEBOX or WS_CLIPCHILDREN;
  sButtonDef = sChild or BS_PUSHBUTTON;
  sEdit = sChild or ES_AUTOHSCROLL or WS_TABSTOP or WS_BORDER;
  sMemo = sChild or ES_MULTILINE or WS_HSCROLL or ES_AUTOHSCROLL or WS_VSCROLL or ES_AUTOVSCROLL or WS_BORDER;
  sEdOut = sChild or ES_MULTILINE or WS_HSCROLL or ES_AUTOHSCROLL or WS_VSCROLL or ES_AUTOVSCROLL or ES_READONLY or WS_BORDER;
  sStatic = sChild or ES_AUTOHSCROLL or ES_READONLY;

var
  WindowProc: TWndProc;

  MainPath: string;

  ExceptionCode: integer;

  MainForm: hWnd;
  MainIcon: hIcon;
  IdleProc: procedure;

  aTag: integer;

  Wnds: array [word] of PWindow;
  TabList: array [word] of PWindow;
  GetFocusEx: THWNDFunction = GetFocus;

  Font: hFont;

function ToEvent(p : TPProc; d : pointer = nil) : TEvent;
function NoEvent : TEvent;

function ToWide(s: string): WideString;
procedure GetMessages;
procedure CreateForm(var Window: TWindow; Name: string; Parent: HWND; Style: Cardinal);
procedure SetClientSize(H: hWnd; X, Y: integer);
procedure CorrectSize(H: hWnd; Mode: integer; minX, minY, maxX, maxY: integer; var R: TRect);

procedure CreateAnyWindow(var Window: TWindow; ClassName, Name: string; X, Y, sizeX, sizeY: integer; Parent: HWND; Style: cardinal;
  aTabOrder: boolean = False; aEvent: TPProc = nil; dEvent : pointer = nil);
procedure CreateRadioGroup(var Window: TRadioGroup; Names: array of string; X, Y, sizeX, sizeY: integer; Parent: HWND;
  aTabOrder: boolean = False);
function RadioGroupChoise(Window: TRadioGroup): integer;
procedure MoveRadioGroup(var Window : TRadioGroup; x,y,sizex,sizey: integer);
procedure CreateMainMenu(var Window: TWindow; Parent: HWND);
procedure CreateSubMenu(var Window: TWindow; Name: string; Parent: HWND);
procedure CreateMenuItem(var Window: TWindow; Name: string; Parent: HWND; aEvent: TPProc = nil; dEvent : pointer = nil; Radio : boolean = false);
procedure CreateBitmap(var Bitmap: TBitmap; sizeX, sizeY: integer; Bits: byte);
function LoadBitmap(var Bitmap: TBitmap; FileName: string): boolean;
procedure ChgFont(Name: string; Size: integer);

function GetClassName(H: HWND): string;

procedure CheckControl(H: hWnd; ID, HiWP: word);
procedure Click(H: hWnd; ID, HiWP: word);
procedure DoTab;

procedure ShowForm(var Window: TWindow; Maximized: boolean = false);
procedure ShowFormByPtr (p : pointer);
procedure HideTopForm;

procedure DeleteBitmap(var Bitmap: TBitmap);
procedure DeleteWindow(var Window: TWindow);

function GetCaption(H: HWND): string;
procedure SetCaption(H: HWND; S: string);
function MsgBox(hWnd: HWND; text, caption: string; uType: UINT): Integer;
procedure ShowError(Text: string);
function WaitingMessageBox: boolean;
procedure LoadStruct(var F: TFileOfByte; var V; Size: cardinal);

function ToAccels(A: array of word): THandle;

function IntToStr(N: integer): string;
function IntToHex(N: integer): string;
function FloatToStr(E: extended; W, D: integer): string;
function FloatToStrE(E: extended; W: integer): string;
function FloatToStrEE(E: extended; W, ED: integer): string;

function GetTimer: int64;

implementation

{$IFDEF DEBUG}
uses SysUtils;
{$ENDIF}

var
  LastID, LastTab: integer;
  FormStack: PFormStack;

  HList: array of array of array of array of PWindow;
  FlagWaitingMessageBox : boolean = false;


function GetTimer: int64;
begin
  QueryPerformanceCounter(result);
end;

function IntToStr;
begin
  Str(N, Result);
end;

function IntToHex;
const
  S = '0123456789ABCDEF';
var
  i: integer;
begin
  SetLength(Result, 8);
  for i := 0 to 7 do Result[8 - i] := S[N shr (i shl 2) and $0F + 1];
end;

function FloatToStr(E: extended; W, D: integer): string;
begin
  Str(E: W: D, Result);
end;

function FloatToStrE(E: extended; W: integer): string;
begin
  Str(E: W, Result);
end;

function FloatToStrEE(E: extended; W, ED: integer): string;
begin
  Str(E: W+(4-ED), Result);
  Assert(Result[Length(Result)-5]='E');
  Delete(Result, Length(Result)-3, 4-ED);
end;

function ToEvent(p : TPProc; d : pointer = nil) : TEvent;
begin
  Result.p := p;
  Result.d := d;
end;

function NoEvent : TEvent;
begin
  Result.p := nil;
  Result.d := nil;
end;

function ToWide;
var
  cchw: integer;
const CP_THREAD_ACP = 3;
begin
  cchw := MultiByteToWideChar(CP_THREAD_ACP, 0, PChar(s), Length(s), nil, 0);
  if cchw <> 0 then begin
    SetLength(Result, cchw);
    MultiByteToWideChar (CP_THREAD_ACP, 0, PChar(s), Length(s), PWideChar(Result), cchw);
  end;
end;

function GetAcc(H: hWnd): THandle;
var
  bt: array [0 .. 3] of byte absolute H;
  p: pointer;
begin
  Result := 0;
  p := HList;
  if p = nil then Exit;
  p := ppointer(integer(p) + bt[0] shl 2)^;
  if p = nil then Exit;
  p := ppointer(integer(p) + bt[1] shl 2)^;
  if p = nil then Exit;
  p := ppointer(integer(p) + bt[2] shl 2)^;
  if p = nil then Exit;
  p := ppointer(integer(p) + bt[3] shl 2)^;
  if p = nil then Exit;
  Result := PWindow(p).Accels;
end;

procedure GetMessages;
var
  Message: TMsg;
  AW: hWnd;
begin
  repeat
    try
      if not PeekMessage(Message, 0, 0, 0, 0) and (@IdleProc <> nil) then IdleProc
      else begin
        if not GetMessage(Message, 0, 0, 0) then Break;
        AW := GetActiveWindow;
        if TranslateAccelerator(AW, GetAcc(AW), Message) = 0 then begin
          TranslateMessage(Message);
          DispatchMessage(Message);
        end;
      end;
    except
      {$IFDEF DEBUG}
    on E: exception do
      MsgBox(MainForm, E.Message, 'ERROR', mb_OK or mb_IconError);
      {$ELSE}
      MsgBox(MainForm, 'Runtime error ' + IntToHex(ExceptionCode), 'ERROR', mb_OK or mb_IconError);
      {$ENDIF}
    end;
  until False;
end;

procedure DB (DC: HDC);
// толку нихуя
var
  PFD : TPixelFormatDescriptor;
  N: integer;
begin          
  FillChar(PFD, SizeOf(PFD), 0);
  PFD.nSize := SizeOf(PFD);
  PFD.dwFlags := PFD_DOUBLEBUFFER;
  N := ChoosePixelFormat(DC, @PFD);
  SetPixelFormat(DC, N, @PFD);
end;

procedure CreateForm;
var
  Sx, Sy, X, Y: integer;
  bt: array [0 .. 3] of byte absolute Window.Handle;

  procedure RegClass(Name: string);
  var
    WindowClass: TWndClass;
  begin
    with WindowClass do begin
      Style := 0;//cs_HRedraw or cs_VRedraw or cs_OwnDC;
      lpfnWndProc := @WindowProc;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := MainInstance;
      MainIcon := LoadIcon(MainInstance, 'MAINICON');
      hIcon := MainIcon;
      hCursor := LoadCursor(0, idc_Arrow);
      hbrBackground := COLOR_WINDOW;
      lpszMenuName := nil;
      lpszClassName := PChar(Name);
    end;
    RegisterClass(WindowClass);
  end;

begin
  X := 600;
  Y := 400;
  RegClass(PChar('T' + Name));
  Sx := GetDeviceCaps(GetDC(0), HorzRes);
  Sy := GetDeviceCaps(GetDC(0), VertRes);
  with Window do begin
    ID := LastId;
    Inc(LastID);
    Handle := CreateWindow(PChar('T' + Name), PChar(Name), Style,
      (Sx - X) div 2, (Sy - Y) div 2, X, Y,
      Parent, 0, MainInstance, nil);

    Wnds[ID] := @Window;
    if MainForm = 0 then MainForm := Handle;

    //DC := GetDC(Handle);
    //DB(DC);

    MinID := 0;
    MaxID := 0;
    Accels := 0;

    Event := NoEvent;

    SetLength(HList, 256);
    SetLength(HList[bt[0]], 256);
    SetLength(HList[bt[0], bt[1]], 256);
    SetLength(HList[bt[0], bt[1], bt[2]], 256);

    HList[bt[0], bt[1], bt[2], bt[3]] := @Window;

    Tag := aTag;
  end;
end;

procedure SetClientSize;
var
  WR, CR: TRect;
begin
  GetWindowRect(H, WR);
  GetClientRect(H, CR);                  
  if X = -1 then X := CR.Right - CR.Left;
  if Y = -1 then Y := CR.Bottom - CR.Top;
  MoveWindow(H,
    WR.Left - (X - (CR.Right - CR.Left)) div 2,
    WR.Top - (Y - (CR.Bottom - CR.Top)) div 2,
      X + (WR.Right - WR.Left) - (CR.Right - CR.Left),
      Y + (WR.Bottom - WR.Top) - (CR.Bottom - CR.Top), True);
end;

procedure CorrectSize;
var
  D, D1, D2: integer;
  WR, CR: TRect;
begin          
  GetWindowRect(H, WR);
  GetClientRect(H, CR);
  D1 := MinX + (WR.Right - WR.Left) - (CR.Right - CR.Left);
  if MaxX = 0 then D2 := MaxInt else D2 := MaxX + (WR.Right - WR.Left) - (CR.Right - CR.Left);
  D := R.Right - R.Left;
  if D < D1 then D := D1 else if D > D2 then D := D2;
  case Mode of                                                          
    wmsz_TopLeft, wmsz_Left, wmsz_BottomLeft: R.Left := R.Right - D;
    else R.Right := R.Left + D;
  end;
  D1 := MinY + (WR.Bottom - WR.Top) - (CR.Bottom - CR.Top);
  if MaxY = 0 then D2 := MaxInt else D2 := MaxY + (WR.Bottom - WR.Top) - (CR.Bottom - CR.Top);
  D := R.Bottom - R.Top;
  if D < D1 then D := D1 else if D > D2 then D := D2;
  case Mode of
    wmsz_TopRight, wmsz_Top, wmsz_TopLeft: R.Top := R.Bottom - D;
    else R.Bottom := R.Top + D;
  end;
end;

procedure CreateAnyWindow (var Window: TWindow; ClassName, Name: string; X, Y, sizeX, sizeY: integer; Parent: HWND; Style: cardinal;
  aTabOrder: boolean = False; aEvent: TPProc = nil; dEvent : pointer = nil);
var
  ExStyle: DWORD;
begin
  with Window do begin
    ID := LastId;
    Inc(LastID);

    if Style and WS_BORDER <> 0 then begin
      ExStyle := WS_EX_CLIENTEDGE;
      Style := Style and not WS_BORDER;
    end else ExStyle := 0;

    if aTabOrder then Style := Style or WS_TABSTOP;
    
    Handle := CreateWindowExW(ExStyle, PWideChar(ToWide(ClassName)), PWideChar(ToWide(Name)), Style,
      X, Y, sizeX, sizeY,
      Parent, ID, hInstance, nil);

    SetCaption(Handle, Name);
    Wnds[ID] := @Window;
    //DC := GetDC(Handle);
    //DB(DC);
    SendMessage(Handle, wm_SetFont, WParam(Font), 0);
    MinID := 0;
    MaxID := 0;
    Tag := aTag;
    Event := ToEvent(aEvent, dEvent);
    if aTaborder then begin
      TabOrder := LastTab;
      Inc(LastTab);
      if (TabOrder >= 0) and (TabOrder < $10000) then TabList[TabOrder] := @Window;
    end else TabOrder := -10
  end;
end;

procedure MoveRadioGroup(var Window : TRadioGroup; x,y,sizex,sizey: integer);
var
  i:integer;
  sy : integer;
begin
  sy := (sizey-2) div Length(Window);
  for i := 0 to Length(Window) - 1 do with Window[i] do begin
    if i = 0 then begin
      if Window[i].Handle <> 0 then MoveWindow(Window[i].Handle, x,y, sizex,sizey, true)
    end else MoveWindow(Window[i].Handle, x+2, y + sy*i, sizex - 4, sy, true);
  end;
end;

procedure CreateRadioGroup;
var
  i: integer;
  sY: integer;
  Min, Max: integer;
begin
  SetLength(Window, Length(Names));
  sY := (sizeY - 2) div Length(Names);
  Min := LastId + 1;
  Max := LastId + Length(Window) - 1;
  for i := 0 to Length(Window) - 1 do with Window[i] do begin
    if i = 0 then begin
     if Names[i] = '' then Window[i].Handle := 0
     else CreateAnyWindow(Window[i], 'button', Names[i],
        X, Y, sizeX, sizeY, Parent, ws_Visible or ws_Child or bs_GroupBox)
    end else CreateAnyWindow(Window[i], 'button', Names[i],
      X + 2, Y + sY * i, sizeX - 4, sY, Parent, ws_Visible or ws_Child or bs_AutoRadioButton or (ws_Group * integer(i = 1)));
    Window[i].MinID := Min;
    Window[i].MaxID := Max;
    if i = 0 then begin
      Tag := aTag;
      if aTaborder then begin
        TabOrder := LastTab;
        Inc(LastTab);
        if (TabOrder >= 0) and (TabOrder < $10000) then TabList[TabOrder] := @Window[0];
      end else TabOrder := -10;
    end;
  end;
end;

function GetClassName(H: HWND): string;
var
  i: integer;
begin
  SetLength(Result, 255);
  if Windows.GetClassName(H, @Result[1], Length(Result)) = 0 then Result := '' else begin
    i := 0; while Result[i+1]<>#0 do Inc(i);
    SetLength(Result, i);
  end;
end;

procedure Click;
begin          
  if Wnds[ID] <> nil then with Wnds[ID]^ do begin
    if @Event.p <> nil then begin
      if (GetClassName(Handle)='ComboBox') and (HiWP <> CBN_SELCHANGE) then Exit;
      Event.p(Event.d);
    end;
  end;
end;

procedure CheckControl;
begin
  if Wnds[ID] <> nil then with Wnds[ID]^ do begin
    SetFocus(Handle);
    if MinID > 0 then CheckRadioButton(H, MinID, MaxID, ID);
    if (GetClassName(Handle) = 'Button') and (GetWindowLong(Handle, GWL_STYLE) and BS_AUTOCHECKBOX <> 0) then begin
      SendMessage(Handle, WM_LBUTTONDOWN, 0, 0);                                                                         
      SendMessage(Handle, WM_LBUTTONUP  , 0, 0); 
    end else if GetClassName(Handle) = 'ComboBox' then
    else Click(H, ID, HiWP);
  end;
end;         

procedure DoTab;     // народ, как это нормально делается?!
var
  i, N, sN: integer;
  GF: hWnd;
begin
  GF := GetFocusEx;
  N := -1;
  for i := 0 to Length(TabList) - 1 do if (TabList[i] <> nil) then begin
    if TabList[i].Handle = GF then begin
      N := i;
      Break;
    end;
  end else Break;               
  sN := N;
  repeat
    Inc(N);
    if (N > High(TabList)) or (TabList[N] = nil) then N := Low(TabList)-1;
    if (N >= Low(TabList)) and (TabList[N] <> nil) and IsWindowVisible(TabList[N].Handle) and IsWindowEnabled(TabList[N].Handle) then begin
      SetFocus(TabList[N].Handle);
      if GetFocusEx = TabList[N].Handle then
        Break;
    end;
  until N = sN;
end;

function RadioGroupChoise;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(Window) - 1 do if SendMessage(Window[i].Handle, bm_GetCheck, 0, 0) = 1 then begin
    Result := i;
    Break;
  end;
end;

procedure CreateMainMenu;
begin
  with Window do begin
    ID := LastId;
    Inc(LastID);
    Handle := CreateMenu;
    Wnds[ID] := @Window;
    //DC := 0;
    SetMenu(Parent, Handle);
    MinID := 0;
    MaxID := 0;
  end;
end;

function Ins(Menu, SubMenu: HWND; Name: string; ID: uint; Radio: boolean): boolean;
var
  MenuItem: MenuItemInfo;
begin
  FillChar(MenuItem, SizeOf(MenuItem), 0);
  with MenuItem do begin
    cbSize := SizeOf(MenuItem);
    fMask := miim_State or miim_Type or miim_SubMenu or miim_ID;// or MIIM_CHECKMARKS;
    if Name = '-' then fType := mft_Separator
    else if Radio then fType := mft_RadioCheck
    else fType := mft_String;
    fState := mfs_Enabled;
    wID := ID;
    hSubMenu := SubMenu;
    dwItemData := 0;
    dwTypeData := PAnsiChar(Name);
    cch := Length(Name);
  end;
  Result := InsertMenuItem(Menu, 0, false, MenuItem);
end;

procedure CreateSubMenu;
begin
  with Window do begin
    ID := LastId;
    Inc(LastID);
    Handle := CreatePopupMenu;
    Wnds[ID] := @Window;
    //DC := 0;
    Ins(Parent, Handle, Name, ID, false);
    MinID := 0;
    MaxID := 0;
    Event := noEvent;
  end;
end;

procedure CreateMenuItem(var Window: TWindow; Name: string; Parent: HWND; aEvent: TPProc = nil; dEvent : pointer = nil; Radio : boolean = false);
begin
  with Window do begin
    ID := LastId;
    Inc(LastID);
    Handle := 0;
    Wnds[ID] := @Window;
    //DC := 0;
    Ins(Parent, 0, Name, ID, Radio);
    MinID := 0;
    MaxID := 0;
    Event := ToEvent(aEvent, dEvent);
  end;
end;

procedure CreateBitmap;
var
  BI: TBitmapInfo;
  ScreenDC: HDC;
begin          
  FillChar(BI, SizeOf(BI), 0);
  Bitmap.SizeX := SizeX;
  Bitmap.SizeY := SizeY;
  with BI.bmiHeader do begin
    biSize := SizeOf(BI.bmiHeader);
    biWidth := sizeX;
    biHeight := sizeY;
    biPlanes := 1;
    biBitCount := Bits;
  end;
  ScreenDC := GetDC(0);
  with Bitmap do begin
    DC := CreateCompatibleDC(ScreenDC);
    Handle := CreateDIBSection(DC, BI, DIB_RGB_COLORS, pointer(Mem), 0, 0);
    SelectObject(DC, Handle);
    SelectObject(DC, Font);
    ReleaseDC(0, ScreenDC);
  end;
end;

function LoadBitmap;
var
  BI: packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0 .. 255] of TRGBQuad;
  end;
  Header: TBitmapFileHeader;
  ScreenDC: HDC;
  F: TFileOfByte;
begin
  Result := False;
  IOResult;
  Assign(F, FileName);
  Reset(F);
  if IOResult = 0 then begin
    BlockRead(F, Header, SizeOf(Header));
    LoadStruct(F, BI.bmiHeader, SizeOf(BI.bmiHeader));
    Bitmap.SizeX := BI.bmiHeader.biWidth;
    Bitmap.SizeY := BI.bmiHeader.biHeight;
    if BI.bmiHeader.biBitCount <= 8 then BlockRead(F, BI.bmiColors, 1 shl (BI.bmiHeader.biBitCount + 2));
    ScreenDC := GetDC(0);
    with Bitmap do begin
      DC := CreateCompatibleDC(ScreenDC);
      Handle := CreateDIBSection(DC, PBitmapInfo(@BI)^, DIB_RGB_COLORS, pointer(Mem), 0, 0);
      Seek(F, Header.bfOffBits);
      BlockRead(F, Mem^, BI.bmiHeader.biSizeImage);
      SelectObject(DC, Handle);
      ReleaseDC(0, ScreenDC);
    end;
    Close(F);
    Result := True;
  end;
end;

procedure DeleteBitmap;
begin
  with Bitmap do if Handle <> 0 then begin
    DeleteDC(DC);
    DeleteObject(Handle);
    Handle := 0;
  end;
end;

procedure DeleteWindow;
begin
  with Window do begin
    //ReleaseDC(Handle, DC);
    //DeleteDC(DC);
    DestroyWindow(Handle);
  end;
end;

procedure ShowForm(var Window: TWindow; Maximized: boolean = false);
var
  Tmp: PFormStack;
begin
  if FormStack = nil then begin
    MainForm := Window.Handle;
    if Maximized then ShowWindow(Window.Handle, SW_SHOWMAXIMIZED)
                 else ShowWindow(Window.Handle, CMDSHOW);
    UpdateWindow(Window.Handle);
  end else begin
    if Maximized then ShowWindow(Window.Handle, SW_SHOWMAXIMIZED)
                 else ShowWindow(Window.Handle, SW_NORMAL);
    EnableWindow(FormStack.Handle, False);
  end;
  New(Tmp);
  Tmp.Handle := Window.Handle;
  Tmp.Accels := Window.Accels;
  Tmp.Next := FormStack;
  FormStack := Tmp;
  MainForm := FormStack.Handle;
end;

procedure ShowFormByPtr (p : pointer);
begin
  ShowForm(PWindow(p)^);
end;

procedure HideTopForm;
var
  Tmp: PFormStack;
begin
  if FormStack = nil then Exit;
  Tmp := FormStack.Next;
  if Tmp = nil then begin
    // хз, надо выйти нафиг, наверное
  end else begin
    EnableWindow(Tmp.Handle, True);
    BringWindowToTop(Tmp.Handle);
    ShowWindow(FormStack.Handle, sw_Hide);
    Dispose(FormStack);
    FormStack := Tmp;       
    MainForm := FormStack.Handle;
  end;
end;

function MsgBox(hWnd: HWND; text, caption: string; uType: UINT): Integer;
begin
  if flagWaitingMessageBox then begin
    DebugBreak;
    MessageBox(MainForm, 'Незакрытая рекурсия с диалоговыми окнами!', 'ERROR', mb_OK or mb_IconError);     // рекурсия и хуй знает что делать
    Halt;
  end;
  flagWaitingMessageBox := True;
  Result := MessageBox (hWnd, PChar(text), PChar(caption), uType);
  flagWaitingMessageBox := False;
end;

procedure ShowError;
begin
  MsgBox(MainForm, Text, 'ERROR', mb_OK or mb_IconError);
end;

function WaitingMessageBox: boolean;
begin
  result := flagWaitingMessageBox;
end;

function GetCaption;
begin
  SetLength(Result, SendMessage(H, wm_GetTextLength, 0, 0));
  if Result <> '' then SendMessage(H, wm_GetText, Length(Result)+1, LPARAM(Result));
end;

procedure SetCaption;
begin
  SendMessage(H, WM_SETTEXT, 0, LPARAM(S));
end;

function ToAccels;
begin
  Result := CreateAcceleratorTable(A[0], Length(A) div 3);
end;

procedure LoadStruct;
var
  sz: cardinal absolute V;
begin
  BlockRead(F, sz, 4);
  if sz = Size then begin
    BlockRead(F, pointer(cardinal(@V) + 4)^, sz - 4);
  end else if sz > Size then begin
    BlockRead(F, pointer(cardinal(@V) + 4)^, Size - 4);
    Seek(F, FilePos(F) + integer(sz - Size));
  end else begin
    BlockRead(F, pointer(cardinal(@V) + 4)^, sz - 4);
    FillChar(pointer(cardinal(@V) + sz)^, Size - sz, 0);
  end;
end;

procedure ChgFont;
begin           
  Font := CreateFont(-Size, 0, 0, 0,
    0, 0, 0, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS,
    CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH or FF_DONTCARE,
    PChar(Name));
end;

function Exception(Code: PInt): TObject;
begin
  ExceptionCode := Code^;
  Result := TObject.Create;
end;

procedure AddEsp16;
asm
  ret 16
end;

procedure AssertProc (const Message, Filename: string; LineNumber: Integer; ErrorAddr: Pointer);
begin
  ShowError('Вышибает на ассерте "' + Message
    + '" в файле "' + Filename
    + '" на линии ' + IntToStr(LineNumber)
    + ' по адресу ' + IntToHex(Integer(ErrorAddr)) + '!');    
  DebugBreak;    
end;

var
  i: integer;
  OldException: pointer;

initialization
  OldException := ExceptObjProc;
  {$IFNDEF DEBUG}
  ExceptObjProc := @Exception;
  RaiseExceptionProc := @AddEsp16;
  {$ENDIF}
  MainForm := 0;
  LastID := 1;
  LastTab := 0;
 // FormStack := nil;
  MainPath := Paramstr(0);
  i := Length(MainPath);
  while (i > 0) and (MainPath[i] <> '\') do Dec(i);
  SetLength(MainPath, i);
  aTag := 0;
  Font := GetStockObject(Default_Gui_Font);

  {$IFDEF DEBUG}
  AssertErrorProc := AssertProc;
  {$ENDIF}

finalization
  ExceptObjProc := OldException;

end.
