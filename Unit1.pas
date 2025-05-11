unit Unit1;

(*
  "EyeSaver" ver 1.3 – Developed by Mohsen E.Davatgar
  Built with Borland Delphi 7
  All rights reserved.
  https://github.com/Ya-Zahra
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, StrUtils,
  SysConst, MonitorLabels, EnhancedIniFiles;

const
  WM_APPINICHANGED = WM_USER + 1;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Timer2: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FIniFileAge: Integer;
    FMonitorCount: Integer;
    Panel1: TLabels;
    FDebug: Boolean;
    FUseRandomLCDFontColors: Boolean;
    FHideMouseCursor: Boolean;
    FAllowCloseWithAltF4: Boolean;
    FIniPath: string;
    FSecondsToShowForm: Integer;
    FSecondsToPause: Integer;
    FFadeInMiliseconds: Integer;
    FFadeOutMiliseconds: Integer;
    FLCDFontColor: Integer;
    FLCDColor: Integer;
    FFontCount: DWORD;
    FResFontHandle: THandle;
    FAppMutex: THandle;
    procedure HideForm;
    procedure ShowForm;
    procedure DoHideForm;
    procedure WMIniChanged(var Message: TMessage); message WM_APPINICHANGED;
    procedure WMDisplayChange(var Message: TMessage); message WM_DISPLAYCHANGE;
    procedure LoadCfg;
    procedure InitCfg;
    procedure HideMouseCursor;
    procedure ShowMouseCursor;
    procedure LoadResourceFonts;
    function GetLCDFontColor: Integer;
    function GetLCDFontName: string;
    procedure UpdatePanel;
    procedure UpdateFormSizePos;
  public
    { Public declarations }
  protected
  end;

var
  Form1: TForm1;

implementation

uses
  Registry, Math, IniFiles;

{$R *.dfm}
{$R 'fonts.res' 'fonts.rc'}
{$R 'manifest.res' 'manifest.rc'}

var
  ChangeHandle: THandle;
  WatchThread: TThread;

const
  CAppShortName = 'HE_EyeSaver';
  CResourceFontName = 'Digital-7 Mono'; // 'DIGITAL7MONO';
  CResourceFontID = 'DIGITAL7MONO';

  CDefaultLCDFontName = 'Consolas';

  CUseRandomLCDFontColors = true;
  CLCDColor = Integer(clBlack);
  CLCDFontColor = Integer(clAqua);
  CFadeInMiliseconds = 2000;
  CFadeOutMiliseconds = 2000;
  CHideMouseCursor = true;
  CMinSecondsToShowForm = 300; // 5 Minuts
  CSecondsToShowForm = 1200; // 20 Minuts
  CSecondsToPause = 90; // 1.5 Minuts
  CAllowCloseWithAltF4 = true;
  _Debug = 'Debug';
  _UseRandomLCDFontColors = 'UseRandomLCDFontColors';
  _LCDColor = 'LCDColor';
  _LCDFontColor = 'LCDFontColor';
  _FadeInMiliseconds = 'FadeInMiliseconds';
  _FadeOutMiliseconds = 'FadeOutMiliseconds';
  _HideMouseCursor = 'HideMouseCursor';
  _SecondsToShowForm = 'SecondsToShowForm';
  _SecondsToPause = 'SecondsToPause';
  _AllowCloseWithAltF4 = 'AllowCloseWithAltF4';
  _Main = 'Main';

var
  GSecondsToShowForm: Integer;
  GSecondsToPause: Integer;

type
  TChangeMonitorThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TChangeMonitorThread.Execute;
var
  Obj: DWORD;
begin
  while not Terminated do
  begin
    Obj := WaitForSingleObject(ChangeHandle, INFINITE);
    if Obj = WAIT_OBJECT_0 then
    begin
      PostMessage(Form1.Handle, WM_APPINICHANGED, 0, 0);
      if not FindNextChangeNotification(ChangeHandle) then
        break;
    end
    else
      break;
  end;
end;

procedure TForm1.HideMouseCursor;
begin
  if FHideMouseCursor then
    ShowCursor(False);
end;

procedure TForm1.ShowMouseCursor;
begin
  if FHideMouseCursor then
    ShowCursor(True);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Dec(FSecondsToShowForm);
  if FSecondsToShowForm <= 0 then
  begin
    Timer1.Enabled := false;
    FSecondsToPause := GSecondsToPause;
    UpdatePanel;
    ShowForm;
    Timer2.Enabled := true;
  end;
end;

function TForm1.GetLCDFontName(): string;
begin
  if (FResFontHandle <> 0) and (FFontCount > 0) then
    Result := CResourceFontName
  else
    Result := CDefaultLCDFontName;
end;

function GenerateOptimalContrastColor(BgColor: TColor): TColor;
var
  R, G, B: Byte;
  Luminance, ContrastRatio: Double;
  TextColor: TColor;

  function CalculateLuminance(R, G, B: Byte): Double;
  begin
    Result := (0.299 * R + 0.587 * G + 0.114 * B) / 255;
  end;

  function CalculateContrast(L1, L2: Double): Double;
  begin
    if L1 > L2 then
      Result := (L1 + 0.05) / (L2 + 0.05)
    else
      Result := (L2 + 0.05) / (L1 + 0.05);
  end;

begin
  BgColor := ColorToRGB(BgColor);
  R := GetRValue(BgColor);
  G := GetGValue(BgColor);
  B := GetBValue(BgColor);

  Luminance := CalculateLuminance(R, G, B);

  if Luminance < 0.5 then
  begin
    repeat
      TextColor := RGB(
        150 + Random(106), // R: 150-255
        150 + Random(106), // G: 150-255
        150 + Random(106) // B: 150-255
        );

      ContrastRatio := CalculateContrast(
        Luminance,
        CalculateLuminance(
        GetRValue(TextColor),
        GetGValue(TextColor),
        GetBValue(TextColor)
        )
        );

    until (ContrastRatio >= 4.5);
  end
  else
  begin
    repeat
      TextColor := RGB(
        Random(106), // R: 0-105
        Random(106), // G: 0-105
        Random(106) // B: 0-105
        );

      ContrastRatio := CalculateContrast(
        Luminance,
        CalculateLuminance(
        GetRValue(TextColor),
        GetGValue(TextColor),
        GetBValue(TextColor)
        )
        );

    until (ContrastRatio >= 4.5);
  end;

  Result := TextColor;
end;

function TForm1.GetLCDFontColor(): Integer;
begin
  if FUseRandomLCDFontColors then
    Result := GenerateOptimalContrastColor(FLCDColor)
  else
    Result := FLCDFontColor;
end;

procedure TForm1.UpdateFormSizePos();
begin
  Top := Screen.DesktopTop;
  Left := Screen.DesktopLeft;
  Width := Screen.DesktopWidth;
  Height := Screen.DesktopHeight;
end;

procedure TForm1.ShowForm();
begin
  with Monitor do //this will update monitors
    ;
  Color := FLCDColor;
  Panel1.FontName := GetLCDFontName;
  Panel1.FontColor := GetLCDFontColor;
  HideMouseCursor;
  UpdateFormSizePos;
  Panel1.Visible := false;
  AnimateWindow(Handle, FFadeInMiliseconds, AW_ACTIVATE or AW_BLEND);
  Sleep(1);
  Panel1.Visible := true;
  Visible := true;
  BringToFront;
  SetForegroundWindow(Handle);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //  ShowForm;
end;

procedure AddToAutoRun(AppName, AppPath: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', True) then
    begin
      Reg.WriteString(AppName, AppPath);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TForm1.InitCfg;
type
  TIniWriteAs = (iwaNormal, iwaBool, iwaInteger, iwaColor);
var
  ini: TEnhancedIniFile;
  procedure WriteINE(Ident: string; Value: Variant; writeAs: TIniWriteAs = iwaNormal);
  begin
    with ini do
      if not ValueExists(_Main, Ident) then
        case VarType(Value) of
          varSmallint, varInteger,
            varByte, varShortInt,
            varWord, varLongWord,
            varInt64:
            case writeAs of
              iwaColor:
                WriteString(_Main, Ident, IntToColorString(Value));
            else
              WriteInteger(_Main, Ident, Value);
            end;
          varSingle, varDouble, varCurrency:
            WriteFloat(_Main, Ident, Value);
          varBoolean:
            case writeAs of
              iwaBool: WriteString(_Main, Ident, BoolToStr(Value, true));
            else
              WriteBool(_Main, Ident, Value);
            end;
          varString, varOleStr:
            WriteString(_Main, Ident, Value);
        end
  end;
begin

  try
    ini := TEnhancedIniFile.Create(FIniPath);
    try
      WriteINE(_UseRandomLCDFontColors, CUseRandomLCDFontColors, iwaBool);
      WriteINE(_LCDColor, CLCDColor, iwaColor);
      WriteINE(_LCDFontColor, CLCDFontColor, iwaColor);
      WriteINE(_FadeInMiliseconds, CFadeInMiliseconds);
      WriteINE(_FadeOutMiliseconds, CFadeOutMiliseconds);
      WriteINE(_HideMouseCursor, CHideMouseCursor, iwaBool);
      WriteINE(_SecondsToShowForm, CSecondsToShowForm);
      WriteINE(_SecondsToPause, CSecondsToPause);
      WriteINE(_AllowCloseWithAltF4, CAllowCloseWithAltF4, iwaBool);
    finally
      ini.Free;
    end;
  except
    //no erros
  end;
end;

function STB(s: string): Boolean;
var
  i: Integer;
begin
  s := LowerCase(s);
  if AnsiMatchStr(s, ['yes', 'true', 'y']) then
    Result := true
  else if AnsiMatchStr(s, ['no', 'false', 'n']) then
    Result := false
  else if TryStrToInt(s, i) then
    Result := i <> 0
  else
    raise EConvertError.CreateResFmt(@SInvalidBoolean, [s]);
end;

procedure TForm1.LoadCfg;
begin
  with TEnhancedIniFile.Create(FIniPath) do
    try
      FDebug := ReadBool(_Main, _Debug, false);
      FUseRandomLCDFontColors := ReadBool(_Main, _UseRandomLCDFontColors, CUseRandomLCDFontColors);
      FLCDColor := ReadColor(_Main, _LCDColor, CLCDColor);
      FLCDFontColor := ReadColor(_Main, _LCDFontColor, CLCDFontColor);
      FFadeInMiliseconds := ReadInteger(_Main, _FadeInMiliseconds, CFadeInMiliseconds);
      FFadeOutMiliseconds := ReadInteger(_Main, _FadeOutMiliseconds, CFadeOutMiliseconds);
      FHideMouseCursor := ReadBool(_Main, _HideMouseCursor, CHideMouseCursor);
      GSecondsToShowForm := ReadInteger(_Main, _SecondsToShowForm, CSecondsToShowForm);
      GSecondsToPause := ReadInteger(_Main, _SecondsToPause, CSecondsToPause);
      FAllowCloseWithAltF4 := ReadBool(_Main, _AllowCloseWithAltF4, CAllowCloseWithAltF4);
    finally
      Free;
    end;
  if not FDebug then
    GSecondsToShowForm := Max(GSecondsToShowForm, CMinSecondsToShowForm);
  FSecondsToShowForm := GSecondsToShowForm;
  FSecondsToPause := GSecondsToPause;
  Timer1.Enabled := true;
  FIniFileAge := FileAge(FIniPath);
end;

procedure TForm1.LoadResourceFonts;
var
  ResStream: TResourceStream;
begin
  ResStream := TResourceStream.Create(HInstance, CResourceFontID, RT_RCDATA);
  try
    FResFontHandle := AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil, @FFontCount);
  finally
    ResStream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FMonitorCount := Screen.MonitorCount;
  Panel1 := TLabels.Create(Self);

  FAppMutex := CreateMutex(nil, True, PChar(CAppShortName));
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    CloseHandle(FAppMutex);
    FAppMutex := 0;
    Application.Terminate;
    exit;
  end;
  AddToAutoRun(CAppShortName, Application.ExeName);
  FIniPath := Application.ExeName + '.ini';
  LoadResourceFonts;
  ChangeHandle := FindFirstChangeNotification(
    PChar(ExtractFilePath(FIniPath)),
    False,
    FILE_NOTIFY_CHANGE_LAST_WRITE);
  if ChangeHandle <> INVALID_HANDLE_VALUE then
    WatchThread := TChangeMonitorThread.Create(False);

  InitCfg;
  //  if FileExists(FIniPath) then
  LoadCfg;
end;

function SecondsToMMSS(Seconds: Integer): string;
var
  Minutes: Integer;
begin
  Minutes := Seconds div 60;
  Seconds := Seconds mod 60;
  Result := Format('%2.2d:%2.2d', [Minutes, Seconds]);
end;

procedure TForm1.HideForm;
begin
  Panel1.Visible := false;
  ShowMouseCursor;
  AnimateWindow(Handle, FFadeOutMiliseconds, AW_HIDE or AW_BLEND);
end;

procedure TForm1.DoHideForm;
begin
  FSecondsToShowForm := GSecondsToShowForm;
  FSecondsToPause := GSecondsToPause;
  Timer2.Enabled := false;
  Timer1.Enabled := true;
  HideForm;
end;

procedure TForm1.UpdatePanel;
begin
  Panel1.Caption := SecondsToMMSS(FSecondsToPause);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Dec(FSecondsToPause);
  UpdatePanel;
  if (FSecondsToPause <= 0) then
  begin
    Application.ProcessMessages;
    DoHideForm;
    Exit;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FormStyle := fsNormal;
  CanClose := FAllowCloseWithAltF4;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('H')) then
    DoHideForm;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := false;
  Timer2.Enabled := false;
  if ChangeHandle <> INVALID_HANDLE_VALUE then
    FindCloseChangeNotification(ChangeHandle);
  if Assigned(WatchThread) then
  begin
    WatchThread.Terminate;
    WatchThread.WaitFor;
    WatchThread.Free;
  end;
  if FAppMutex <> 0 then
    CloseHandle(FAppMutex);
  if FResFontHandle <> 0 then
    RemoveFontMemResourceEx(FResFontHandle);
end;

procedure TForm1.WMIniChanged(var Message: TMessage);
begin
  if FileAge(FIniPath) <> FIniFileAge then
    LoadCfg;
end;

procedure TForm1.WMDisplayChange(var Message: TMessage);
var
  mc: Integer;
begin
  mc := GetSystemMetrics(SM_CMONITORS);
  if mc <> FMonitorCount then
  begin
    FMonitorCount := mc;
    UpdateFormSizePos;
    Panel1.CheckMonitors;
  end;
  inherited;
end;

end.
