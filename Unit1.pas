unit Unit1;

(*
  "EyeSaver" ver 1.1 – Developed by Mohsen E.Davatgar
  Built with Borland Delphi 7
  All rights reserved.
  https://github.com/Ya-Zahra
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, IniFiles;

const
  WM_APPINICHANGED = WM_USER + 1;
  CAppShortName = 'HE_EyeSaver';

type
  TForm1 = class(TForm)
    Panel1: TPanel;
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
    FHideMouseCursor: Boolean;
    FAllowCloseWithAltF4: Boolean;
    FIniPath: string;
    FSecondsToShowForm: Integer;
    FSecondsToPause: Integer;
    FFadeInMiliseconds: Integer;
    FFadeOutMiliseconds: Integer;
    FLCDFontColor: Integer;
    FFontCount: DWORD;
    FResFontHandle: THandle;
    FAppMutex: THandle;
    procedure HideForm;
    procedure ShowForm;
    procedure DoHideForm;
    procedure WMIniChanged(var Message: TMessage); message WM_APPINICHANGED;
    procedure LoadCfg;
    procedure SaveCfg;
    procedure HideMouseCursor;
    procedure ShowMouseCursor;
    procedure LoadResourceFonts;
    function GetLCDFontColor: Integer;
    function GetLCDFontName: string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Registry, Math;

{$R *.dfm}
{$R 'fonts.res' 'fonts.rc'}
{$R 'manifest.res' 'manifest.rc'}

var
  ChangeHandle: THandle;
  WatchThread: TThread;

const
  CResourceFontName = 'Digital-7 Mono';
  CResourceFontID = 'DIGITAL7MONO';

  CDefaultLCDFontName = 'Consolas';

  CLCDFontColor = Integer(clAqua);
  CFadeInMiliseconds = 2000;
  CFadeOutMiliseconds = 2000;
  CHideMouseCursor = true;
  CSecondsToShowForm = 1200; // 20 Minuts
  CSecondsToPause = 90; // 1.5 Minuts
  CAllowCloseWithAltF4 = true;
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
      FindNextChangeNotification(ChangeHandle);
    end;
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
    Timer2.Enabled := true;
    Timer2Timer(nil);
    FSecondsToShowForm := GSecondsToShowForm;
    ShowForm;
  end;
end;

function TForm1.GetLCDFontName(): string;
begin
  if (FResFontHandle <> 0) and (FFontCount > 0) then
    Result := CResourceFontName
  else
    Result := CDefaultLCDFontName;
end;

function TForm1.GetLCDFontColor(): Integer;
begin
  Result := FLCDFontColor;
end;

procedure TForm1.ShowForm();
begin
  Panel1.Font.Name := GetLCDFontName;
  Panel1.Font.Color := GetLCDFontColor;
  Panel1.Visible := true;
  HideMouseCursor;
  Top := 0;
  Left := 0;
  Width := Screen.Width;
  Height := Screen.Height;
  AnimateWindow(Handle, FFadeInMiliseconds, AW_ACTIVATE or AW_BLEND);
  Visible := true;
  BringToFront;
  SetForegroundWindow(Handle);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ShowForm;
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

procedure TForm1.SaveCfg;
begin
  try
    with TIniFile.Create(FIniPath) do
      try
        WriteInteger(_Main, _LCDFontColor, CLCDFontColor);
        WriteInteger(_Main, _FadeInMiliseconds, CFadeInMiliseconds);
        WriteInteger(_Main, _FadeOutMiliseconds, CFadeOutMiliseconds);
        WriteBool(_Main, _HideMouseCursor, CHideMouseCursor);
        WriteInteger(_Main, _SecondsToShowForm, GSecondsToShowForm);
        WriteInteger(_Main, _SecondsToPause, GSecondsToPause);
        WriteBool(_Main, _AllowCloseWithAltF4, CAllowCloseWithAltF4);
      finally
        Free;
      end;
  except
    //no erros
  end;
end;

procedure TForm1.LoadCfg;
begin
  with TIniFile.Create(FIniPath) do
    try
      FLCDFontColor := ReadInteger(_Main, _LCDFontColor, CLCDFontColor);
      FFadeInMiliseconds := ReadInteger(_Main, _FadeInMiliseconds, CFadeInMiliseconds);
      FFadeOutMiliseconds := ReadInteger(_Main, _FadeOutMiliseconds, CFadeOutMiliseconds);
      FHideMouseCursor := ReadBool(_Main, _HideMouseCursor, CHideMouseCursor);
      GSecondsToShowForm := ReadInteger(_Main, _SecondsToShowForm, CSecondsToShowForm);
      GSecondsToPause := ReadInteger(_Main, _SecondsToPause, CSecondsToPause);
      FAllowCloseWithAltF4 := ReadBool(_Main, _AllowCloseWithAltF4, CAllowCloseWithAltF4);
    finally
      Free;
    end;
  FSecondsToShowForm := Max(GSecondsToShowForm, 14);
  FSecondsToPause := GSecondsToPause;
  Timer1.Enabled := true;
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

  WatchThread := TChangeMonitorThread.Create(False);

  GSecondsToShowForm := CSecondsToShowForm;
  GSecondsToPause := CSecondsToPause;
  FAllowCloseWithAltF4 := CAllowCloseWithAltF4;
  if FileExists(FIniPath) then
    LoadCfg
  else
    SaveCfg;
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

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Panel1.Caption := SecondsToMMSS(FSecondsToPause);
  if (FSecondsToPause <= 0) then
  begin
    DoHideForm;
    Exit;
  end;
  Dec(FSecondsToPause);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
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
  if Assigned(WatchThread) then
  begin
    WatchThread.Terminate;
    WatchThread.WaitFor;
    WatchThread.Free;
  end;
  if ChangeHandle <> 0 then
    FindCloseChangeNotification(ChangeHandle);
  if FAppMutex <> 0 then
    CloseHandle(FAppMutex);
  if FResFontHandle <> 0 then
    RemoveFontMemResourceEx(FResFontHandle);
end;

procedure TForm1.WMIniChanged(var Message: TMessage);
begin
  LoadCfg;
end;

end.
