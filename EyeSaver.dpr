program EyeSaver;

(*
  "EyeSaver" ver 1.1 – Developed by Mohsen E.Davatgar
  Built with Borland Delphi 7
  All rights reserved.
  https://github.com/Ya-Zahra
*)

uses
  Forms,
  Windows,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'EyeSaver';
  Application.CreateForm(TForm1, Form1);
  Application.ShowMainForm := false;
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) OR WS_EX_TOOLWINDOW AND NOT WS_EX_APPWINDOW);
  Application.Run;
end.
