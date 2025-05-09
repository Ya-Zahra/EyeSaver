object Form1: TForm1
  Left = 262
  Top = 168
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 501
  ClientWidth = 1028
  Color = clBlack
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 24
    Top = 16
  end
  object Timer2: TTimer
    Enabled = False
    OnTimer = Timer2Timer
    Left = 24
    Top = 56
  end
end
