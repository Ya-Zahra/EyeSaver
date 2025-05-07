object Form1: TForm1
  Left = 192
  Top = 125
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 501
  ClientWidth = 1028
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1028
    Height = 501
    Align = alClient
    BevelOuter = bvNone
    Caption = '05:00'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -96
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
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
