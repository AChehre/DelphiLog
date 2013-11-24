object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 138
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnWriteLog: TButton
    Left = 48
    Top = 40
    Width = 257
    Height = 25
    Caption = 'Write Log'
    TabOrder = 0
    OnClick = btnWriteLogClick
  end
  object btnLogEnabled: TButton
    Left = 48
    Top = 80
    Width = 257
    Height = 25
    Caption = 'Disable Log'
    TabOrder = 1
    OnClick = btnLogEnabledClick
  end
  object edtMessage: TEdit
    Left = 48
    Top = 8
    Width = 257
    Height = 21
    TabOrder = 2
    Text = 'Log Message'
  end
end
