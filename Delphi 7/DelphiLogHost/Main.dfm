object frmMain: TfrmMain
  Left = 310
  Top = 180
  Width = 753
  Height = 426
  Caption = 'Delphi Log-Host'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    737
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object mmoStatus: TMemo
    Left = 5
    Top = 8
    Width = 725
    Height = 337
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmoStatus')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnClose: TButton
    Left = 8
    Top = 359
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object btnSave: TButton
    Left = 96
    Top = 359
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object dlgSave: TSaveDialog
    FileName = 'DelphiLog.txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Left = 232
    Top = 120
  end
end
