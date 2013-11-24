object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'DelphiLog'
  ClientHeight = 301
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    633
    301)
  PixelsPerInch = 96
  TextHeight = 13
  object mmoStatus: TMemo
    Left = 8
    Top = 8
    Width = 617
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmoStatus')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnClose: TButton
    Left = 8
    Top = 268
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object btnSave: TButton
    Left = 89
    Top = 268
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
    Left = 312
    Top = 152
  end
end
