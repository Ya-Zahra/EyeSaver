unit EnhancedIniFiles;

interface

uses
  IniFiles, Graphics, SysUtils, Classes, Math, Windows, ExtCtrls,
  Dialogs;

type
  TEnhancedIniFile = class(TIniFile)
  public
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    function ReadColor(const Section, Ident: string; Default: TColor): TColor;
  private
    function HSLToRGB(H, S, L: Double): TColor;
    function ParseHSLValue(const Value: string; out H, S, L: Double): Boolean;
    function ExtractDelimited(Index: Integer; const S: string; Delimiter: Char): string;
  end;

function IntToColorString(Color: Integer): string;

implementation

const
  clNameBlack = 'Black';
  clNameMaroon = 'Maroon';
  clNameGreen = 'Green';
  clNameOlive = 'Olive';
  clNameNavy = 'Navy';
  clNamePurple = 'Purple';
  clNameTeal = 'Teal';
  clNameGray = 'Gray';
  clNameSilver = 'Silver';
  clNameRed = 'Red';
  clNameLime = 'Lime';
  clNameYellow = 'Yellow';
  clNameBlue = 'Blue';
  clNameFuchsia = 'Fuchsia';
  clNameAqua = 'Aqua';
  clNameWhite = 'White';
  clNameMoneyGreen = 'Money Green';
  clNameSkyBlue = 'Sky Blue';
  clNameCream = 'Cream';
  clNameMedGray = 'Medium Gray';
  clNameActiveBorder = 'Active Border';
  clNameActiveCaption = 'Active Caption';
  clNameAppWorkSpace = 'Application Workspace';
  clNameBackground = 'Background';
  clNameBtnFace = 'Button Face';
  clNameBtnHighlight = 'Button Highlight';
  clNameBtnShadow = 'Button Shadow';
  clNameBtnText = 'Button Text';
  clNameCaptionText = 'Caption Text';
  clNameDefault = 'Default';
  clNameGradientActiveCaption = 'Gradient Active Caption';
  clNameGradientInactiveCaption = 'Gradient Inactive Caption';
  clNameGrayText = 'Gray Text';
  clNameHighlight = 'Highlight Background';
  clNameHighlightText = 'Highlight Text';
  clNameInactiveBorder = 'Inactive Border';
  clNameInactiveCaption = 'Inactive Caption';
  clNameInactiveCaptionText = 'Inactive Caption Text';
  clNameInfoBk = 'Info Background';
  clNameInfoText = 'Info Text';
  clNameMenu = 'Menu Background';
  clNameMenuText = 'Menu Text';
  clNameNone = 'None';
  clNameScrollBar = 'Scroll Bar';
  clName3DDkShadow = '3D Dark Shadow';
  clName3DLight = '3D Light';
  clNameWindow = 'Window Background';
  clNameWindowFrame = 'Window Frame';
  clNameWindowText = 'Window Text';

const
  WebColors: array[0..140] of TIdentMapEntry = (
    (Value: $00FFF8F0; Name: 'AliceBlue'), // #F0F8FF
    (Value: $00D7EBFA; Name: 'AntiqueWhite'), // #FAEBD7
    (Value: $00FFFF00; Name: 'Aqua'), // #00FFFF
    (Value: $00D4FF7F; Name: 'Aquamarine'), // #7FFFD4
    (Value: $00FFFFF0; Name: 'Azure'), // #F0FFFF
    (Value: $00DCF5F5; Name: 'Beige'), // #F5F5DC
    (Value: $00C4E4FF; Name: 'Bisque'), // #FFE4C4
    (Value: $00000000; Name: 'Black'), // #000000
    (Value: $00CDEBFF; Name: 'BlanchedAlmond'), // #FFEBCD
    (Value: $00FF0000; Name: 'Blue'), // #0000FF
    (Value: $00E22B8A; Name: 'BlueViolet'), // #8A2BE2
    (Value: $002A2AA5; Name: 'Brown'), // #A52A2A
    (Value: $0087B8DE; Name: 'BurlyWood'), // #DEB887
    (Value: $00A09E5F; Name: 'CadetBlue'), // #5F9EA0
    (Value: $0000FF7F; Name: 'Chartreuse'), // #7FFF00
    (Value: $001E69D2; Name: 'Chocolate'), // #D2691E
    (Value: $00507FFF; Name: 'Coral'), // #FF7F50
    (Value: $00ED9564; Name: 'CornflowerBlue'), // #6495ED
    (Value: $00DCF8FF; Name: 'Cornsilk'), // #FFF8DC
    (Value: $003C14DC; Name: 'Crimson'), // #DC143C
    (Value: $00FFFF00; Name: 'Cyan'), // #00FFFF
    (Value: $008B0000; Name: 'DarkBlue'), // #00008B
    (Value: $008B8B00; Name: 'DarkCyan'), // #008B8B
    (Value: $000B86B8; Name: 'DarkGoldenRod'), // #B8860B
    (Value: $00A9A9A9; Name: 'DarkGray'), // #A9A9A9
    (Value: $00006400; Name: 'DarkGreen'), // #006400
    (Value: $006BB7BD; Name: 'DarkKhaki'), // #BDB76B
    (Value: $008B008B; Name: 'DarkMagenta'), // #8B008B
    (Value: $002F6B55; Name: 'DarkOliveGreen'), // #556B2F
    (Value: $00008CFF; Name: 'DarkOrange'), // #FF8C00
    (Value: $00CC3299; Name: 'DarkOrchid'), // #9932CC
    (Value: $0000008B; Name: 'DarkRed'), // #8B0000
    (Value: $007A96E9; Name: 'DarkSalmon'), // #E9967A
    (Value: $008FBC8F; Name: 'DarkSeaGreen'), // #8FBC8F
    (Value: $008B3D48; Name: 'DarkSlateBlue'), // #483D8B
    (Value: $004F4F2F; Name: 'DarkSlateGray'), // #2F4F4F
    (Value: $00D1CE00; Name: 'DarkTurquoise'), // #00CED1
    (Value: $00D30094; Name: 'DarkViolet'), // #9400D3
    (Value: $009314FF; Name: 'DeepPink'), // #FF1493
    (Value: $00EBB700; Name: 'DeepSkyBlue'), // #00B7EB
    (Value: $00696969; Name: 'DimGray'), // #696969
    (Value: $00FF901E; Name: 'DodgerBlue'), // #1E90FF
    (Value: $002222B2; Name: 'FireBrick'), // #B22222
    (Value: $00F0FAFF; Name: 'FloralWhite'), // #FFFAF0
    (Value: $00228B22; Name: 'ForestGreen'), // #228B22
    (Value: $00FF00FF; Name: 'Fuchsia'), // #FF00FF
    (Value: $00DCDCDC; Name: 'Gainsboro'), // #DCDCDC
    (Value: $00FFF8F8; Name: 'GhostWhite'), // #F8F8FF
    (Value: $0000D7FF; Name: 'Gold'), // #FFD700
    (Value: $0020A5DA; Name: 'GoldenRod'), // #DAA520
    (Value: $00808080; Name: 'Gray'), // #808080
    (Value: $00008000; Name: 'Green'), // #008000
    (Value: $002FFFAD; Name: 'GreenYellow'), // #ADFF2F
    (Value: $00F0FFF0; Name: 'HoneyDew'), // #F0FFF0
    (Value: $00B469FF; Name: 'HotPink'), // #FF69B4
    (Value: $005C5CCD; Name: 'IndianRed'), // #CD5C5C
    (Value: $0082004B; Name: 'Indigo'), // #4B0082
    (Value: $00F0FFFF; Name: 'Ivory'), // #FFFFF0
    (Value: $008CE6F0; Name: 'Khaki'), // #F0E68C
    (Value: $00FAE6E6; Name: 'Lavender'), // #E6E6FA
    (Value: $00F5F0FF; Name: 'LavenderBlush'), // #FFF0F5
    (Value: $0000FC7C; Name: 'LawnGreen'), // #7CFC00
    (Value: $00CDFAFF; Name: 'LemonChiffon'), // #FFFACD
    (Value: $00E6D8AD; Name: 'LightBlue'), // #ADD8E6
    (Value: $008080F0; Name: 'LightCoral'), // #F08080
    (Value: $00FFFFE0; Name: 'LightCyan'), // #E0FFFF
    (Value: $00D2FAFA; Name: 'LightGoldenRodYellow'), // #FAFAD2
    (Value: $00D3D3D3; Name: 'LightGray'), // #D3D3D3
    (Value: $0090EE90; Name: 'LightGreen'), // #90EE90
    (Value: $00C1B6FF; Name: 'LightPink'), // #FFB6C1
    (Value: $007AA0FF; Name: 'LightSalmon'), // #FFA07A
    (Value: $00AAB220; Name: 'LightSeaGreen'), // #20B2AA
    (Value: $00FACE87; Name: 'LightSkyBlue'), // #87CEFA
    (Value: $00998877; Name: 'LightSlateGray'), // #778899
    (Value: $00DEC4B0; Name: 'LightSteelBlue'), // #B0C4DE
    (Value: $00E0FFFF; Name: 'LightYellow'), // #FFFFE0
    (Value: $0000FF00; Name: 'Lime'), // #00FF00
    (Value: $0032CD32; Name: 'LimeGreen'), // #32CD32
    (Value: $00E6F0FA; Name: 'Linen'), // #FAF0E6
    (Value: $00FF00FF; Name: 'Magenta'), // #FF00FF
    (Value: $00000080; Name: 'Maroon'), // #800000
    (Value: $00AACD66; Name: 'MediumAquaMarine'), // #66CDAA
    (Value: $00CD0000; Name: 'MediumBlue'), // #0000CD
    (Value: $00D355BA; Name: 'MediumOrchid'), // #BA55D3
    (Value: $00DB7093; Name: 'MediumPurple'), // #9370DB
    (Value: $0071B33C; Name: 'MediumSeaGreen'), // #3CB371
    (Value: $00EE687B; Name: 'MediumSlateBlue'), // #7B68EE
    (Value: $009AFA00; Name: 'MediumSpringGreen'), // #00FA9A
    (Value: $00CCD148; Name: 'MediumTurquoise'), // #48D1CC
    (Value: $008515C7; Name: 'MediumVioletRed'), // #C71585
    (Value: $00701919; Name: 'MidnightBlue'), // #191970
    (Value: $00FAFFF5; Name: 'MintCream'), // #F5FFFA
    (Value: $00E1E4FF; Name: 'MistyRose'), // #FFE4E1
    (Value: $00B5E4FF; Name: 'Moccasin'), // #FFE4B5
    (Value: $00ADDEFF; Name: 'NavajoWhite'), // #FFDEAD
    (Value: $00800000; Name: 'Navy'), // #000080
    (Value: $00E6F5FD; Name: 'OldLace'), // #FDF5E6
    (Value: $00008080; Name: 'Olive'), // #808000
    (Value: $00238E6B; Name: 'OliveDrab'), // #6B8E23
    (Value: $0000A5FF; Name: 'Orange'), // #FFA500
    (Value: $000045FF; Name: 'OrangeRed'), // #FF4500
    (Value: $00D670DA; Name: 'Orchid'), // #DA70D6
    (Value: $00AAE8EE; Name: 'PaleGoldenRod'), // #EEE8AA
    (Value: $0098FB98; Name: 'PaleGreen'), // #98FB98
    (Value: $00EEEEAF; Name: 'PaleTurquoise'), // #AFEEEE
    (Value: $009370DB; Name: 'PaleVioletRed'), // #DB7093
    (Value: $00D5EFFF; Name: 'PapayaWhip'), // #FFEFD5
    (Value: $00B9DAFF; Name: 'PeachPuff'), // #FFDAB9
    (Value: $003F85CD; Name: 'Peru'), // #CD853F
    (Value: $00CBC0FF; Name: 'Pink'), // #FFC0CB
    (Value: $00DDA0DD; Name: 'Plum'), // #DDA0DD
    (Value: $00E6E0B0; Name: 'PowderBlue'), // #B0E0E6
    (Value: $00800080; Name: 'Purple'), // #800080
    (Value: $00993366; Name: 'RebeccaPurple'), // #663399
    (Value: $000000FF; Name: 'Red'), // #FF0000
    (Value: $008F8FBC; Name: 'RosyBrown'), // #BC8F8F
    (Value: $00E16941; Name: 'RoyalBlue'), // #4169E1
    (Value: $0013458B; Name: 'SaddleBrown'), // #8B4513
    (Value: $007280FA; Name: 'Salmon'), // #FA8072
    (Value: $0060A4F4; Name: 'SandyBrown'), // #F4A460
    (Value: $00578B2E; Name: 'SeaGreen'), // #2E8B57
    (Value: $00EEF5FF; Name: 'SeaShell'), // #FFF5EE
    (Value: $002D52A0; Name: 'Sienna'), // #A0522D
    (Value: $00C0C0C0; Name: 'Silver'), // #C0C0C0
    (Value: $00EBCE87; Name: 'SkyBlue'), // #87CEEB
    (Value: $00CD5A6A; Name: 'SlateBlue'), // #6A5ACD
    (Value: $00908070; Name: 'SlateGray'), // #708090
    (Value: $00FAFAFF; Name: 'Snow'), // #FFFAFA
    (Value: $007FFF00; Name: 'SpringGreen'), // #00FF7F
    (Value: $00B48246; Name: 'SteelBlue'), // #4682B4
    (Value: $008CB4D2; Name: 'Tan'), // #D2B48C
    (Value: $00808000; Name: 'Teal'), // #008080
    (Value: $00D8BFD8; Name: 'Thistle'), // #D8BFD8
    (Value: $004763FF; Name: 'Tomato'), // #FF6347
    (Value: $00D0E040; Name: 'Turquoise'), // #40E0D0
    (Value: $00EE82EE; Name: 'Violet'), // #EE82EE
    (Value: $00B3DEF5; Name: 'Wheat'), // #F5DEB3
    (Value: $00FFFFFF; Name: 'White'), // #FFFFFF
    (Value: $00F5F5F5; Name: 'WhiteSmoke'), // #F5F5F5
    (Value: $0000FFFF; Name: 'Yellow'), // #FFFF00
    (Value: $0032CD9A; Name: 'YellowGreen') // #9ACD32
    );

  ColorToPretyName: array[0..46] of TIdentMapEntry = (
    (Value: clBlack; Name: clNameBlack),
    (Value: clMaroon; Name: clNameMaroon),
    (Value: clGreen; Name: clNameGreen),
    (Value: clOlive; Name: clNameOlive),
    (Value: clNavy; Name: clNameNavy),
    (Value: clPurple; Name: clNamePurple),
    (Value: clTeal; Name: clNameTeal),
    (Value: clGray; Name: clNameGray),
    (Value: clSilver; Name: clNameSilver),
    (Value: clRed; Name: clNameRed),
    (Value: clLime; Name: clNameLime),
    (Value: clYellow; Name: clNameYellow),
    (Value: clBlue; Name: clNameBlue),
    (Value: clFuchsia; Name: clNameFuchsia),
    (Value: clAqua; Name: clNameAqua),
    (Value: clWhite; Name: clNameWhite),
    (Value: clMoneyGreen; Name: clNameMoneyGreen),
    (Value: clSkyBlue; Name: clNameSkyBlue),
    (Value: clCream; Name: clNameCream),
    (Value: clMedGray; Name: clNameMedGray),

    (Value: clActiveBorder; Name: clNameActiveBorder),
    (Value: clActiveCaption; Name: clNameActiveCaption),
    (Value: clAppWorkSpace; Name: clNameAppWorkSpace),
    (Value: clBackground; Name: clNameBackground),
    (Value: clBtnFace; Name: clNameBtnFace),
    (Value: clBtnHighlight; Name: clNameBtnHighlight),
    (Value: clBtnShadow; Name: clNameBtnShadow),
    (Value: clBtnText; Name: clNameBtnText),
    (Value: clCaptionText; Name: clNameCaptionText),
    (Value: clDefault; Name: clNameDefault),
    (Value: clGrayText; Name: clNameGrayText),
    (Value: clHighlight; Name: clNameHighlight),
    (Value: clHighlightText; Name: clNameHighlightText),
    (Value: clInactiveBorder; Name: clNameInactiveBorder),
    (Value: clInactiveCaption; Name: clNameInactiveCaption),
    (Value: clInactiveCaptionText; Name: clNameInactiveCaptionText),
    (Value: clInfoBk; Name: clNameInfoBk),
    (Value: clInfoText; Name: clNameInfoText),
    (Value: clMenu; Name: clNameMenu),
    (Value: clMenuText; Name: clNameMenuText),
    (Value: clNone; Name: clNameNone),
    (Value: clScrollBar; Name: clNameScrollBar),
    (Value: cl3DDkShadow; Name: clName3DDkShadow),
    (Value: cl3DLight; Name: clName3DLight),
    (Value: clWindow; Name: clNameWindow),
    (Value: clWindowFrame; Name: clNameWindowFrame),
    (Value: clWindowText; Name: clNameWindowText));

function TEnhancedIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
var
  Value: string;
  i: Integer;
begin
  Value := ReadString(Section, Ident, '');
  Value := LowerCase(Trim(Value));

  if Value = '' then
    Result := Default
  else if (Value = 'true') or (Value = 'yes') or (Value = 'y') then
    Result := True
  else if (Value = 'false') or (Value = 'no') or (Value = 'n') then
    Result := False
  else if TryStrToInt(Value, i) then
    Result := i <> 0
  else
    Result := Default;
end;

function TEnhancedIniFile.HSLToRGB(H, S, L: Double): TColor;
var
  R, G, B: Double;
  C, X, m: Double;
begin
  // Normalize H to [0, 360)
  H := H - Floor(H / 360) * 360;

  // Convert HSL to RGB
  C := (1 - Abs(2 * L - 1)) * S;
  X := C * (1 - Abs(Frac(H / 60) - 1));
  m := L - C / 2;

  if (H >= 0) and (H < 60) then
  begin
    R := C;
    G := X;
    B := 0;
  end
  else if (H >= 60) and (H < 120) then
  begin
    R := X;
    G := C;
    B := 0;
  end
  else if (H >= 120) and (H < 180) then
  begin
    R := 0;
    G := C;
    B := X;
  end
  else if (H >= 180) and (H < 240) then
  begin
    R := 0;
    G := X;
    B := C;
  end
  else if (H >= 240) and (H < 300) then
  begin
    R := X;
    G := 0;
    B := C;
  end
  else
  begin
    R := C;
    G := 0;
    B := X;
  end;

  // Scale to [0,255] and apply offset
  Result := RGB(
    Round((R + m) * 255),
    Round((G + m) * 255),
    Round((B + m) * 255)
    );
end;

function TEnhancedIniFile.ExtractDelimited(Index: Integer; const S: string; Delimiter: Char): string;
var
  P, P1: PChar;
  i: Integer;
begin
  Result := '';
  if (Index < 1) or (S = '') then
    Exit;

  P := PChar(S);
  for i := 1 to Index do
  begin
    P1 := P;
    while (P^ <> #0) and (P^ <> Delimiter) do
      Inc(P);
    if i = Index then
    begin
      SetString(Result, P1, P - P1);
      Exit;
    end;
    if P^ = #0 then
      Exit;
    Inc(P);
  end;
end;

function TEnhancedIniFile.ParseHSLValue(const Value: string; out H, S, L: Double): Boolean;
var
  TempStr: string;
  Parts: TStringList;
begin
  Result := False;
  TempStr := LowerCase(Trim(Value));

  if (Pos('hsl(', TempStr) = 1) and (TempStr[Length(TempStr)] = ')') then
  begin
    TempStr := Copy(TempStr, 5, Length(TempStr) - 5);
    TempStr := StringReplace(TempStr, '%', '', [rfReplaceAll]);
    TempStr := StringReplace(TempStr, ' ', '', [rfReplaceAll]);

    Parts := TStringList.Create;
    try
      Parts.Delimiter := ',';
      Parts.DelimitedText := TempStr;

      if Parts.Count = 3 then
      begin
        try
          H := StrToFloat(Parts[0]);
          S := StrToFloat(Parts[1]) / 100;
          L := StrToFloat(Parts[2]) / 100;

          if (H >= 0) and (S >= 0) and (S <= 1) and (L >= 0) and (L <= 1) then
            Result := True;
        except

        end;
      end;
    finally
      Parts.Free;
    end;
  end;
end;

function IntToColorString(Color: Integer): string;
begin
  if IntToIdent(Color, Result, WebColors) or
    IntToIdent(Color, Result, ColorToPretyName) then
    exit
  else
    Result := ColorToString(Color);
end;

function TEnhancedIniFile.ReadColor(const Section, Ident: string; Default: TColor): TColor;
var
  Value: string;
  ColorStr: string;
  R, G, B: Integer;
  H, S, L: Double;
begin
  Value := Trim(ReadString(Section, Ident, ''));

  if Value = '' then
  begin
    Result := Default;
    Exit;
  end;

  if IdentToInt(Value, Longint(Result), WebColors)
    or IdentToInt(Value, Longint(Result), ColorToPretyName)
    or IdentToColor('cl' + Value, Longint(Result))
    or IdentToColor(Value, Longint(Result)) then
    exit;

  if (Pos('#', Value) = 1)
    or (Pos('$', Value) = 1)
    or (Pos('0x', LowerCase(Value)) = 1) then
  begin
    if Pos('#', Value) = 1 then
      ColorStr := '$' + Copy(Value, 2, Length(Value))
    else if Pos('0x', LowerCase(Value)) = 1 then
      ColorStr := '$' + Copy(Value, 3, Length(Value))
    else
      ColorStr := Value;
    try
      Result := StringToColor(ColorStr);
      Exit;
    except

    end;
  end;

  if Pos('rgb(', LowerCase(Value)) = 1 then
  begin
    Value := Copy(Value, 5, Length(Value) - 5);
    Value := StringReplace(Value, ' ', '', [rfReplaceAll]);

    try
      R := StrToInt(ExtractDelimited(1, Value, ','));
      G := StrToInt(ExtractDelimited(2, Value, ','));
      B := StrToInt(ExtractDelimited(3, Value, ','));
      Result := RGB(R, G, B);
      Exit;
    except
    end;
  end;

  if ParseHSLValue(Value, H, S, L) then
  begin
    Result := HSLToRGB(H, S, L);
    Exit;
  end;

  try
    Result := StringToColor(Value);
  except
    Result := Default;
  end;
end;

end.
