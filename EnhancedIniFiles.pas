unit EnhancedIniFiles;

interface

uses
  IniFiles, Graphics, SysUtils, Classes, Math, Windows, ExtCtrls;

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
  WebColors: array[0..139] of TIdentMapEntry = (
    (Value: $F0F8FF; Name: 'AliceBlue'),
    (Value: $FAEBD7; Name: 'AntiqueWhite'),
    (Value: $00FFFF; Name: 'Aqua'),
    (Value: $7FFFD4; Name: 'Aquamarine'),
    (Value: $F0FFFF; Name: 'Azure'),
    (Value: $F5F5DC; Name: 'Beige'),
    (Value: $FFE4C4; Name: 'Bisque'),
    (Value: $000000; Name: 'Black'),
    (Value: $FFEBCD; Name: 'BlanchedAlmond'),
    (Value: $0000FF; Name: 'Blue'),
    (Value: $8A2BE2; Name: 'BlueViolet'),
    (Value: $A52A2A; Name: 'Brown'),
    (Value: $DEB887; Name: 'BurlyWood'),
    (Value: $5F9EA0; Name: 'CadetBlue'),
    (Value: $7FFF00; Name: 'Chartreuse'),
    (Value: $D2691E; Name: 'Chocolate'),
    (Value: $FF7F50; Name: 'Coral'),
    (Value: $6495ED; Name: 'CornflowerBlue'),
    (Value: $FFF8DC; Name: 'Cornsilk'),
    (Value: $DC143C; Name: 'Crimson'),
    (Value: $00FFFF; Name: 'Cyan'),
    (Value: $00008B; Name: 'DarkBlue'),
    (Value: $008B8B; Name: 'DarkCyan'),
    (Value: $B8860B; Name: 'DarkGoldenRod'),
    (Value: $A9A9A9; Name: 'DarkGray'),
    (Value: $006400; Name: 'DarkGreen'),
    (Value: $BDB76B; Name: 'DarkKhaki'),
    (Value: $8B008B; Name: 'DarkMagenta'),
    (Value: $556B2F; Name: 'DarkOliveGreen'),
    (Value: $FF8C00; Name: 'DarkOrange'),
    (Value: $9932CC; Name: 'DarkOrchid'),
    (Value: $8B0000; Name: 'DarkRed'),
    (Value: $E9967A; Name: 'DarkSalmon'),
    (Value: $8FBC8F; Name: 'DarkSeaGreen'),
    (Value: $483D8B; Name: 'DarkSlateBlue'),
    (Value: $2F4F4F; Name: 'DarkSlateGray'),
    (Value: $00CED1; Name: 'DarkTurquoise'),
    (Value: $9400D3; Name: 'DarkViolet'),
    (Value: $FF1493; Name: 'DeepPink'),
    (Value: $00BFFF; Name: 'DeepSkyBlue'),
    (Value: $696969; Name: 'DimGray'),
    (Value: $1E90FF; Name: 'DodgerBlue'),
    (Value: $B22222; Name: 'FireBrick'),
    (Value: $FFFAF0; Name: 'FloralWhite'),
    (Value: $228B22; Name: 'ForestGreen'),
    (Value: $FF00FF; Name: 'Fuchsia'),
    (Value: $DCDCDC; Name: 'Gainsboro'),
    (Value: $F8F8FF; Name: 'GhostWhite'),
    (Value: $FFD700; Name: 'Gold'),
    (Value: $DAA520; Name: 'GoldenRod'),
    (Value: $808080; Name: 'Gray'),
    (Value: $008000; Name: 'Green'),
    (Value: $ADFF2F; Name: 'GreenYellow'),
    (Value: $F0FFF0; Name: 'HoneyDew'),
    (Value: $FF69B4; Name: 'HotPink'),
    (Value: $CD5C5C; Name: 'IndianRed'),
    (Value: $4B0082; Name: 'Indigo'),
    (Value: $FFFFF0; Name: 'Ivory'),
    (Value: $F0E68C; Name: 'Khaki'),
    (Value: $E6E6FA; Name: 'Lavender'),
    (Value: $FFF0F5; Name: 'LavenderBlush'),
    (Value: $7CFC00; Name: 'LawnGreen'),
    (Value: $FFFACD; Name: 'LemonChiffon'),
    (Value: $ADD8E6; Name: 'LightBlue'),
    (Value: $F08080; Name: 'LightCoral'),
    (Value: $E0FFFF; Name: 'LightCyan'),
    (Value: $FAFAD2; Name: 'LightGoldenRodYellow'),
    (Value: $D3D3D3; Name: 'LightGray'),
    (Value: $90EE90; Name: 'LightGreen'),
    (Value: $FFB6C1; Name: 'LightPink'),
    (Value: $FFA07A; Name: 'LightSalmon'),
    (Value: $20B2AA; Name: 'LightSeaGreen'),
    (Value: $87CEFA; Name: 'LightSkyBlue'),
    (Value: $778899; Name: 'LightSlateGray'),
    (Value: $B0C4DE; Name: 'LightSteelBlue'),
    (Value: $FFFFE0; Name: 'LightYellow'),
    (Value: $00FF00; Name: 'Lime'),
    (Value: $32CD32; Name: 'LimeGreen'),
    (Value: $FAF0E6; Name: 'Linen'),
    (Value: $FF00FF; Name: 'Magenta'),
    (Value: $800000; Name: 'Maroon'),
    (Value: $66CDAA; Name: 'MediumAquaMarine'),
    (Value: $0000CD; Name: 'MediumBlue'),
    (Value: $BA55D3; Name: 'MediumOrchid'),
    (Value: $9370DB; Name: 'MediumPurple'),
    (Value: $3CB371; Name: 'MediumSeaGreen'),
    (Value: $7B68EE; Name: 'MediumSlateBlue'),
    (Value: $00FA9A; Name: 'MediumSpringGreen'),
    (Value: $48D1CC; Name: 'MediumTurquoise'),
    (Value: $C71585; Name: 'MediumVioletRed'),
    (Value: $191970; Name: 'MidnightBlue'),
    (Value: $F5FFFA; Name: 'MintCream'),
    (Value: $FFE4E1; Name: 'MistyRose'),
    (Value: $FFE4B5; Name: 'Moccasin'),
    (Value: $FFDEAD; Name: 'NavajoWhite'),
    (Value: $000080; Name: 'Navy'),
    (Value: $FDF5E6; Name: 'OldLace'),
    (Value: $808000; Name: 'Olive'),
    (Value: $6B8E23; Name: 'OliveDrab'),
    (Value: $FFA500; Name: 'Orange'),
    (Value: $FF4500; Name: 'OrangeRed'),
    (Value: $DA70D6; Name: 'Orchid'),
    (Value: $EEE8AA; Name: 'PaleGoldenRod'),
    (Value: $98FB98; Name: 'PaleGreen'),
    (Value: $AFEEEE; Name: 'PaleTurquoise'),
    (Value: $DB7093; Name: 'PaleVioletRed'),
    (Value: $FFEFD5; Name: 'PapayaWhip'),
    (Value: $FFDAB9; Name: 'PeachPuff'),
    (Value: $CD853F; Name: 'Peru'),
    (Value: $FFC0CB; Name: 'Pink'),
    (Value: $DDA0DD; Name: 'Plum'),
    (Value: $B0E0E6; Name: 'PowderBlue'),
    (Value: $800080; Name: 'Purple'),
    (Value: $FF0000; Name: 'Red'),
    (Value: $BC8F8F; Name: 'RosyBrown'),
    (Value: $4169E1; Name: 'RoyalBlue'),
    (Value: $8B4513; Name: 'SaddleBrown'),
    (Value: $FA8072; Name: 'Salmon'),
    (Value: $F4A460; Name: 'SandyBrown'),
    (Value: $2E8B57; Name: 'SeaGreen'),
    (Value: $FFF5EE; Name: 'SeaShell'),
    (Value: $A0522D; Name: 'Sienna'),
    (Value: $C0C0C0; Name: 'Silver'),
    (Value: $87CEEB; Name: 'SkyBlue'),
    (Value: $6A5ACD; Name: 'SlateBlue'),
    (Value: $708090; Name: 'SlateGray'),
    (Value: $FFFAFA; Name: 'Snow'),
    (Value: $00FF7F; Name: 'SpringGreen'),
    (Value: $4682B4; Name: 'SteelBlue'),
    (Value: $D2B48C; Name: 'Tan'),
    (Value: $008080; Name: 'Teal'),
    (Value: $D8BFD8; Name: 'Thistle'),
    (Value: $FF6347; Name: 'Tomato'),
    (Value: $40E0D0; Name: 'Turquoise'),
    (Value: $EE82EE; Name: 'Violet'),
    (Value: $F5DEB3; Name: 'Wheat'),
    (Value: $FFFFFF; Name: 'White'),
    (Value: $F5F5F5; Name: 'WhiteSmoke'),
    (Value: $FFFF00; Name: 'Yellow'),
    (Value: $9ACD32; Name: 'YellowGreen')
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
    Exit;

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
