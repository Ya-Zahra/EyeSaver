unit MonitorLabels;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TLabels = class(TComponent)
  private
    FFontName: string;
    FFontColor: TColor;
    FVisible: Boolean;
    FCaption: TCaption;
    FForm: TForm;
    FLabels: TList;
    procedure CreateLabels;
    procedure ClearLabels;
    procedure SetFontName(const Value: string);
    procedure SetCaption(const Value: TCaption);
    procedure SetFontColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(AForm: TForm); reintroduce;
    destructor Destroy; override;
    procedure UpdateLabels;
    property FontName: string read FFontName write SetFontName;
    property FontColor: TColor read FFontColor write SetFontColor;
    property Visible: Boolean read FVisible write SetVisible;
    property Caption: TCaption read FCaption write SetCaption;

  end;

implementation

constructor TLabels.Create(AForm: TForm);
begin
  inherited Create(AForm);
  FFontName := 'Consolas';
  FFontColor := clWhite;
  FVisible := true;
  FCaption := '';
  FForm := AForm;
  FLabels := TList.Create;
  CreateLabels;
end;

destructor TLabels.Destroy;
begin
  FLabels.Free;
  inherited Destroy;
end;

procedure TLabels.CreateLabels;
var
  i: Integer;
  NewLabel: TLabel;
begin
  ClearLabels;
  with FForm.Monitor do //this will update monitors
    ;
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    NewLabel := TLabel.Create(FForm);
    NewLabel.Parent := FForm;
    NewLabel.Alignment := taCenter;
    NewLabel.Layout := tlCenter;
    NewLabel.AutoSize := False;
    NewLabel.Width := Screen.Monitors[i].Width;
    NewLabel.Height := Screen.Monitors[i].Height;
    NewLabel.Top := Screen.Monitors[i].Top;
    NewLabel.Left := Screen.Monitors[i].Left;
    NewLabel.Transparent := True;
    NewLabel.Font.Name := FFontName;
    NewLabel.Font.Color := FFontColor;
    NewLabel.Visible := FVisible;
    NewLabel.Caption := FCaption;
    NewLabel.Hint := IntToStr(i);
    NewLabel.Font.Height := -96;
    FLabels.Add(NewLabel);
  end;
end;

procedure TLabels.ClearLabels;
var
  i: Integer;
begin
  for i := 0 to FLabels.Count - 1 do
    TLabel(FLabels[i]).Free;
  FLabels.Clear;
end;

procedure TLabels.UpdateLabels;
begin
  CreateLabels;
end;

procedure TLabels.SetFontName(const Value: string);
var
  i: Integer;
begin
  if Value <> FFontName then
  begin
    for i := 0 to FLabels.Count - 1 do
      TLabel(FLabels[i]).Font.Name := Value;
    FFontName := Value;
  end;
end;

procedure TLabels.SetCaption(const Value: TCaption);
var
  i: Integer;
begin
  if Value <> FCaption then
  begin
    for i := 0 to FLabels.Count - 1 do
    begin
      with TLabel(FLabels[i]) do
        Caption := Value;
    end;
    FCaption := Value;
  end;
end;

procedure TLabels.SetFontColor(const Value: TColor);
var
  i: Integer;
begin
  if Value <> FFontColor then
  begin
    for i := 0 to FLabels.Count - 1 do
      TLabel(FLabels[i]).Font.Color := Value;
    FFontColor := Value;
  end;
end;

procedure TLabels.SetVisible(const Value: Boolean);
var
  i: Integer;
begin
  if Value <> FVisible then
  begin
    for i := 0 to FLabels.Count - 1 do
      TLabel(FLabels[i]).Visible := Value;
    FVisible := Value;
  end;
end;

end.
