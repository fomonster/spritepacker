unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRAVirtualScreen, bgrabitmap, bgrabitmaptypes, bgrawebp;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Button1: TButton;
    Button2: TButton;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    bgrab: TBGRABitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(Application.Location + 'powered_by.png');
  bmp.SaveToWebPFile(Application.Location + 'file.webp', 100);
  bmp.Free;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.Fill(BGRAWhite);
  Bitmap.PutImage(0, 0, bgrab, dmDrawWithTransparency);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  bgrab.LoadFromWebPFile(Application.Location + 'test.webp');
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bgrab := TBGRABitmap.Create(0,0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bgrab.Free;
end;

end.

