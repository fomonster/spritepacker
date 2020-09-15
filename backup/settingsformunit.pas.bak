unit SettingsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, EditBtn, ComboEx, ButtonPanel, Spin, FPimage, FileCtrl, types;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Label16: TLabel;
    NamesWithPathComboBox1: TComboBox;
    Label15: TLabel;
    MaxWidthComboBox: TComboBox;
    NamesWithExtComboBox1: TComboBox;
    SpritesMarginComboBox: TComboBox;
    MaxHeightComboBox: TComboBox;
    ClampWidthAndHeightComboBox: TComboBox;
    SpritesOptimizationComboBox: TComboBox;
    PreSortingComboBox: TComboBox;
    SortingMethodComboBox: TComboBox;
    SpritesRotationComboBox: TComboBox;
    PostProcessComboBox: TComboBox;
    OutputImageComboBox: TComboBox;
    OutputDataComboBox: TComboBoxEx;
    OutputDataFileNameEdit: TFileNameEdit;
    OutputImageFileNameEdit: TFileNameEdit;
    ImageList1: TImageList;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NewButton: TSpeedButton;
    PageControl1: TPageControl;
    RemoveButton: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToolBar1: TToolBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure MaxWidthComboBoxEnter(Sender: TObject);
    procedure MaxWidthComboBoxKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SettingsForm: TSettingsForm;

implementation

   Uses mainformunit, constantsunit, logformUnit, intfgraphics;
{$R *.lfm}

{ TSettingsForm }

procedure TSettingsForm.NewButtonClick(Sender: TObject);
begin
  // page 1
  MainForm.spritePacker.maxWidth := StrToIntDef(MaxWidthComboBox.Text,2048);
  MainForm.spritePacker.maxHeight := StrToIntDef(MaxHeightComboBox.Text,2048);
  MainForm.spritePacker.clampWidthAndHeight:=ClampWidthAndHeightComboBox.ItemIndex;
  MainForm.spritePacker.spritesOptimization:=SpritesOptimizationComboBox.ItemIndex;
  MainForm.spritePacker.spritesRotation:=SpritesRotationComboBox.ItemIndex;
  MainForm.spritePacker.spritesMargin:=SpritesMarginComboBox.ItemIndex;
  MainForm.spritePacker.preSorting:=PreSortingComboBox.ItemIndex;
  MainForm.spritePacker.sortingMethod:=SortingMethodComboBox.ItemIndex;
  MainForm.spritePacker.postProcess:=PostProcessComboBox.ItemIndex;
  MainForm.spritePacker.namesWithPath:=NamesWithPathComboBox1.ItemIndex;
  MainForm.spritePacker.namesWithExt:=NamesWithExtComboBox1.ItemIndex;

  // page 2
  MainForm.spritePacker.outputDataFileName:=ExtractFileNameWithoutExt(OutputDataFileNameEdit.Text);
  if ( OutputDataComboBox.ItemIndex >= 0 ) and ( OutputDataComboBox.ItemIndex < OutputDataComboBox.ItemsEx.Count ) then begin
    MainForm.spritePacker.outputDataFileFormat:=OutputDataComboBox.ItemsEx.Items[OutputDataComboBox.ItemIndex].Caption;
  end else begin
    MainForm.spritePacker.outputDataFileFormat:='';
  end;

  // page 3
  MainForm.spritePacker.outputImageFileName:=ExtractFileNameWithoutExt(OutputImageFileNameEdit.Text);
  MainForm.spritePacker.outputImageFileFormat:=OutputImageComboBox.Text;

  //
  MainForm.spritePacker.IsChanged:=true;
  IsDocSaved:=false;
  Close;
end;

procedure TSettingsForm.Label5Click(Sender: TObject);
begin

end;

procedure TSettingsForm.ListBox1Click(Sender: TObject);
begin

end;

procedure TSettingsForm.CheckBox1Change(Sender: TObject);
begin

end;

procedure TSettingsForm.MaxWidthComboBoxEnter(Sender: TObject);
begin

end;

procedure TSettingsForm.MaxWidthComboBoxKeyPress(Sender: TObject; var Key: char);
begin
  if (( Key >= '0' ) and ( Key <= '9' )) or ( Key = #8 ) then begin

  end else begin
    Key:=#0;
  end;
end;

function LoadImageIntoList(aFileName:String; aList:TImageList;out aImageIndex:Integer):Boolean;
var
  mImage:   TPicture;
  mBitmap:  TBitmap;
Begin
  result:=False;
  aImageIndex:=-1;
  if aList<>NIl then
  begin
    if FileExists(aFilename) then
    begin
      mImage:=TPicture.Create;
      try

        (* Attempt to load image file *)
        try
          mImage.LoadFromFile(aFilename);
        except
          on exception do;
        end;

        (* image successfully loaded? *)
        if (mImage.Width>0)
        and (mImage.Height>0) then
        begin
          (* Draw image to a bitmap *)
          mBitmap:=TBitmap.Create;
          try
            (* copy pixels + transparency info to bitmap *)
            mBitmap.Assign(mImage.Graphic);

            (* Add bitmap to image-list, return index *)
            aImageIndex:=aList.add(mBitmap,NIL);
            result:=true;
          finally
            mBitmap.Free;
          end;
        end;
      finally
        mImage.Free;
      end;
    end;
  end;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
  var
    Info:TSearchRec;
    searchPath:AnsiString;
    imageFileName:AnsiString;
    dataFormatName:AnsiString;
    itemIndex:integer;
    i:integer;
    imageIndex:integer;

begin
  MaxWidthComboBox.Text:=inttostr(MainForm.spritePacker.maxWidth);
  MaxHeightComboBox.Text:=inttostr(MainForm.spritePacker.maxHeight);
  ClampWidthAndHeightComboBox.ItemIndex:=MainForm.spritePacker.clampWidthAndHeight;
  SpritesOptimizationComboBox.ItemIndex:=MainForm.spritePacker.spritesOptimization;
  SpritesRotationComboBox.ItemIndex:=MainForm.spritePacker.spritesRotation;
  SpritesMarginComboBox.ItemIndex:=MainForm.spritePacker.spritesMargin;
  PreSortingComboBox.ItemIndex:=MainForm.spritePacker.preSorting;
  SortingMethodComboBox.ItemIndex:=MainForm.spritePacker.sortingMethod;
  PostProcessComboBox.ItemIndex:=MainForm.spritePacker.postProcess;
  NamesWithPathComboBox1.ItemIndex:=MainForm.spritePacker.namesWithPath;
  NamesWithExtComboBox1.ItemIndex:=MainForm.spritePacker.namesWithExt;

  OutputImageFileNameEdit.Text := MainForm.spritePacker.outputImageFileName;
  OutputImageComboBox.Text := MainForm.spritePacker.outputImageFileFormat;

  try

    // Обновление скриптов экспорта
    ImageList1.Clear;
    OutputDataComboBox.Clear;
    searchPath := ExtractFilePath(paramstr(0))+'export\';
    itemIndex:=-1;
    i:=0;
    If FindFirst( searchPath + '*.psc', faAnyFile, Info) = 0 then begin
      Repeat
        if ( Info.Name <> '.' ) and ( Info.Name <> '..' ) then begin

           if ( (Info.Attr and faDirectory ) = 0) then begin

             imageIndex:=-1;

             // icon
             imageFileName := searchPath+ExtractFileNameWithoutExt(Info.Name)+'.png';
             if ( FileExistsUTF8( imageFileName ) ) then begin
                 if not LoadImageIntoList(imageFileName, ImageList1, imageIndex) then imageIndex:=-1;
             end;

             dataFormatName:=ExtractFileNameWithoutExt(Info.Name);
             if ( MainForm.spritePacker.outputDataFileFormat = dataFormatName ) then begin
               itemIndex:=i;
             end;
             OutputDataComboBox.ItemsEx.AddItem(dataFormatName,imageIndex,-1,imageIndex,-1,nil);
             i:=i+1;
           end;

        end;

      Until FindNext(Info)<>0;
    end;
    FindClose(Info);
  except
    on e:Exception do begin
      AddLog(e.Message);
      ShowLog;
    end;
  end;
  OutputDataFileNameEdit.Text := MainForm.spritePacker.outputDataFileName;
  OutputDataComboBox.ItemIndex:=itemIndex;

end;

procedure TSettingsForm.PageControl1Change(Sender: TObject);
begin

end;

procedure TSettingsForm.RemoveButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSettingsForm.SpinEdit1Change(Sender: TObject);
begin
  //

end;

procedure TSettingsForm.SpinEdit2Change(Sender: TObject);
begin
  //
end;

procedure TSettingsForm.TabSheet1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

end.

