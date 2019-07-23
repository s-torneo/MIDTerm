//#load "Matrix.fsx"
//#load "Controls.fsx"
open System.Windows.Forms
open System.Drawing
open System.ComponentModel

//open Matrix
//open Controls

let PI = System.Math.PI
let deg2rad (a:single) =
  PI * float(a / 180.f) |> single
// Libreria

type WVMatrix () =
  let wv = new Drawing2D.Matrix()
  let vw = new Drawing2D.Matrix()

  member this.TranslateW (tx, ty) =
    wv.Translate(tx, ty)
    vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleW (sx, sy) =
    wv.Scale(sx, sy)
    vw.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

  member this.RotateW (a) =
    wv.Rotate(a)
    vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.RotateV (a) =
    vw.Rotate(a)
    wv.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.TranslateV (tx, ty) =
    vw.Translate(tx, ty)
    wv.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleV (sx, sy) =
    vw.Scale(sx, sy)
    wv.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)
  
  member this.TransformPointV (p:PointF) =
    let a = [| p |]
    vw.TransformPoints(a)
    a.[0]

  member this.TransformPointW (p:PointF) =
    let a = [| p |]
    wv.TransformPoints(a)
    a.[0]

  member this.VW with get() = vw
  member this.WV with get() = wv

type LWCControl() =
  let wv = WVMatrix()

  let mutable sz = SizeF(120.f, 120.f)
  
  let mutable pos = PointF()

  let mutable parent : LWCContainer option = None

  let mutable Color = Brushes.Red

  let mutable Selected = false

  let mutable Image = null

  let mutable manigliaS = PointF()

  let mutable manigliaA = PointF()

  let mutable cclass = null

  let mutable name = null

  member this.WV = wv

  member this.Parent
    with get() = parent
    and set(v) = parent <- v

  abstract OnPaint : PaintEventArgs -> unit
  default this.OnPaint (e) = ()

  abstract OnMouseDown : MouseEventArgs -> unit
  default this.OnMouseDown (e) = ()

  abstract OnMouseUp : MouseEventArgs -> unit
  default this.OnMouseUp (e) = ()

  abstract OnMouseMove : MouseEventArgs -> unit
  default this.OnMouseMove (e) = ()

  member this.Invalidate() =
    match parent with
    | Some p -> p.Invalidate()
    | None -> ()

  member this.HitTest(p:Point) =
    let pt = wv.TransformPointV(PointF(single p.X, single p.Y))
    let boundingbox = RectangleF(0.f, 0.f, sz.Width, sz.Height)
    boundingbox.Contains(pt)

  member this.ClientSize
    with get() = sz
    and set(v) = 
      sz <- v
      this.Invalidate()

  member this.Position
    with get() = pos
    and set(v) =
      wv.TranslateV(pos.X, pos.Y)
      pos <- v
      wv.TranslateV(-pos.X, -pos.Y)
      this.Invalidate()
   
  member this.ManigliaS
    with get() = manigliaS
    and set(v) =
      manigliaS <- v
      this.Invalidate()
  
  member this.ManigliaA
    with get() = manigliaA
    and set(v) =
      manigliaA <- v
      this.Invalidate()

  member this.Class
    with get() = cclass 
    and set(v) = 
      cclass <- v
      this.Invalidate()
  
  member this.Name
    with get() = name
    and set(v) = 
      name <- v
      this.Invalidate()

  member this.PositionInt with get() = Point(int pos.X, int pos.Y)

  member this.ClientSizeInt with get() = Size(int sz.Width, int sz.Height)

  member this.color 
    with get() = Color
    and set(v) = 
      Color <- v
      this.Invalidate()

  member this.selected 
    with get() = Selected
    and set(v) = 
      Selected <- v
      this.Invalidate()
  
  member this.image
    with get() = Image
    and set(v:Bitmap) = 
      Image <- v

  member this.Left = pos.X
  
  member this.Top = pos.Y

  member this.Width = sz.Width
  member this.Height = sz.Height

and LWCContainer() as this =
  inherit UserControl()

  let controls = System.Collections.ObjectModel.ObservableCollection<LWCControl>()

  let mutable newbox = None
  let mutable drag = None
  let mutable resize = None
  let mutable selectionButton = false
  let mutable selectionMode = false
  let mutable removeMode = false
  let mutable imageMode = false
  let mutable resizeMode = false
  let mutable myPicture : Bitmap = new Bitmap(@"C:\Users\step9\OneDrive\Desktop\Documenti\PDF\UNIPI\Dispense\Terzo anno\Primo Semestre\PI\PreparazioneMID\earth.jpg")
  let timer = new Timer(Interval=100)
  let mkrect (sx, sy) (ex, ey) =
    Rectangle(min sx ex, min sy ey, abs(sx - ex), abs(sy - ey))

  let bimage = new Button(Text="Image", Location=Point(10, 220))
  let mutable new_planet = false

  do 
    this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
    controls.CollectionChanged.Add(fun e ->
      for i in e.NewItems do
        (i :?> LWCControl).Parent <- Some(this)
      )
    let bmpPicture : Bitmap = new Bitmap(@"C:\Users\step9\OneDrive\Desktop\Documenti\PDF\UNIPI\Dispense\Terzo anno\Primo Semestre\PI\PreparazioneMID\ship.png")
    controls.Add(LWButton(Position=PointF(100.f, 100.f),ClientSize=SizeF(30.f, 30.f),color=(Brushes.Red),selected=false,Class="ship",image=bmpPicture))
    controls.Add(LWButton(Name="New Planet",Position=PointF(10.f, 10.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="Up",Position=PointF(10.f, 40.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="Down",Position=PointF(10.f, 70.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="Left",Position=PointF(10.f, 100.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="Right",Position=PointF(10.f, 130.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="RotateR",Position=PointF(10.f, 160.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="RotateL",Position=PointF(10.f, 190.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="ZoomIn",Position=PointF(10.f, 220.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="ZoomOut",Position=PointF(10.f, 250.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="Dimension+",Position=PointF(10.f, 280.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
    controls.Add(LWButton(Name="Dimension-",Position=PointF(10.f, 310.f),ClientSize=SizeF(55.f,20.f),color=Brushes.Yellow))
   
  member this.LWControls with get() = controls

  member this.NewPlanet() =
    imageMode <- true
    //resizeMode <- true
  member this.Up() =
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "ship" && c.Name = null) then
          c.WV.TranslateV(0.f, 10.f)
        this.Invalidate()
        )

  member this.Down() = 
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "ship" && c.Name = null) then
          c.WV.TranslateV(0.f, -10.f)
        this.Invalidate()
        )
  
  member this.Left() = 
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "ship" && c.Name = null) then
          c.WV.TranslateV(10.f, 0.f)
        this.Invalidate()
        )
  member this.Right() = 
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "ship" && c.Name = null) then
          c.WV.TranslateV(-10.f, 0.f)
        this.Invalidate()
        )
  member this.RotateR() = 
    let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
    controls 
        |> Seq.iter(fun c ->
        if(c.Name = null) then
          c.WV.TranslateV(cx, cy)
          c.WV.RotateV(10.f)
          c.WV.TranslateV(-cx, -cy)
        this.Invalidate()
        )
  member this.RotateL() = 
    let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
    controls 
        |> Seq.iter(fun c ->
        if(c.Name = null) then
          c.WV.TranslateV(cx, cy)
          c.WV.RotateV(-10.f)
          c.WV.TranslateV(-cx, -cy)
        this.Invalidate()
        )
  member this.ZoomOut() = 
      let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
      controls 
        |> Seq.iter(fun c ->
        if(c.Name = null) then
          //let cx, cy = c.Position.X + c.Width/2.f |> single, c.Position.Y + c.Height/2.f |> single
          c.WV.TranslateV(cx, cy)
          c.WV.ScaleV(1.1f, 1.1f)
          c.WV.TranslateV(-cx, -cy)
        )
      this.Invalidate()
  member this.ZoomIn() = 
      let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
      controls 
        |> Seq.iter(fun c ->
        if(c.Name = null) then
          //let cx, cy = c.Position.X + c.Width/2.f |> single, c.Position.Y + c.Height/2.f |> single
          c.WV.TranslateV(cx, cy)
          c.WV.ScaleV(1.f/1.1f, 1.f/1.1f)
          c.WV.TranslateV(-cx, -cy)
        )
      this.Invalidate()

  member this.IncrDim() = 
      controls 
        |> Seq.iter(fun c ->
        if(c.Name = null && c.selected) then
          c.ClientSize <- SizeF(c.Width + 10.f, c.Height + 10.f)
        )
      this.Invalidate()
  member this.DecrDim() = 
      controls 
        |> Seq.iter(fun c ->
        if(c.Name = null && c.selected) then
          c.ClientSize <- SizeF(c.Width - 10.f, c.Height - 10.f)
        )
      this.Invalidate()
  override this.OnMouseDown (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      controls.Move(controls.IndexOf(c), controls.Count-1) // in modo che prende sempre quello di sopra
      if (c.selected) then
        c.selected <- false
      else
        c.selected <- true
      //let pt = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      //let boundingbox = RectangleF(0.f, c.Height/2.f, 10.f, 10.f)
      //if(boundingbox.Contains(pt)) then
      if (resizeMode) then
        resize <- Some(c, e.X, e.Y, "s")
      //let boundingbox = RectangleF(c.Width/2.f, 0.f, 10.f, 10.f)
      //if(boundingbox.Contains(pt)) then
        //resize <- Some(c, e.X, e.Y, "a")
      let dx, dy = e.X - int(c.Position.X), e.Y - int(c.Position.Y)
      if (selectionMode = false && c.Name = null) then
        drag <- Some (c, dx, dy)
      else if (c.Name = null) then
        drag <- Some (c, e.X, e.Y)
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseDown(evt)
      this.Invalidate()
    | None -> 
        if (imageMode) then
          let dlg = new OpenFileDialog()
          dlg.Filter <- "|*.BMP;*.JPG;*.GIF;*.PNG"
          if dlg.ShowDialog() = DialogResult.OK then
            let imagename = dlg.FileName
            myPicture <- new Bitmap(imagename)
          newbox <- Some (e.X, e.Y)

  override this.OnMouseUp (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseUp(evt)
      this.Invalidate()
    | None -> 
      controls 
          |> Seq.iter(fun c ->
            if(c.Name = null) then
              c.color <- Brushes.Red  
              c.selected <- false
          )
      resizeMode <- false
      selectionMode <- false
    match newbox with
     | Some(ex, ey) ->
       (*if (selectionButton = true) then
          controls 
          |> Seq.iter(fun c ->
            let w = single(abs(sx -  e.X))
            let h = single(abs(sy - e.Y))
            //let r = mkrect (sx, sy) (int(e.X), int(e.Y))
            //let pt = c.WV.TransformPointV(PointF(single c.Position.X, single c.Position.Y))
            let boundingbox = RectangleF(single sx, single sy, w, h)
            let rect = RectangleF(c.Position.X, c.Position.Y, c.Width, c.Height)
            //printfn "px: %A y: %A bx: %A by: %A w: %A h: %A" pt.X pt.Y boundingbox.X boundingbox.Y w h
            if(boundingbox.Contains(rect)) then
            //printfn "px: %A py: %A sx: %A sy: %A w: %A h: %A" c.Position.X c.Position.Y sx sy w h
            //if ((c.Position.X > single(sx) && c.Position.X < (single(sx) + w) && c.Position.Y > single(sy) && c.Position.Y < (single(sy) + h)) then
              c.color <- Brushes.Yellow
              c.selected <- true
              selectionMode <- true
          ) 
       else *)
       controls.Add(LWButton(image=myPicture,(*ManigliaA=PointF(single(abs( sx - e.X))/2.f, 0.f),ManigliaS=PointF(0.f, single(abs( sy - e.Y))/2.f),*)Position=PointF(single ex, single ey),ClientSize=SizeF(100.f, 100.f),selected=false))
       newbox <- None
       imageMode <- false
       selectionButton <- false
       this.Invalidate()
     | None -> 
       drag <- None
       resize <- None

  override this.OnMouseMove (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseMove(evt)
      this.Invalidate()
    | None -> () 
    match drag with
    | Some(c, dx, dy) ->
      if (selectionMode = false ) then
         c.Position <- PointF(single(e.X - int(dx)),single( e.Y - int(dy)))
      else
        controls 
          |> Seq.iter(fun c ->
          if(c.selected) then
            c.Position <- PointF(c.Position.X + single e.X - single dx, c.Position.Y + single e.Y - single dy)
        )
        drag <- Some(c,e.X, e.Y)
      this.Invalidate()
    | _ -> ()
    match resize with
    | Some(c, dx, dy, s) ->
      if (s = "s") then
        c.ClientSize <- SizeF(c.Width - (single e.X - single dx), c.Height)
      else
        c.ClientSize <- SizeF(c.Width, c.Height - (single e.Y - single dy))
      resize <- Some (c, e.X, e.Y, s)
      this.Invalidate()
    | _ -> ()

  override this.OnPaint(e) =
    controls 
    |> Seq.iter(fun c ->
      let bkg = e.Graphics.Save()
      // esercizio: impostare il rettangolo in client space
      let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.ClientSizeInt))
      //bug: non supporta la rotazione
      //e.Graphics.SetClip(new RectangleF(c.Position, c.ClientSize))
      e.Graphics.Transform <- c.WV.WV // cambiamo la matrice di trasformazione
      c.OnPaint(evt)
      e.Graphics.Restore(bkg)
    )

  override this.OnKeyDown e =
    let mutable ship = LWCControl()
    match e.KeyCode with
    | Keys.W -> 
      controls 
      |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.TranslateW(0.f, -10.f)
          ship <- c
      )
      controls.Move(controls.IndexOf(ship), controls.Count-1) 
    | Keys.A -> 
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.TranslateW(-10.f, 0.f)
          ship <- c
      )
      controls.Move(controls.IndexOf(ship), controls.Count-1) 
    | Keys.S -> 
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.TranslateW(0.f, 10.f)
          ship <- c
        )
      controls.Move(controls.IndexOf(ship), controls.Count-1) 
    | Keys.D ->
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.TranslateW(10.f, 0.f)
          ship <- c
        )
      controls.Move(controls.IndexOf(ship), controls.Count-1) 
    | Keys.Q ->
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          let cx, cy = c.Width/2.f |> single, c.Height/2.f |> single
          c.WV.TranslateW(cx, cy)
          c.WV.RotateW(10.f)
          c.WV.TranslateW(-cx, -cy)
        )
    | Keys.E ->
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          let cx, cy = c.Width/2.f |> single, c.Height/2.f |> single
          c.WV.TranslateW(cx, cy)
          c.WV.RotateW(-10.f)
          c.WV.TranslateW(-cx, -cy)
        )
    | Keys.Z ->
      let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.ScaleW(1.1f, 1.1f)
      )
    | Keys.X ->
      let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.ScaleW(1.f/1.1f, 1.f/1.1f)
      )
    | _ -> ()
    this.Invalidate()

// Utente Libreria
and LWButton() as this =
  inherit LWCControl()

  override this.OnPaint(e) =
    let g = e.Graphics
    if (this.image <> null) then
      //g.FillRectangle(Brushes.Transparent, 0.f, 0.f, this.Width, this.Height)
      //g.FillRectangle(Brushes.Black, 0.f, 0.f, 10.f, 10.f)
      //g.FillRectangle(Brushes.Black, this.Width/2.f, 0.f, 10.f, 10.f)
      g.DrawImage(this.image, RectangleF(0.f, 0.f, this.Height,this.Width))
    else if (this.Width <> 0.f) then
      g.FillRectangle(this.color, 0.f, 0.f, this.Width, this.Height)
      //g.FillRectangle(Brushes.Black, 0.f, this.Height/2.f, 10.f, 10.f)
      //g.FillRectangle(Brushes.Black, this.Width/2.f, 0.f, 10.f, 10.f)
      g.DrawString(this.Name, new Font("Calibri", 8.f), Brushes.Black, 0.f, 0.f)
  
  override this.OnMouseDown(e) =
    let p = this.Parent
    match this.Parent with
     | Some p -> 
        match this.Name with
        | "New Planet" -> p.NewPlanet()
        | "Up" -> p.Up() 
        | "Down" -> p.Down()
        | "Left" -> p.Left()
        | "Right" -> p.Right()
        | "RotateR" -> p.RotateR()
        | "RotateL" -> p.RotateL()
        | "ZoomIn" -> p.ZoomIn()
        | "ZoomOut" -> p.ZoomOut()
        | "Dimension+" -> p.IncrDim()
        | "Dimension-" -> p.DecrDim()
        | _ -> ()
     | _ -> ()

let lwcc = new LWCContainer(Dock=DockStyle.Fill)

let f = new Form(Text="Prova", Location=Point(500,500), Width=1000, Height=500)
f.Controls.Add(lwcc)


f.Show()