open System.Windows.Forms
open System.Drawing

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
  let mutable Color = null
  let mutable Selected = false
  let mutable img : Image = null
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
    with get() = img
    and set(v:Image) = 
      img <- v

  member this.Left = pos.X
  
  member this.Top = pos.Y

  member this.Width = sz.Width
  member this.Height = sz.Height

and LWCContainer() as this =
  inherit UserControl()

  let controls = System.Collections.ObjectModel.ObservableCollection<LWCControl>()

  let mutable newplanet = None
  let mutable drag = None
  let mutable newMode = false
  let mutable myPicture : Bitmap = null
  let timer = new Timer(Interval=25)
  let timer2 = new Timer(Interval=35)
  let mutable count1 = 0
  let mutable count2 = 0
  let mutable limit = 1

  do 
    this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
    controls.CollectionChanged.Add(fun e ->
      for i in e.NewItems do
        (i :?> LWCControl).Parent <- Some(this)
      )
    let mutable flag = true
    while (flag) do
      let dlg = new OpenFileDialog()
      dlg.Filter <- "|*.BMP;*.JPG;*.GIF;*.PNG"
      if dlg.ShowDialog() = DialogResult.OK then
        let imagename = dlg.FileName
        myPicture <- new Bitmap(imagename)
        flag <- false
    controls.Add(LWButton(Position=PointF(100.f, 100.f),ClientSize=SizeF(30.f, 30.f),selected=false,Class="ship", image=myPicture))
    controls.Add(LWButton(Class="Op", Name="New Planet",Position=PointF(10.f, 10.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="Up",Position=PointF(10.f, 40.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="Down",Position=PointF(10.f, 70.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="Left",Position=PointF(10.f, 100.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="Right",Position=PointF(10.f, 130.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="RotateR",Position=PointF(10.f, 160.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="RotateL",Position=PointF(10.f, 190.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="ZoomIn",Position=PointF(10.f, 220.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="ZoomOut",Position=PointF(10.f, 250.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="Dimension+",Position=PointF(10.f, 280.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    controls.Add(LWButton(Class="Op", Name="Dimension-",Position=PointF(10.f, 310.f),ClientSize=SizeF(65.f,20.f),color=Brushes.SkyBlue))
    
    timer.Tick.Add( fun _ ->
       timer2.Stop()
       controls 
           |> Seq.iter(fun c ->
           if(c.Class = "ship") then
             c.WV.TranslateW(10.f, 0.f)
           )
       this.Invalidate()
       count1 <- count1 + 1
       if (count1 = 15) then
         count1 <- 0
         timer.Stop()
         timer2.Start()
    )
    timer2.Tick.Add(fun _ ->
     controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.TranslateW(2.f, 0.f)
        )
     this.Invalidate()
     count2 <- count2 + 1
     if (count2 = limit) then
       count2 <- 0
       limit <- 1
       timer2.Stop()
    )
   
  member this.LWControls with get() = controls

  member this.NewPlanet() =
    newMode <- true

  member this.Up() =
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(0.f, 10.f)
        )
    this.Invalidate()

  member this.Down() = 
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(0.f, -10.f)
        )
    this.Invalidate()
  
  member this.Left() = 
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(10.f, 0.f)
        )
    this.Invalidate()

  member this.Right() = 
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(-10.f, 0.f)
        )
    this.Invalidate()

  member this.RotateR() = 
    let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(cx, cy)
          c.WV.RotateV(-10.f)
          c.WV.TranslateV(-cx, -cy)
        )
    this.Invalidate()

  member this.RotateL() = 
    let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
    controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(cx, cy)
          c.WV.RotateV(10.f)
          c.WV.TranslateV(-cx, -cy)
        )
    this.Invalidate()

  member this.ZoomOut() = 
      let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
      controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(cx, cy)
          c.WV.ScaleV(1.1f, 1.1f)
          c.WV.TranslateV(-cx, -cy)
        )
      this.Invalidate()

  member this.ZoomIn() = 
      let cx, cy = this.ClientSize.Width / 2 |> single, this.ClientSize.Height / 2 |> single
      controls 
        |> Seq.iter(fun c ->
        if(c.Class <> "Op") then
          c.WV.TranslateV(cx, cy)
          c.WV.ScaleV(1.f/1.1f, 1.f/1.1f)
          c.WV.TranslateV(-cx, -cy)
        )
      this.Invalidate()

  member this.IncrDim() = 
      controls 
        |> Seq.iter(fun c ->
        if(c.selected) then
          c.WV.ScaleW(1.1f, 1.1f)
        )
      this.Invalidate()

  member this.DecrDim() = 
      controls 
        |> Seq.iter(fun c ->
        if(c.selected) then
          c.WV.ScaleW(1.f/1.1f, 1.f/1.1f)
        )
      this.Invalidate()

  override this.OnMouseDown (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      if (c.selected) then
        c.selected <- false
      else if (c.Class <> "Op") then
        c.selected <- true
      let dx, dy = e.X - int(c.Position.X), e.Y - int(c.Position.Y)
      if (c.Class <> "Op") then
        drag <- Some (c, dx, dy)
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseDown(evt)
      this.Invalidate()
    | None -> 
        if (newMode) then
          let dlg = new OpenFileDialog()
          dlg.Filter <- "|*.BMP;*.JPG;*.GIF;*.PNG"
          if dlg.ShowDialog() = DialogResult.OK then
            let imagename = dlg.FileName
            myPicture <- new Bitmap(imagename)
            newplanet <- Some (e.X, e.Y)
          else
            newMode <- false

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
            if(c.Class <> "Op") then 
              c.selected <- false
          )
    match newplanet with
     | Some(ex, ey) ->
       controls.Add(LWButton(image=myPicture,Position=PointF(single ex, single ey),ClientSize=SizeF(100.f, 100.f),selected=false,Class="planet"))
       newplanet <- None
       newMode <- false
       this.Invalidate()
     | None -> 
       drag <- None

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
      c.Position <- PointF(single(e.X - int(dx)),single( e.Y - int(dy)))
      c.selected <- false
      this.Invalidate()
    | _ -> ()

  override this.OnPaint(e) =
    controls 
    |> Seq.iter(fun c ->
      let bkg = e.Graphics.Save()
      let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt , c.ClientSizeInt))
      e.Graphics.Transform <- c.WV.WV // cambiamo la matrice di trasformazione
      c.OnPaint(evt)
      e.Graphics.Restore(bkg)
    )

  override this.OnKeyUp e = 
    match e.KeyCode with
    | Keys.W ->
      count1 <- 0
      count2 <- 0
      timer.Start()
    | _ -> ()

  override this.OnKeyDown e =
    let mutable ship = LWCControl()
    match e.KeyCode with
    | Keys.W ->
      controls 
        |> Seq.iter(fun c ->
        if(c.Class = "ship") then
          c.WV.TranslateW(10.f, 0.f)
          ship <- c
          limit <- limit + 5
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
    | _ -> ()
    this.Invalidate()

// Utente Libreria
and LWButton() as this =
  inherit LWCControl()

  override this.OnPaint(e) =
    let g = e.Graphics
    if (this.image <> null) then
      g.DrawImage(this.image, RectangleF(0.f, 0.f, this.Height,this.Width))
    else if (this.Width <> 0.f) then
      g.FillRectangle(this.color, 0.f, 0.f, this.Width, this.Height)
      g.DrawString(this.Name, new Font("Calibri", 10.f), Brushes.Black, 0.f, 0.f)
  
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

let f = new Form(Text="MidTerm - Torneo Stefano", Location=Point(500,500), Width=1000, Height=500)

f.Controls.Add(lwcc)

f.Show()