#load "Matrix.fsx"

open System.Windows.Forms
open System.Drawing

open Matrix

type LWCControl() =
  let wv = WVMatrix()

  let mutable sz = SizeF(120.f, 120.f)
  let mutable pos = PointF()
  let mutable parent : UserControl option = None
  let mutable Color : Brush = null
  let mutable Selected = false
  let mutable img : Image = null
  let mutable cclass : string = null
  let mutable name : string = null

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