module UiLayoutLang.Engine(resolveWindow,resolveLayout,inside)where

import UiLayoutLang.Types

resolveWindow::Window->Resolved
resolveWindow(Window _ w h lay)=resolveLayout 0 0 w h lay

resolveLayout::Int->Int->Int->Int->Layout->Resolved
resolveLayout x y maxW maxH(Box ps kids)=Resolved x y w h(pColor ps)rs where
 w=clamp 0 maxW(sizeVal maxW(pWidth ps))
 h=clamp 0 maxH(sizeVal maxH(pHeight ps))
 rs=resolveChildren(pDir ps)x y w h kids

resolveChildren::Direction->Int->Int->Int->Int->[Layout]->[Resolved]
resolveChildren _ _ _ _ _ []=[]
resolveChildren Row x y w h kids=go x w kids where
 go _ _ []=[]
 go cx remW(k:ks)=
  let r0=resolveLayout cx y w h k
      rw'=min remW (rw r0)
      r=r0{rw=rw'}
  in r:go(cx+rw')(remW-rw') ks
resolveChildren Col x y w h kids=go y h kids where
 go _ _ []=[]
 go cy remH(k:ks)=
  let r0=resolveLayout x cy w h k
      rh'=min remH (rh r0)
      r=r0{rh=rh'}
  in r:go(cy+rh')(remH-rh') ks

sizeVal::Int->Size->Int
sizeVal parent Auto=parent
sizeVal _ (Px n)=n
sizeVal parent(Pct p)=round(fromIntegral parent*p)

clamp::Int->Int->Int->Int
clamp a b n=max a(min b n)

inside::Resolved->Bool
inside r=all childOk(rChildren r)&&all inside(rChildren r)where
 childOk c=rx c>=rx r&&ry c>=ry r&&rx c+rw c<=rx r+rw r&&ry c+rh c<=ry r+rh r
