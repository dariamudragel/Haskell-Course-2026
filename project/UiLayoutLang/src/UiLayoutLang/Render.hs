module UiLayoutLang.Render(renderSvg)where

import UiLayoutLang.Types

renderSvg::Resolved->String
renderSvg r="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""++show(rw r)++"\" height=\""++show(rh r)++"\">\n"++draw r++"</svg>\n"

draw::Resolved->String
draw r=rect r++concatMap draw(rChildren r)

rect::Resolved->String
rect r="<rect x=\""++show(rx r)++"\" y=\""++show(ry r)++"\" width=\""++show(rw r)++"\" height=\""++show(rh r)++"\" fill=\""++fill++"\" stroke=\"black\"/>\n" where
 fill=case rColor r of
  Just c->c
  Nothing->"none"
