module UiLayoutLang.Types where

data Size=Px Int|Pct Double|Auto deriving(Eq,Show)
data Direction=Row|Col deriving(Eq,Show)
data Props=Props{pWidth::Size,pHeight::Size,pDir::Direction,pColor::Maybe String}deriving(Eq,Show)
data Layout=Box Props [Layout]deriving(Eq,Show)
data Window=Window String Int Int Layout deriving(Eq,Show)
data Resolved=Resolved{rx::Int,ry::Int,rw::Int,rh::Int,rColor::Maybe String,rChildren::[Resolved]}deriving(Eq,Show)

defaultProps::Props
defaultProps=Props Auto Auto Col Nothing
