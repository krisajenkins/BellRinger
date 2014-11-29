{-# LANGUAGE OverloadedStrings #-}

-- NOTE: This is very much an interim namespace. I'd prefer to use Lucid.
module HTML where

import Prelude hiding (span)

import Virtual

span :: String -> HTML
span = vtext "span"

navbar, well, container, row, col3, col9, col12, thead, tbody, th, td, tr, table :: [HTML] -> HTML
container = vnode "div.container"
row = vnode "div.row"
well = vnode "div.well"
col3 = vnode "div.col-sm-3"
col9 = vnode "div.col-sm-9"
col12 = vnode "div.col-sm-12"
th = vnode "th"
thead = vnode "thead"
tbody = vnode "tbody"
td = vnode "td"
tr = vnode "tr"
table = vnode "table.table"
navbar = vnode "nav.navbar.navbar-default"

type TableDef a = [(String, a -> String)]

simpleTable :: TableDef a -> [a] -> HTML
simpleTable tableDef xs =
  table [thead [tr (map heading tableDef)]
        ,tbody (map (simpleTableBodyRow tableDef) xs)]
  where heading (title,_) = th [span title]

simpleTableBodyRow :: TableDef a -> a -> HTML
simpleTableBodyRow tableDef x =
  tr (map cell tableDef)
  where cell (_,f) = td [span (f x)]
