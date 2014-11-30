{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Virtual (
  vnode,
  vnodeFull,
  svg,
  rect,
  vtext,
  vbutton,
  renderSetup,
  rerender,
  documentBody,
  diff,
  patch,
  TreeState(..),
  makeTreeState,
  Size(..),
  Position(..),
  HTML()
  ) where

import GHCJS.Foreign
import GHCJS.Types
import System.IO.Unsafe -- TODO This is, of course, bad.

data VNode
data DOMNode
data Patch
data JSProperties
data TreeState =
  TreeState {_node :: JSRef DOMNode
            ,_tree :: HTML}
newtype HTML =
  HTML {nodeRef :: JSRef VNode}
newtype HTMLPatch =
  HTMLPatch {patchRef :: JSRef Patch}
newtype Properties =
  Properties {propsRef :: JSRef JSProperties}

makeTreeState :: JSRef DOMNode -> HTML -> TreeState
makeTreeState n t= TreeState { _node = n, _tree = t}

#ifndef HLINT
foreign import javascript unsafe
  "document.body" documentBody :: IO (JSRef DOMNode)

foreign import javascript unsafe
  "$1.appendChild($2)" appendChild :: JSRef DOMNode -> JSRef DOMNode -> IO ()

foreign import javascript unsafe
  "h($1, $2, $3)" vnode_ :: JSString -> JSRef JSProperties -> JSArray VNode -> IO (JSRef VNode)

foreign import javascript unsafe
  "$r = h($1, {'ev-click': $2, 'key': $3}, $4)" vbutton_ :: JSString -> JSFun (JSRef a -> IO ()) -> JSString -> JSString -> IO (JSRef VNode)

foreign import javascript unsafe
  "h($1, $2)" vtext_ :: JSString -> JSString -> IO (JSRef VNode)

foreign import javascript unsafe
  "$r = createElement($1)" createDOMNode_ :: JSRef VNode -> IO (JSRef DOMNode)

foreign import javascript safe
  "diff($1, $2)" diff_ :: JSRef VNode -> JSRef VNode -> JSRef Patch

foreign import javascript safe
  "patch($1, $2)" patch_ :: JSRef DOMNode -> JSRef Patch -> IO (JSRef DOMNode)

foreign import javascript unsafe
  "$r = {};" noproperty_ :: IO (JSRef JSProperties)

foreign import javascript unsafe
  "$r = svg('svg', { width: $1, height: $2 }, $3);" svg_ :: JSString -> JSString -> JSArray VNode -> IO (JSRef VNode)

foreign import javascript unsafe
  "$r = svg('rect', { width: $1, height: $2, x: $3, y: $4 });" rect_ :: JSString -> JSString -> JSString -> JSString -> JSRef VNode
#endif

-- property :: String -> String -> Properties
-- property k v =
--   Properties $
--   property_ (toJSString k)
--             (toJSString v)

noProperty :: Properties
noProperty = Properties $ unsafePerformIO noproperty_

data Size = Size String String deriving (Show,Eq)
data Position = Position String String deriving (Show,Eq)

svg :: Size -> [HTML] -> HTML
svg (Size w h) children =
  HTML $
  unsafePerformIO $
  do jsChildren <- toArray (fmap nodeRef children)
     svg_ (toJSString w)
          (toJSString h)
          jsChildren

rect :: Size -> Position -> HTML
rect (Size w h) (Position x y) =
  HTML $
  rect_ (toJSString w)
        (toJSString h)
        (toJSString x)
        (toJSString y)

vnodeFull :: String -> Properties -> [HTML] -> HTML
vnodeFull tag properties children =
  HTML $
  unsafePerformIO $
  do jsChildren <- toArray (fmap nodeRef children)
     vnode_ (toJSString tag)
            (propsRef properties)
            jsChildren


vnode :: String -> [HTML] -> HTML
vnode tag children =
  HTML $
  unsafePerformIO $
  do jsChildren <- toArray (fmap nodeRef children)
     props <- noproperty_
     vnode_ (toJSString tag) props jsChildren

vbutton :: String -> (JSRef a -> IO ()) -> String -> HTML
vbutton tag f s =
  HTML $
  unsafePerformIO $
  do f' <- syncCallback1 AlwaysRetain True f
     vbutton_ (toJSString tag)
              f'
              (toJSString s)
              (toJSString s)

vtext :: String -> String -> HTML
vtext tag text = HTML $ unsafePerformIO $ vtext_ (toJSString tag) (toJSString text)

createDOMNode :: HTML -> IO (JSRef DOMNode)
createDOMNode (HTML x) = createDOMNode_ x

patch :: JSRef DOMNode -> HTMLPatch -> IO (JSRef DOMNode)
patch n (HTMLPatch p) = patch_ n p

diff :: HTML -> HTML -> HTMLPatch
diff (HTML old) (HTML new) = HTMLPatch (diff_ old new)

rerender :: (a -> HTML) -> a -> TreeState -> IO TreeState
rerender render x (TreeState {_node = oldNode, _tree = oldTree}) = do
  let newTree = render x
      patches = diff oldTree newTree
  newNode <- patch oldNode patches
  return (makeTreeState newNode newTree)

renderSetup :: (a -> HTML) -> a -> IO TreeState
renderSetup render x = do
  body <- documentBody
  let tree = render x
  node <- createDOMNode tree
  _ <- appendChild body node
  return $ makeTreeState node tree
