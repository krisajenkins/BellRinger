module Main where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Trans.State.Strict
import Pipes
import Pipes.Concurrent
import MVC

import Virtual
import Messages
import qualified Render

-- | Construct an @HTML@ tree from the world with a callback for clicks
model :: (World -> HTML) -> Model World Message HTML
model view =
  asPipe $
  forever $
  do m <- await
     w <- lift get
     let w' = process m w
     lift $ put w'
     yield $ view w'

-- | A callback the sends @Message@s to an @Output@
sendMessage :: Output Message -> Message -> IO ()
sendMessage output msg =
  atomically $
  void $
  send output msg

-- | Processes click events, possibly dispatching to second order producers
clickProcessor :: Input Message -> IO (Input Message)
clickProcessor uiClicks =
  do (outbox,inbox) <- spawn Unbounded
     void $
       forkIO $
       runEffect $
       for (fromInput uiClicks) $
       flip queue outbox
     return inbox

eventLoop :: ((Message -> IO ()) -> World -> HTML) -> World -> IO ()
eventLoop eventView initialWorld =
  do (clicksOutbox,clicksInbox) <- spawn (Bounded 10)
     processedClicks <- clickProcessor clicksInbox
     let renderer =
           eventView (sendMessage clicksOutbox)
     void $
       runMVC initialWorld (model renderer) $
       managed $
       \k ->
         do treeState <- newMVar =<<
                         renderSetup renderer initialWorld
            k (asSink (reRenderDiff treeState),asInput processedClicks)
  where reRenderDiff treeStateVar newTree =
          modifyMVar treeStateVar $
          \(TreeState{_node = oldNode,_tree = oldTree}) ->
            do let patches = diff oldTree newTree
               newNode <- patch oldNode patches
               return (makeTreeState newNode newTree,())

main :: IO ()
main = eventLoop Render.rootView (0,0,const True,NotRequested)
