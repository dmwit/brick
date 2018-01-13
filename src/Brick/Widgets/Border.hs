{-# LANGUAGE OverloadedStrings #-}
-- | This module provides border widgets: vertical borders, horizontal
-- borders, and a box border wrapper widget. All functions in this
-- module use the rendering context's active 'BorderStyle'; to change
-- the 'BorderStyle', use 'withBorderStyle'.
module Brick.Widgets.Border
  ( -- * Border wrapper
    border
  , borderWithLabel

  -- * Horizontal border
  , hBorder
  , hBorderWithLabel

  -- * Vertical border
  , vBorder

  -- * Drawing single border elements
  , borderElem

  -- * Border attribute names
  , borderAttr
  , vBorderAttr
  , hBorderAttr
  , hBorderLabelAttr
  , tlCornerAttr
  , trCornerAttr
  , blCornerAttr
  , brCornerAttr
  )
where

import Data.Monoid ((<>))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Applicative (liftA2, liftA3)
import Lens.Micro ((^.), (.~), (&), to)
import Graphics.Vty (imageHeight, imageWidth)
import qualified Data.Map as M
import qualified Data.Set as S

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border.Style (BorderStyle(..))

-- | The top-level border attribute name.
borderAttr :: AttrName
borderAttr = "border"

-- | The vertical border attribute name.
vBorderAttr :: AttrName
vBorderAttr = borderAttr <> "vertical"

-- | The horizontal border attribute name.
hBorderAttr :: AttrName
hBorderAttr = borderAttr <> "horizontal"

-- | The attribute used for horizontal border labels.
hBorderLabelAttr :: AttrName
hBorderLabelAttr = hBorderAttr <> "label"

-- | The attribute used for border box top-left corners.
tlCornerAttr :: AttrName
tlCornerAttr = borderAttr <> "corner" <> "tl"

-- | The attribute used for border box top-right corners.
trCornerAttr :: AttrName
trCornerAttr = borderAttr <> "corner" <> "tr"

-- | The attribute used for border box bottom-left corners.
blCornerAttr :: AttrName
blCornerAttr = borderAttr <> "corner" <> "bl"

-- | The attribute used for border box bottom-right corners.
brCornerAttr :: AttrName
brCornerAttr = borderAttr <> "corner" <> "br"

-- | Draw the specified border element using the active border style
-- using 'borderAttr'.
borderElem :: (BorderStyle -> Char) -> Widget n
borderElem f =
    Widget Fixed Fixed $ do
      bs <- ctxBorderStyle <$> getContext
      render $ withAttr borderAttr $ str [f bs]

-- | Put a border around the specified widget.
border :: Widget n -> Widget n
border = border_ Nothing

-- | Put a border around the specified widget with the specified label
-- widget placed in the middle of the top horizontal border.
--
-- Note that a border will wrap its child widget as tightly as possible,
-- which means that if the child widget is narrower than the label
-- widget, the label widget will be truncated. If you want to avoid
-- this behavior, add a 'fill' or other space-filling wrapper to the
-- bordered widget so that it takes up enough room to make the border
-- horizontally able to avoid truncating the label.
borderWithLabel :: Widget n
                -- ^ The label widget
                -> Widget n
                -- ^ The widget to put a border around
                -> Widget n
borderWithLabel label = border_ (Just label)

border_ :: Maybe (Widget n) -> Widget n -> Widget n
border_ label wrapped =
    Widget (hSize wrapped) (vSize wrapped) $ do
      bs <- ctxBorderStyle <$> getContext
      c <- getContext

      middleResult <- render $ hLimit (c^.availWidthL - 2)
                             $ vLimit (c^.availHeightL - 2)
                             $ wrapped

      let top = (withAttr tlCornerAttr $ str [bsCornerTL bs])
                <+> hBorder_ label <+>
                (withAttr trCornerAttr $ str [bsCornerTR bs])
          bottom = (withAttr blCornerAttr $ str [bsCornerBL bs])
                   <+> hBorder <+>
                   (withAttr brCornerAttr $ str [bsCornerBR bs])
          middle = vBorder <+> (Widget Fixed Fixed $ return middleResult) <+> vBorder
          total = top <=> middle <=> bottom

      render $ hLimit (middleResult^.imageL.to imageWidth + 2)
             $ vLimit (middleResult^.imageL.to imageHeight + 2)
             $ total

-- | A horizontal border.  Fills all horizontal space.
hBorder :: Widget n
hBorder = Widget Greedy Fixed $ do
    ctx <- getContext
    let fillChar = bsHorizontal (ctxBorderStyle ctx)
    result <- render $ vLimit 1 $ withAttr hBorderAttr $ fill fillChar
    let dbs = dynamicBorder ctx
            & eaLeftL.acceptorsL .~ M.empty
            & eaRightL.acceptorsL .~ M.empty
            & eaTopL.offersL .~ M.empty
            & eaBottomL.offersL .~ M.empty
            & eaRightL.coordinateL .~ (availWidth ctx - 1)
    return (result & bordersL .~ dbs)

-- | A horizontal border with a label placed in the center of the
-- border. Fills all horizontal space.
hBorderWithLabel :: Widget n
                 -- ^ The label widget
                 -> Widget n
hBorderWithLabel label = Widget Greedy Fixed $ do
    c <- getContext
    lblRes <- render . vLimit 1 . withAttr hBorderAttr $ label
    let remainingWidth = c^.availWidthL - lblRes^.imageL.to imageWidth
        leftPadding  = max 0 $ remainingWidth `div` 2
        rightPadding = max 0 $ remainingWidth - leftPadding
        offerRight = (& eaRightL.offerJoinL .~ True)
        offerLeft  = (& eaLeftL .offerJoinL .~ True)
    -- For maximum flexibility, we always offer to join with the centered
    -- widget. If we just used whatever was in the context, we would have no
    -- way to have a border which didn't offer to join in a particular
    -- direction but did join with the centered label.
    --
    -- You might worry that always offering to join is also inflexible, in that
    -- the caller may want a widget which does not join with the border.
    -- Luckily, joining is a negotiation, so the user can specify a widget
    -- which does not accept joins, and no joining will be performed.
    lRes <- render . hLimit leftPadding  . modifyBorderDynamics offerRight $ hBorder
    rRes <- render . hLimit rightPadding . modifyBorderDynamics offerLeft  $ hBorder
    catResults hBoxRenderer [lRes, lblRes, rRes]

hBorder_ :: Maybe (Widget n) -> Widget n
hBorder_ = maybe hBorder hBorderWithLabel

-- | A vertical border.  Fills all vertical space.
vBorder :: Widget n
vBorder = Widget Fixed Greedy $ do
    ctx <- getContext
    let fillChar = bsVertical (ctxBorderStyle ctx)
    result <- render $ hLimit 1 $ withAttr vBorderAttr $ fill fillChar
    let dbs = dynamicBorder ctx
            & eaTopL.acceptorsL .~ M.empty
            & eaBottomL.acceptorsL .~ M.empty
            & eaLeftL.offersL .~ M.empty
            & eaRightL.offersL .~ M.empty
            & eaBottomL.coordinateL .~ (availHeight ctx - 1)
    return (result & bordersL .~ dbs)

dynamicBorder :: Context -> EdgeAnnotation DynamicBorder
dynamicBorder ctx = liftA3 DynamicBorder (point ctx) (segment ctx) (pure 0)

-- | Given the current rendering context, produce segments appropriate for
-- inclusion at the various boundaries of a widget.
segment :: Context -> EdgeAnnotation (M.Map Int AcceptJoinSegment)
segment ctx =
    let style = ctxBorderStyle ctx
        dyn   = ctxBorderDynamics ctx
        maybeKeep js len a =
            if acceptJoin js && len > 0
            then M.singleton 0 a{ jsLength = len }
            else M.empty
        lengths = EdgeAnnotation
            { eaTop = availWidth ctx
            , eaBottom = availWidth ctx
            , eaLeft = availHeight ctx
            , eaRight = availHeight ctx
            }
        mkAJS attr par perp startL endL startBoth endBoth startIn startOut middleIn middleOut endIn endOut =
            let offerStartJoin = dyn ^. startL.offerJoinL
                offerEndJoin   = dyn ^.   endL.offerJoinL
            in AcceptJoinSegment
            { jsLength = 0 -- will be overwritten in maybeKeep
            , jsStyle = attrMapLookup attr (ctxAttrMap ctx)
            , jsInnerJoinPoints = S.empty
            , jsParallel      = par  style
            , jsPerpendicular = perp style
            , jsStartInward  = (if offerStartJoin then middleIn        else startIn  ) style
            , jsStartOutward = (if offerStartJoin then middleOut       else startOut ) style
            , jsStartBoth    = (if offerStartJoin then bsIntersectFull else startBoth) style
            , jsMiddleInward  = middleIn        style
            , jsMiddleOutward = middleOut       style
            , jsMiddleBoth    = bsIntersectFull style
            , jsEndInward  = (if offerEndJoin then middleIn        else endIn  ) style
            , jsEndOutward = (if offerEndJoin then middleOut       else endOut ) style
            , jsEndBoth    = (if offerEndJoin then bsIntersectFull else endBoth) style
            }
        hAJS = mkAJS hBorderAttr bsHorizontal bsVertical eaLeftL eaRightL bsIntersectL bsIntersectR
        vAJS = mkAJS vBorderAttr bsVertical bsHorizontal eaTopL eaBottomL bsIntersectT bsIntersectB
    in liftA3 maybeKeep dyn lengths EdgeAnnotation
        { eaTop    = hAJS bsCornerTL bsCornerBL bsIntersectT bsIntersectB bsCornerTR bsCornerBR
        , eaBottom = hAJS bsCornerBL bsCornerTL bsIntersectB bsIntersectT bsCornerBR bsCornerTR
        , eaLeft   = vAJS bsCornerTL bsCornerTR bsIntersectL bsIntersectR bsCornerBL bsCornerBR
        , eaRight  = vAJS bsCornerTR bsCornerTL bsIntersectR bsIntersectL bsCornerBR bsCornerBL
        }

point :: Context -> EdgeAnnotation (M.Map Int OfferJoinPoint)
point ctx =
    let horizAttr = attrMapLookup hBorderAttr (ctxAttrMap ctx)
        vertAttr  = attrMapLookup vBorderAttr (ctxAttrMap ctx)
        horizOJP  = OfferJoinPoint (bsHorizontal (ctx ^. ctxBorderStyleL)) horizAttr
        vertOJP   = OfferJoinPoint (bsVertical   (ctx ^. ctxBorderStyleL)) vertAttr
        dirs = EdgeAnnotation
            { eaTop = vertOJP
            , eaBottom = vertOJP
            , eaLeft = horizOJP
            , eaRight = horizOJP
            }
        offer ojp joinStyle = M.fromAscList [(0, ojp) | offerJoin joinStyle]
    in liftA2 offer dirs (ctxBorderDynamics ctx)
