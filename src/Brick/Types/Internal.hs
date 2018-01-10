{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Brick.Types.Internal
  ( ScrollRequest(..)
  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL
  , Location(..)
  , locL
  , origin
  , TerminalLocation(..)
  , Viewport(..)
  , ViewportType(..)
  , RenderState(..)
  , Direction(..)
  , CursorLocation(..)
  , cursorLocationL
  , cursorLocationNameL
  , Context(..)
  , EventState(..)
  , EventRO(..)
  , Next(..)
  , Result(..)
  , Extent(..)
  , CacheInvalidateRequest(..)
  , BrickEvent(..)

  , EdgeAnnotation(..)
  , JoinStyle(..)
  , AcceptJoinSegment(..)
  , OfferJoinPoint(..)
  , DynamicBorder(..)

  , rsScrollRequestsL
  , viewportMapL
  , clickableNamesL
  , renderCacheL
  , observedNamesL
  , vpSize
  , vpLeft
  , vpTop
  , acceptJoinL
  , offerJoinL
  , eaTopL
  , eaBottomL
  , eaLeftL
  , eaRightL
  , imageL
  , cursorsL
  , extentsL
  , visibilityRequestsL
  , bordersL
  , offersL
  , acceptorsL
  , coordinateL
  , emptyResult
  , defaultJoinStyle
  , defaultBorderDynamics
  , performPairwiseJoins
  )
where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Lens.Micro (_1, _2, Lens', (%~), (&), (^.), lens)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Internal (Field1, Field2)
import qualified Data.Set as S
import qualified Data.Map as M
import Graphics.Vty (Attr, Vty, Event, Button, Modifier, DisplayRegion, Image, emptyImage, char)

import Brick.Types.TH
import Brick.AttrMap (AttrName, AttrMap)
import Brick.Widgets.Border.Style (BorderStyle)

data ScrollRequest = HScrollBy Int
                   | HScrollPage Direction
                   | HScrollToBeginning
                   | HScrollToEnd
                   | VScrollBy Int
                   | VScrollPage Direction
                   | VScrollToBeginning
                   | VScrollToEnd
                   | SetTop Int
                   | SetLeft Int

data VisibilityRequest =
    VR { vrPosition :: Location
       , vrSize :: DisplayRegion
       }
       deriving (Show, Eq)

-- | Describes the state of a viewport as it appears as its most recent
-- rendering.
data Viewport =
    VP { _vpLeft :: Int
       -- ^ The column offset of left side of the viewport.
       , _vpTop :: Int
       -- ^ The row offset of the top of the viewport.
       , _vpSize :: DisplayRegion
       -- ^ The size of the viewport.
       }
       deriving Show

-- | The type of viewports that indicates the direction(s) in which a
-- viewport is scrollable.
data ViewportType = Vertical
                  -- ^ Viewports of this type are scrollable only vertically.
                  | Horizontal
                  -- ^ Viewports of this type are scrollable only horizontally.
                  | Both
                  -- ^ Viewports of this type are scrollable vertically and horizontally.
                  deriving (Show, Eq)

data CacheInvalidateRequest n = InvalidateSingle n
                              | InvalidateEntire

data EventState n = ES { esScrollRequests :: [(n, ScrollRequest)]
                       , cacheInvalidateRequests :: [CacheInvalidateRequest n]
                       }

-- | An extent of a named area: its size, location, and origin.
data Extent n = Extent { extentName      :: n
                       , extentUpperLeft :: Location
                       , extentSize      :: (Int, Int)
                       , extentOffset    :: Location
                       }
              deriving (Show)

data EventRO n = EventRO { eventViewportMap :: M.Map n Viewport
                         , eventVtyHandle :: Vty
                         , latestExtents :: [Extent n]
                         }

-- | The type of actions to take upon completion of an event handler.
data Next a = Continue a
            | SuspendAndResume (IO a)
            | Halt a
            deriving Functor

-- | Scrolling direction.
data Direction = Up
               -- ^ Up/left
               | Down
               -- ^ Down/right
               deriving (Show, Eq)

-- | A terminal screen location.
data Location = Location { loc :: (Int, Int)
                         -- ^ (Column, Row)
                         }
                deriving (Show, Eq, Ord)

suffixLenses ''Location

instance Field1 Location Location Int Int where
    _1 = locL._1

instance Field2 Location Location Int Int where
    _2 = locL._2

-- | The class of types that behave like terminal locations.
class TerminalLocation a where
    -- | Get the column out of the value
    locationColumnL :: Lens' a Int
    locationColumn :: a -> Int

    -- | Get the row out of the value
    locationRowL :: Lens' a Int
    locationRow :: a -> Int

instance TerminalLocation Location where
    locationColumnL = _1
    locationColumn (Location t) = fst t
    locationRowL = _2
    locationRow (Location t) = snd t

-- | The origin (upper-left corner).
origin :: Location
origin = Location (0, 0)

instance Monoid Location where
    mempty = origin
    mappend (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

-- | A cursor location.  These are returned by the rendering process.
data CursorLocation n =
    CursorLocation { cursorLocation :: !Location
                   -- ^ The location
                   , cursorLocationName :: !(Maybe n)
                   -- ^ The name of the widget associated with the location
                   }
                   deriving Show

data EdgeAnnotation a =
    EdgeAnnotation { eaTop, eaBottom, eaLeft, eaRight :: a }
    deriving (Eq, Ord, Read, Show, Functor)

suffixLenses ''EdgeAnnotation

instance Applicative EdgeAnnotation where
    pure v = EdgeAnnotation v v v v
    EdgeAnnotation ft fb fl fr <*> EdgeAnnotation vt vb vl vr =
        EdgeAnnotation (ft vt) (fb vb) (fl vl) (fr vr)

-- Just for completeness; it's not clear where one might want this.
instance Monad EdgeAnnotation where
    EdgeAnnotation vt vb vl vr >>= f =
        EdgeAnnotation
            (eaTop    (f vt))
            (eaBottom (f vb))
            (eaLeft   (f vl))
            (eaRight  (f vr))

-- | A part of the drawing context that tells border widgets how to advertise
-- their existence.
data JoinStyle =
    JoinStyle { acceptJoin :: !Bool
              -- ^ Should widgets look at their neighbors to influence how they
              -- draw their borders?
              , offerJoin :: !Bool
              -- ^ Should widgets try to influence how their neighbors draw
              -- their borders?
              } deriving (Eq, Ord, Read, Show)

suffixLenses ''JoinStyle

defaultJoinStyle :: JoinStyle
defaultJoinStyle = JoinStyle False False

defaultBorderDynamics :: EdgeAnnotation JoinStyle
defaultBorderDynamics = pure defaultJoinStyle

-- | A dynamic border that may need further processing to be drawn correctly.
-- Segments are associated with a single border of a widget (e.g. 'top',
-- 'bottom', 'left', or 'right'). We record a variety of box-drawing characters
-- associated with the border, one for each combination of "inner" and "outer"
-- joins that might be needed at either the beginning, middle, or end of the
-- segment. "Inner" means "towards the inside of the widget"; for example, for
-- top borders, inward is down, outward is up, parallel is horizontal, and
-- perpendicular is vertical.
data AcceptJoinSegment =
    AcceptJoinSegment { jsLength :: !Int
                      -- ^ Length of the segment
                      , jsStyle :: !Attr
                      -- ^ The attribute to use when drawing this segment;
                      -- will only try to join with perpendicular segments
                      -- that use the same attribute
                      , jsInnerJoinPoints :: !(S.Set Int)
                      -- ^ Locations where we need to join inwards, given as
                      -- offsets from the start of the segment
                      , jsParallel :: !Char
                      -- ^ Character used to draw this segment when there are no joins needed
                      , jsPerpendicular :: !Char
                      -- ^ Only try to join with perpendicular segments that use this character
                      , jsStartInward :: !Char
                      , jsStartOutward :: !Char
                      , jsStartBoth :: !Char
                      , jsMiddleInward :: !Char
                      , jsMiddleOutward :: !Char
                      , jsMiddleBoth :: !Char
                      , jsEndInward :: !Char
                      , jsEndOutward :: !Char
                      , jsEndBoth :: !Char
                      } deriving (Eq, Read, Show)

jsInnerJoinPointsL :: Lens' AcceptJoinSegment (S.Set Int)
jsInnerJoinPointsL = lens jsInnerJoinPoints (\js ps -> js { jsInnerJoinPoints = ps })

-- | An unfinished straight line that is notionally "jutting out" of a widget
-- into its neighbor.
data OfferJoinPoint =
    OfferJoinPoint { jpParallel :: !Char
                   -- ^ The kind of straight character that neighboring widgets
                   -- should smoothly connect to
                   , jpStyle :: !Attr
                   -- ^ The style the straight line is drawn in; used to make
                   -- sure that we don't abruptly change styles when
                   -- transitioning to the neighboring widget
                   } deriving (Eq, Read, Show)

-- | Information about how a widget's border should be modified to connect with
-- neighboring widgets.
data DynamicBorder =
    DynamicBorder { offers :: !(M.Map Int OfferJoinPoint)
                  -- ^ Keys are offsets along the boundary
                  , acceptors :: !(M.Map Int AcceptJoinSegment)
                  -- ^ Invariant: the segments are disjoint. That is, if @k@
                  -- maps to @v@, then keys @k+1@ through @k+jsLength v-1@ do
                  -- not exist in the 'Map'.
                  , coordinate :: !Int
                  -- ^ Where in the widget-local coordinates the border exists.
                  -- For example, for a top border, this will normally be row 0
                  -- (unlike all the other 'Int's keys in this data structure,
                  -- which refer to columns, this one refers to a row), while
                  -- for a right border, this will normally be some positive
                  -- column number. This is used to decide which borders to
                  -- discard during cropping operations.
                  } deriving (Eq, Show)

suffixLenses ''DynamicBorder

emptyDynamicBorder :: DynamicBorder
emptyDynamicBorder = DynamicBorder M.empty M.empty 0

emptyBorders :: EdgeAnnotation DynamicBorder
emptyBorders = pure emptyDynamicBorder

-- | Given an offer and its offset, and an acceptor and its offset, return a
-- list that either has a single rewrite (as an offset+image pair) or is empty
-- (if the offer's style or border type don't match the acceptor's).
performJoin :: (Int, OfferJoinPoint) -> (Int, AcceptJoinSegment) -> [(Int, Image)]
performJoin (io, o) (ia, a) =
    [ (io, char (jsStyle a) (charSelector a))
    | jpParallel o == jsPerpendicular a
    , jpStyle o == jsStyle a
    , 0 <= off && off < jsLength a -- defensive programming, should never actually be true
    ]
    where
    off = io - ia
    charSelector = case (off `S.member` jsInnerJoinPoints a, off == 0, off == jsLength a - 1) of
        (False, False, False) -> jsMiddleOutward
        (False, False, True ) -> jsEndOutward
        (False, True , False) -> jsStartOutward
        -- Edge case: an accepting join segment which is only one character
        -- long. If we were to use a corner character, either one would look
        -- wrong (and there's no good way to choose between them). So use a T
        -- character instead, which will look rightish probably.
        (False, True , True ) -> jsMiddleOutward
        (True , False, False) -> jsMiddleBoth
        (True , False, True ) -> jsEndBoth
        (True , True , False) -> jsStartBoth
        -- Same edge case as above.
        (True , True , True ) -> jsMiddleBoth

performMapJoins :: M.Map Int OfferJoinPoint -> M.Map Int AcceptJoinSegment -> M.Map Int Image
performMapJoins om am = M.fromAscList (performListJoins (M.toAscList om) (M.toAscList am)) where
    performListJoins oAll@(ot@(io, _):oRest) aAll@(at@(ia, a):aRest)
        | io < ia = performListJoins oRest aAll
        | io >= ia + jsLength a = performListJoins oAll aRest
        | otherwise = performJoin ot at ++ performListJoins oRest aAll
    performListJoins _ _ = []

-- | Given some new inner join points, include them in the appropriate
-- segments.
addInnerJoinPoints :: S.Set Int -> M.Map Int AcceptJoinSegment -> M.Map Int AcceptJoinSegment
addInnerJoinPoints points segments = M.fromAscList (merge (S.toAscList points) (M.toAscList segments)) where
    merge ps@(p:pt) jss@((ijs, js):jst)
        | p < ijs = merge pt jss
        | p >= ijs + jsLength js = (ijs, js) : merge ps jst
        | otherwise = merge pt ((ijs, js & jsInnerJoinPointsL %~ S.insert (p-ijs)):jst)
    merge _ jss = jss

-- | Internal use only.
performUnidirectionalJoins
    :: (borders ~ EdgeAnnotation DynamicBorder, border ~ DynamicBorder)
    => Lens' borders border
    -> Lens' borders border
    -> borders
    -> borders
    -> (borders, M.Map Int Image)
performUnidirectionalJoins toA toB a b =
    let rewrites = performMapJoins (b ^. toA.offersL) (a ^. toB.acceptorsL)
        withNewJoinPoints =
            if (a ^. toB.coordinateL) == (a ^. toA.coordinateL)
            then a & toA.acceptorsL %~ addInnerJoinPoints (M.keysSet rewrites)
            else a
    in (withNewJoinPoints, rewrites)

-- | Join the dynamic borders of two widgets in a particular direction. For
-- example, the invocation @performBidirectionalJoins eaTopL eaBottomL b1 b2@
-- will connect the top border of @b2@ with the bottom border of @b1@. It
-- returns the rewrites needed for each, and also returns new dynamic borders
-- objects because the 'acceptors'\'s 'jsInnerJoinPoints' may have been
-- updated.
performBidirectionalJoins
    :: (borders ~ EdgeAnnotation DynamicBorder, border ~ DynamicBorder)
    => Lens' borders border
    -> Lens' borders border
    -> borders
    -> borders
    -> ((borders, M.Map Int Image), (borders, M.Map Int Image))
performBidirectionalJoins toA toB a b =
    ( performUnidirectionalJoins toA toB a b
    , performUnidirectionalJoins toB toA b a
    )

-- | Join the dynamic borders of each pair of adjacent elements in a list.
-- Return the updated borders information and the vty rewrites needed to keep
-- the visuals in synch with the dynamic border information. For example, the
-- invocation @performPairwiseJoins eaTopL eaBottomL bs@ will connect the
-- bottom borders in @init bs@ with the top borders in @tail bs@ (and vice
-- versa). If the triple @(b, r, r')@ is in the result, @b@ will be the updated
-- border information, @r@ will be the rewrite needed for the top row, and @r'@
-- will be the rewrite needed for the bottom row. In case the top and bottom
-- rows coincide, the rewrites should be applied in order (first @r@, then
-- @r'@).
performPairwiseJoins
    :: ( borders ~ EdgeAnnotation DynamicBorder
       , border  ~ DynamicBorder
       , rewrite ~ M.Map Int Image
       )
    => Lens' borders border
    -> Lens' borders border
    -> [borders]
    -> [(borders, rewrite, rewrite)]
performPairwiseJoins _ _ [] = []
performPairwiseJoins toHead toTail (bFirst:bRest) = go bFirst M.empty bRest where
    go bh rewriteh [] = [(bh, rewriteh, M.empty)]
    go bh rewriteh (bt:bs) =
        case performBidirectionalJoins toHead toTail bh bt of
            ((bh', rewriteh'), (bt', rewritet))
                -> (bh', rewriteh, rewriteh') : go bt' rewritet bs

-- | The type of result returned by a widget's rendering function. The
-- result provides the image, cursor positions, and visibility requests
-- that resulted from the rendering process.
data Result n =
    Result { image :: Image
           -- ^ The final rendered image for a widget
           , cursors :: [CursorLocation n]
           -- ^ The list of reported cursor positions for the
           -- application to choose from
           , visibilityRequests :: [VisibilityRequest]
           -- ^ The list of visibility requests made by widgets rendered
           -- while rendering this one (used by viewports)
           , extents :: [Extent n]
           , borders :: EdgeAnnotation DynamicBorder
           -- ^ Used to allow neighboring widgets to influence each other's
           -- drawing so that borders can connect.
           }
           deriving Show

suffixLenses ''Result

emptyResult :: Result n
emptyResult = Result emptyImage [] [] [] emptyBorders

-- | The type of events.
data BrickEvent n e = VtyEvent Event
                    -- ^ The event was a Vty event.
                    | AppEvent e
                    -- ^ The event was an application event.
                    | MouseDown n Button [Modifier] Location
                    -- ^ A mouse-down event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    | MouseUp n (Maybe Button) Location
                    -- ^ A mouse-up event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    deriving (Show, Eq, Ord)

data RenderState n =
    RS { viewportMap :: M.Map n Viewport
       , rsScrollRequests :: [(n, ScrollRequest)]
       , observedNames :: !(S.Set n)
       , renderCache :: M.Map n (Result n)
       , clickableNames :: [n]
       }

-- | The rendering context. This tells widgets how to render: how much
-- space they have in which to render, which attribute they should use
-- to render, which bordering style should be used, and the attribute map
-- available for rendering.
data Context =
    Context { ctxAttrName :: AttrName
            , availWidth :: Int
            , availHeight :: Int
            , ctxBorderStyle :: BorderStyle
            , ctxAttrMap :: AttrMap
            , ctxBorderDynamics :: EdgeAnnotation JoinStyle
            }
            deriving Show

suffixLenses ''RenderState
suffixLenses ''VisibilityRequest
suffixLenses ''CursorLocation
makeLenses ''Viewport
