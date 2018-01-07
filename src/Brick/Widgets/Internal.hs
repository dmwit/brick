{-# LANGUAGE BangPatterns #-}
module Brick.Widgets.Internal
  ( renderFinal
  , cropToContext
  , cropResultToContext
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Lens.Micro ((^.), (&), (%~))
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Data.Maybe (catMaybes)
import qualified Graphics.Vty as V
import qualified Data.Map as M
import qualified Data.Set as S

import Brick.Types
import Brick.Types.Internal
import Brick.AttrMap
import Brick.Widgets.Border.Style

renderFinal :: AttrMap
            -> [Widget n]
            -> V.DisplayRegion
            -> ([CursorLocation n] -> Maybe (CursorLocation n))
            -> RenderState n
            -> (RenderState n, V.Picture, Maybe (CursorLocation n), [Extent n])
renderFinal aMap layerRenders sz chooseCursor rs = (newRS, picWithBg, theCursor, concat layerExtents)
    where
        (layerResults, !newRS) = flip runState rs $ sequence $
            (\p -> runReaderT p ctx) <$>
            (render <$> cropToContext <$> layerRenders)
        ctx = Context mempty (fst sz) (snd sz) defaultBorderStyle aMap defaultBorderDynamics
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.imageL) <$> layerResults
        -- picWithBg is a workaround for runaway attributes.
        -- See https://github.com/coreyoconnor/vty/issues/95
        picWithBg = pic { V.picBackground = V.Background ' ' V.defAttr }
        layerCursors = (^.cursorsL) <$> layerResults
        layerExtents = reverse $ (^.extentsL) <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

-- | After rendering the specified widget, crop its result image to the
-- dimensions in the rendering context.
cropToContext :: Widget n -> Widget n
cropToContext p =
    Widget (hSize p) (vSize p) (render p >>= cropResultToContext)

cropResultToContext :: Result n -> RenderM n (Result n)
cropResultToContext result = do
    c <- getContext
    return $ result & imageL   %~ cropImage   c
                    & cursorsL %~ cropCursors c
                    & extentsL %~ cropExtents c
                    & bordersL %~ cropBorders c

cropImage :: Context -> V.Image -> V.Image
cropImage c = V.crop (max 0 $ c^.availWidthL) (max 0 $ c^.availHeightL)

cropCursors :: Context -> [CursorLocation n] -> [CursorLocation n]
cropCursors ctx cs = catMaybes $ cropCursor <$> cs
    where
        -- A cursor location is removed if it is not within the region
        -- described by the context.
        cropCursor c | outOfContext c = Nothing
                     | otherwise      = Just c
        outOfContext c =
            or [ c^.cursorLocationL.locationRowL    < 0
               , c^.cursorLocationL.locationColumnL < 0
               , c^.cursorLocationL.locationRowL    >= ctx^.availHeightL
               , c^.cursorLocationL.locationColumnL >= ctx^.availWidthL
               ]

cropExtents :: Context -> [Extent n] -> [Extent n]
cropExtents ctx es = catMaybes $ cropExtent <$> es
    where
        -- An extent is cropped in places where it is not within the
        -- region described by the context.
        --
        -- If its entirety is outside the context region, it is dropped.
        --
        -- Otherwise its size and upper left corner are adjusted so that
        -- they are contained within the context region.
        cropExtent (Extent n (Location (c, r)) (w, h) (Location (oC, oR))) =
            -- First, clamp the upper-left corner to at least (0, 0).
            let c' = max c 0
                r' = max r 0
                -- Compute deltas for the offset since if the upper-left
                -- corner moved, so should the offset.
                dc = c' - c
                dr = r' - r
                -- Then, determine the new lower-right corner based on
                -- the clamped corner.
                endCol = c' + w
                endRow = r' + h
                -- Then clamp the lower-right corner based on the
                -- context
                endCol' = min (ctx^.availWidthL) endCol
                endRow' = min (ctx^.availHeightL) endRow
                -- Then compute the new width and height from the
                -- clamped lower-right corner.
                w' = endCol' - c'
                h' = endRow' - r'
                e = Extent n (Location (c', r')) (w', h') (Location (oC + dc, oR + dr))
            in if w' < 0 || h' < 0
               then Nothing
               else Just e

cropBorders :: Context -> EdgeAnnotation DynamicBorder -> EdgeAnnotation DynamicBorder
cropBorders ctx bs = cropFunctions <*> bs where
    cropFunctions = EdgeAnnotation
        { eaTop    = cropDB 0      cols
        , eaBottom = cropDB maxRow cols
        , eaLeft   = cropDB 0      rows
        , eaRight  = cropDB maxCol rows
        }

    -- Keep only the keys from 0 (inclusive) to the indicated size (exclusive)
    cropMap :: (Num k, Ord k) => k -> M.Map k v -> M.Map k v
    cropMap size m =
        let (_, m')  = M.split (-1) m
            (m'', _) = M.split size m'
        in m''

    -- Keep only the values from 0 (inclusive) to the indicated size (exclusive)
    cropSet :: (Num a, Ord a) => a -> S.Set a -> S.Set a
    cropSet size s =
        let (_, s')  = S.split (-1) s
            (s'', _) = S.split size s'
        in s''

    -- Given a maximum size, the offset that a segment starts at, and the
    -- segment itself, crop the segment to the range [0, size-1]. Returns
    -- Nothing if there is nothing left of the segment.
    cropSegment :: Int -> (Int, AcceptJoinSegment) -> Maybe (Int, AcceptJoinSegment)
    cropSegment size (start, ajs)
        | start + jsLength ajs - 1 < 0 = Nothing
        | start >= size = Nothing -- should be impossible to hit this case
        | otherwise = Just . (,) newStart $ ajs
            { jsLength = min size (start + jsLength ajs) - newStart
            , jsInnerJoinPoints = cropSet size
                                . (if startChanged then S.mapMonotonic ((start-newStart)+) else id)
                                $ jsInnerJoinPoints ajs
            , jsStartInward  = (if startChanged then jsMiddleInward  else jsStartInward ) ajs
            , jsStartOutward = (if startChanged then jsMiddleOutward else jsStartOutward) ajs
            , jsStartBoth    = (if startChanged then jsMiddleBoth    else jsStartBoth   ) ajs
            , jsEndInward    = (if endChanged   then jsMiddleInward  else jsEndInward   ) ajs
            , jsEndOutward   = (if endChanged   then jsMiddleOutward else jsEndOutward  ) ajs
            , jsEndBoth      = (if endChanged   then jsMiddleBoth    else jsEndBoth     ) ajs
            }
        where
        newStart = max 0 start
        startChanged = start /= newStart
        endChanged = size < start + jsLength ajs

    -- Ideally, we would just keep the keys that are in the right range and be
    -- done. But this is not quite right for two reasons:
    --
    -- 1. There may be a segment that starts at a coordinate below 0, but is
    --    long enough to extend into the non-negative coordinates. We must cut
    --    such a segment in half and include it at position 0 in the cropped
    --    segment map.
    -- 2. There may be a segment that starts at a coordinate that's within
    --    range, but extends out of range. We must again cut such a segment in
    --    half, and update it without changing its key.
    --
    -- There is an edge case where a segment may start at a coordinate below 0
    -- and extend beyond the end of the cropping region. Therefore cropSegment
    -- above needs to be prepared to handle segments that extend both below 0
    -- and above the cropping region. So we can reuse it for the "cut a segment
    -- in half" operation in both parts (1) and (2); in the edge case where
    -- there is a long segment at negative coordinate, we'll do a tiny bit of
    -- duplicated work by calling it twice when just once would do.
    --
    -- We rely on the invariant that segment maps do not encode overlapping
    -- segments here: we insert two extra cropped segments at the end without
    -- checking that we might be overwriting something.
    cropSegmentMap :: Int -> M.Map Int AcceptJoinSegment -> M.Map Int AcceptJoinSegment
    cropSegmentMap size m =
        let extraInitialVal = M.lookupLT 0    m >>= cropSegment size
            extraFinalVal   = M.lookupLT size m >>= cropSegment size
            maybeInsert = maybe id (uncurry M.insert)
        in m & cropMap size
             & maybeInsert extraInitialVal
             & maybeInsert extraFinalVal

    -- Given an exact location where the border should be drawn, together with
    -- the length of the available space for the border, crop the border. If
    -- the border wasn't already in the right exact location, delete it
    -- outright.
    cropDB :: Int -> Int -> DynamicBorder -> DynamicBorder
    cropDB coord size db
        | (db ^. coordinateL) /= coord = DynamicBorder M.empty M.empty coord
        | otherwise = db & offersL    %~ cropMap size
                         & acceptorsL %~ cropSegmentMap size

    rows = ctx ^. availHeightL
    cols = ctx ^. availWidthL
    maxRow = rows - 1
    maxCol = cols - 1
