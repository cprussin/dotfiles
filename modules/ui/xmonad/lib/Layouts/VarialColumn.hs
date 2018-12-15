{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

module Layouts.VarialColumn where

import qualified XMonad.StackSet as W
import XMonad hiding (tile, splitVertically, splitHorizontallyBy, Expand, Shrink)
import XMonad.Util.XUtils

import qualified Data.Sequence as S
import Data.Sequence ( (><), (|>) )
import qualified Data.Foldable as F
import Data.Maybe
import Data.Int (Int32)
import Data.Ratio ((%))
import Data.List (elemIndex)

import qualified Debug.Trace as D

data Axis = V | H deriving (Read, Show)

type Col = S.Seq Rational
data Cols = Cols (S.Seq Rational) (S.Seq Col) deriving (Read, Show)
-- Layout State

data LS a = LS
          {
            cols :: Cols,
            -- ^ where the splits are

            gap :: Int,
            -- ^ gap between wins

            balanceColumns :: Int,
            -- ^ when we get to this many columns, things are rebalanced automatically
            insertColumns :: Int,
            -- ^ when there are new windows, we insert columns up to
            -- this many (so if this was 3, the layout would prefer 3
            -- column)

            draggers :: [WDragger],
            -- ^ internal storage of drag handles
            lastContent :: [a]
            -- ^ what windows did we show last; this is to make new
            -- windows appear where wanted.
          } deriving (Read, Show)

data Dragger = WindowDragger Position Dimension Int Int |
               ColumnDragger Position Dimension Int
             deriving (Read, Show)

type WDragger = (Window, Dragger)

-- default layout that you may want to use
varial = LS { cols = (Cols S.empty S.empty),
              gap = 5,
              balanceColumns = 1,
              insertColumns = 2,
              draggers = [],
              lastContent = []
              }

ins :: S.Seq a -> Int -> S.Seq a -> S.Seq a
ins a n s = ((S.take n s) >< a) >< (S.drop n s)

-- remove a 1 and reweight the remainder
del1 :: Int -> S.Seq Rational -> S.Seq Rational
del1 n s =
  let k = (S.index s n)/(fromIntegral $ (S.length s) - 1) in
    norm $
    fmap (+ k) $
    del n s

norm :: S.Seq Rational -> S.Seq Rational
norm s = fmap scale s
  where
    scale :: Rational -> Rational
    scale x = (max 0.05 (x / sum))
    sum = F.sum s

del n s = (S.take n s) >< (S.drop (1 + n) s)

ins1 :: Int -> S.Seq Rational -> S.Seq Rational
ins1 n s = norm $ ins newthings n s
  where newthings = (S.singleton $ if S.null s then 1 else (1 % (fromIntegral $ S.length s)))

insc = ins $ S.singleton $ newCol 1

newCol n = S.fromList $ replicate n 1

minimumSize :: Rational
minimumSize = 0.05

-- resize the thing at the position to have the given size in a way
-- that preserves the total and doesn't make anything less than 0.1
changeS :: Int -> Rational -> S.Seq Rational -> S.Seq Rational
changeS n r' s
  | null s = s
  | length s == 1 = s
  | otherwise = let lim = minimumSize
                    a0 = S.index s n
                    a1 = S.index s (n+1)
                    r = min (a0 + a1 - lim) $ max r' lim in
                  norm $ S.update n r $ S.update (n+1) (a1 - (r - a0)) s

-- insert a new column at n
insertCol :: Int -> LS a -> LS a
insertCol n s@(LS {cols = (Cols rs cs)}) = s { cols = cols' }
  where cols' = Cols (ins1 n rs) (insc n cs)

-- insert a new row in column c at existing row n
-- however, if that would make column 1 too big, insert it into the next smallest column
insertRow :: Int -> Int -> LS a -> LS a
insertRow c r s@(LS {cols = (Cols rs cs)}) = s { cols = cols' }
  where cols' = Cols rs $ S.adjust (\x -> ins1 r x) c cs

-- delete a row in column c at existing row r
deleteRow :: Int -> Int -> LS a -> LS a
deleteRow c r s@(LS {cols = (Cols rs cs)}) = s {cols = cols'}
  where (rs', cs') =
          if S.length (S.index cs c) == 1 then
            (del1 c rs, del c cs)
          else
            (rs, S.adjust (\x -> del1 r x) c cs)
        cols' = Cols rs' cs'

popRowToColumn :: Int -> Int -> LS a -> LS a
popRowToColumn c r ls = insertCol c $ deleteRow r c ls

balance :: LS a -> LS a
balance s@(LS {cols = (Cols rs cs), balanceColumns = bc })
  | (bc >= S.length cs)= -- any row with more than one window can be popped out
      let splitCol = S.findIndexL ((> 1) . S.length) cs in
        case splitCol of
          Just c -> balance $ popRowToColumn c 0 s
          Nothing -> s
  | otherwise = s

times :: Int -> (a -> a) -> (a -> a)
times n f = (!! n) . iterate f

timesX :: Int -> X () -> X ()
timesX n a
  | n <= 0 = return ()
  | otherwise = timesX (n - 1) a >> a

-- also need to handle the deletion of a row / column. the
-- values need rebalancing to equal the total, or the next window needs to get it.

-- LAYOUT ALGO
layout :: LS a -> Rectangle -> W.Stack a -> ([(a, Rectangle)], Maybe (LS a))
layout ls screen stack =
  let ls'    = update ls stack
      rects  = windowRects (fromMaybe ls ls') screen stack in
    (rects, ls')

-- handle windows added/removed
update :: LS a -> W.Stack a -> Maybe (LS a)
update
  ls@(LS { cols = columns@(Cols rs cs) ,
           insertColumns = ic })
  stack@(W.Stack f u d)
  | newCount < oldCount =
    Just $ balance $ times (oldCount - newCount) (deleteRow delFromC delFromR) ls
  | (newCount > oldCount) && (ic > numColumns) = -- windows added, stick them all in a column and balance
    Just $ balance $ times (newCount - oldCount - 1) (insertRow focusCol 0) $ insertCol focusCol ls
  | newCount > oldCount = -- windows added, but no columns spare, insert some rows
    Just $ times (newCount - oldCount) (insertRow focusCol focusRow) ls
  | otherwise = Nothing
  where
    numColumns = S.length cs
    newCount = length $ W.integrate stack
    oldCount = windowCount ls
    (focusCol, focusRow) = fromMaybe (0,0) $ findWindow (length u) cs
    (delFromC, delFromR) = fromMaybe (0,0) $ findWindow (max 0 (1 + (length u) -
                                              oldCount + newCount)) cs

findWindow :: Int -> S.Seq Col -> Maybe (Int, Int)
findWindow n cs
  | n >= length coordinates = Nothing
  | otherwise = Just $ coordinates !! n
  where coordinates = [(col, row) | (col, rows) <- zip [0..] (F.toList cs),
                                    row <- take (S.length rows) [0..]]

windowCount :: LS a -> Int
windowCount LS { cols = (Cols _ a) } = F.sum $ fmap S.length a

-- put the windows in the screen rectangle
windowRects :: LS a -> Rectangle -> W.Stack a -> [(a, Rectangle)]
-- step 1: cut the big rectangle into column rectangles
-- step 2: cut each column rectangle into row rectangles
-- step 3: zip the rectangles with the windows
windowRects (LS { cols = (Cols ws rs), gap = g }) screen stack =
  let allWindows = W.integrate stack
      colRectangles = cutup H g screen ws
      winRectangles = concatMap id $ zipWith (cutup V g) colRectangles $ F.toList rs
  in zip allWindows winRectangles

-- chop a rectangle up into subrectangles on its axis, with gaps between them
-- handles choice of axis by swapping coordinates and then unswapping them
-- which is a bit ugly.
cutup :: Axis -> Int -> Rectangle -> S.Seq Rational -> [Rectangle]
cutup ax g screen slices
  | S.null slices = []
  | S.length slices == 1 = [screen]
  | otherwise = F.toList rects
  where
    total :: Rational
    total   = F.sum slices
    slices' :: S.Seq Rational
    slices' = S.scanl (+) 0 $ fmap (flip (/) total) slices
    screen' = rot ax screen
    rects   = fmap ((rot ax) . (sub screen')) $ S.zip slices' $ S.drop 1 slices'

    rot V r = r
    rot H (Rectangle x y w h) = Rectangle y x h w

    sub :: Rectangle -> (Rational, Rational) -> Rectangle
    sub (Rectangle x y w h) (l, r) = Rectangle x y' w h'
      where
        g' = if l > 0 then g else 0
        h' :: Dimension
        h' = (floor ((fromIntegral h) * (r - l))) - (fromIntegral g')
        y' :: Position
        y' = (floor  ((fromIntegral y) + (fromIntegral h) * l)) + (fromIntegral g')

-- the instances which do the stuff in the X monad

splitPlaces :: [a] -> [Int] -> [[a]]
splitPlaces _ [] = []
splitPlaces as (0:is) = splitPlaces as is
splitPlaces as (i:is) = a0:(splitPlaces a1 is)
  where (a0, a1) = splitAt i as

data Msg =
  ToNewColumn  Window |
  OccupyMaster Window |
  UpOrLeft Window |
  DownOrRight Window |
  SetColumn Int Rational |
  SetRow Int Int Rational |
  GrabRow Window |
  GrabColumn Window |
  EqualizeColumn Rational Window |
  Embiggen Rational Rational Window

  deriving (Read, Show, Typeable)

  -- mouse drag messages
instance Message Msg

instance LayoutClass LS Window where
  description (LS {cols = (Cols rs cs)}) = (show $ S.length cs) ++ "col"

  doLayout state screen stack = do
    let (ws, statem) = layout state screen stack
    destroyDraggers state
    state' <- addDraggers screen ws (fromMaybe state statem)
    let state'' = state'
               { lastContent = W.integrate stack }
    return (ws, Just state'')

  handleMessage state@(LS {cols = (Cols rs cs), lastContent = lcon}) msg
    -- row/column operations (shift column, move window out of column or into column, eat column)
    -- todo: tidy up and wrap around
    | (Just (DownOrRight w)) <- fromMessage msg = findWindowAnd w goDownOrRight
    | (Just (UpOrLeft w)) <- fromMessage msg = findWindowAnd w goUpOrLeft
    | (Just (ToNewColumn  w)) <- fromMessage msg =
        findWindowAnd w $ \(col,row,index) ->
                            do focus w
                               timesX row (windows $ W.swapUp)
                               return $ Just $ insertCol col $ deleteRow col row state

    | (Just (OccupyMaster w)) <- fromMessage msg =
        do focus w
           windows $ W.shiftMaster
           let n = (S.length $ S.index cs 0) - 1
           return $ Just $ (times n $ deleteRow 0 0) $ (times n $ insertRow 1 0) state

    | (Just (GrabColumn w)) <- fromMessage msg =
        findWindowAnd w $ \(col,row,_) ->
                            return $ Just $ state { cols = (Cols rs (S.adjust (grab row) col cs)) }

    | (Just (GrabRow w)) <- fromMessage msg =
        findWindowAnd w $ \(col,_ , _) ->
                            return $ Just $ state { cols = (Cols (grab col rs) cs) }

    | (Just (EqualizeColumn gen w)) <- fromMessage msg =
        findWindowAnd w $ \(col, _, _) ->
                            return $ Just $ state { cols = (Cols rs ((S.update
                                                                      col
                                                                      (norm $
                                                                       S.fromList $
                                                                       take (S.length $ S.index cs col) $
                                                                       iterate (* gen) 1)
                                                                      cs)))}
    | (Just (Embiggen dr dc w)) <- fromMessage msg =
        let change q s a = norm $ S.adjust (+ q) s $ fmap ((flip (-)) (q / (fromIntegral $ S.length a))) a
        in
        findWindowAnd w $ \(col, row, _) ->
                            return $ Just $ state
                               { cols = (Cols
                                         (change dr col rs)
                                         (S.adjust (change dc row) col cs)) }


    | (Just (SetColumn c r)) <- fromMessage msg =
        let rs' = changeS c r rs in
          return $ Just $ state { cols = (Cols rs' cs) }

    | (Just (SetRow c r ra)) <- fromMessage msg =
        let cs' = S.adjust (changeS r ra) c cs in
            return $ Just $ state { cols = (Cols rs cs') }

    | (Just Hide) <- fromMessage msg =
        destroyDraggers state >> return (Just $ state { draggers = [], lastContent = [] })

    | (Just ReleaseResources) <- fromMessage msg =
        destroyDraggers state >> return (Just $ state { draggers = [], lastContent = [] })

    | (Just e) <- fromMessage msg :: Maybe Event =
        handleResize e (draggers state) >> return Nothing

    -- resize operations (set fraction, expand in direction)
    | otherwise = return Nothing
    where lastCol = (S.length cs) - 1
          -- this is not sufficient because our layout may be in a modifier which is hiding some windows.
          findWindowAnd :: Window -> ((Int, Int, Int) -> X (Maybe (LS Window))) -> X (Maybe (LS Window))
          findWindowAnd w f = do st <- gets (W.stack . W.workspace . W.current . windowset)
                                 let maybeArgs = do stack@(W.Stack f u d) <- st
                                                    windowIndex <- elemIndex w lcon
--                                                                   if f == w then Just $ length u
--                                                                   else elemIndex w $ W.integrate stack
                                                    (c, r) <- findWindow windowIndex cs
                                                    return (c, r, windowIndex)
                                 case maybeArgs of
                                   Just a -> f a
                                   _ -> return Nothing

          goUpOrLeft (col, row, index) =
            let rowlength = S.length $ S.index cs col
                leftmost = col == 0
                prmax = S.length $ S.index cs (col - 1)
                result
                  | row > 0 = (windows $ W.swapUp) >> return Nothing
                  | rowlength == 1 && not leftmost = return $ Just $ insertRow (col - 1) (prmax - 1) $ deleteRow col row state
                  | otherwise = return $ Just $ insertCol col $ deleteRow col row state
            in result

          goDownOrRight (col, row, index) =
            let rowlength = S.length $ S.index cs col
                rightmost = col + 1 == S.length cs
                result
                  | row + 1 < rowlength = (windows $ W.swapDown) >> return Nothing
                  | rowlength == 1 && not rightmost = return $ Just $ insertRow col 0 $ deleteRow col row state
                  | otherwise = return $ Just $ deleteRow col row $ insertCol (col + 1) state
            in result

          grab :: Int -> S.Seq Rational -> S.Seq Rational
          grab n s = let k = S.length s
                         small = minimumSize * 1.5 in
                       S.update n ((1%1) - (fromIntegral k) * small) $
                       S.fromList $
                       replicate k small

dragHandler :: Dragger -> X ()
dragHandler (ColumnDragger x0 d c) =
  flip mouseDrag (return ()) $
  \x _ -> sendMessage $ --DT.traceShow (p, d, c, x) $
          SetColumn c ((fromIntegral (x - x0) :: Integer) % (fromIntegral d :: Integer))

dragHandler (WindowDragger y0 d c r) =
  flip mouseDrag (return ()) $
  \_ y -> sendMessage $ -- DT.traceShow (y0, y, d, c, r) $
          SetRow c r ((fromIntegral (y - y0) :: Integer) % (fromIntegral d :: Integer))

handleResize :: Event -> [WDragger] -> X ()
handleResize ButtonEvent { ev_window = ew, ev_event_type = et } draggers
  | et == buttonPress, Just x <- lookup ew draggers = dragHandler x
  | otherwise = return ()

handleResize _ _ = return ()

destroyDraggers :: LS (Window) -> X ()
destroyDraggers x = mapM_ (deleteWindow . fst) $ draggers x

addDraggers :: Rectangle -> [(Window, Rectangle)] -> LS Window -> X (LS Window)
addDraggers (Rectangle sx sy sw sh) ws state@(LS { cols = (Cols rs cs) }) =
  -- chop ws into piles
  -- each pile except the last produces a dragger for its right hand side
  -- each window in a pile except the last produces a dragger for its lower edge
  let g = (fromIntegral $ (gap state)) :: Dimension

      windowColumns = zip [0..] $ splitPlaces ws $ map S.length $ F.toList cs

      columnDraggers bw = map (columnDragger bw) $ drop 1 $ reverse windowColumns
      columnDragger bw (col, ((_,(Rectangle wx wy ww wh)):_)) =
        let x = (wx + (fromIntegral ww)) in
          ((ColumnDragger wx sw col), (Rectangle (x - (fromIntegral bw)) sy (g + 2 * bw) sh))

      -- windowDragger :: (a, Rectangle) -> (a, Rectangle)
      -- windowDragger (w, ) =

      windowDraggers bw = (flip concatMap) windowColumns $
                          \(column, windows) -> map (windowDragger bw column) $
                                                drop 1 $ reverse $ zip [0 .. ] windows

      windowDragger :: Dimension -> Int -> (Int, (Window, Rectangle)) -> (Dragger, Rectangle)
      windowDragger bw c (r, (_, (Rectangle wx wy ww wh))) =
        let y = (wy+(fromIntegral wh)) in
          ((WindowDragger wy sh c r), (Rectangle wx (y - (fromIntegral bw)) ww (g + 2 * bw)))

      createDragger :: (Dragger, Rectangle) -> X (WDragger)
      createDragger (d, r) = do w <- makeDraggerWindow (glyphFor d) r
                                return (w, d)

      glyphFor (WindowDragger _ _ _ _) = xC_sb_v_double_arrow
      glyphFor (ColumnDragger _ _ _) = xC_sb_h_double_arrow

      makeDraggerWindow :: Glyph -> Rectangle -> X Window
      makeDraggerWindow g (Rectangle x y w h) = withDisplay $ \d -> do
        win <- do
          rw <- asks theRoot
          let screen = defaultScreenOfDisplay d
              visual = defaultVisualOfScreen screen
              attrmask = cWOverrideRedirect
          io $ allocaSetWindowAttributes $ \a -> do
            set_override_redirect a True
            createWindow d rw x y w h 0 0 inputOnly visual attrmask a

        io $ selectInput d win (exposureMask .|. buttonPressMask)
        cursor <- io $ createFontCursor d g
        io $ defineCursor d win cursor
        io $ freeCursor d cursor
        showWindow win
        return win
  in
    do bw <- asks (borderWidth . config)
       draggers <- mapM createDragger ((columnDraggers bw) ++ (windowDraggers bw))
       return state { draggers = draggers }
