{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.Loops (whileM_)
import Data.Functor ((<$>))
import qualified Data.Vector.Storable as VS
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Comonad
import Control.Monad (forM, replicateM, when)
import System.Random
import Data.IORef
import Data.Word
import Control.Concurrent
import System.Exit
import Data.Maybe

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Low
import Linear

data Life a = Life
  { lifeWidth  :: Int
  , lifeCursorUnpacked :: (Int,Int,Int)
  , lifeCursorPacked :: Int
  , lifeCells  :: Vector a }
    deriving (Show)

data Rule = Rule
  { lowerLimit :: Int
  , upperLimit :: Int }
    deriving Show

unpack :: Int -> Int -> (Int,Int,Int)
unpack c w = (x,y,z) where
  (z,r) = c `divMod` (w*w)
  (y,x) = r `divMod` w

pack :: Int -> Int -> Int -> Int -> Int
pack x y z w = x + y*w + z*w*w

instance Functor Life where
  fmap f (Life w c c' cells) = Life w c c' (fmap f cells)

instance Comonad Life where
  extract (Life _ _ c cells) = cells ! c
  duplicate (Life w (x,y,z) c cells) = Life w (x,y,z) c cells' where
    cells' = V.imap (\i cell -> (Life w (unpack i w) i cells)) cells
  
forLife :: Monad m => Life a -> ((Int,Int,Int) -> a -> m b) -> m [b]
forLife (Life w _ _ cells) visit = do
  let range = [0 .. w-1]
  forM [(x,y,z) | x <- range, y <- range, z <- range] $ \(x,y,z) -> do
    let cell = cells ! (pack x y z w)
    visit (x,y,z) cell

lifeStep :: Rule -> Life Bool -> Life Bool
lifeStep rule l = l =>> \(Life w (x,y,z) _ cells) ->
  lifeRule rule (countNeighbors cells w (x,y,z))

lifeRule :: Rule -> Int -> Bool
lifeRule (Rule l u) n = l < n && n < u

countNeighbors :: Vector Bool -> Int -> (Int,Int,Int) -> Int
countNeighbors cells w (x,y,z) = let f a b c = rawCheck cells w x y z a b c in
  f 0 0 0 +
  f 0 0 1 +
  f 0 0 2 +
  f 0 1 0 +
  f 0 1 1 +
  f 0 1 2 +
  f 0 2 0 +
  f 0 2 1 +
  f 0 2 2 +
  f 1 0 0 +
  f 1 0 1 +
  f 1 0 2 +
  f 1 1 0 +
  --f 1 1 1 +
  f 1 1 2 +
  f 1 2 0 +
  f 1 2 1 +
  f 1 2 2 +
  f 2 0 0 +
  f 2 0 1 +
  f 2 0 2 +
  f 2 1 0 +
  f 2 1 1 +
  f 2 1 2 +
  f 2 2 0 +
  f 2 2 1 +
  f 2 2 2

rawCheck cells w x y z a b c = answer where
  x' = (x - 1 + a) `mod` w
  y' = (y - 1 + b) `mod` w
  z' = (z - 1 + c) `mod` w
  answer = if cells ! (pack x' y' z' w) then 1 else 0

genCells :: Int -> IO (Life Bool)
genCells w = do
  let range = [0 .. w-1]
  cellData <- replicateM (w*w*w) randomIO
  return $ Life w (0,0,0) 0 (V.fromList cellData)

population :: Life Bool -> Int
population _ = 1

lifeSize = 8
rule10_15 = Rule 10 15
rule9_15 = Rule 9 15
rule8_15 = Rule 8 15
rule8_16 = Rule 8 16
rule9_16 = Rule 9 16
rule7_15 = Rule 7 15

-- GLFW will be the shell of the demo
main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  mwin <- GLFW.createWindow 800 600 "3D Life" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      l0 <- genCells lifeSize
      ref <- newIORef l0
      setup
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        t <- (realToFrac . fromJust) <$> GLFW.getTime
        print t
        draw t =<< readIORef ref
        GLFW.swapBuffers win
        --count <- population <$> readIORef ref
        --when (count == 0) exitSuccess
        modifyIORef ref (lifeStep (Rule 3 7))

setup = do
  vao <- newVAO
  bindVAO vao

  vsource <- readFile "life.vert"
  fsource <- readFile "life.frag"
  prog <- newProgram vsource fsource
  useProgram prog
  let camera' = lookAt (V3 (-0.75) 0.75 (-0.75)) 0 (V3 0 1 0) 
  let persp = perspective (pi/2) (800.0/600.0) 0.1 50
  let camera = persp !*! camera'
  setUniform44 "camera" [transpose camera]
  setUniform1f "cellsPerEdge" [fromIntegral lifeSize]

  let blob = VS.fromList
        [ 0, 0, 0, 0.1
        , 0, 0, 1, 0.2
        , 0, 1, 0, 0.3
        , 0, 1, 1, 0.4
        , 1, 0, 0, 0.5
        , 1, 0, 1, 0.6
        , 1, 1, 0, 0.7
        , 1, 1, 1, 0.8 ] :: VS.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  setVertexLayout [ Attrib "position" 3 GLFloat
                  , Attrib "weight"   1 GLFloat ]

  let blob = VS.fromList -- a cube has 12 triangles
        [ 0, 1, 2
        , 3, 2, 1
        , 4, 6, 5
        , 7, 5, 6
        , 0, 4, 1
        , 5, 1, 4
        , 2, 3, 6
        , 7, 6, 3
        , 2, 6, 4
        , 0, 2, 4
        , 3, 1, 5
        , 7, 3, 5
        ] :: VS.Vector Word8
  ele <- newElementArray blob StaticDraw
  bindElementArray ele
  enableDepthTest

draw t life = do
  clearColorBuffer (0,0,0)
  clearDepthBuffer
  setUniform1f "time" [t]
  forLife life $ \(i,j,k) on -> do
    when on $ do
      let x = fromIntegral i
      let y = fromIntegral j
      let z = fromIntegral k
      setUniform3f "xyz" [V3 x y z]
      drawIndexedTriangles 36 UByteIndices
      
