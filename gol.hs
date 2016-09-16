module Main where

import qualified Data.Vector.Unboxed as V
import System.Random (randomIO)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main :: IO ()
main = do
    vector <- V.generateM (16*16) (const randomIO :: a ->IO Bool)
    _ <- getArgsAndInitialize
    _ <- createWindow "GOL1.-5"
    let vertices :: [(GLfloat, GLfloat, GLfloat)]
        vertices = concatMap (uncurry $ makeSquare 16 16) $ indexOfTrue (V.toList vector)
    displayCallback $= do
        clear [ColorBuffer]
        render vertices
        swapBuffers
    mainLoop

render :: VertexComponent a => [(a, a, a)] -> IO ()
render vertices = renderPrimitive Triangles $
    mapM_ (vertex . uncurry3 Vertex3) vertices

indexOfTrue :: [Bool] -> [(Integer, Integer)]
indexOfTrue = map fst . filter snd . zip [(x, y) | y <- [0..15], x <- [0..15]]

makeSquare :: Integer -> Integer -> Integer -> Integer -> [(GLfloat, GLfloat, GLfloat)]
makeSquare maxx maxy x y = map toNegative [
  (rx, ry, 0),
  (rx + deltax, ry, 0),
  (rx + deltax, ry + deltay, 0),
  (rx + deltax, ry + deltay, 0),
  (rx, ry + deltay, 0),
  (rx, ry, 0)]
  where
    rx = fromIntegral x / fromIntegral maxx
    ry = fromIntegral y / fromIntegral maxy
    deltax = 1.0 / fromIntegral maxx 
    deltay = 1.0 / fromIntegral maxy 
    toNegative (a, b, c) = (a*2 - 1.0, b*2 - 1.0, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

--loop som rngar varje gång -> faktisk logik
