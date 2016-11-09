module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Triangle"
    displayCallback $= do
        clear [ColorBuffer]
        triangle
        flush
    mainLoop

triangle :: IO ()
triangle = renderPrimitive Polygon $ do
    let vertices :: [(GLfloat, GLfloat, GLfloat)]
        vertices = [(0,0,0), (1,0,0), (1,1,0), (0,1,0)]
    mapM_ (vertex . uncurry3 Vertex3) vertices

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

--vec4 :: Int a => ([a], [a], [a], [a])
--next4 ((n:ns), (e:es), (s:ss), (w:ws)) = (ns, es, ss, ws) -- oob
--head4 ((n:_), (e:_), (s:_), (w:_)) = (n, e, s, w)
