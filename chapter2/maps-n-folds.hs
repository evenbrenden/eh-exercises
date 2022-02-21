fr = \f g -> foldr g 0 . map f
gr = \f g -> foldr (g . f) 0
fl = \f g -> foldl g 0 . map f
gl = \f g -> foldl (g . f) 0

a = fr (+ 1) (-) [1 .. 3] -- 3
b = gr (+ 1) (-) [1 .. 3] -- 3
c = fl (+ 1) (-) [1 .. 3] -- -9
d = gl (+ 1) (-) [1 .. 3] -- -3
