f :: [a] -> [IO a]
f xs = return <$> xs

g :: [IO a] -> IO [a]
g = mapM id

h :: [IO a] -> IO [a]
h = sequence
