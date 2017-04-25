main = do
  s <- getContents
  let mierdas=lines(s)
      (caca:caca1:cacas)=mierdas
  putStr (s)
  print("el arbolin "++caca1)
