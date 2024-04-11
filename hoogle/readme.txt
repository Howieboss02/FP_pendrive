Commands assume you are running them inside this directory.
Also, unzip database.hoo before you try to do anything.

Run cli:
./hoogle --database ./database.hoo <your query>
./hoogle --info --database ./database.hoo <your query>

Rtart server on "http://localhost:8080/":
./hoogle server ./database.hoo <your query>

GHCI integration (you can add whatever args you want):
:def hoogle \x -> return $ ":!./hoogle --database ./database.hoo \"" ++ x ++ "\""

I also have "D:\Programs\Programming\Haskell-Stack\bin" in my PATH, idk if it matters.
