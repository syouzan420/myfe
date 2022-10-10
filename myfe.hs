import System.IO(IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr,
                 hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char(isDigit)
import Useful(getIndex,sepChar,joinChar,sorting,toList,isNum,isChar,isStr)
import Mydate(today,howLong,hmDays,isDay,addDay)
import Mydata
import Myfile(fileIn,fileOut,isFile)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  e <- isFile 
  c <- if e then fileOut
       else fileIn "" >> return ""
  sLoop c

sLoop :: Contents -> IO ()
sLoop c = do
  tdy <- today
  mapM_ putStrLn (showData c)
  mapM_ putStrLn (showTodo c tdy)
  o <- comLoop "" "" 0 "" demands
  putStrLn o
  let (f:ord) = o
      cs = lines c
  nc <- case f of
    f' | f'=='a' || f'=='s' -> do
      b <- confirm "add"
      let ord' = if (f'=='s') then 's':ord else
                 if ((head ord)=='t') then 
                   ord++(joinChar ',' (map show (conTodo$last$sepChar ';' ord)))++";" else  ord
          dpt = if((head ord)=='t') then 2 else 1
          s = idSame dpt ord' cs
          im = head ord == 'm'
      cs' <- if (b && s>(-1) && not im) then do
               putStrLn "There is a data of the same name. "
               putStrLn (show (cs!!s))
               r <- confirm "replace"
               if r then return$replOrd s ord' cs else return cs
                    else return cs
      if b then do
              let nc = unlines (cs' ++ (if (s>(-1) && not im) then [] else [ord']))
              fileIn nc
              putStrLn "wrote to myfe.txt. success!"
              return nc
           else putStrLn "add data -- canceled." >> return c
    'd' -> do
      let dpt = if((head ord)=='t') then 2 else 1
          s = idSame dpt ord cs
      cs' <- if (s>(-1)) then do
              putStrLn "The target data is"
              putStrLn (show (cs!!s))
              b <- confirm "delete"
              if b then return$delOrd s cs else return cs
                   else putStrLn "There is no target data-- delete canceled." >> return cs
      if(cs==cs') then return c
                  else do
                    let nc = unlines cs'
                    fileIn nc
                    putStrLn "delete a data from myfe.txt. success!" >> return nc
    _ -> return c
  if (f=='q') then return () else putStr ("\n"++nc++"\n") >> sLoop nc

replOrd :: Int -> Orders -> [Orders] -> [Orders]
replOrd id ord cs = take id cs ++ [ord] ++ drop (id+1) cs

delOrd :: Int -> [Orders] -> [Orders]
delOrd id cs = take id cs ++ drop (id+1) cs

idSame :: Int -> Orders -> [Orders] -> Int 
idSame dpt ord cs = 
  let oname = concat$take dpt (sepChar ';' ord)
      csname = map (concat . (take dpt) . (sepChar ';')) cs
   in if (elem oname csname) then getIndex oname csname else (-1)

confirm :: String -> IO Bool
confirm s = do
  putStrLn ("Are you sure to "++s++"? (Y/n)")
  y <- getLine
  if (y=="n" || y=="N" || y=="no") then return False else return True

noticeWeek :: Char -> IO ()
noticeWeek w = do
  let wn = weekTList !! (read [w]::Int)
  putStrLn ("For "++wn++":")

comLoop :: String -> String -> Int -> Orders -> Dms -> IO Orders 
comLoop _ _ _ o (Dm _ _ []) = return o
comLoop ex r co o dm@(Dm s i d) = do
  let ((Dm ns _ _):_) = if (d==[]) then [Dm "-" 0 []] else d
      (hs:ts) = ns
      r' = if(ts=="H") then if (co==1) then tail r else r else r
  if (ts=="H") then noticeWeek (head r) else return ()
  let (ecs:mes) = sepChar ';' (messages!!(i+co))
  g <- if (ex=="") then do
    mapM_ putStrLn mes 
    putStr "> "
    getLine
                   else return ex
  let (no, nd, nex) = checkInput g o d
  (no', nd') <- if (hs=='R') then repeatInput r' no nd ts else return (no, nd) 
  let iwr = ts=="H" && co==1 && o/="" && howLong (last$sepChar ';' o) g < 0
      ec = if iwr then 9 else read ecs 
      nd'' = if iwr then Er else nd'
  case nd'' of
    Er -> do
      putStrLn ("ERROR!!: "++(errors!!ec))
      putStrLn "--Press Enter To Continue--"
      getLine
      comLoop nex r co o dm 
    Qi -> return "q" 
    Rp -> do
      let co' = if (ts=="H" && (not iwr)) then if (co==0) then 1 else 0 else co
      comLoop nex r' co' no' dm
    _  -> do
      let nr = if (s=="w") then tail$last$sepChar ';' no else r
      no'' <- if(ns=="DT" && g=="") then today >>= (\td -> return (no'++td++";"))
                                    else return no'
      comLoop nex nr 0 no'' nd'

repeatInput :: String -> Orders -> Dms -> String -> IO (Orders, Dms) 
repeatInput r o d s = do
  if (d==Er || d==Qi) then return (o,d) else do
    y <- if (s=="H") then return "" else do
      putStr "Another Data? (Y/n) :"
      getLine
    if (y=="n" || y=="N" || y=="no") then return (o++";",d) else do
      let lo = last$sepChar ';' o
          len = length lo
          ip = case s of "M" -> len < 29; "W" -> len < 8; _ -> True
          ir = length r > 0
      if (ip && ir) then return (o, Rp) 
                    else do
              if (s=="H") then return (o, d) else do
                putStrLn "Can't add data!"
                return (o++";", d)
          

checkInput :: String -> Orders -> [Dms] -> (Orders, Dms, String) 
checkInput g o dm@(d:ds) =
  let cms = map (\(Dm m _ _) -> m) dm
      hc@(h:t) = head$cms
      len = length hc 
      iq = g==":q" || g=="exit"
      ich = not$elem h "$DIJPRS"
      (cm,ex) = if ich then (if (length g>=len) then (take len g,drop len g) else ("",""))
                       else if (g=="") then ("","") else
                          let hgs = head$sepChar ';' g; ln = length hgs in (hgs,drop (ln+1) g)
      nd = if iq then Qi else
           if ich then (if (elem cm cms) then dm!!(getIndex cm cms) else Er) else
           case h of
             'D' -> if (g=="n" || g=="N" || g=="No") then head$ds else d
             'S' -> if (isChar cm t) then d else Er
             'P' -> if (isTodo cm) then d else Er
             'I' -> if (isNum cm) then d else Er
             '$' -> d
             h' | h'=='R' || h'=='J' -> if (isDay t cm) then d else Er
             _   -> Er
      no = case hc of
             hc' | hc'=="$" || hc'=="I" || hc'=="P" -> o++cm++";"
             ('R':xs) -> o++(addDay (last$sepChar ';' o) cm xs)
             ('J':_)  -> o++cm++";"
             ('S':_)  -> o++cm++";"
             _   -> o++cm
      ex' = if (nd==Er || nd==Qi) then "" else ex
   in (no, nd, ex')

showTodo :: Contents -> String -> [String]
showTodo [] _ = []
showTodo c day =
  let cs = lines c
      std = chooseData "t" cs
   in showTodoEach std day

showTodoEach :: [String] -> String -> [String]
showTodoEach [] _ = []
showTodoEach (td:tds) day =
  let scs = sepChar ';' td
      nm = head scs
      (t:ds) = scs!!1
      dl = scs!!2
      cona = scs!!3
      cona' = map show (conTodo cona)
      cony = scs!!4
      cony' = sepChar ',' cony
      lena = fromIntegral$length cona'
      leny = fromIntegral$length cony'
      par = floor$(lena-leny)/lena*100
      par2 = div par 10
      bar = concat$["|"] ++ replicate par2 "=>" ++ replicate (10-par2) "--" ++ ["|"]
      rday = hmDays day dl
      res = "Todo: "++(nameTodo nm)++"-"++ds++"\n"++(todoType t)++": "++cony++"\n"
                       ++bar++" "++(show par)++"% done -- "
                       ++(if (rday>1) then (show rday)++" days ahead" else
                          if (rday==1) then "tomorrow" else
                          if (rday==0) then "today" else 
                          if (rday==(-1)) then "yesterday" else (show (-rday))++" days behind")
                       ++"\n"
   in res:(showTodoEach tds day) 

showData :: Contents -> [String]
showData [] = []
showData c =
  let cs = lines c
      sds = chooseData "s" cs
   in showFunc cs sds

showFunc :: [String] -> [String] -> [String]
showFunc _ [] = [[]]
showFunc cs ((t:ts):xs) = 
  case t of
    'm' -> let ords = sepChar ';' ts
               nm = head ords
               dls = chooseData ("m"++nm++";") cs
               dls' = map (head . (sepChar ';')) dls
               days = map (last . (sepChar ';')) dls
               sdl = sorting$zip days dls'
               rsl = sumUp sdl "" 0
               ic = elem 'c' (ords!!1)
               lrs = length rsl
            in if (ic && lrs>1) then (("Money: "++(nameMoney nm)):(showMC rsl ("",0)))
                                      ++(showFunc cs xs) 
                                else showFunc cs xs 
    _   -> showFunc cs xs 
               
showMC :: [(String,Int)] -> (String,Int) -> [String]
showMC [] _ = []
showMC ((day,amo):xs) ("",0) = showMC xs (day,amo)
showMC ((day,amo):xs) (pday,pamo) =
  let dl = hmDays day pday
      sa = amo-pamo
      ip = sa>=0
      rate = floor$(fromIntegral sa)/(fromIntegral dl)
   in [pday++" to "++day++": change: "++(show sa)
           ++" ("++(if ip then "" else "-")++(show rate)++"/day)"]
      ++ showMC xs (day,amo)

sumUp :: [(String,String)] -> String -> Int -> [(String,Int)]
sumUp [] pday pam = [(pday,pam)]
sumUp ((day,(fl:amo)):xs) pday pam =
  if (day==pday) then 
      let nam = case fl of 'a' -> pam+(read amo); 'r' -> read amo
       in sumUp xs day nam
                 else if (pday=="") then sumUp xs day (read amo)
                                    else (pday,pam):(sumUp xs day (read amo))
      
chooseData :: String -> [String] -> [String]
chooseData _ [] = []
chooseData h (x:xs) =
  let lh = length h
   in if ((take lh x)==h) then (drop lh x):(chooseData h xs) else chooseData h xs

isTodo :: String -> Bool
isTodo s = 
  let tdl = sepChar ',' s
   in and$map isto tdl

conTodo :: String -> [Td]
conTodo s =
  let tdl = sepChar ',' s
   in if(isTodo s) then concat$map conto tdl else []

conto :: String -> [Td]
conto s =
  if(elem '-' s) then
      let (a:b:_) = sepChar '-' s
       in cvTdList (cvTd a) (cvTd b) 
                 else [cvTd s]

cvTdList :: Td -> Td -> [Td]
cvTdList (N a) (N b) = map (cvTd . show) (toList a b) 
cvTdList (L a) (L b) = map (cvTd . (flip (:) [])) (toList a b) 
cvTdList (LN c a) (LN _ b) = map (cvTd . ((:) c) . show) (toList a b) 
cvTdList _ _ = []
  
isto :: String -> Bool
isto s =
  if(elem '-' s) then 
      let (a:b:c) = sepChar '-' s
       in if(c==[]) then canListTd (cvTd a) (cvTd b) 
                    else False
                 else if (cvTd s==Ot) then False else True

canListTd :: Td -> Td -> Bool
canListTd (N _) (N _) = True
canListTd (L _) (L _) = True
canListTd (LN a _) (LN b _) = a==b
canListTd _ _ = False

cvTd :: String -> Td 
cvTd [] = Ot
cvTd s@(h:t)
  | (isDigit h) && (isNum t)     = N (read s)
  | t==[] && (not$isDigit h)     = L h
  | (not$isDigit h) && (isNum t) = LN h (read t)
  | isStr s                      = S s
  | otherwise                    = Ot 
---------------------
