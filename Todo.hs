module Todo (isTodo,conTodo,showTodo) where

import Mydata(Td(..),Contents,nameTodo,todoType)
import Useful(sepChar,isNum,isStr,toList,chooseData)
import Mydate(hmDays)
import Data.Char(isDigit)

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
      icomp = length scs==4 
      nm = head scs
      (t:ds) = scs!!1
      dl = scs!!2
      cona = scs!!3
      cona' = map show (conTodo cona)
      cony = if icomp then "" else scs!!4
      cony' = if icomp then [] else sepChar ',' cony
      lena = fromIntegral$length cona'
      leny = fromIntegral$length cony'
      par = floor$(lena-leny)/lena*100
      par2 = div par 10
      bar = concat$["|"] ++ replicate par2 "=>" ++ replicate (10-par2) "--" ++ ["|"]
      rday = hmDays day dl
      pace = if (rday>0 && leny>0) then (show$floor$leny)++" work(s)--steady pace of "
                       ++(show$ceiling$leny/(fromIntegral rday))++" work(s)/day" else ""
      res = "Todo: "++(nameTodo nm)++"("++nm++")-"++ds++"\n"++(todoType t)++": "++cony++"\n"
                       ++bar++" "++(show par)++"% done -- "
                       ++(if (rday>1) then (show rday)++" days ahead" else
                          if (rday==1) then "tomorrow" else
                          if (rday==0) then "today" else 
                          if (rday==(-1)) then "yesterday" else (show (-rday))++" days behind")
                       ++" (deadline: "++dl++")"++"\n"++pace++"\n"
   in res:(showTodoEach tds day) 

