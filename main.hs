import Data.Maybe
import Data.Tuple
import Data.List
import Debug.Trace


type OBJECT = String

data SENTENCE a = Null | NODE a [SENTENCE a]
        deriving (Eq, Show, Functor, Foldable)


data FUNCTOR a v = F [a] v
             | F' [a] v
        deriving (Eq, Show, Functor, Foldable)


data CAT = Sentence (SENTENCE CAT)
        | Object OBJECT
        | NULL
        | Functor (FUNCTOR CAT VAR)
        deriving (Eq, Show)


type VAR = String
type Env = [(VAR, CAT)]


printSent :: SENTENCE CAT -> IO ()
printSent sent =  (putStr . senttoStr) sent >> putStrLn ""


extEnv :: Env -> Env -> Env
extEnv e0 e1
   = e0 ++ e1

appEnv :: VAR -> Env -> CAT
appEnv v [] = error $ "all your fault: vocab" ++ v ++ " not found"
appEnv v (e:es) =
    let (var, value) = e in
        if var == v then value else appEnv v es

findOBJ :: VAR -> Env -> CAT
findOBJ var env
    = fromMaybe (error $ var   ++ ": vocab not found") $ var `lookup` env

findVar :: CAT -> Env -> VAR
findVar cat env
    = fromMaybe (error $ cattoStr cat ++ " :not found") $ cat `lookup` map swap env

def :: VAR -> CAT -> Env -> Env
def x c = extEnv [(x,c)]


morp :: [VAR] -> VAR -> Env -> SENTENCE CAT
morp vars s e =
        let fs =  map ( (\(Functor x) -> x) .(`appEnv` e) ) vars in
        let cs = ((\(Sentence x) -> x) . (`appEnv` e)) s in
        --        ( senttoStr cs ++ concatMap (cattoStr . (`appEnv` e)) vars)`trace` 
               foldr (morpOne e) cs fs

morpOne :: Env -> FUNCTOR CAT VAR -> SENTENCE CAT -> SENTENCE CAT
morpOne e f s  =
        case f of
                F cs v ->
                        let real = appEnv v e
                        in ins real (fromMaybe real (find (/= NULL) (findCat e cs <$> s))) s
                F' cs v ->
                        let real = appEnv v e
                        -- in let realCat = 
                        --         if elemCat e cs s
                        --         then findCat e cs s
                        --         else NULL
                        in conv real (fromMaybe real (find (/= NULL) (findCat e cs <$> s))) s


elemCat ::  Env -> [CAT] -> SENTENCE CAT -> Bool
elemCat e ca = foldr (((||) . (/=NULL)) . findCat e ca) False

findCat ::  Env -> [CAT] -> CAT -> CAT
findCat e ca c  =
        case c of
                Sentence c' -> if any ((c `elem`) . (\(Sentence x) -> x )) c' then c else NULL
                Object o ->
                -- (o ++ "/" ++ concatMap cattoStr ca ++ show  (c `elem` ca))`trace` 
                        if c `elem` ca then c else NULL
                NULL -> NULL
                Functor f ->
                        (case f of
                                F cats v ->
                                        let real = appEnv v e
                                        in if real `elem` ca then c else NULL
                                F' cats v ->
                                        let real = appEnv v e
                                        in if real `elem` ca then c else NULL)


ins :: CAT -> CAT -> SENTENCE CAT -> SENTENCE CAT
ins inserted target s =
        case s of
                Null -> Null
                NODE v leafs ->
                        case leafs of
                                [] -> if v == target then NODE v [NODE inserted []] else NODE v leafs
                                leafs ->
                                        if any (elem target) leafs then NODE v $ map (ins inserted target) leafs else NODE v leafs

conv :: CAT -> CAT -> SENTENCE CAT -> SENTENCE CAT
conv inserted target s =
        case s of
                Null -> Null
                NODE v leafs -> NODE (if v == target then inserted else v) (conv inserted target <$> leafs)
                        {-(senttoStr s ++ "/" ++cattoStr v ++"/" ++ concatMap senttoStr leafs) `trace`-}
                        -- case v of 
                        --         target -> NODE inserted (conv inserted target <$> leafs)  
                        --         _ -> NODE v (conv inserted target <$> leafs)

cattoStr :: CAT ->  String
cattoStr c = case c of
    Object o -> o
    Functor f ->
        case f of
            F os v -> "@" ++ v ++ "+" ++ concatMap ((++","). cattoStr) os  ++ "@"
            F' os c -> "#" ++c ++"+" ++  concatMap ((++","). cattoStr)  os ++ "#"
    Sentence c -> "(" ++ senttoStr c ++ ")"
    NULL -> "_"

senttoStr :: SENTENCE CAT -> String
senttoStr s =
        case s of
                NODE v leafs -> "<" ++ cattoStr v ++ ">\nL"++ concatMap ((++ "-") . senttoStr) leafs
                _ -> "_"
