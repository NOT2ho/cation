import Data.Maybe
import Data.Tuple
import Data.List
import Debug.Trace


type OBJECT = String


data SENTENCE a = LEAF a
                | BRANCH (SENTENCE a) (SENTENCE a)
        deriving (Eq, Show, Foldable)

instance Functor SENTENCE where
    fmap :: (a -> b) -> SENTENCE a -> SENTENCE b
    fmap f (LEAF x) = LEAF (f x)
    fmap f (BRANCH a b) = BRANCH (fmap f a) (fmap f b)


data FUNCTOR v = F [CAT] v
             | F' [CAT] v
        deriving (Eq, Show)



data CAT = Sentence (SENTENCE CAT)
        | Object OBJECT
        | NULL
        | Functor (FUNCTOR VAR)
        deriving (Eq, Show)


type VAR = String
type Env = [(VAR, CAT)]


printSent :: SENTENCE CAT -> IO ()
printSent cat =  mapM_ (putStr . cattoStr) cat >> putStrLn ""


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
        let fs = map (`appEnv` e) vars in
        let cs = (`appEnv` e) s in
                morpF (map (\(Functor x) -> x) fs) ((\(Sentence x) -> x) cs) e

morpF :: [FUNCTOR VAR] -> SENTENCE CAT -> Env -> SENTENCE CAT
-- morpF (va:rs) sentence env = morpF rs (morpOne va sentence env) env
-- morpF v sentence env = sentence
morpF f s e = foldl (\ s f -> morpOne f s e) s f

elemCat ::  Env -> CAT -> CAT -> SENTENCE Bool
elemCat e c ca =
        case ca of
                Sentence c' -> trace (senttoStr' ((c `elem`) . (\(Sentence x) -> x ) <$> c'))  ((c `elem`) . (\(Sentence x) -> x ) <$> c')
                Object o -> --show (if ca == c then LEAF True else LEAF False) `trace` 
                         if ca == c then LEAF True else LEAF False
                NULL -> "null" `trace` LEAF False
                Functor f ->
                        case f of
                                F cats v ->
                                        --show (if c ==  appEnv v e || any (foldElemCat e c) cats then LEAF True else LEAF False)
                                        --`trace` 
                                        (if c == appEnv v e || any (foldElemCat e c) cats then LEAF True else LEAF False)
                                F' cats v -> --show (if c ==  appEnv v e || any (foldElemCat e c) cats then LEAF True else LEAF False)
                                        --`trace` 
                                         if c == appEnv v e || any (foldElemCat e c) cats then LEAF True else LEAF False

foldElemCat :: Env -> CAT -> CAT -> Bool
foldElemCat e c1 c2 = or (elemCat e c1 c2)

morpOne :: FUNCTOR VAR -> SENTENCE CAT -> Env -> SENTENCE CAT
morpOne f s e =
        case f of
                F cs v ->
                        let real = appEnv v e in
                                (\x -> if any (foldElemCat e x) cs then 
                                        Sentence (BRANCH (LEAF (fromMaybe (error "how?") (find (foldElemCat e x) cs))) (LEAF x))
                                        else x ) <$> s
                F' cs v ->
                        let real = appEnv v e in
                                (\x -> if any (foldElemCat e x) cs then Sentence (BRANCH (LEAF real) (LEAF x)) else x ) <$> s

cattoStr :: CAT ->  String
cattoStr c = case c of
    Object o -> o
    Functor f ->
        case f of
            F os v -> "@" ++ v ++ "+" ++ concatMap cattoStr os  ++ "@"
            F' os c -> "#" ++c ++"+" ++  concatMap cattoStr  os ++ "#"
    Sentence c -> "(" ++ senttoStr c ++ ")"
    NULL -> "_"

senttoStr :: SENTENCE CAT -> String
senttoStr s =
        case s of
                LEAF a -> "<" ++ cattoStr a ++ ">"
                BRANCH c1 c2 -> senttoStr c1 ++ "-" ++ senttoStr c2

senttoStr' :: SENTENCE Bool -> String
senttoStr' s =
        case s of
                LEAF a -> "<" ++ show a ++ ">"
                BRANCH c1 c2 -> senttoStr' c1 ++ "-" ++ senttoStr' c2
