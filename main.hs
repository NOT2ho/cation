import Data.Maybe
import Data.Tuple


type OBJECT = String


data SENTENCE a = LEAF a
                | BRANCH (SENTENCE a) (SENTENCE a)
        deriving (Eq, Show)

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

morpF :: [FUNCTOR VAR] -> SENTENCE CAT -> Env -> SENTENCE CAT
morpF (va:rs) sentence env = morpF rs (morpOne va sentence env) env
morpF v sentence env = sentence

morpOne :: FUNCTOR VAR -> SENTENCE CAT -> Env -> SENTENCE CAT
morpOne f s e =
        case f of
                F cs v ->
                        let real = appEnv v e in
                                ( \x -> if x `elem` cs then real else x ) <$> s
                F' cs v ->
                        let real = appEnv v e in
                                ( \x -> if x `elem` cs then Sentence (BRANCH (LEAF NULL) (LEAF real)) else x ) <$> s

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
                LEAF a -> cattoStr a 
                BRANCH c1 c2 -> senttoStr c1 ++ "-" ++ senttoStr c2 
