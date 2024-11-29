import Data.Char
import Data.Maybe
import Data.Tuple
import Data.List
import Debug.Trace

type OBJECTIVE = String --용언
type OBJECT = String --체언


data FUNCTOR = FM [OBJECT] VAR -- 수식어([명사]) 
            | NM [OBJECTIVE] VAR -- [수식어] (수식어)
            | F [OBJECT] VAR -- [명사] -> 명사
            | N [FUNCTOR] VAR -- [수식어] -> 수식어
            | A VAR VAR -- [여기서 정의한 이름] -> 여기서 정의한 이름

       deriving (Eq, Show)

data CAT = Object OBJECT
            | Objective OBJECTIVE
            | Functor FUNCTOR
       deriving (Eq, Show)


type VAR = String
type Env = [(VAR, CAT)]

printOBJ :: CAT -> IO ()
printOBJ (Object o) = putStrLn o
printOBJ (Objective o) = putStrLn o


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
    = fromMaybe (error "vocab not found") $ var `lookup` env

findVar :: CAT -> Env -> VAR
findVar cat env
    = fromMaybe (error "val not found") $ cat `lookup` map swap env

def :: VAR -> CAT -> Env -> Env
def x c = extEnv [(x,c)]



morpF :: FUNCTOR -> CAT -> Env -> CAT
morpF f x e =
            case f of
                    FM o1 o2 ->
                        let Object o = appEnv o2 e in
                        case x of
                            Object cat -> if cat `elem` o1 then Object (o ++ "/" ++ cat) else error $ "notExistObj: error caused by you " ++ o ++ " is Dead"
                            Objective cat -> error $ "you caused serious grammer issue " ++ cat ++ " is not noun"
                            Functor cat -> morpF cat (Object o) e
                    NM o1 o2 ->
                        let Objective o = appEnv o2 e in
                            case x of
                                Objective cat ->
                                    if cat `elem` o1 then
                                        Objective $ o ++ cat
                                        else error $ "notExistFn: error caused by you" ++ o2 ++ " doesn`t exist"
                                Object cat ->
                                        if cat `elem` o1 then
                                        Objective $ o ++ cat
                                        else error $ "notExistFn: error caused by you" ++ o2 ++ " doesn`t exist"

                    F o1 o2 ->
                        let Object o = appEnv o2 e in
                        case x of
                            Object object ->
                                if object `elem` o1 then Object (object ++ "/" ++ o) else error $ "notExistMorp: error caused by you " ++ o ++ " is Dead"
                            Objective objective ->
                                if objective `elem` o1 then Object (objective ++ " " ++ o) else error $ "notExistMorp: error caused by you " ++ o ++ " is Dead"
                            Functor functor -> error $ "you wrote wrong" ++ show functor ++ "is functor"
                    N o1 o2 ->
                        case appEnv o2 e of
                            Functor fun ->
                                let Functor functor = x in
                                        if functor `elem` o1 then morpF fun x e else error $ "notExistMorp: error caused by you " ++ show functor ++ " is Dead"
                            Objective obj -> 
                                let Object object = x in
                                    Objective (obj  ++ object)
                                
                    A x1 x2 -> let v = appEnv x1 e in
                            let v2 = appEnv x2 e in
                            case v of
                            Object object ->
                                morp x1 x2 $ extEnv [(x1, x), (x2, v)] e
                            Objective objective ->
                                error $ "grammarERr: error caused by you " ++ objective ++ " is not noun"
                            Functor functor ->
                                morp x1 x2 $ extEnv [(x1, x), (x2, v)] e
                    _ -> error "UNREACHABLE CODE REACHED!!"



morp :: VAR -> VAR -> Env -> CAT
morp c x' e =
    case appEnv c e of
        Object o -> let x = appEnv x' e in
            case x of
                Objective cat -> Object (cat ++ " " ++ o)
                Object cat -> Object (o ++ cat)
                Functor cat -> morp o c $ extEnv [(o, x), (c, Functor cat)] e
        Objective o -> let x = appEnv x' e in
            case x of
                Objective cat -> Object (cat ++ " " ++ o)
                Object cat -> Object (cat ++ " " ++ o)
                Functor cat -> morp o c $ extEnv [(o, x), (c, Functor cat)] e
        Functor f' -> let x = appEnv x' e in
            morpF f' x e

            --tokenizer--
data TokenType = LEFTPARAN
                | RIGHTPARAN
                | LEFTBRACE
                | RIGHTBRACE
                | COLON
                | PLUS
                | MINUS
                | STAR
                | SLASH
                | SEMICOLON
                | SHARP
                | WAVEDASH
                | DASH
                | GREATERTHEN
                | NUM
                | UPPERCASE
                | LOWERCASE
                | EQUAL
                | AT
            deriving (Show)

type Token = ([Char], TokenType)

tokenizer :: String -> [Token]
tokenizer (s:ss)  =
    case s of
        '(' -> ([s], LEFTPARAN) : tokenizer ss
        ')' -> ([s], RIGHTPARAN) : tokenizer ss
        '{' -> ([s], LEFTBRACE) : tokenizer ss
        '}' -> ([s], RIGHTBRACE) : tokenizer ss
        ':' -> ([s], COLON) : tokenizer ss
        ';' -> ([s], SEMICOLON) : tokenizer ss
        '+' -> ([s], PLUS) : tokenizer ss
        '-' -> ([s], MINUS) : tokenizer ss
        '*' -> ([s], STAR) : tokenizer ss
        '/' -> ([s], SLASH) : tokenizer ss
        '#' -> ([s], SHARP) : tokenizer ss
        '~' -> ([s], WAVEDASH) : tokenizer ss
        '>' -> ([s], GREATERTHEN) : tokenizer ss
        '=' -> ([s], EQUAL) : tokenizer ss
        '@' -> ([s], AT) : tokenizer ss
        ' ' -> tokenizer ss
        _ -> if isDigit s
                then (takeWhile isDigit (s:ss), NUM)
                    : tokenizer (dropWhile isDigit ss)
                else if isUpper s
                    then (takeWhile isAlphaNum (s:ss), UPPERCASE)
                        : tokenizer (dropWhile isAlphaNum ss)
                    else if isLower s
                        then (takeWhile ((||) <$> isDigit <*> isLower) (s:ss), LOWERCASE)
                            : tokenizer (dropWhile ((||) <$> isDigit <*> isLower) ss)
                        else error "UserFault"
tokenizer [] = []