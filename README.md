# Cation
이 프로젝트의 목적은 `object`와 `objective` 서로와 또는 자기 자신과 가능한  
자연어에는 없는 새로운 `FUNCTOR`를 잔뜩 만들어 내는 것입니다.  물론 그러려면  `Env` 가 저장되어야겠지만 아직 그런 기능은 없습니다. 추가예정

   
원래는 자연어 처리를 하려고 했습니다. 근데 너무 끔찍해서 그냥 인공어를 만들고 싶어졌는데, 다른 게 된 것 같습니다.  
아직 파서가 없습니다. 그러니까 아래처럼 입력하시면 됩니다.  
만드는중..


```
printOBJ $ morp "핵" "고양이"
  (def "핵" (
    Functor (N [NM ["짐승","인간"] "귀여운것"] "너무너무" ))
    ((def "징그러운"
        ( Functor ( FM ["짐승", "인간"] "귀여운것") )
        ( def "고양이" (Object "짐승")
              [("귀여운것", Object "귀여-운것"), ("너무너무", Objective "심하게")] ))))

```

출력:
```
심하게짐승
```

# 규칙

단어에는 간단한 타입이 있습니다.
```
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

```
`VAR`는 immutable 한 변수입니다.  
  
`Env`는 단어 사전입니다. 모든 단어는 여기에 등록되어 있어야만 쓸 수 있으며   
인공어 단어는 자연어 단어에서 가져오거나, `def FUNCTOR` 로 새로 정의해야 합니다.  
def는 단어에서 단어로 가는 사상을 완전히 새로 정의하거나 기존 자연어에 있던 것을 그대로 가져올 수 있는 함수입니다.

  
자연어의 어휘는 `object` 또는 `objective`입니다.  
`object`는 그냥 명사입니다.  
`objective`는 용언인데, 인공어 타입 `FUNCTOR`와 사실상 같습니다.  
    
    
`FUNCTOR`는 4가지 종류가 있는데 
`FM`과 `NM`은 자연어에 있는 `Objective`를 그대로 가져오는 역할을 하고, `F`와 `N`은 완전히 새로운 사상을 만드는 일을 합니다.  
`FUNCTOR`는 이름이 있을 수도 없을 수도 있습니다.
N이 붙은 것은 사상에서 사상으로 가는 변환입니다.   

`morp`는 단어와 수식어를 집어넣으면 더 이상 갈 수 없을 때까지 단어를 `FUNCTOR`에 집어넣는 함수입니다.   
  


todo: 파서, Env 사전 파일

