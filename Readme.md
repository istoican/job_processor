## Cateva cuvinte despre alogoritmul folosit.

Cred ca exista 2 moduri de rezolvare a problemei.

1. Prima varianta de rezolvarea presupune parcurgerea listei de taskuri si mutarea intr-o noua lista (a taskurilor ordonate) a acelora care nu au dependinte. 
   Daca toate taskurile au dependinte atunci inseamna ca taskurile nu por fi ordonate (avem de-a face ori cu dependinte ciclice or cu dependinte care nu se afla in lista de taskuri).
   Se repeta pasul de mai sus pentru taskurile ramase in lista (vor fi mutate taskurile ale caror dependinte au fost deja rezolvate anterior).
   Algoritmul se repeta paca cand nu mai este nici un task de mutat.
   Pentru a vizualiza algoritmul, ne putem imagina taskurile (intr-un mod simplificat, pentru ca problema e cu graf, nu un arbore, dar arborii sunt usor de vizualizat) sub forma unui arbore in care nodurile copil sunt dependinte. Algorimtul ar presupune parcurgerea mai intai a nodurilor frunza, indreptandu-ne cate un nivel catre nodurile radacina. 

   Avantajul acestui algoritm este ca este usor de inteles (intr-un limbaj procedural nici nu este nevoie de recursivitate, un simplu loop e de ajuns) si de implementat.

2. A doua varianta presupune inceperea cu primul nod, si rezolvarea in mod recursiv a dependintelor sale. Pasul se repeta pentru toate taskurile din lista. Din nou, imaginandu-ne taskurile sub forma unui arbore (intr-un mod simplificat), alogoritmul ar presupune traversarea arborelui in postordine. Libraria 'digraph_utils', parte a librariei standar in erlang, implementeaza acest algoritm. Un eventual avantaj al acestui algoritm este acela ca inceraca sa pastreza pe cat posibil ordinea initiala a taskurile (bineinteles depinde de implemetarea specifica).

Dat fiind faptul ca problema nu cere pastrarea pe cat posibil a ordinii initiale a taskurilor, cat si datorita modului succint in care se poate implementa in erlang, si faptul ca varianta 2 e deja implemtata ca librarie standard, eu am ales sa implementez varianta 1.

## Fisiere de interes

* src/job_processor.erl: Include implementarea propriu-zisa a algoritmului. 
* src/job_processor_htp_handler.erl: Cuprinde handlerul care proceseaza requestul http. Atat pentru raspunsul de tip json, cat si pentru cel de tip script se foloseste acelasi endpoint [POST] "http://127.0.0.1:8080/", cu deosibirea ca daca se doreste un raspuns de tip script trebuie trimis un query-param "format=script" (http://127.0.0.1:8080/?format=script). 
* test/job_processor_tests.erl: Teste eUnit pentru testarea implementarii.

## Pornirea serverului http

```shell
rebar3 compile
rebar3 shell
```

## Rularea testelor eUnit

```shell
rebar3 eunit
```

## Testarea endpoint-ului http atunci cand dorim un raspuns `json`

```shell
curl -d @test-data.json http://127.0.0.1:8080
```

## Testarea endpoint-ului http atunci cand dorim un raspuns de tip `script`

```shell
curl -d @test-data.json http://127.0.0.1:8080?format=script
```