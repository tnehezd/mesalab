Útmutató a JOSS paper.md Fájlhoz

A JOSS cikke egy rövid (tipikusan ~1000 szó), tömör leírás a szoftveredről, amelynek célja, hogy a kutatók könnyen idézhessék a munkádat és megértsék a szoftver funkcióját. Fontos, hogy a hangsúly a szoftverre kerüljön, nem az általa elért új tudományos eredményekre (azok a hagyományosabb tudományos cikkek témája).

Íme a főbb részek és tanácsok a paper.md elkészítéséhez:

1. A paper.md Fájl Kötelező Részei

A JOSS megköveteli, hogy a paper.md fájl a szoftvered GitHub (vagy más Git-alapú) repójával együtt legyen tárolva. A főbb szakaszok a következők:

Cím (Title):
Legyen tömör és leíró.
Tartalmazza a szoftver nevét, és röviden utaljon a fő funkciójára.
Példa: "MESA Blue Loop Analyzer: A Python Package for Automated Post-Main Sequence Evolution Analysis"
Szerzők és Affiliációk (Authors and Affiliations):
Sorold fel az összes hozzájáruló szerzőt a teljes nevükkel és intézményi affiliációjukkal.
Minden szerzőnek bele kell egyeznie a listázásba.
Összefoglaló (Summary):
Egy rövid absztrakt (általában 1-2 bekezdés).
Ismertesse, mi a szoftver, mi a fő célja, és mely tudományos problémát oldja meg.
NE tartalmazzon új tudományos eredményeket. Célja a szoftver bemutatása.
Szükségesség Nyilatkozata (Statement of Need):
Ez egy kritikus szakasz, és gyakran a legfontosabb része a JOSS cikknek.
Itt kell megmagyaráznod, miért van szükség erre a szoftverre a tudományos közösség számára.
Vázold fel a problémát, amit a szoftver megold (pl. MESA kimenetek manuális elemzésének nehézsége, kék hurkok robusztus azonosításának kihívása).
Magyarázd el, hogyan javítja meg a szoftver a jelenlegi helyzetet (pl. gyorsabbá, megbízhatóbbá, reprodukálhatóbbá teszi az elemzést).
Utalhatsz más hasonló eszközökre, és rámutathatsz, miben egyedi vagy jobb a te megoldásod.
Referenciák (References):
Ez egy BibTeX fájlból generálódik, amelyet a paper.md mellett kell elhelyezni (pl. paper.bib).
Tartalmazza a JOSS cikkben hivatkozott kulcsfontosságú publikációkat (pl. MESA-ra vonatkozó cikkek, asztrofizikai referenciák az Instabilitási Sávhoz, vagy bármilyen más szoftver, amit használsz).
2. Tartalmi Iránymutatások és Tippek

Fókusz a Szoftverre:
Határozottan írd le a szoftver funkcionalitását, architektúráját és a benne alkalmazott algoritmusokat.
A te esetedben ez magában foglalhatja:
Hogyan olvassa be és dolgozza fel a MESA history fájlokat.
A kék hurok azonosításának algoritmusát (MS vége, RGB csúcs, He kiégés, Instabilitási Sáv keresztezések, fizikai szűrők).
A bolometrikus korrekciók kezelését (ha a plotting modulod végzi).
A generált ábrák típusait (HRD, CMD, heatmap).
Telepítés és Használat (Installation and Usage):
Röviden magyarázd el, hogyan kell telepíteni a szoftveredet (pl. Git klónozás, pip install ha csomagoltad).
Adjon rövid példákat a szoftver használatára. Ez lehet egy parancssori példa, vagy egy nagyon egyszerű kód snippet, ami demonstrálja a kulcsfunkciókat.
Ez segít a bírálóknak gyorsan elkezdeni a szoftvered tesztelését.
Licenc: Ismételten említsd meg a cikkben, hogy a szoftvered egy OSI-jóváhagyott licenc alatt van kiadva (pl. MIT License).
Tesztelés:
Térj ki arra, hogy a szoftver rendelkezik-e automata tesztekkel, és ha igen, milyen mértékben (pl. "The software includes a comprehensive suite of unit tests to ensure the correctness of the analysis algorithms.").
Ez nagyon fontos a JOSS számára a kód megbízhatóságának igazolására.
Karbantarthatóság és Bővíthetőség:
Röviden utalj arra, hogy a szoftvert hogyan tervezték a jövőbeni karbantartásra és bővítésre (pl. "The modular design of the codebase facilitates future extensions and integration with other astrophysical tools.").
Ábrák (Figures):
A paper.md fájlba ne illessz be közvetlenül képeket (pl. ![Image](path/to/image.png)).
A JOSS-hoz való beküldésnél az ábrákat külön fájlokként kell feltölteni (pl. figures/hrd_example.png), és a paper.md-ben kell hivatkozni rájuk, de a JOSS maga generálja a papírt, és kezeli az ábrák beillesztését. Nézd meg a JOSS example paper struktúráját.
Rövid és Tömör:
A JOSS cikkek rövidek. Kerüld a terjengős nyelvezetet és a felesleges ismétléseket. Minden mondatnak hozzá kell járulnia a szoftver leírásához vagy szükségességének alátámasztásához.
3. További Tippek a GitHub Repóhoz

Bár ez a paper.md útmutatója, a JOSS bírálói a teljes repódat is vizsgálják:

README.md: A GitHub repóban legyen egy nagyon alapos README.md, ami tartalmazza a paper.md összefoglalóját, telepítési utasításokat, használati példákat, és a projekt szerkezetének leírását.
Példafájlok: Csomagolj a repódba néhány kis MESA history fájl példát, amivel a bírálók könnyen tesztelni tudják a szoftveredet.
pyproject.toml (vagy setup.py): Készítsd elő a projektet Python csomaggá, hogy pip installálható legyen. Ez egyértelműen mutatja a "megfelelő csomagolást".
Ha ezeket a pontokat figyelembe veszed a paper.md megírásakor, és a szoftvered is megfelel a korábban megbeszélt technikai elvárásoknak (LOC, tesztek, licenc), akkor jó úton haladsz a JOSS-publikáció felé.

Van még valami, amit szeretnél átbeszélni a paper.md vagy a JOSS beküldés kapcsán?