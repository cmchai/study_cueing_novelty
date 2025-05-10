var consentText = ['<div class = "InstrBx"><h1>Welkom in dit experiment</h1>'+
                   '<p>Dit experiment maakt deel uit van een onderzoek dat wordt uitgevoerd aan de Universiteit Gent. De data die we tijdens dit experiment verzamelen zullen niet \
                   gelinkt worden aan potentieel identificeerbare informatie, en zal enkel voor onderzoeksdoeleinden gebruikt worden. Geanonimiseerde data van de studie zal geregistreerd \
                   en gearchiveerd worden op een betrouwbare publieke data repository, in lijn met de huidige richtlijnen rond data delen. Je bent vrij ten aller tijde het experiment stop \
                   te zetten door uw browser af te sluiten, wat geen negatieve gevolgen zal hebben. Door aan te geven dat je toestemt met dit document, bevestig je dat je minstens 18 jaar \
                   bent en begrijpt wat de verwachting is omtrent uw deelname aan dit onderzoek. Als u liever toch niet wenst deel te nemen, kan u dit venster afsluiten.<p></div>'];
var mainInstruction = [
                       //////////////////////////////////// the first page //////////////////////////////////////////
                       '<div class = "InstrBx"><h1>Welkom in dit experiment!</h1>' +
                       '<p>Vandaag zal je een experiment uitvoeren waarin getest wordt hoe snel en accuraat je telkens nieuwe taken kan uitvoeren.</p>' +
                      
                       '<p>In het begin van elke taak zal je telkens twee regels zien op het scherm. Wanneer je op de spatiebalk drukt, zal je vervolgens twee woorden op het scherm zien. Bijvoorbeeld:</p>' +

                       '<p><img src="maininstruction_CueStimImage_BE.jpg" width="520" height="130"></img></p>' +

                       '<p>De bovenste regel zal telkens verduidelijken volgens welke <b>relatie</b> je de woorden moet beoordelen. Deze regel kan een van de volgende zes opties zijn: BEIDE, GEEN, \
                       TEN MINSTE EEN, EERSTE, TWEEDE, of VERSCHILLEND.</p>' +

                       '<p>De onderste taakregel zal telkens verduidelijken welk <b>kenmerk</b> je moet beoordelen. Ook hier zijn er zes mogelijkheden: GROEN, ZACHT, LUID, ZOET, GEUREND, of LEVEND.</p>' + 

                       '<p>In het voorbeeld hierboven is je taak dus om te beoordelen of <b>BEIDE</b> objecten munt en lemonade <b>GROEN</b> zijn.</p>',
                       
                       //////////////////////////////////// the second page /////////////////////////////////////////
                       '<p>Zoals afgebeeld op het vorige scherm worden twee woorden gepresenteerd van zodra je de spatiebalk hebt ingedrukt na het zien van de regels. In dit voorbeeld is munt iets \
                       wat typisch groen is, terwijl limonade meestal niet groen is. In dit geval zijn dus niet beide objecten groen en moet je de toets indrukken die overeenkomt met het vals \
                       (<span style="color:red;">&#10006</span>).  In het voorbeeld hieronder staat het <span style="color:red;">&#10006</span> rechts dus zou je de rechter toets moeten indrukken (\"<b>J</b>\" op het keyboard). \
                       Als het rood kruis links had gestaan, zou je de linker toets moeten indrukken (\"<b>F</b>\" op het keyboard). <b>Grebruik altijd \"F\" of \"J\" om je antwoord aan te duiden</b>.</p>'+

                       '<p><img src="maininstruction_stimimage_BE.jpg" width="240" height="180"></img></p>' +

                       '<p>Je kan de taak efficient uitvoeren door je linker en rechter wijsvinger op de toetsen \"F\" en \"J\" te plaatsen, en je duimen op de spatiebalk.</p>' +

                       '<p><img src="keyboard.jpg" width="600" height="200"></img></p>',

                       ////////////////////////////////// the third page ////////////////////////////////////////////

                       '<p>De <b>relatie</b>-regels kunnen een van de volgende zes regels zijn, en moeten als volgt begrepen worden (telkens met het voorbeeld GROEN):</p>' + 

                       '<p> &#8226 BEIDE: Zijn beide objecten groen?' +
                       '<br> &#8226 GEEN: Zijn geen enkel van de twee objecten groen?' + 
                       '<br> &#8226 TEN MINSTE EEN: Is ten minste een van de twee objecten groen?' +
                       '<br> &#8226 EERSTE: Is het eerste object (bovenaan het scherm) groen?' +
                       '<br> &#8226 TWEEDE: Is het tweede object (onderaan het scherm) groen?' +
                       '<br> &#8226 VERSCHILLEND: Verschillen beide objecten op als ze al dan niet groen zijn (een is groen en het ander is niet groen)?</p>' +

                       '<p>De <b>kenmerken</b> (GROEN, ZACHT, LUID, ZOET, GEUREND, en LEVEND) moeten beoordeeld worden op "gezond verstand". Bijvoorbeeld, als je taak is om te \
                       evalueren of een woord ZACHT is, moet je de woorden die je zal zien beoordelen als zacht als ze typisch of gewoonlijk als zacht aanzien worden. Je moet \
                       je antwoord dus baseren op hoe je denkt dat de meeste mensen deze zouden evalueren. Denk hier best niet te lang over na.</p>' + 

                       '<p>Het volledig experiment kan opgedeeld worden in <b>twee delen</b>. Het eerste deel is de trainingsessie, waar je zal leren om zes taken uit te voeren, \
                       een per een. In het tweede deel zal je zowel deze zes <b>gekende taken</b> als heel wat <b>nieuwe taken</b> uitvoeren die je nog niet gezien had.</p>' +

                       '<p>Je kan nu op Volgende klikken om het eerste oefenblok te starten. Veel succes!</p></div>'
                       ];

var testMainInstruction = ['<p>Goed zo! Je bent nu klaar met het eerste deel van het experiment, en dus over de helft.</p>'+
                           '<p>In dit tweede deel van het experiment zal je zowel <b>gekende</b> taken (taken die je tot hiertoe uitgevoerd hebt) als <b>nieuwe</b> taken moeten uitvoeren. Er \
                           zullen vier blokken zijn in het totaal. In sommige blokken zal je wat extra hulp krijgen. Namelijk, daar zal je telkens een cue te zien krijgen die \
                           aankondigt of er een gekende of een nieuwe taak zal verschijnen. Bijvoorbeeld:</p>'+
                                                     
                           '<p>* nieuwe taak *</p>'+
                           
                           '<p><b>Probeer deze cue te gebruiken om je zo goed mogelijk voor te bereiden op de daarop volgende taak!</b> In andere blokken zullen deze cues helemaal niet informatief \
                           zijn. In die blokken hoef je niets voor te bereiden en zullen de cues er als volgt uitzien:</p>'+

                           '<p>* xxxxxx xxxx *</p>' +

                           '<p>Voor de rest zal een beurt precies hetzelfde verlopen als voorheen. Er wordt vanaf nu niet langer feedback gegeven over of je antwoord juist of fout was. Uiteraard \
                           is het nog steeds de bedoeling zo snel en correct mogelijk te antwoorden.</p>' +

                           '<p>Druk op Volgende om het eerste blok van dit tweede deel te starten.</p>'];

var pracBlockInstruction = ['<p>Je krijgt zodra nog eens de instructive te zien waarna je op spatie moet drukken meteen nadat je de taak begrijpt en onthouden hebt. \
                            Vervolgens zullen er 60 paren aan woorden verschijnen, en is het jouw taak aan te geven of deze voldoen aan deze regels. Er zal ook feedback \
                            verschijnen na elke beurt die aangeeft of je antwoord correct is of niet. <b>Probeer zo snel en zo accuraat mogelijk te antwoorden</b>.</p>' +

                            '<p>Deze taakinformatie zal slechts <b>eenmalig</b> verschijnen aan het begin van het blok. Let dus goed op!</p>' +

                            '<p>Gelieve op Volgende te drukken om dit blok te beginnen. Veel succes!</p>'];

var pracMixInstruction = ['<p>Tijdens het volgende blok zal je alle taken moeten uitvoeren die je tot nu toe hebt uitgevoerd. De juiste taakinstructie zal nu verschijnen \
                           voor elke beurt. Druk op spatiebalk van zodra je klaar bent om de taak uit te voeren. Net als de vorige blokken bestaat dit blok uit 60 beurten.</p>'+ 
                          '<p>Druk op Volgende om dit blok te starten. Veel succes!</p>'];

var testCueInstruction = ['<p>In dit blok zal een taak steeds vooraf gegaan worden door een cue die meegeeft of een taak gekend is of nieuw. Bijvoorbeeld:</p>'+
                          '<p>* gekende taak *</p>' +  
                          '<p>of:</p>' +
                          '<p>* nieuwe task *</p>' +
                          '<p><b>Probeer deze cue te gebruiken om je zo goed mogelijk voor te bereiden op de daarop volgende taak!</b></p>' +
                          '<p>Gelieve op Volgende te drukken om dit blok te beginnen. Veel succes!</p>'];

var testNoCueInstruction = ['<p>In dit blok zal een taak steeds vooraf gegaan worden door een cue die niet informatief is. Bijvoorbeeld:</p>'+
                            '<p>* xxxxxx xxxx *</p>'+
                            '<p>Gezien deze cue geen extra informatie meegeeft hoef en kan je je dus ook niet speciaal voorbereiden.</p>'+
                            '<p>Gelieve op Volgende te drukken om dit blok te beginnen.</p>']

var endMessage = ['<p>Proficiat! Je hebt het experiment succesvol vervolledigd! </p>'+ 
                  '<p>Bedankt voor uw deelname!</p>'+
                  '<p>Opgelet! De credits zullen handmatig toegekend worden binnen <b>een week</b> na het volledig uitvoeren van dit experiment. Als dit niet het geval zou zijn \
                  kan je altijd contact opnemen met de proefleider via email: Mengqiao.Chai@ugent.be.</p>'];