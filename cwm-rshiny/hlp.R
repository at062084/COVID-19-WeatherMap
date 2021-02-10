
htmlFrontPageTop <- "<p>
                       Dieses Dashboard beschäftigt sich mit der <b>Entwicklung der TagesInzidenz</b> der COVID-19 Pandemie in Österreich, 
                       basierend auf den Einmeldungen im EMS (Epidemiologisches Melde System) der AGES. Es verfolgt drei <b>Ziele</b>:
                     </p>
                     <p>
                     <li>Vermittlung des aktuellen Standes, der Tendenz sowie einer Prognose der TagesInzidenz in Form einer <b>Wetterkarte</b>
                         (Punkte 'Bundesländer' und 'Bezirke')</li>
                     <li>Motivation den bisherigen Verlauf der Pandemie besser als <b>Stufenmodell</b> denn in Form von Wellen zu verstehen
                         (Punkte 'Inzidenz Bundesländer und Bezirke')</li>
                     <li>Eine Darstellungsform für die Inzidenz Zahlen zu etablieren, die dem <b>exponentiellen Charakter der Verbreitung</b> angemessen ist
                         (Stufen mit jeweils doppelter TagesInzidenz) </li>
                      </p><p>
                        Unterschiedliche Varianten für die <b>Prognose der Inzidenz</b> auf Bundesländer Ebene können interaktiv unter Punkt 'Prognose' gerechnet werden.
                        Parallel zum Stufenmodell werden unter Punkt '<b>Geschwindigkeit</b>' die Ausbreitungsschübe  betrachtet.
                        Der '<b>Rückblick 2020</b>' motiviert die Möglichkeit einer mittelfristigen Prognose bei gegebenem Verhalten in der Bevölkerung.
                      </p>
                    "



htmlFrontPageBot <- "<h4>Bedienungsanleitung</h4>
                    <p>Das Menü auf der linken Seite ermöglicht es Einfluss auf verschiedene Darstellungen zu nehmen
                    </p>
                    <p>
                     <li>Welche Controls für eine Graphik aktiv sind steht unter 'Menüauswahl' jeweils oben links</li>
                     <li>Die Auswahl der Bundesländer ist jeweils mit dem Button 'Anzeigen' zu bestätigen</li>
                     <li>Die beiden 'Prognose' Controls gelten nur für den Punkt 'Prognose'</li>
                    </p>
                    <p> Die Skala für die Inzidenz ist in Stufen mit jeweils doppelter Inzidenz geteilt, mit Werten von 1,2,4,8,16,32,64,128, pro Tag und 100.000 Einwohner, 
                       wobei eine <b>WochenInzidenz von 50</b> einer <b>TagesInzidenz von 7</b> entspricht, bzw. ca. GesamtInzidenz von 560 für ganz Österreich.
                    </p>
                    <p> Die Ermittlung der <b>tatsächlichen Inzidenz</b> ist wegen der starken Schwankungen und Verzögerungen bei den Einmeldungen in das EMS nicht eindeutig.
                        In diesem Dashboard wird für die tagesaktuellen Werte und Prognosen ein 
                        gewichtetes lineares Modell der logarithmierten Inzidenz aus den letzten zehn Tagen herangezogen.
                    </p>
                    "


htmlWeatherMap <- "<p><b>Tages Inzidenz: Lage und Vorhersage</b></p>
                   <p>Die Tages Inzidenz ist die  <b>Anzahl der positiv Getesteten pro Tag pro 100.000 Einwohner</b>.
                      Von der AGES und den Medien in Österreich wird oft die '<b>Sieben Tage Inzidenz</b>' verwendet, das ist die Summe der Inzidenzen aus sieben Tagen.
                    </p><p>
                     Die Karte zeigt den <b>aktuellen Stand der Tages Inzidenz</b>, den <b>Trend</b>, und eine <b>Prognose</b> für die Tagesinzidenz in einer Woche.
                     Die Prognose wird aus der <b>Entwicklung der vergangenen zehn Tage</b> abgeschätzt. 
                     Alternative Berechnungsmethoden können auf der Seite '<b>Inzidenz Prognose</b>' ausprobiert werden.
                     </p><p>
                     Die <b>drei Symbole</b>, von links nach rechts, bedeuten: <b>Aktuelle Lage, Tendenz und Prognose</b> der TagesInzidenz. 
                    </p>
                   <p>
                          <p><b>Wetter Symbole:</b> Tages Inzidenz</p>
                      <center>
                        <table>
                          <tr><th>Symbol</th><th>Tages Inzidenz</th></tr>
                          <tr><td>Sonne </td><td align='right'> 0-4, Stufen 0,1,2</td></tr>
                          <tr><td>Sonne+Wolken </td><td align='right'> 4-8, Stufe 3</td></tr>
                          <tr><td>Sonne+Regen </td><td align='right'> 8-16, Stufe 4</td></tr>
                          <tr><td>Regen </td><td align='right'> 16-32, Stufe 5</td></tr>
                          <tr><td>Gewitter </td><td align='right'> 32+, Stufen 6+</td></tr>
                        </table>
                      </center>
                    </p>
                    <p>
                          <p><b>Tendenz Symbole:</b> <br>Anzahl Wochen bis zur Halbierung bzw. Verdoppelung der Fallzahlen</p>
                      <center>
                        <table>
                          <tr><th>Symbol</th><th>Wochen</th></tr>
                          <tr><td>Pfeil S </td><td align='right'> 0-2</td></tr>
                          <tr><td>Pfeil SSE </td><td align='right'> 2-4</td></tr>
                          <tr><td>Pfeil ESE </td><td align='right'> 4-8 </td></tr>
                          <tr><td>Pfeil E </td><td align='right'> mehr als 8</td></tr>
                          <tr><td>Pfeil ENE </td><td align='right'> 4-8</td></tr>
                          <tr><td>Pfeil NNE </td><td align='right'> 2-4</td></tr>
                          <tr><td>Pfeil N </td><td align='right'> 0-2</td></tr>
                        </table>
                      </center>
                    </p><p> Als <b>Grenzwert für Lockerungen</b> aus einem LockDown wurde von offizieller Seite immer wieder ein Sinken auf eine Wochen Inzidenz 
                      von 50 genannt, d.h. eine <b>Tages Inzidenz von ca. 8</b>. Als Grenzwert für ein neuerliches <b>Inkrafttreten eines Lockdown</b> wurde am 1.2.2021 eine Wochen Inzidenz 
                      von 200 genannt, d.h. eine <b>Tages Inzidenz ca. 32</b>.
                    </p>
                    <p><b>Anmerkung 1: Interpretation</b><br>Die AGES Daten unterscheiden nicht zwischen 'Positiv getestet im Massentest' und 'Positiv getestet bei 1450'. 
                          Damit ist die Bedeutung der Zahlen semi-definiert und ein Rückschluss auf das epidemiologische Geschehen nur eingeschränkt möglich
                    </p>
                    <p><b>Anmerkung 2: Berechnung</b><br>Die Berechnung des aktuellen Standes der Tages Inzidenz wird durch die starken täglichen Schwankungen bei den Einmeldungen
                                     in das EMS (Epidemiologisches Meldesystem) erheblich erschwert. Die dargestellten Werte basieren auf den Rückmeldungen der letzten 10 Tage.
                                     Details und alternative Berechnungmethoden siehe 'Inzidenz Prognose'
                    </p>"

htmlWeatherMapCounties <- "htmlWeatherMapCounties"

htmlIncidencePrediction <- "<p><b>Berechnung und Prognose der Tages Inzidenz</b></p>
                            <p><b>Prognose Menü (links unten) </b><li><b>BerechnungsTage:</b> Anzahl der vergangenen Tage, aus denen die Prognose erstellt wird.</li>
                                                                  <li><b>Berechnungsmodell:</b>  Lineare oder quadratische Regression der logarithmierten TagesInzidenzen</li>
                            </p>
                            <p><b>Einmeldung in EMS</b><br>Die Tages Inzidenz ist die Anzahl der Positiv Getesteten pro Tag pro 100.000 Einwohner, basierend auf den Einmeldungen in das EMS  (Epidemiologisches Meldesystem) der AGES.
                               Als Zeitpunkt wird von den Ministerien der Tag der Einmeldung, von der AGES der Tag der Auswertung eines Tests herangezogen.
                               Damit sind die von der AGES ausgewiesenene Daten näher am epidemiologischen Geschehen, führen aber zu Änderungen an den Zahlen vergangener Tage.
                               <br><b>Tägliche Schwankungen</b><br>Unabhängig davon schwankt die Anzahl der durchgeführten Tests stark mit Sonn und Feiertagen. Um die täglichen Schwankungen einigermassen auszugleichen wird das Wochenmittel berechnet.
                               Das hat den Nachteil, dass bei steigenden bzw. fallenden Infektionszahlen die ausgewiesenen Werte dem epidemiologischen Geschehen hinterherhinken bzw vorauseilen.
                               <br><b>Wochenmittel</b><br>
                               Beispiel: Steigt die Inzidenz während einer Woche von 1 auf 7, dann ergibt am 7ten Tag das Wochenmittel 4. Das ist aus epidemiologischer Sicht der korrekte Wert für den vierten Tag.
                               Das epidmiologische Geschehen der letzten drei Tage ist also über das Wochenmittel nicht beschreibbar.
                               <br><b>Letzte drei Tage</b><br> Das tatsächliche epidemiologische Geschehen der letzten drei Tage kann auf unterschiedliche Arten abgeschätzt werden.
                               <li>Fortsetzung des Entwicklung der letzten Tage: Die einfachste Variante ist die gleichförmige, d.h. lineare Weiterentwicklung.
                                   Findet gerade ein Verlangsamung oder Beschleunigung der Inzidenz statt, dann ist z.B. eine quadratische Kurve oft die bessere Wahl.
                                   Dieses Verfahren hat den Nachteil, dass die Einmeldungen der letzten Tage nicht berücksicht werden</li>
                               <li>Korrektur der Einmeldungen der letzten drei Tage auf Basis der Daten der gleichen Wochentage aus den vergangenen Wochen:
                                   Die Differenz zwischen Wochenmittel für einen Tag (aus den vergangenen und folgenden drei Tagen) zeigt systematische Abweichungen nach unten für das Wochenende
                                   und überdurchschnittlich viele oftmals z.B. für den Dienstag. Dieses Verfahren ist zu Zeiten vieler Feiertage oder sich ändernder Teststrategie nicht sehr zuverlässig.</li>
                                <p><b>Wetterkarte</b><br>Für diese Dashboard wurde ein Mittelweg gewählt: Die Berechnung der Inzidenz der letzten drei Tage basiert auf 1) den Wochentags Korrekturen
                                    aus den letzten 5 Wochen, und 2) aus den Mittelwerten der letzten 5 bzw. 3 Tage für vorgestern bzw. gestern.</p>
                                <p>Die <b>Berechnung</b> der Tages Inzidenz und der Prognose für die kommende Woche für die Wetterkarte ist eine gewichtete lineare Regression der letzten zehn Tage
                                    (die bestehen aus 7 Tagen Wochenmittel, und den letzten drei Tagen).
                                    Die Gewichtung nimmt Richtung Vergangenheit linear ab, und gibt den berechneten Werten für die letzten drei Tage weniger Bedeutung</p> 
"

htmlIncidenceStates <- "<p><b>Historie der Inzidenz in Bundesländern</b></p>
                        <p><b>Gestzmäßigkeit der Ansteckung</b><br>
                           Die Übertragung von COVID-19 erfolgt im wesentlichen durch die Atemluft. Viren werden beim Ausatmen an die Umwelt abgegeben, und beim Einatmen aufgenommen.
                           Jede infizierte Person infiziert weitere Personen, die immer mehr weitere Personen anstecken. Die Ausbreitung der Epidemie erfolgt in immer größerem Tempo.
                           Man spricht von 'b>exponentiellem Wachstum</b>'. Diese Art von Wachstum wird in den Anfangsphasen kaum wahrgenommen, erreicht aber in kurzer Zeit unüberschaubare Ausmasse.
                           Das liegt daran, dass die Anzahl der infizierten Personen sich in gleichen Zeitraum verdoppelt bzw. verzehnfacht. 
                           </p>
                          <p><b>Vorhersehbarkeit</b><br>Gleichwohl lässt sich die bevorstehende Katastrophe bereits in den Anfangsphasen erkennen, 
                           wenn man den explosionsartigen Charakter der Vermehrung miteinberechnet.
                           Dazu ist es notwendig den Zeitraum bis zur <b>Verdoppelung der Fallzahlen</b> zu beachten, und daraus die künftige Entwicklung abzuschätzen. 
                           Die Einordnung der Tagesinzidenz erfolgt in diesem Dashboard daher in einen 'Verdoppelungsraster' mit den <b>acht Inzidenz Stufen</b> 
                           [0,1,2,4,8,16,32,64,128], statt in die übliche lineare [0,10,20,30,40,50,...,100] Skala.
                        </p>
                        <p> Im <b>Menü links</b> kann zwischen der 'linearen' (ORF und Ministerium Darstellung) und der 'logarithmischen' (Verdoppelung) Darstellung umgeschaltet werden.
                        </p> 
                         "

htmlIncidenceCounties <- "<p><b>Historie der Inzidenz in Bezirken</b></p>
                          <p>Die Einmeldungen der Fallzahlen, d.h. der positiv Getesteten, in des EMS erfolgt auf Bezirksebene. 
                             Österreich ist in 90 Bezirke eingeteilt, wobei Wien in den von AGES veröffentlichten Daten als ein Bezirk geführt wird.
                          </p><p>Auffällige Zeitbereiche sind zunächst der <b>Begin der Epidemie</b> Anfang März 2020, 
                          in dem sich die Zahl der Erkrankungen in <b>einer Woche verzehnfacht</b> hat. 
                          </p>
                          <p><b>Vergleich Meereswellen</b><br>Führt man sich dieses Verhalten anhand von z.B. Wellen am Meer vor Augen, dann ergibt sich folgendes Bild:
                           Ausgangslage sind Anfang der ersten Woche <b>1cm</b> hohe Wellen. Anfang der zweiten Woche sind die Wellen <b>10cm</b>hoch, 
                           Anfang der dritten Woche <b>1m</b>. In der vierten Woche hinterlassen die <b>10m</b> hohen Wellen ein Bild der Verwüstung
                          </p><p>Dieses Verhalten lässt sich in den Monaten August bis Oktober beobachten. Näheres dazu siehe '<b>Rückblick 2020</b>.
                          </p><p> Weitere Auffälligkeit ist die <b>Änderung des Verhaltens</b> in den letzten beiden Oktober Wochen. 
                            Die Inzidenz steigt von grob 4-32 Mitte Oktober um 2 Stufen auf 16-128 Anfang November. Das entspricht in etwa einer Verdoppelung innerhalb einer Woche.
                            Mögliche Erklärung könnte das <b>Zusammenbrechen des Contact-Tracing</b> sein. Ab der ersten November Woche werden die Auswirkungen des Lockdown durch das Abflachen der Kurven sichtbar
                          </p>"

htmlChangeRateStates <- "<p><b>Ausbreitungsgeschwindigkeit</b></p>
                         <p> Die Geschwindigkeit der Verbreitung der Epidemie wird wird oftmals durch die sog. <b>Reproduktionszahl</b> beschrieben. 
                             Das ist die Anzahl an Menschen, die ein Infizierter seinerseits ansteckt. Diese Zahl wird u.a. von den Ministerien, dem ORF und der AGES verwendet.
                             Die genaue Berechnung ist nicht ganz einfach und ist auf der AGES Website in einem vierseitigen Artikel genau beschrieben.
                             </p><p> Eine andere, etwas einfachere, Möglichkeit die Ausbreitungsgeschwindigkeit zu messen ist die <b>prozentuelle Änderung der Fallzahlen</b> gegenüber dem Vortag.
                             Dieser Wert ist eng verknüpft mit der Anzahl der <b>Tage bis zur Verdoppelung</b> oder auch Verzehnfachung  der Fallzahlen.
                             Während die Reproduktionszahl und die prozentuelle Änderung der Fallzahlen die <b>Treiber des exponentiellen Wachstums</b> sind, 
                             beschreibt die Dauer bis zur Verdoppelung der Inzidenz die Auswirkungen anhand der täglichen Einmeldungen in das EMS. 
                          </p><p>
                             In nebenstehender Graphik sind beide Größen eingezeichnet. Links die Änderung der Inzidenz in % vom Vortag, 
                             rechts die entsprechende Dauer bis zu Verdoppelung der Fallzahlen. 
                             Die Werte der letzten drei Tage sind oftmals von den Schwankungen der Einmeldungen in das EMS verfälscht.
                         </p>"

htmlExponential <- "<p><b>Rückblick 2020</b></p>
                    <p>
                      Die drei Graphiken auf dieser Seie gehen der Frage nach, was aus der Entwicklung der <b>Inzidenz Zahlen im Sommer und Herbst 2020</b> abgeleitet werden kann.
                    </p>
                    <p><b>Inzidenz</b><br><p>
                      Aus der ersten Graphik wird deutlich, dass seit Ende Juli die <b>Fallzahlen gleichmäßig steigen</b>, mit einigen Abweichungen, aber in allen Bundesländern ähnlich.
                      Die Explosion, d.h. das exponentielle Wachstum, ist langsamer als im März 2020. Damals haben sich die Inzidenz Zahlen in einer Woche verzehnfacht.
                      Ab Anfang August ist die Geschwindigkeit der Ausbreitung mit 7-11 Wochen bis zur Verzehnfachung wesentlich langsamer (je nach Bundesland),
                      folgt aber gleichwohl dem <b>explosiven Muster der Verdoppelung</b> in festen Zeitintervallen.
                      Dieses Verhalten wird erst durch die Betrachtung mit 'Verdoppelungsstufen', d.h.  in einer logarithmischen Skala, deutlich.
                      Die in Ministerien und ORF üblicherweise gewählte 'lineare' Skala verschleiert diese der Pandemie inherente Gesetzmässigkeit. 
                      Die 'lineare' Skala kann durch enfernen des Häckchens bei 'LogScale' im Menü links entfernt werden.
                    </p><p>
                      Weiters lässt sich aus der ersten Graphik ablesen, dass seit Anfang August die <b>Inzidenz in drei Stufen gestiegen</b> ist.
                      Betrachtet man Österreich gesamt, so hat die erste Stufe Anfang August die Inzidenz von 1 auf 3 angehoben, 
                      die zweite Stufe Anfang Sepember von 3 auf 8, und die dritte Stufe ab Mitte Oktober von 8 auf 80.
                      Diese dritte Stufe (seit August 2020) wird von Ministerien und ORF als <b>'zweite Welle'</b>   bezeichnet.
                      Das Verhalten ist in allen Bundesländern ähnlich, z.T. zeitgleich, z.T. mit leichten Verschiebungen oder kleineren Zwischenstufen.
                      Gleichwohl mach die Graphik anhand der ersten beiden Stufen deutlich, 
                      daß eine dritte Stufe mit dramatischen Fallzahlen ohne rechtzeitige Gegenmassnahmen nicht vermeidbar ist.
                      Der <b>Zeitpunkt für die Gegenmassnahmen</b> ist das erreichen einer kritischen Inzidenz. 
                      Legt man diese kritische Inzidenz auf 10, so ist das der 5.Okt., bei 15 der 12.Okt, und bei 20 der 19.Okt. 
                      Auffällig ist die <b>starke Zunahme der Inzidenz ab 19.Okt.</b>, 
                      auch in Bundesländern mit bis dahin sehr gleichmäßigem Anstieg (e.g. Burgenland oder Kärnten) 
                    </p>
                     <p><b>Ausbreitungsgeschwindigkeit</b><br>Die zweite Graphik zeigt Geschwindigkeit der Zunahme bzw. Abnahme der Inzidenz. 
                        Die Messung erfolgt in Änderung in Prozent vom Vortag, 
                        und ist in der Skala rechts umgerechnet auf die 'Anzahl Tage bis Verdoppelung' bzw. Halbierung der Inzidenz.
                        An dieser Darstellung lassen sich die drei Stufen aus dem letzten Abschnitt eindeutig identifizieren.
                        Zu beachten ist, dass, solange die Ausbreitungsgeschwindigkeit größer als 0% ist, die Infektionszahlen zunehmen. 
                        Das ist zwischen August und November bis auf wenige Tage immer der Fall.
                    </p><p><b>Inzidenz und Ausbreitungsgeschwindigkeit</b><br>Für die Beurteilung der aktuellen Lage muss die Inzidenz
                      zusammen mit der Ausbreitungsgeschwindigkeit (dem Reproduktionsfaktor) betrachtet werden.
                      (wie insbesondere vom Gesundheitsministerium immer wieder betont wird).
                      Die dritte Graphik zeigt den Zusammenhang zwischen Inzidenz und Ausbreitungsgeschwindigkeit. Die Kurve beschreibt den <b>Weg Österreichs 
                      durch die Pandemie</b> (bzw. eines Bundeslandes). Die Monate sind durch unterschiedliche Symbole erkennbar.
                      Die Lage ist kritisch bei hoher Inzidenz und hoher Ausbreitungsgeschwindigkeit, also im oberen rechten Quadranten der Graphik.
                      Die Situation verbessert sich, je weiter die Inzidenz und Ausbreitungsgeschwindigkeit abnehmen, 
                      d.h. die Zahlen sich nach links und unten bewegen. <br> Für diese Graphik ist eine Auswahl von nur ein oder zwei Bundesländern vorteilhaft. 
                    </p>"
                  

htmlDescription <- read_file("./doc/COVID-19-WeatherMap.html")

