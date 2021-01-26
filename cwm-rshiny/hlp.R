

htmlWeatherMap <- "<p><b>Tages Inzidenz: Lage und Vorhersage</b></p>
                   <p>Bedeutung der drei Symbole, von links nach rechts: <b>Aktuelle Lage, Tendenz und Prognose</b> der TagesInzidenz. 
                      Die Tages Inzidenz ist die  Anzahl der positiv Getesteten pro Tag pro 100.000 Einwohner
                    </p>
                   <p>
                          <p>Wetter Symbole: Tages Inzidenz</p>
                      <center>
                        <table>
                          <tr><th>Symbol</th><th>Tages Inzidenz</th></tr>
                          <tr><td>Sonne </td><td> 0-4</td></tr>
                          <tr><td>Sonne+Wolken </td><td> 4-8</td></tr>
                          <tr><td>Sonne+Regen </td><td> 8-16</td></tr>
                          <tr><td>Regen </td><td> 16-32</td></tr>
                          <tr><td>Gewitter </td><td> 32 und mehr</td></tr>
                        </table>
                      </center>
                    </p>
                    <p>
                          <p>Tendenz Symbole: <br>Wochen bis zur Halbierung bzw. Verdoppelung</p>
                      <center>
                        <table>
                          <tr><th>Symbol</th><th>Wochen</th></tr>
                          <tr><td>Pfeil S </td><td> 0-2</td></tr>
                          <tr><td>Pfeil SSE </td><td> 2-4</td></tr>
                          <tr><td>Pfeil ESE </td><td> 4-8 </td></tr>
                          <tr><td>Pfeil E </td><td> mehr als 8</td></tr>
                          <tr><td>Pfeil ENE </td><td> 4-8</td></tr>
                          <tr><td>Pfeil NNE </td><td> 2-4</td></tr>
                          <tr><td>Pfeil N </td><td> 0-2</td></tr>
                        </table>
                      </center>
                    </p>
                    <p><b>Problem 1: Interpretation</b><br>Die AGES Daten unterscheiden nicht zwischen 'Positiv getestet im Massentest' und 'Positiv getestet bei 1450'. 
                          Damit ist die Bedeutung der Zahlen semi-definiert und ein Rückschluss auf das epidemiologische Geschehen nur eingeschränkt möglich
                    </p>
                    <p><b>Problem 2: Berechnung</b><br>Die Berechnung des aktuellen Standes der Tages Inzidenz wird durch die starken täglichen Schwankungen bei den Einmeldungen
                                     in das EMS (Epidemiologisches Meldesystem) erheblich erschwert. Die dargestellten Werte basieren auf den Rückmeldungen der letzten 10 Tage.
                                     Details und alternative Berechnungmethoden siehe 'Inzidenz Prognose'
                    </p>"

htmlIncidencePrediction <- "<p><b>Berechnung und Prognose der Tages Inzidenz</b></p>
                            <p><b>Prognose Menü (links unten) </b><li>BerechnungsTage: Anzahl der vergangenen Tage, aus denen die Prognose erstellt wird.</li>
                                                                  <li>Berechnungsmodell:  Lineare oder quadratische Regression der logarithmierten TagesInzidenzen</li>
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

htmlIncidenceStates <- "<p><b>Historie der Tages Inzidenz</b></p>
                        <p><b>Ansteckung und explosionsartige Vermehrung</b><br>
                           Die Übertragung von COVID-19 erfolgt im wesentlichen durch die Atemluft. Viren werden beim Ausatmen an die Umwelt abgegeben, und beim Einatmen aufgenommen.
                           Jede infizierte Person infiziert weitere Personen, die wiederum mehr weitere Personen anstecken. Die Ausbreitung der Epidemie erfolgt in immer größerem Tempo.
                           Man spricht von 'exponentiellem Wachstum'. Diese Art von Wachstum wird in den Anfangsphasen kaum wahrgenommen, erreicht aber in kurzer Zeit unüberschaubare Ausmasse.
                           Das liegt daran, dass die Anzahl der infizierten Personen sich in gleichen Zeitraum verdoppelt bzw. verzehnfacht. 
                           So hat sich z.B. am Anfang der Epidemie Anfang März 2020 die Anzahl der positv getesteten Personen in einer Woche verzehnfacht. 
                          </p>
                          <p><b>Beispiel Meereswellen</b><br>Führt man sich dieses Verhalten anhand z.B. Wellen am Meer vor Augen, dann ergibt sich folgendes Bild:
                           In der ersten Woche fallen die <b>1cm</b> hohen Wellen nicht auf. Auch in der zweiten Woche stören die <b>10cm</b> Wellen nicht.
                           Die <b>1m</b> hohen Wellen in der dritten Woche sorgen für Badespass. In der vierten Woche hinterlassen die <b>10m</b> hohen Wellen ein Bild der Verwüstung
                          </p>
                          <p><b>Exponentielles Wachstum</b><br>Gleichwohl lässt sich die bevorstehende Katastrophe bereits in den Anfangsphasen absehen, wenn man die explosionsartige Vermehrung miteinberechnet.
                           Dazu ist es notwendig den Zeitraum bis zur <b>Verdoppelung der Fallzahlen</b> zu beachten, und daraus die künftige Entwicklung abzuschätzen. 
                           Die Einordnung der Tagesinzidenz erfolgt daher in einen 'Verdoppelungsraster', also 1,2,4,8,16,32,64,128, statt in die übliche lineare 0,10,20,30,40,50,...,100 Skala.
                        </p>
                        <p> Im <b>Menü links</b> kann zwischen der 'linearen' (ORF und Ministerium Darstellung) und der 'logarithmischen' (Verdoppelung) Darstellung umgeschaltet werden.
                        </p> 
                         "

htmlIncidenceCounties <- "<p><b>Tages Inzidenz in den Bezirken</b></p><p></p>"

htmlChangeRateStates <- "<p><b>Ausbreitungsgeschwindigkeit</b></p><p></p>"

htmlExponential <- "<p><b>Rückblick 2020</b></p><p></p>"

htmlDescription <- read_file("./doc/COVID-19-WeatherMap.html")

