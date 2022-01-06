

htmlFrontPageNews <- "<i><b>News:</b>
                      Mit Version 1.0.0 vom 16.3.2021 berücksichtigt die Prognose der COVID Inzidenz die <b>Effekte 
                      von Öffnungen (Beschleunigung) und Lockdowns (Abbremsung)</b> in den Bundesländern.
                      Details dazu den Reiter '<b>Bundesländer</b>'und '<b>Prognose</b></i>'
                    "
htmlFrontPageTop <- "<b>News:</b><i>
                      <br>Version 1.5.0: (03.01.2022) Neue Version Vergleich 2.3.4 Welle und Wirksamkeit der Impfung.  
                      <br>Version 1.4.0: (31.08.2021) Drei Darstellungen zu Wirksamkeit der Impfungen, Sterblichkeit und benutzten Formeln
                      <br>Version 1.3.0: (24.05.2021) Korrektur Wetterkarte und Prognose der Bezirke (Prognose ident mit Bundesländern)
                      <br>Version 1.2.0: (21.04.2021) Daten aus dem Dashboard des Gesundheitsministeriums ab 1.2.2021 für die 'Dritte Welle'
                      <br>Version 1.1.0: (04.04.2021) Zeitlicher Verlauf der Einmeldungen in das EMS (letzte 5 Wochen)
                      <br>Version 1.0.0 (16.3.2021) Die Prognose der COVID Inzidenz für die Bundesländer berücksichtigt die Effekte 
                      von <b>Öffnungen (Beschleunigung)</b> und <b>Lockdowns (Abbremsung)</b>.
                      Details siehe  '<b>Bundesländer</b>' und '<b>Prognose</b></i>'
                      <h4>Zielsetzung und Funktionsweise</h4>
                     <p>
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
                      <b>Umschaltung 'ORF Darstellung' <-> 'StufenModell'</b> mit der Checkbox im Menü links
                      </p><p>
                        Unterschiedliche Varianten für die <b>Prognose der Inzidenz</b> auf Bundesländer Ebene können interaktiv unter Punkt 'Prognose' gerechnet werden.
                        Parallel zum Stufenmodell werden unter Punkt '<b>Geschwindigkeit</b>' die Ausbreitungsschübe  betrachtet.
                        Der '<b>Rückblick 2020</b>' motiviert die Möglichkeit einer mittelfristigen Prognose bei gegebenem Verhalten in der Bevölkerung.
                      </p>
                      
                    "



htmlFrontPageBot <- "<h4>Bedienungsanleitung</h4>
                    <p>Das <b>Menü auf der linken Seite</b> ermöglicht es Einfluss auf den Inhalt von Graphiken ud Tabellen zu nehmen (die 'Wetterkarten' sind fix)
                    </p>
                    <p>
                     <li>Welche Controls eine Graphik beeinflussen sind steht unter 'Menüauswahl' oben links</li>
                     <li>Die <b>Auswahl der Bundesländer</b> ist jeweils mit dem Button 'Anzeigen' zu bestätigen</li>
                     <li>Die <b>Anzahl der angezeigten Monate</b> kann mit 'ZeitRaum' eingestellt werden</li>
                     <li>Die beiden <b>'Prognose'</b> Controls gelten nur für den Punkt 'Prognose'</li>
                    </p>
                    <h4>StufenModell</h4>
                    <p> Die Skala für die Inzidenz ist in <b>Stufen mit jeweils doppelter Inzidenz</b> geteilt:
                    <center><table>
                       <tr><td>Stufe&nbsp;</td><td>TagesInzidenz&nbsp;</td><td>WochenInzidenz&nbsp;</td> <td>&nbsp;</td> <td>Stufe&nbsp;</td><td>TagesInzidenz&nbsp;</td><td>WochenInzidenz</td</tr>
                       <tr><td>0</td><td>0-1</td><td>0-7</td><td>&nbsp;</td><td>4</td><td>8-16</td><td>56-112</td></tr>
                       <tr><td>1</td><td>1-2</td><td>7-14</td><td>&nbsp;</td><td>5</td><td>16-32</td><td>112-224</td></tr>
                       <tr><td>2</td><td>2-4</td><td>14-28</td><td>&nbsp;</td><td>6</td><td>32-64</td><td>224-448</td></tr>
                       <tr><td>3</td><td>4-8</td><td>28-56</td><td>&nbsp;</td><td>7</td><td>64-128</td><td>448-896</td></tr>
                    </table></center>
                    </p>
                    <p> Die Ermittlung der <b>tatsächlichen Inzidenz</b> ist wegen der starken Schwankungen und Verzögerungen bei den Einmeldungen in das EMS nicht eindeutig.
                        In diesem Dashboard werden die tagesaktuellen Werte und Prognosen mit einem 
                        gewichteten linearen Modell der logarithmierten Inzidenzen aus den letzten zehn Tagen abgeschätzt.
                    </p>
                    "


htmlWeatherMap <- "<p><b>Tages Inzidenz: Lage und Vorhersage</b></p>
                   <p>Die Tages Inzidenz ist die  <b>Anzahl der positiv Getesteten pro Tag pro 100.000 Einwohner</b>.
                      Von der AGES und den Medien in Österreich wird oft die '<b>Sieben Tage Inzidenz</b>' verwendet, das ist die Summe der Inzidenzen aus sieben Tagen.
                    </p><p>
                     Die Karte zeigt den <b>aktuellen Stand der Tages Inzidenz</b>, den <b>Trend</b>, und eine <b>Prognose</b> für die Tagesinzidenz in einer Woche anhand von 
                     5 Wetter- und 7 Tendenzsymbolen.
                     
                     </p><p>
                     Die <b>drei Symbole</b>, von links nach rechts, bedeuten: <b>Aktuelle Lage, Tendenz und Prognose</b> der TagesInzidenz. 
                    </p>
                   <p>
                          <p><b>Wetter Symbole:</b> Tages Inzidenz</p>
                      <center>
                        <table>
                          <tr><th>Symbol</th><th>Tages Inzidenz</th></tr>
                          <tr><td>Sonne </td><td align='right'> 0-4</td></tr>
                          <tr><td>Sonne+Wolken </td><td align='right'> 4-8</td></tr>
                          <tr><td>Sonne+Regen </td><td align='right'> 8-16</td></tr>
                          <tr><td>Regen </td><td align='right'> 16-32</td></tr>
                          <tr><td>Gewitter </td><td align='right'> 32</td></tr>
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
                    </p><p>
                        Die Progonose berücksichtigt seit Version 1.0.0 eine <b>Verlangsamung oder Beschleunigung</b> des Ausbreitungsgeschehens. 
                        In beiden Fällen ist die entstehende Kurve eine Parabel, und hat ein <b>Maximum bzw. Minimum</b>. 
                        Maximum und Minimum können in der <b>Vergangenheit oder Zukunft</b> liegen.
                        Für die Berechnung der Parabel werden die letzen 28 Tage berücksichtigt (siehe auch Tab 'Prognose'), das entspricht etwas konservativ den zeitlichen Verzögerungen bei <b>Lockdowns oder Lockerungen</b>
                        So flacht sich z.B. die Inzidenzkurve etwa 10 Tage nach Inkrafttreten eines Lockdowns ab, erreicht nach 2-3 Wochen ein Maximum, und beginnt danach wieder zu sinken.
                    </p><p> Als <b>Grenzwert für Lockerungen aus einem LockDown</b> wurde von offizieller Seite immer wieder ein Sinken auf eine Wochen Inzidenz 
                      von 50 genannt, d.h. eine <b>Tages Inzidenz von ca. 8</b>. Als Grenzwert für ein neuerliches <b>Inkrafttreten eines Lockdown</b> wurde 
                      am 1.2.2021 vom Gesundheitsministerium eine Wochen Inzidenz von 200 genannt, d.h. eine <b>Tages Inzidenz ca. 32</b>.
                    </p>"

htmlWeatherMapCounties <- "<p><b>Inzidenz Bezirke: Lage und Vorhersage</b></p>
                          <p>Die Anzahl der positiv getesteten Personen wird von AGES für <b>94 Bezirke</b> veröffentlicht.
                              Wien ist zu einem Bezirk zusammengefasst. 
                           </p><p>
                              Die <b>Abschätzung der tatsächlichen Inzidenz</b> sowie die Berechnung der Prognose ist aufgrund 
                              der geringen Statistik in einigen Bezirken sowie der <b>Abhängigkeit von zeitlichen und lokalen Gegebenheiten</b> 
                              wie Testkampagnen oder COVID Clustern z.T. mit einer <b>grösseren Unsicherheit</b> versehen.
                              Größere Differenzen zu den AGES Tagesmeldungen sind ein Hinweis auf solche Unwägbarkeiten.
                          </p>
                          <p>
                          <li><b>Änderung</b> gibt die Änderung der Inzidienz in Prozent zum Vortag an</li>
                          <li><b>TageDoppelt</b> bzw. <b>TageHälfte</b> ist die geschätzte Anzahl der Tage bis zur Verdoppelung bzw Halbierung der Inzidenz aus der Entwicklung  
                                 der letzten 10 Tage</li>
                          </p>
                          <p>
                             <b>Anmerkung1: Lineares Modell</b><br>Im Gegensatz zur Prognose für die Bundesländer, die Beschleunigung bzw. Abschwächung berücksichtigt, 
                             wird für die Bezirke ein lineares Modell, d.h. gleichförmige Ausbreitung, aus den letzten zwei Wochen gerechnet.
                          </p>
                            <p><b>Anmerkung2: Interpretation</b><br>Die AGES Daten unterscheiden nicht zwischen 'Positiv getestet im Massentest' und 'Positiv getestet bei 1450'. 
                          Damit ist die Bedeutung der Zahlen semi-definiert und ein Rückschluss auf das epidemiologische Geschehen nur eingeschränkt möglich
                          </p>"

other <- "                <li><b>Inzidenz8</b> ist die Anzahl Tag bis erreichen der Inzidenz=8 (Ende LockDown)</li>
                          <li><b>Inzidenz32</b> ist die Anzahl Tag bis erreichen der Inzidenz=32 (Anfang LockDown)</li>
                          <p>
                          Das entspricht den ursprünglich für Ende des LockDowns angestrebten WochenInzidenz von 50, und dem für ein neuerliches
                          Inkrafttreten eines Lockdowns genannten Gesamt TagesInzidenz von 4000
                          </p>
                            <p><b>Anmerkung 2: Berechnung</b><br>Die Berechnung des aktuellen Standes der Tages Inzidenz wird durch die starken täglichen Schwankungen bei den Einmeldungen
                          in das EMS (Epidemiologisches Meldesystem) erheblich erschwert. Die dargestellten Werte basieren auf den Rückmeldungen der letzten 10 Tage.
                          Details und alternative Berechnungmethoden siehe 'Inzidenz Prognose'
                          </p>
                                <p><b>Vergleich Meereswellen</b><br>Führt man sich dieses Verhalten anhand von z.B. Wellen am Meer vor Augen, dann ergibt sich folgendes Bild:
                           Ausgangslage sind Anfang der ersten Woche <b>1cm</b> hohe Wellen. Anfang der zweiten Woche sind die Wellen <b>10cm</b>hoch, 
                           Anfang der dritten Woche <b>1m</b>. In der vierten Woche hinterlassen die <b>10m</b> hohen Wellen ein Bild der Verwüstung
                          </p>
                          Dieses Verhalten lässt sich in den Monaten August bis Oktober beobachten. Näheres dazu siehe '<b>Rückblick 2020</b>.

                          <p><b>Vorhersehbarkeit</b><br>Gleichwohl lässt sich die bevorstehende Katastrophe bereits in den Anfangsphasen erkennen, 
                           wenn man den explosionsartigen Charakter der Vermehrung miteinberechnet.
                           Dazu ist es notwendig den Zeitraum bis zur <b>Verdoppelung der Fallzahlen</b> zu beachten, und daraus die künftige Entwicklung abzuschätzen. 
                           Die Einordnung der Tagesinzidenz erfolgt in diesem Dashboard daher in einen 'Verdoppelungsraster' mit den <b>acht Inzidenz Stufen</b> 
                           [0,1,2,4,8,16,32,64,128], statt in die übliche lineare [0,10,20,30,40,50,...,100] Skala.
                           Für die Wetterkarte bietet sich mit der Baufort Skala für die Windstärke eine Entsprechung zur Stufenskala an.
                           Die Einmeldungen der Fallzahlen, d.h. der positiv Getesteten, in des EMS erfolgt auf Bezirksebene. 
                        </p>
"

htmlIncidencePrediction <- "<p><b>Berechnung und Prognose der Tages Inzidenz</b></p>
                            <p><b>Prognose Menü (links unten) </b><li><b>BerechnungsTage:</b> Anzahl der vergangenen Tage, aus denen die Prognose erstellt wird.</li>
                                                                  <li><b>Berechnungsmodell:</b>  Lineare oder quadratische Regression der logarithmierten TagesInzidenzen</li>
                            </p>
                            <p>Die Erstellung einer Prognose des epidemiologischen Geschehens von COVID-19 steht vor einer Reihe von Herausforderungen.
                               Der Vorgang der Ansteckung selbst ist nicht beobachtbar, die Kenntnis über den Verbreitungsgrad Krankheit ist indirekt über 
                               Symptome, Tests und deren Einmeldung in das EMS vermittelt. Und das mit Wochentag und Bundesland spezifischen Abläufen und 
                               zeitlichen Abständen zwischen Test und Verfügbarkeit der zugehörigen Daten.
                            </p><p>Als <b>Zeitpunkt für die Erkrankung</b>  wird von den Ministerien der <b>Tag der Einmeldung</b>, 
                               von der AGES der <b>Tag der Auswertung</b> eines positiven Tests angesetzt. 
                               Damit sind die von der AGES ausgewiesenene Daten um die <b>Dauer der Datenübermittlung</b> näher am epidemiologischen Geschehen, 
                               führen aber zu nachträglichen Änderungen an den Zahlen vergangener Tage.
                               Bei Tests von symptomatischen Personen ist in beiden Varianten der Zeitraum zwischen dem Auftreten der Symptome und der Durchführung des Tests nicht bekannt.
                               Weiters ist nicht veröffentlicht, ob ein Test an einer symptomatischen oder asymptomatischen Person durchgeführt wurde. 
                             </p><p><b>Tägliche Schwankungen: </b> Unabhängig davon schwankt die Anzahl der durchgeführten Tests stark mit Sonn und Feiertagen. 
                                 Um die täglichen Schwankungen auszugleichen wird von AGES die <b>Sieben Tage Inzidenz</b> als Summe der Inzidenzen der letzten Woche berechnet.
                                 Das ermöglicht eine einigermassen stabile Entwicklung, hat aber den Nachteil, dass bei steigenden bzw. fallenden Infektionszahlen 
                                 die ausgewiesenen Werte dem epidemiologischen Geschehen hinterherhinken bzw vorauseilen.
                                 <br>Steigt z.B. die Inzidenz während einer Woche von 1 auf 7, dann ergibt das am 7ten Tag das Wochenmittel 4. 
                                 Das ist aus epidemiologischer Sicht aber der korrekte Wert für den vierten Tag.
                                 Das epidemiologische Geschehen der letzten drei Tage ist also über das Wochenmittel nicht direkt beschreibbar.
                             </p><p>Dieses Dashboard versucht vor der Berechnung der Prognose die <b>tatsächliche Inzidenz der letzten drei Tage</b> aus den eingemeldeten Daten abzuschätzen:
                                 <li>Die übliche Höhe der <b>nachträglichen Einmeldungen</b> wird aus den vergangenen Wochen pro Bundesland und Wochentag  ermittelt und 
                                     auf die Einmeldungen der letzten drei Tage aufgeschlagen.
                                </li><li>Die <b>wochentagsabhängige Differenz</b> zwischen der täglich gemeldeten Inzidenz und dem Wochenmittel wird 
                                    aus den vergangenen Wochen pro Bundesland und Wochentag ermittelt und die Einmeldungen der letzten drei Tage entsprechend skaliert.
                                </li>
                                   Die so berechneten Werte sind z.B. zu Zeiten vieler Feiertage oder sich ändernder Teststrategie nicht sehr zuverlässig und 
                                   werden daher für die Prognose mit etwas verringertem Gewicht berücksicht.
                             </p><p>
                                   Auf Basis dieser Daten wird das Wochenmittel als Basis für die Erstellung der Prognose berechnet
                                   (moving mean mit window size 7, bzw. window size 5,3,1 für die letzen drei Tage) 
                              </p><p><b>Prognose Parameter:</b> Für die Berechnung der Prognose der <b>Wetterkarte</b> wird ein lineares Modell auf Basis der letzten 14 Tage gerechnet.
                                Um dem inherent exponentiellen Charakter der Verbreitung von COVID-19 gerecht zu werden, 
                                wird, wie im Stufenmodell mit seinem Verdoppelungsraster angezeigt, ausschließlich mit logarithmierten Inzidenz Werten gearbeitet.
                             </p><p><b>LockDown und Lockerungen:</b> Führen zu einer Bremsung bzw. Beschleunigung des exponentiellen Wachstums. 
                                Die Auswirkungen auf die Prognose sind statt mit einer Geraden (lineares Modell) besser mit einer einfachen Kurve (quadratisches Modell, Parabel) abzuschätzen. 
                                Diese beiden Varianten können, zusammen mit der Anzahl der zu berücksichtigen Tage für das Modell, im <b>Menü links unten</b> eingestellt werden. 
                              </p>
                                "

htmlIncidenceStates <- "<p><b>Historie der Inzidenz in Bundesländern</b></p>
                        <p><b>Gestzmäßigkeit der Ansteckung</b><br>
                           Die Übertragung von COVID-19 erfolgt im wesentlichen durch die <b>Atemluft</b>. Viren werden beim Ausatmen an die Umwelt abgegeben, und beim Einatmen aufgenommen.
                           Jede infizierte Person infiziert weitere Personen, die immer mehr weitere Personen anstecken. Es kommt zum Multiplikationseffekt.
                           Man spricht von '<b>exponentiellem Wachstum</b>'.
                           </p><p>
                             Diese Art von Wachstum wird in den Anfangsphasen kaum wahrgenommen, kann aber in kurzer Zeit unüberschaubare Ausmasse annehmen.
                             Das liegt daran, dass sich bei exponentielem Wachstum die Anzahl der infizierten Personen sich in gleichen Zeiträumen verdoppelt bzw. verzehnfacht. 
                           </p>
                           <p><b>Stufen Modell:</b> Für die Darstellung der Inzidenz wird daher ein Stufen Modell gewählt, wobei jede Stufe doppelt so hoch ist wie die vorangegangene.
                           Die Stufen werden von 0 bis 8 durchnummeriert, mit <br><b>Stufe 0 = Inzidenz 0-1, <br>Stufe 1 = Inzidenz 1-2, <br>Stufe 2 = Inzidenz 2-4</b>, usw<br>
                           Die Nummer der Stufe entspricht der 2er Potenz der Inzidenz, 2^0=1, 2^1=2, 2^2=4, 2^3=8,...,2^8=256.<br> 
                           Diese Darstellung bietet den Vorteil, dass die Steigerung auch bei niedrigen Inzidenz Werten am Anfang der Ausbreitung gut sichtbar wird.
                           </p>
                           <p>Wird für nebenstehende Graphik ein Zeitraum ab Juli oder August 2020 gewählt, dann ist die Gültigkeit des Stufenmodells für den Zeitraum bis Mitte November offensichtlich.
                           Gleichzeitig wird offensichtlich, daß die sog. <b>'Zweite Welle' nur die (vorhersehbare) Fortsetzung der Entwicklung seit Anfang August</b> ist.
                           In diesem Zeitraum steigt die Inzidenz im Schnitt in ca. 3 Wochen auf das Doppelte, bzw in ca. 10 Wochen auf das zehnfache.
                           </p>
                        <p> Dieser Zusammenhang verschwindet, wenn im <b>Menü links</b> das Häckchen bei <b>LogScale</b> (verwendet von z.B. ORF und Ministerium) entfernt.
                        </p> 
                         "

htmlIncidenceCounties <- "<p><b>Historie der Inzidenz in Bezirken</b></p>
                          <p>Österreich ist in 90 Bezirke eingeteilt, wobei Wien in den von AGES veröffentlichten Daten als ein Bezirk geführt wird.
                          </p><p>Auffällige Zeitbereiche sind zunächst der <b>Begin der Epidemie</b> Anfang März 2020, 
                          in dem sich die Zahl der Erkrankungen in <b>einer Woche verzehnfacht</b> hat (Zeitraum im Menü links auf z.B. 12 Monate stellen). 
                          </p><p>
                            Dann von Anfang August bis Anfang November das durch Abstandsregeln verlangsamte exponentielle Wachstum, 
                            mit einer<b>Verzehnfachung der Inzidenz in ca. 10 Wochen</b>.
                          </p><p> 
                            Weitere Auffälligkeit ist die <b>Änderung des Verhaltens</b> in den letzten beiden Oktober Wochen. 
                            Die Inzidenz in diesem Zeitraum um zwei Stufen. Das entspricht einer Verdoppelung innerhalb einer Woche.
                            Mögliche Erklärung könnte das <b>Zusammenbrechen des Contact-Tracing</b> sein. Ab der ersten November Woche werden die Auswirkungen des Lockdown sichtbar.
                          </p>
                          <p>
                            <b>Aktuelle Entwicklung</b>: Seit Ende Dezember 2020 hat sich die Inzidenz um nur knapp eine halbe Stufe veringert.
                            Wenn die neuen Massnahmen nicht nachhaltig gegen die Öffnung und die Mutationen gewinnen, ist der <b>nächste Lockdown vorprogrammiert</b>.
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

htmlExponential_1 <- "<p><b>Rückblick 2020</b></p>
                    <p>
                      Die drei Graphiken auf dieser Seie gehen der Frage nach, was aus der Entwicklung der <b>Inzidenz Zahlen im Sommer und Herbst 2020</b> abgeleitet werden kann.
                    <br>
                    <i> Die Darstellungen werden übersichtlicher, wenn im Menü links nur sehr wenige Bundesländer ausgewählt sind (z.B. Österreich und ein Bundesland)</i>
                    </p>
                    <p><b>Inzidenz</b><br><p>
                      Aus der ersten Graphik wird deutlich, dass seit Ende Juli die <b>Fallzahlen gleichmäßig steigen</b>, mit einigen Abweichungen, aber in allen Bundesländern ähnlich.
                      Die Explosion, d.h. das exponentielle Wachstum, ist langsamer als im März 2020. Damals haben sich die Inzidenz Zahlen in einer Woche verzehnfacht.
                      Ab Anfang August ist die Geschwindigkeit der Ausbreitung mit 7-11 Wochen bis zur Verzehnfachung wesentlich langsamer (je nach Bundesland),
                      folgt aber gleichwohl dem <b>explosiven Muster der Verdoppelung</b> in festen Zeitintervallen.
                      Dieses Verhalten wird erst durch die Betrachtung mit 'Verdoppelungsstufen', d.h.  in einer logarithmischen Skala, deutlich.
                      Die in Ministerien und ORF üblicherweise gewählte 'lineare' Skala verschleiert diese der Pandemie inherente Gesetzmässigkeit. 
                      Die 'lineare' Skala kann durch enfernen des Häckchens bei 'LogScale' im Menü links entfernt werden.
                    </p>"
htmlExponential_2 <- "
                    <p><b>Ausbreitungsgeschwindigkeit</b><br>Die zweite Graphik zeigt Geschwindigkeit der Zunahme bzw. Abnahme der Inzidenz. 
                        Die Messung erfolgt in Änderung in Prozent vom Vortag, 
                        und ist in der Skala rechts umgerechnet auf die 'Anzahl Tage bis Verdoppelung' bzw. Halbierung der Inzidenz.
                        An dieser Darstellung lassen sich die drei Stufen aus dem letzten Abschnitt eindeutig identifizieren.
                        Zu beachten ist, dass, solange die Ausbreitungsgeschwindigkeit größer als 0% ist, die Infektionszahlen zunehmen. 
                        Das ist zwischen August und November bis auf wenige Tage immer der Fall.
                        </p><p>
                      Weiters lässt sich aus den ersten beiden Graphiken ablesen, dass seit Anfang August die <b>Inzidenz in drei Stufen gestiegen</b> ist.
                      Betrachtet man Österreich gesamt, so hat die erste Stufe Anfang August die Inzidenz von 1 auf 3 angehoben, 
                      die zweite Stufe Anfang Sepember von 3 auf 8, und die dritte Stufe ab Mitte Oktober von 8 auf 80.
                      Diese dritte Stufe (seit August 2020) wird von Ministerien und ORF als <b>'zweite Welle'</b>   bezeichnet.
                      Das Verhalten ist in allen Bundesländern ähnlich, z.T. zeitgleich, z.T. mit leichten Verschiebungen oder kleineren Zwischenstufen.
                      Gleichwohl machen die Graphiken anhand der ersten beiden Stufen deutlich, 
                      daß eine dritte Stufe mit dramatischen Fallzahlen ohne rechtzeitige Gegenmassnahmen nicht vermeidbar ist.
                      Der <b>Zeitpunkt für die Gegenmassnahmen</b> ist das erreichen einer kritischen Inzidenz. 
                      Legt man diese kritische Inzidenz auf 10, so ist das der 5.Okt., bei 15 der 12.Okt, und bei 20 der 19.Okt. 
                      Auffällig ist die <b>starke Zunahme der Inzidenz ab 19.Okt.</b>, 
                      auch in Bundesländern mit bis dahin sehr gleichmäßigem Anstieg (e.g. Burgenland oder Kärnten) 
                    </p>"
htmlExponential_3 <- "
                    <p><b>Inzidenz und Ausbreitungsgeschwindigkeit</b><br>Für die Beurteilung der aktuellen Lage muss die Inzidenz
                      zusammen mit der Ausbreitungsgeschwindigkeit (dem Reproduktionsfaktor) betrachtet werden.
                      (wie insbesondere vom Gesundheitsministerium immer wieder betont wird).
                      Die dritte Graphik zeigt den Zusammenhang zwischen Inzidenz und Ausbreitungsgeschwindigkeit. Die Kurve beschreibt den <b>Weg Österreichs 
                      durch die Pandemie</b> (bzw. eines Bundeslandes). Die Monate sind durch unterschiedliche Symbole erkennbar.
                      Die Lage ist kritisch bei hoher Inzidenz und hoher Ausbreitungsgeschwindigkeit, also im oberen rechten Quadranten der Graphik.
                      Die Situation verbessert sich, je weiter die Inzidenz und Ausbreitungsgeschwindigkeit abnehmen, 
                      d.h. die Zahlen sich nach links und unten bewegen. <br> Für diese Graphik ist eine Auswahl von nur ein oder zwei Bundesländern vorteilhaft. 
                    </p>"
                  
hlpBmsgpk <- "<p><b>GesundheitsMinisterium</b></p>"

htmlWave42Intro <- "<p>Ein <b>direkter Vergleich</b> der 2. und 4. Welle in 10-12/2020 bzw. 10-12/2021, sowie der 3. Welle in 02-04/2021, 
              ist wegen der unterschiedlichen Rahmenbedingungen (Anzahl/Art Tests, Virus Variante, Spitalsbehandlung, Impfung, Datenerfassung, etc.)<b> mit grossen Schwierigkeiten verbunden</b>.
              Insbesondere ist eine stabile Referenzgrösse z.B. in Form von regelmässigen Prevalenzstudien nicht verfügbar.
              Die folgende Zusammenstellung ist daher eher ein Versuch die vorhandenen <b>Daten aus verschiedenen Blickwinkeln</b> zu betrachten und sowohl
              <b>mögliche Interpretationen</b> als auch Schwierigkeiten damit aufzuzeigen. 
              </p>"
htmlWave42_1 <-"<p>
              <b>Sterblichkeit anhand der <i>Gesamtanzahl</i> der Infizierten und Verstorbenen je Welle</b>
              </p><p>
              Die erste Graphik zeigt in der oberen Reihe die Wahrscheinlichkeit an Covid-19 zu sterben, nach Geschlecht und Alter.
              Auffällig die <b><i>starke Altersabhängigkeit</b></i>, sowie die <b><i>Unterschiede zwischen Frauen und Männern</b></i>. Am kritischsten ist die Situation für Männer über 85.
              <br>Mangels entsprechender Daten kann nicht zwischen Immunisierten und NichtImmunisierten unterschieden werden. 
              <b><i>Anzeichen für die Wirksamkeit der Impfung</b></i> lassen sich aber an der vergleichsweise geringeren Sterblichkeit in den Altersgruppen 
              mit den höheren Durchimpfungsraten erkennen (Verbesserung 2.->3.->4. Welle).
              </p><p>
              <b>Vergleich der Sterblichkeit in der 3. und 4. Welle mit der 2. Welle</b><br> 
              In der unteren Reihe der ersten Graphik wird die Sterblichkeit in der 2. Welle als Referenz (100%) verwendet. 
              Die Sterblichkeit in der 3. und 4. Welle wird relativ dazu betrachtet, nach Altersgruppe und Geschlecht.
              <li>Die relative Sterblichkeit in der 3. Welle ist wenig einheitlich, liegt aber in den höheren Altersgruppen niedriger als bei den Jüngeren, 
              in grober Übereinstimmung mit dem Trend der Durchimpfungsrate.
              </li><li>Die Sterblichkeit in der 4. Welle liegt bei den Jüngeren bei grob 70%, bei den Älteren unter 50% im Vergleich zur 2. Welle, grob entlang der Höhe der Durchimpfungsrate. 
              </li>
              </p><p>
              <b>Schlussfolgerungen</b><br>
              Diese Daten legen nahe, dass die <b><i>Impfung das Risiko an COVID-19 zu sterben beträchtlich senkt, für Immunisierte auf vermutlich grob 35% bei Infektion</b></i>. 
              Dieser Wert ist grob verträglich mit einem <b><a href='https://goeg.at/Intensivpflege_COVID', target='_blank'>Bericht der Gesundheit Österreich (GÖ)</a></b> aus 12/2021, 
              in der der <b><i>Schutzfaktor der Impfung gegen schweren Verlauf mit 73% </b></i> beziffert  wird, basierend auf dem Impfstatus von Spitalspatienten in der 4. Welle.
              <br>Die in der  Einleitung genannten unterschiedlichen Rahmenbedingungen, sowie insbesondere 
              <b><i>fehlende Daten</b></i> zum Impfstatus der Infizierten, Hospitalisierten und Verstorbenen nach Region, Alter und Geschlecht <b><i>verhindern eine bessere Quantifizierung</i></b>.
              </p>"
htmlWave42_2 <-"<p>
              <b>Vergleich der Inzidenzen, Hospitalisierungen, Intensivbetreuungen und Sterbefälle</b>  
              </p><p>
              In der zweiten Graphik werden die drei Wellen anhand der Anteile von schweren und schwersten Verläufen betrachtet.
              Verglichen wird die Gesamtanzahl der Infektionen, Hospitalisierungen, ICU Aufenthalte und Sterbefälle in den drei Wellen.
              <br>
              Die Verstorbenen umfassen die Sterbefälle auf ICU (22% 2.Welle, 40% 3.Welle), auf Normalstationen (56% 2.Welle, 55% 3.Welle) und ausserhalb eines Spitals (22% 2.Welle, 5% 3.Welle)
              Der Zeitbereich für die Erfassung der Sterbefälle ist um 14 Tage gegenüber dem Zeitbereich der Inzidenz verschoben.
              <br>Die Charakteristika der drei Wellen sind in den Bundenländern z.T. sehr unterschiedlich. 
              Einige Gemeinsamkeiten im Vergleich zur 2. Welle lassen sich erkennen: 
              </p><p>
              <b>Zusammenfassung</b> der Plots von links nach rechts:
              <br>[Auswahl der Bundesländer über das Menü links]
               <li>Weniger als 50% Hospitalisierungen <b>pro Infektion</b> in der 4. Welle 
               </li><li>Mehr ICU Aufenthalte in der 3. Welle, weniger in der 4. Welle
               </li><li>Weniger als 30% Verstorbene pro Infektion in der 4. Welle
               </li><li>Mehr ICU Aufenthalte <b>pro Hospitalisierung</b> in der 3. und 4. Welle
               </li><li>50%-70% Verstorbene pro Hospitalisierung in der 4. Welle
               </li><li>Weniger als 50% Verstorbene <b>pro ICU Aufenthalt</b>
               </li>
              </p><p>
              <b>Anmerkungen</b>
               <li>Die Zählung der Positiven als Referenz in Spalte 1-3 ist wegen der 10-20 mal häufigeren Tests in der 3. und 4. Welle problematisch
               </li><li>Der erhöhte Anteil an ICU Aufenthalten pro Hospitalisierung in der 3. und 4. Welle kann viele Ursachen haben, 
                    von schwererem Krankheitsverlauf bei den Varianten bis geänderte Behandlungsmethoden
               </li><li>Vor diesem Hintergrund ist die Abschätzung des Schutzes der Impfung vor schwerstem Verlauf auf Basis der öffentlich verfügbaren Zahlen mit einigen Unsicherheiten behaftet
               </li>
              </p><p><i>Eine bessere Abschätzung liefert der o.g. <b>Bericht der 'Gesundheit Österreich'</b> auf Basis detailierter Daten zur Spitalsbelegung seit Anfang der Pandiemie</i> 
              </p>
              "
htmlWave42_3 <-"<p>
              <b>Vergleich des zeitlichen Verlaufs der 2. und 4. Welle</b>  
              </p><p>
              In der dritten Graphik wird der zeitliche Verlauf der in der zweiten Graphik betrachteten Größen untersucht.
              Betrachtet wird die tägliche Zahl der Tests, Infektionen, Spitalsaufenthalte, ICU Aufenthalte und Sterbefälle pro 100.000 Einwohner (Population).
               </p><p>
               Dargestellte Größen, von oben nach unten:
               <li><b>Tests</b> (Reihe 1, newTestPop)
               </li><li><b>Tagesinzidenz</b> (Reihe 2, newConfPop)
               </li><li><b>Hospitalisierungen</b> (Reihe 3, newHospPop)
               </li><li><b>Aufenthalte auf Intensiv</b> (Reihe 4, newICUPop)
               </li><li><b>Sterbefälle</b> (Reihe 5, newDeathPop)
               </li>
               </p><p>
               <i>Zur Betrachtung unterschiedlicher Aspekte kann im <b>Menü links</b> der Masstab der y-Achse mittels der <b>Checkbox 'StufenModell'</b> zwischen linear und logarithmisch umgeschalten werden,
               sowie die <b>Normierung der Kenngrössen</b> unter 'Vergleich 2. und 4. Welle' verändert werden:</i>
               <li><b>Anzahl pro 100k Einwohner</b> (new*Pop)</li>
               <li><b>Prozent vom Maximum 2.Welle</b> (new*Pop) </li>
               <li><b>Prozent Einw/Tested/Positiv</b> (rel**) <br>%Getestete pro Einwohner (relConfPop), <br>%Positive pro Getestete (relConfTest), <br>%Spital/%ICU/%Verstorben pro Positiven (rel*Conf) </li>
               </p><p>
               Der <b>Vergleich 4. gegen 2. Welle</b> entlang der Zeitachse zeigt (Stand 2021-12-31):
               <li><b>Begin beider Wellen im Sommer (4:Juli bzw 2:August)</b></li>
               <li><b>Starke Unterschiede zwischen den Bundesländern</b></li>
               <li>Rund <b>15 bis 25 (Wien) mal so viele Tests</b>: Während der 2.Welle wurden täglich rund 250 Tests pro 100k Einwohner durchgeführt (0.25%), in der 4.Welle grob 5000 (5%) (erste Reihe) </li>
               </p><p>
               Ein <b>Vergleich der Maxima der 4.Welle gegenüber der 2.Welle</b> lässt sich bei Skalierung der Graphiken auf 'Prozent vom Maximum 2.Welle' bei ausgeschaltenem 'StufenModell' ablesen:
               <li>Inzidenz 150%-250%</li>
               <li>Hospitalisierung 60%-100%</li>
               <li>Intensivstation 70%-160%</li>
               <li>Verstobene 25%-60%</li>
               </p><p>
               Dieser Vergleich der Maxima der beiden Wellen gibt einen relevanten Eindruck zu den Unterschieden hinsichtlich Auslastung der Spitäler und Intensivstationen.
               Die maximale Inzidenz ist in der 4. Welle bei 20x mehr Tests etwa doppelt so hoch, die Hospitaliserungen sind niedriger, die Intensivstationen ähnlich belegt wie in der 2.Welle, 
               mit bisher deutlich weniger Sterbefällen, jeweils mit starken Unterschieden zwischen den Bundesländern. 
               </p>
               "
htmlDissemination1 <- "<p><b>Inzidenz und Ausbreitungsgeschwindigkeit</b><br>Für die Beurteilung der aktuellen Lage muss die Inzidenz
                      zusammen mit der Ausbreitungsgeschwindigkeit (dem Reproduktionsfaktor) betrachtet werden.
                      (wie insbesondere vom Gesundheitsministerium immer wieder betont wird).
                      Die dritte Graphik zeigt den Zusammenhang zwischen Inzidenz und Ausbreitungsgeschwindigkeit. Die Kurve beschreibt den <b>Weg Österreichs 
                      durch die Pandemie</b> (bzw. eines Bundeslandes). Die Monate sind durch unterschiedliche Symbole erkennbar.
                      Die Lage ist kritisch bei hoher Inzidenz und hoher Ausbreitungsgeschwindigkeit, also im oberen rechten Quadranten der Graphik.
                      Die Situation verbessert sich, je weiter die Inzidenz und Ausbreitungsgeschwindigkeit abnehmen, 
                      d.h. die Zahlen sich nach links und unten bewegen. <br> Für diese Graphik ist eine Auswahl von nur ein oder zwei Bundesländern vorteilhaft. 
                    </p>"
htmlDissemination2 <- "<p><b>Inzidenz und Ausbreitungsgeschwindigkeit</b><br>Für die Beurteilung der aktuellen Lage muss die Inzidenz
                      zusammen mit der Ausbreitungsgeschwindigkeit (dem Reproduktionsfaktor) betrachtet werden.
                      (wie insbesondere vom Gesundheitsministerium immer wieder betont wird).
                      Die dritte Graphik zeigt den Zusammenhang zwischen Inzidenz und Ausbreitungsgeschwindigkeit. Die Kurve beschreibt den <b>Weg Österreichs 
                      durch die Pandemie</b> (bzw. eines Bundeslandes). Die Monate sind durch unterschiedliche Symbole erkennbar.
                      Die Lage ist kritisch bei hoher Inzidenz und hoher Ausbreitungsgeschwindigkeit, also im oberen rechten Quadranten der Graphik.
                      Die Situation verbessert sich, je weiter die Inzidenz und Ausbreitungsgeschwindigkeit abnehmen, 
                      d.h. die Zahlen sich nach links und unten bewegen. <br> Für diese Graphik ist eine Auswahl von nur ein oder zwei Bundesländern vorteilhaft. 
                    </p>"

htmlDescription <- read_file("./blog/COVID-19-WeatherMap.html")
htmlNews <- read_file("./blog/COVID-19-News.html")
htmlAge <- read_file("./blog/COVID-19-GeneralMortality.html")
htmlFormulas <- read_file("./blog/COVID-19-Formulas.html")


more <- "
- ![image](../www/iconWeather-Sun.png) Inzidenz unter 4 
- ![image](../www/iconWeather-Sun-Cloud.png) Inzidenz 4-8 
- ![image](../www/iconWeather-Rain.png) Inzidenz 8-16 
- ![image](../www/iconWeather-Rain.png) Inzidenz 16-32 
- ![image](../www/iconWeather-Thunder.png) Inzidenz über 32 

- ![image](../www/iconDirection-S.png) Halbierung der Inzidenz in weniger als 2 Wochen
- ![image](../www/iconDirection-SSE.png) Halbierung der Inzidenz in 2-4 Wochen
- ![image](../www/iconDirection-ESE.png) Halbierung der Inzidenz in 4-8 Woche
- ![image](../www/iconDirection-E.png) Halbierung / Verdoppelung in mehr als 8 Wochen
- ![image](../www/iconDirection-ENE.png) Verdoppelung in 4-8  Wochen
- ![image](../www/iconDirection-NNE.png) Verdoppelung in 2-4 Wochen
- ![image](../www/iconDirection-N.png) Verdoppelung in weniger als 2 Wochen

"
