

htmlWeatherMap <- "<p><b>WeatherMap</b></p>
                   <p>Bedeutung der drei Icons, von links nach rechts: Aktuelle Lage, Tendenz und Prognose der TagesInzidenz. 
                      Die Tages Inzidenz ist die  Anzahl der positiv Getesteten pro Tag pro 100.000 Einwohner
                    </p>
                   <p>Wetter Icons: Inzidenz<br>
                      <center>
                            <table>
                              <tr><td><b>Symbol </b></td><td><b> Tages Inzidenz</b></td></tr>
                              <tr><td>Sonne </td><td> 0-4</td></tr>
                              <tr><td>Sonne+Wolken </td><td> 4-8</td></tr>
                              <tr><td>Sonne+Regen </td><td> 8-16</td></tr>
                              <tr><td>Regen </td><td> 16-32</td></tr>
                              <tr><td>Gewitter </td><td> 32 und mehr</td></tr>
                            </table>
                      </center>
                    </p>
                    <p>Tendenz Icons: <br>Anzahl Wochen bis zur Halbierung bzw. Verdoppelung der TagesInzidenz <br>
                      <center>
                            <table>
                              <tr><td><b>Symbol </b></td><td><b> Wochen</b></td></tr>
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
                    <p><b>Anmerkung:</b><br>Die AGES Daten unterscheiden nicht zwischen 'Positiv getestet im Massentest' und 'Positiv getestet bei 1450'. 
                          Damit ist die Bedeutung der Zahlen semi-definiert und ein Rückschluss auf das epidemiologische Geschehen nur eingeschränkt möglich
                    </p>"

htmlIncidencePrediction <- "<p><b>IncidencePrediciton</b></p>
                            <p>Prognose der Tages Tages Inzidenz, d.h. der Anzahl der Positiv Getesteten pro Tag pro 100.000 Einwohner</p>
                            <p><li>Prognose Tage: <br>Anzahl der vergangenen Tage, aus denen die Prognose erstellt wird (siehe Menü links).</li>
                               <li>Berechnungsmethode: <br> Gewichtete quadratische Regression der logarithmierten TagesInzidenzen der vergangenen Tage. 
                                                            Gewichte linear abnehmend Richtung Vergangenheit</li></p>
                            <p><b>Anmerkung:</b><br>Die AGES Daten unterscheiden nicht zwischen 'Positiv getestet im Massentest' und 'Positiv getestet bei 1450'. 
                            Damit ist die Bedeutung der Zahlen semi-definiert und ein Rückschluss auf das epidemiologische Geschehen nur eingeschränkt möglich</p>"

htmlIncidenceStates <- "<p><b>IncidenceStates</b></p><p></p>"

htmlIncidenceCounties <- "<p><b>IncidenceCounties</b></p><p></p>"

htmlDescription <- read_file("./doc/COVID-19-WeatherMap.html")
