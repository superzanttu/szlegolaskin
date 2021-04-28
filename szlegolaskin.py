# import scala.collection.mutable.{ArrayBuffer, ListBuffer}

# object Main extends App {

# Algoritmit palikoiden pakkaamiseen

def koodaa(x,x,y,asento):
    return (str(z * 2048 + (x + 6) * 64 + (y + 6) * 2 + asento))

def dekoodaa(pala):
    num = int(pala)
    z = int(num/2048)
    x = int((num % 2048)/ 64)-6
    y = int((num % 64) / 2) -6
    return(x,y,z, num % 2)

# Apufunktioita kombojen käsittelyyn
def haeMinimit(kombo):
    # Hakee yhdistelmän pienimmät z-, x- ja y-arvot
    minZ = 100
    minX = 100
    minY = 100
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        if (z < minZ):
            minZ = z
        if (x < minX):
            minX = x
        if (y < minY):
            minY = y
    return(minZ,minX,minY)

def haeMitat(kombo,leveys,korkeus):
    # Hakee yhdistelmän ulottuvuudet z-, x- ja y-akseleilla
    mittaZ = -100
    mittaX = -100
    mittaY = -100
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        if (asento == 0):
            lev = leveys
        else:
            lev = korkeus
        if (asento == 0):
            kor = korkeus
        else:
            kor = leveys
        if (z > mittaZ):
            mittaZ = z
        if (x + lev > mittaX):
            mittaX = x + lev
        if (y + kor > mittaY):
            mittaY = y + kor
    return (mittaZ, mittaX, mittaY)

def normalisoi(kombo):
      # Siirtää yhdistelmää koordinaatistossa niin, että pienin x- j y-koordinaatti on 0.
      # Z-akselilla siirtoa ei tarvita, koska pienin z on aina 0.
    def siirra(pala: Char, zz: Int, xx: Int, yy: Int): Char = {
      val (z, x, y, asento) = dekoodaa(pala)
      koodaa(z + zz, x + xx, y + yy, asento)
    }
    normalisoitu.clear()
    val (minZ, minX, minY) = haeMinimit(kombo)
    for (pala <- kombo) {
      normalisoitu.append(siirra(pala, 0, -minX, -minY))
    }
    normalisoitu.sortInPlace()
  }

def normalisoi(kombo)

  def paivitaVersiot(kombo: String): Unit = {
    // Luo yhdistelmästä 90, 180 ja 270 astetta käännetyt versiot.
    k90.clear()
    k180.clear()
    k270.clear()
    versiot.clear()
    versiot.addOne(kombo)
    var (mittaZ, mittaX, mittaY) = haeMitat(kombo, leveys, korkeus)
    for (pala <- kombo) {
      val (z, x, y, asento) = dekoodaa(pala)
      if (leveys == korkeus) {
        k90.append(koodaa(z, -y - korkeus + mittaY, x, 0))
        k180.append(koodaa(z, -x - leveys + mittaX, -y - korkeus + mittaY, 0))
        k270.append(koodaa(z, y, -x - leveys + mittaX, 0))
      } else {
        val lev = if (asento == 0) leveys else korkeus
        val kor = if (asento == 0) korkeus else leveys
        k90.append(koodaa(z, -y - kor + mittaY, x, 1 - asento))
        k180.append(koodaa(z, -x - lev + mittaX, -y - kor + mittaY, asento))
        k270.append(koodaa(z, y, -x - lev + mittaX, 1 - asento))
      }
    }
    versiot.addOne(k90.sortInPlace().toString())
    versiot.addOne(k180.sortInPlace().toString())
    versiot.addOne(k270.sortInPlace().toString())
  }

  def paivitaSopivat(kombo: String, syvyys: Int): Unit = {
    // Etsii palikat, jotka kiinnittyvät annettuun yhdistelmään.
    // Ensin käydään läpi yhdistelmän palikat ja kerätään kaikki palikat, jotka kiinnittyvät niihin.
    sopivatPalat.clear()
    for (pala <- kombo) {
      val (z, x, y, asento) = dekoodaa(pala)
      // Apumuuttujia lev ja kor (leveys ja korkeus) käytetään alempana silmukoissa.
      // Ne määritellään tässä käsillä olevan palikan asennon mukaan:
      val lev = if (asento == 0) leveys else korkeus
      val kor = if (asento == 0) korkeus else leveys
      // Ensimmäisessä silmukassa kerätään kiinnittyvät palikat, joiden asento on sama kuin käsillä olevan.
      for (xx <- (-lev + 1) until lev) {
        for (yy <- (-kor + 1) until kor) {
          // Kaksoiskappaleiden tuotannon vähentämiseksi voidaan toista palikkaa lisättäessä
          // jättää huomiotta palikat, joiden x < 0. Samat yhdistelmät nimittäin syntyvät 180 astetta
          // käännettyinä
          // x:n ollessa yli 0. Siksi seuraava ehto:
          if (syvyys > 2 || xx >= 0) {
            sopivatPalat.add(koodaa(z + 1, x + xx, y + yy, asento))
            // Ensimmäinen palikka on aina yhdistelmän alin palikka. Ensimmäisestä palikasta
            // alaspäin ei tarvitse rakentaa, koska niin syntyvät yhdistelmät syntyvät muutenkin:
            if (z > 0) {
              sopivatPalat.add(koodaa(z - 1, x + xx, y + yy, asento))
            }
          }
        }
      }
      // Toisessa silmukassa kerätään kiinnittyvät palikat, joiden asento on eri kuin käsillä olevan.
      // Tämä on tietysti tarpeen ainoastaan, jos palikan muoto on pitkulainen eli leveys != korkeus.
      if (leveys != korkeus) {
        for (xx <- (-kor + 1) until lev) {
          for (yy <- (-lev + 1) until kor) {
            // Seuraavalla rajauksella rajoitetaan jälleen kaksoiskappaleiden syntyä:
            if (syvyys > 2 || xx >= (- ((kor - 1) / 2): Int)) {
              sopivatPalat.add(koodaa(z + 1, x + xx, y + yy, 1 - asento))
              if (z > 0) {
                sopivatPalat.add(koodaa(z - 1, x + xx, y + yy, 1 - asento))
              }
            }
          }
        }
      }
    }
    // Seuraavaksi poistetaan kerätystä kiinnittyvien palikoiden joukosta ne, jotka ovat päällekkäisiä
    // jonkin yhdistelmän palikan kanssa.
    paallekkaisetPalat.clear()
    for (pala <- kombo) {
      val (z, x, y, asento) = dekoodaa(pala)
      if (asento == 0) {
        for (xx <- (-leveys + 1) until leveys) {
          for (yy <- (-korkeus + 1) until korkeus ) {
            paallekkaisetPalat.add(koodaa(z, x + xx, y + yy, 0))
          }
        }
        if (leveys != korkeus) {
          for (xx <- (-korkeus + 1) until leveys) {
            for (yy <- (-leveys + 1) until korkeus) {
              paallekkaisetPalat.add(koodaa(z, x + xx, y + yy, 1))
            }
          }
        }
      } else {
        for (xx <- (-korkeus + 1) until korkeus) {
          for (yy <- (-leveys + 1) until leveys ) {
            paallekkaisetPalat.add(koodaa(z, x + xx, y + yy, 1))
          }
        }
        for (xx <- (-leveys + 1) until korkeus) {
          for (yy <- (-korkeus + 1) until leveys) {
            paallekkaisetPalat.add(koodaa(z, x + xx, y + yy, 0))
          }
        }
      }
    }
    sopivatPalat.subtractAll(paallekkaisetPalat)
  }

  def haeRatkaisut(kierros: Int): Unit = {
    // Tämä funktio luo (kierros + 1) palikan yhdistelmät. Tietokannasta haetaan (kierros) palikan
    // yhdistelmät (muistin säästämiseksi 1000 kerrallaan) ja niihin lisätään yksi kerrallaan
    // kaikki kiinnittyvät palikat. Näin syntyvät yhdistelmät kirjoitetaan tietokantaan.
    uudetKombot.clear()
    if (kierros == 1) {
      // Ensimmäinen palikka ei vaadi laskentaa.
      KomboDB.lisaaKombo(kierros, koodaa(0, 0, 0, 0).toString)
    } else {
      val maara = KomboDB.lukumaara(kierros - 1) // (kierros) palikan yhdistelmien määrä
      var jaljella = maara // niistä vielä käsittelemättä
      var laskuri : Long = 0 // laskuri prosessin seurantaa varten
      while (jaljella > 0) {
        val era = if (jaljella < 1000) jaljella else 1000
        val edellisetKombot = KomboDB.haeKombot(kierros - 1, maara - jaljella, era).toList
        jaljella -= era
        for (kombo <- edellisetKombot) {
          paivitaSopivat(kombo, kierros)
          for (palikka <- sopivatPalat) {
            // Lisätään yksi palikka ja normalisoidaan syntynyt yhdistelmä
            normalisoi(kombo + palikka.toString)
            // Luodusta yhdistelmästä tehdään 90, 180 ja 270 astetta käännetyt versiot.
            // Kaikista neljästä versiosta jäljelle jätetään vain se, jonko merkkijono on arvoltaan pienin.
            // Kun toimitaan näin, vältetään käännettyjen kaksoiskappaleiden synty.
            paivitaVersiot(normalisoitu.toString)
            uudetKombot.addOne(versiot.min)
          }
          // Luodut yhdistelmät kirjoitetaan tietokantaan sadan kappaleen erissä. Tietokanta on
          // määritelty niin, että vain uniikit yhdistelmät tallentuvat.
          // Samalle kerrotaan käyttäjälle, missä mennään.
          laskuri += 1
          if (laskuri % 5000 == 0 || laskuri == maara) {
            KomboDB.lisaaKombot(kierros, uudetKombot)
            uudetKombot.clear()
            val prosentti = (laskuri:Float) * 100 / maara
            print("\r" + (kierros) + " palikkaa: " + f"$prosentti%1.2f" + " %")
          }
        }
      }
    }
  }

  def tulostaKombo(kombo: String): Unit = {
    // Funktio tulostaa yhdistelmän palikat luettavassa muodossa.
    for (pala <- kombo) {
      val (z, x, y, asento) = dekoodaa(pala)
      println("z = " + z + ", x = " + x + ", y = " + y + ", asento = " + asento)
    }
  }

  // Alla luetaan komentorivin argumenteista seuraavat tiedot, tai jos argumentteja ei ole annettu,
  // asetetaan niille oletusarvot:
  // 1. tietokantatiedoston nimi (oletus: kombot.db)
  // 2. yhdistelmiin käytettävien palikoiden määrä (oletus: 6)
  // 3. palikan korkeus (oletus: 4)
  // 4. palikan leveys (oletus: 2)
  // 5. kuinka monen palikan yhdistelmistä aloitetaan (oletus: 1)
  //    (Jos esimerkiksi tiedät tietokannassa olevan valmiina kolmen palikan yhdistelmät, voit
  //    säästää aikaa antamalla aloitusarvoksi 4, jolloin valmiina olevia yhdistelmiä ei lasketa uudelleen.)
  // Voit antaa komentoriviltä kaikki nämä arvot tai vain osan niistä, jolloin puuttuville annetaan oletusarvo.
  // Numeeriset argumentit luetaan juuri tässä järjestyksessä (palikoiden määrä, korkeus, leveys, aloitus).
  // Jos argumentti ei ole numeerinen, se tulkitaan tiedostonimeksi.
  val numeroargumentit = new ListBuffer[Int]
  var tiedostonimi = "kombot.db"
  for (a <- args) {
    try {
      val arvo = a.toInt
      numeroargumentit.addOne(arvo)
    } catch {
      case _: Throwable => tiedostonimi = a
    }
  }
  val palikoita = if (numeroargumentit.length > 0) numeroargumentit(0).toInt else 6
  val korkeus = if (numeroargumentit.length > 1) numeroargumentit(1).toInt else 4
  val leveys = if (numeroargumentit.length > 2) numeroargumentit(2).toInt else 2
  val aloitus = if (numeroargumentit.length > 3) numeroargumentit(3).toInt else 1

  // Luodaan rutiineissa käytettävät tietorakenteet.
  val sopivatPalat = scala.collection.mutable.Set.empty[Char]
  val paallekkaisetPalat = scala.collection.mutable.Set.empty[Char]
  val k90, k180, k270, normalisoitu = new StringBuilder
  val versiot = new ArrayBuffer[String]
  val uudetKombot = new ArrayBuffer[String]

  println("Luodaan " + palikoita + " " + korkeus + "×" + leveys + " palikan yhdistelmät")

  val aloitusaika = System.nanoTime

  // Avataan yhteys tietokantaan.
  KomboDB.alusta(tiedostonimi)

  // Pääluuppi
  KomboDB.poistaTaulut(aloitus) // Puhdistetaan tietokannasta kaikki taulut paitsi ne joita käytetään laskennan pohjana
  for (kierros <- aloitus to palikoita) {
    KomboDB.luoTaulu(kierros)
    haeRatkaisut(kierros)
    println("\r" + (kierros) + " palikka" + (if (kierros == 1) ": " else "a: ") +
      KomboDB.lukumaara(kierros) + " ratkaisu"
      + (if (kierros == 1) "                  " else "a                  "))
    val aika = (System.nanoTime - aloitusaika) / 1e9d
    println("  Kulunut aika " + f"$aika%1.2f" + " sekuntia.")
  }

  // Suljetaan tietokantayhteys.
  KomboDB.sulje()
}
