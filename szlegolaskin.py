"""
szlegolaskin.py - LEGO-palikoiden yhdistelmälaskin

Laskee kaikki mahdolliset tavat yhdistää tietty määrä LEGO-palikoita.
Käyttää SQLite-tietokantaa yhdistelmien tallennukseen.
"""

import sys
import time
from kombodb import KomboDB


# Algoritmit palikoiden pakkaamiseen

def koodaa(z, x, y, asento):
    """Koodaa palikan sijainti ja asento yhdeksi kokonaisluvuksi."""
    return z * 2048 + (x + 6) * 64 + (y + 6) * 2 + asento


def dekoodaa(pala):
    """Purkaa koodatun palikan sijainti ja asento."""
    num = int(pala)
    z = num // 2048
    x = (num % 2048) // 64 - 6
    y = (num % 64) // 2 - 6
    asento = num % 2
    return (z, x, y, asento)


# Apufunktioita kombojen käsittelyyn

def hae_minimit(kombo):
    """Hakee yhdistelmän pienimmät z-, x- ja y-arvot."""
    min_z = 100
    min_x = 100
    min_y = 100
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        if z < min_z:
            min_z = z
        if x < min_x:
            min_x = x
        if y < min_y:
            min_y = y
    return (min_z, min_x, min_y)


def hae_mitat(kombo, leveys, korkeus):
    """Hakee yhdistelmän ulottuvuudet z-, x- ja y-akseleilla."""
    mitta_z = -100
    mitta_x = -100
    mitta_y = -100
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        lev = leveys if asento == 0 else korkeus
        kor = korkeus if asento == 0 else leveys
        if z > mitta_z:
            mitta_z = z
        if x + lev > mitta_x:
            mitta_x = x + lev
        if y + kor > mitta_y:
            mitta_y = y + kor
    return (mitta_z, mitta_x, mitta_y)


def normalisoi(kombo):
    """Siirtää yhdistelmää koordinaatistossa niin, että pienin x- ja y-koordinaatti on 0.
    Z-akselilla siirtoa ei tarvita, koska pienin z on aina 0."""
    def siirra(pala, zz, xx, yy):
        z, x, y, asento = dekoodaa(pala)
        return koodaa(z + zz, x + xx, y + yy, asento)

    min_z, min_x, min_y = hae_minimit(kombo)
    normalisoitu = []
    for pala in kombo:
        normalisoitu.append(siirra(pala, 0, -min_x, -min_y))
    normalisoitu.sort()
    return normalisoitu


def paivita_versiot(kombo, leveys, korkeus):
    """Luo yhdistelmästä 90, 180 ja 270 astetta käännetyt versiot."""
    k90 = []
    k180 = []
    k270 = []
    versiot = []
    versiot.append(kombo_merkkijonoksi(kombo))
    mitta_z, mitta_x, mitta_y = hae_mitat(kombo, leveys, korkeus)
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        if leveys == korkeus:
            k90.append(koodaa(z, -y - korkeus + mitta_y, x, 0))
            k180.append(koodaa(z, -x - leveys + mitta_x, -y - korkeus + mitta_y, 0))
            k270.append(koodaa(z, y, -x - leveys + mitta_x, 0))
        else:
            lev = leveys if asento == 0 else korkeus
            kor = korkeus if asento == 0 else leveys
            k90.append(koodaa(z, -y - kor + mitta_y, x, 1 - asento))
            k180.append(koodaa(z, -x - lev + mitta_x, -y - kor + mitta_y, asento))
            k270.append(koodaa(z, y, -x - lev + mitta_x, 1 - asento))
    k90.sort()
    k180.sort()
    k270.sort()
    versiot.append(kombo_merkkijonoksi(k90))
    versiot.append(kombo_merkkijonoksi(k180))
    versiot.append(kombo_merkkijonoksi(k270))
    return versiot


def paivita_sopivat(kombo, syvyys, leveys, korkeus):
    """Etsii palikat, jotka kiinnittyvät annettuun yhdistelmään."""
    # Ensin käydään läpi yhdistelmän palikat ja kerätään kaikki palikat,
    # jotka kiinnittyvät niihin.
    sopivat_palat = set()
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        # Apumuuttujia lev ja kor (leveys ja korkeus) käytetään alempana silmukoissa.
        # Ne määritellään tässä käsillä olevan palikan asennon mukaan:
        lev = leveys if asento == 0 else korkeus
        kor = korkeus if asento == 0 else leveys
        # Ensimmäisessä silmukassa kerätään kiinnittyvät palikat,
        # joiden asento on sama kuin käsillä olevan.
        for xx in range(-lev + 1, lev):
            for yy in range(-kor + 1, kor):
                # Kaksoiskappaleiden tuotannon vähentämiseksi voidaan toista palikkaa
                # lisättäessä jättää huomiotta palikat, joiden x < 0.
                if syvyys > 2 or xx >= 0:
                    sopivat_palat.add(koodaa(z + 1, x + xx, y + yy, asento))
                    # Ensimmäinen palikka on aina yhdistelmän alin palikka.
                    # Ensimmäisestä palikasta alaspäin ei tarvitse rakentaa.
                    if z > 0:
                        sopivat_palat.add(koodaa(z - 1, x + xx, y + yy, asento))
        # Toisessa silmukassa kerätään kiinnittyvät palikat, joiden asento on eri
        # kuin käsillä olevan. Tarpeen vain jos leveys != korkeus.
        if leveys != korkeus:
            for xx in range(-kor + 1, lev):
                for yy in range(-lev + 1, kor):
                    if syvyys > 2 or xx >= -(kor - 1) // 2:
                        sopivat_palat.add(koodaa(z + 1, x + xx, y + yy, 1 - asento))
                        if z > 0:
                            sopivat_palat.add(koodaa(z - 1, x + xx, y + yy, 1 - asento))

    # Seuraavaksi poistetaan kerätystä kiinnittyvien palikoiden joukosta ne,
    # jotka ovat päällekkäisiä jonkin yhdistelmän palikan kanssa.
    paallekkaiset_palat = set()
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        if asento == 0:
            for xx in range(-leveys + 1, leveys):
                for yy in range(-korkeus + 1, korkeus):
                    paallekkaiset_palat.add(koodaa(z, x + xx, y + yy, 0))
            if leveys != korkeus:
                for xx in range(-korkeus + 1, leveys):
                    for yy in range(-leveys + 1, korkeus):
                        paallekkaiset_palat.add(koodaa(z, x + xx, y + yy, 1))
        else:
            for xx in range(-korkeus + 1, korkeus):
                for yy in range(-leveys + 1, leveys):
                    paallekkaiset_palat.add(koodaa(z, x + xx, y + yy, 1))
            for xx in range(-leveys + 1, korkeus):
                for yy in range(-korkeus + 1, leveys):
                    paallekkaiset_palat.add(koodaa(z, x + xx, y + yy, 0))

    sopivat_palat -= paallekkaiset_palat
    return sopivat_palat


def kombo_merkkijonoksi(kombo):
    """Muuntaa kombon (lista kokonaislukuja) merkkijonoksi tallennusta varten."""
    return ",".join(str(p) for p in kombo)


def merkkijono_komboksi(s):
    """Muuntaa merkkijonon takaisin komboksi (lista kokonaislukuja)."""
    if not s:
        return []
    return [int(p) for p in s.split(",")]


def hae_ratkaisut(kierros, leveys, korkeus, db):
    """Luo (kierros) palikan yhdistelmät.
    Tietokannasta haetaan (kierros - 1) palikan yhdistelmät (muistin säästämiseksi
    1000 kerrallaan) ja niihin lisätään yksi kerrallaan kaikki kiinnittyvät palikat.
    Näin syntyvät yhdistelmät kirjoitetaan tietokantaan."""
    uudet_kombot = []
    if kierros == 1:
        # Ensimmäinen palikka ei vaadi laskentaa.
        db.lisaa_kombo(kierros, kombo_merkkijonoksi([koodaa(0, 0, 0, 0)]))
    else:
        maara = db.lukumaara(kierros - 1)
        jaljella = maara
        laskuri = 0
        while jaljella > 0:
            era = min(jaljella, 1000)
            edelliset_kombot = db.hae_kombot(kierros - 1, maara - jaljella, era)
            jaljella -= era
            for kombo_str in edelliset_kombot:
                kombo = merkkijono_komboksi(kombo_str)
                sopivat_palat = paivita_sopivat(kombo, kierros, leveys, korkeus)
                for palikka in sopivat_palat:
                    # Lisätään yksi palikka ja normalisoidaan syntynyt yhdistelmä
                    uusi_kombo = kombo + [palikka]
                    normalisoitu = normalisoi(uusi_kombo)
                    # Luodusta yhdistelmästä tehdään 90, 180 ja 270 astetta käännetyt
                    # versiot. Kaikista neljästä versiosta jäljelle jätetään vain se,
                    # jonka merkkijono on arvoltaan pienin.
                    versiot = paivita_versiot(normalisoitu, leveys, korkeus)
                    uudet_kombot.append(min(versiot))
                # Luodut yhdistelmät kirjoitetaan tietokantaan erissä.
                laskuri += 1
                if laskuri % 5000 == 0 or laskuri == maara:
                    db.lisaa_kombot(kierros, uudet_kombot)
                    uudet_kombot.clear()
                    prosentti = laskuri * 100 / maara
                    print(f"\r{kierros} palikkaa: {prosentti:.2f} %", end="")
        # Kirjoitetaan jäljelle jääneet yhdistelmät
        if uudet_kombot:
            db.lisaa_kombot(kierros, uudet_kombot)
            uudet_kombot.clear()


def tulosta_kombo(kombo):
    """Tulostaa yhdistelmän palikat luettavassa muodossa."""
    for pala in kombo:
        z, x, y, asento = dekoodaa(pala)
        print(f"z = {z}, x = {x}, y = {y}, asento = {asento}")


def main():
    # Luetaan komentorivin argumenteista seuraavat tiedot, tai jos argumentteja
    # ei ole annettu, asetetaan niille oletusarvot:
    # 1. tietokantatiedoston nimi (oletus: kombot.db)
    # 2. yhdistelmiin käytettävien palikoiden määrä (oletus: 6)
    # 3. palikan korkeus (oletus: 4)
    # 4. palikan leveys (oletus: 2)
    # 5. kuinka monen palikan yhdistelmistä aloitetaan (oletus: 1)
    numeroargumentit = []
    tiedostonimi = "kombot.db"
    for a in sys.argv[1:]:
        try:
            arvo = int(a)
            numeroargumentit.append(arvo)
        except ValueError:
            tiedostonimi = a

    palikoita = numeroargumentit[0] if len(numeroargumentit) > 0 else 6
    korkeus = numeroargumentit[1] if len(numeroargumentit) > 1 else 4
    leveys = numeroargumentit[2] if len(numeroargumentit) > 2 else 2
    aloitus = numeroargumentit[3] if len(numeroargumentit) > 3 else 1

    print(f"Luodaan {palikoita} {korkeus}×{leveys} palikan yhdistelmät")

    aloitusaika = time.time()

    # Avataan yhteys tietokantaan.
    db = KomboDB()
    db.alusta(tiedostonimi)

    # Pääluuppi
    # Puhdistetaan tietokannasta kaikki taulut paitsi ne joita käytetään laskennan pohjana
    db.poista_taulut(aloitus)
    for kierros in range(aloitus, palikoita + 1):
        db.luo_taulu(kierros)
        hae_ratkaisut(kierros, leveys, korkeus, db)
        lkm = db.lukumaara(kierros)
        paate = "" if kierros == 1 else "a"
        print(f"\r{kierros} palikka{paate}: {lkm} ratkaisu{paate}")
        aika = time.time() - aloitusaika
        print(f"  Kulunut aika {aika:.2f} sekuntia.")

    # Suljetaan tietokantayhteys.
    db.sulje()


if __name__ == "__main__":
    main()
