"""
KomboDB - Tietokantamoduuli LEGO-palikoiden yhdistelmien tallennukseen.
Käyttää SQLite-tietokantaa.
"""

import sqlite3


class KomboDB:
    def __init__(self):
        self.conn = None
        self.cursor = None

    @staticmethod
    def _taulu_nimi(kierros):
        """Palauttaa taulun nimen kierroksen perusteella. Validoi syötteen."""
        kierros = int(kierros)
        return f"kombot_{kierros}"

    def alusta(self, tiedostonimi):
        """Avaa yhteys tietokantaan."""
        self.conn = sqlite3.connect(tiedostonimi)
        self.cursor = self.conn.cursor()

    def luo_taulu(self, kierros):
        """Luo taulun tiettyä kierrosta varten."""
        taulu = self._taulu_nimi(kierros)
        self.cursor.execute(
            f"CREATE TABLE IF NOT EXISTS {taulu} "
            f"(kombo TEXT UNIQUE)"
        )
        self.conn.commit()

    def poista_taulut(self, aloitus):
        """Poistaa taulut alkaen annetusta kierroksesta."""
        self.cursor.execute(
            "SELECT name FROM sqlite_master WHERE type='table'"
        )
        taulut = self.cursor.fetchall()
        for (taulu,) in taulut:
            if taulu.startswith("kombot_"):
                try:
                    numero = int(taulu.split("_")[1])
                except ValueError:
                    continue
                if numero >= aloitus:
                    self.cursor.execute(
                        f"DROP TABLE IF EXISTS {self._taulu_nimi(numero)}"
                    )
        self.conn.commit()

    def lisaa_kombo(self, kierros, kombo):
        """Lisää yksittäisen yhdistelmän tietokantaan."""
        taulu = self._taulu_nimi(kierros)
        try:
            self.cursor.execute(
                f"INSERT OR IGNORE INTO {taulu} (kombo) VALUES (?)",
                (kombo,)
            )
            self.conn.commit()
        except sqlite3.IntegrityError:
            pass

    def lisaa_kombot(self, kierros, kombot):
        """Lisää useita yhdistelmiä tietokantaan."""
        taulu = self._taulu_nimi(kierros)
        data = [(k,) for k in kombot]
        self.cursor.executemany(
            f"INSERT OR IGNORE INTO {taulu} (kombo) VALUES (?)",
            data
        )
        self.conn.commit()

    def lukumaara(self, kierros):
        """Palauttaa yhdistelmien lukumäärän annetulle kierrokselle."""
        taulu = self._taulu_nimi(kierros)
        self.cursor.execute(f"SELECT COUNT(*) FROM {taulu}")
        return self.cursor.fetchone()[0]

    def hae_kombot(self, kierros, offset, maara):
        """Hakee yhdistelmiä tietokannasta."""
        taulu = self._taulu_nimi(kierros)
        self.cursor.execute(
            f"SELECT kombo FROM {taulu} LIMIT ? OFFSET ?",
            (maara, offset)
        )
        return [rivi[0] for rivi in self.cursor.fetchall()]

    def sulje(self):
        """Sulkee tietokantayhteyden."""
        if self.conn:
            self.conn.close()
