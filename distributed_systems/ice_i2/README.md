# I2 — Efektywne zarządzanie serwantami (ZeroC Ice 3.7)

Aplikacja klient-serwer demonstrująca dwie strategie zarządzania serwantami w Ice.

- **Serwer:** Python 3
- **Klient:** C++17
- **Middleware:** ZeroC Ice 3.7 (`tcp -p 10000`)

## Strategie

| Kategoria Identity | Strategia | Mechanizm Ice |
|---|---|---|
| `dedicated/<name>` | jeden serwant per obiekt, lazy init | `ServantLocator` + ASM (`adapter.add`) |
| `shared/<name>` | jeden serwant dla całej kategorii | `addDefaultServant("shared")` |

## Struktura

```
ice_i2/
├── slice/Counter.ice
├── server/
│   ├── server.py
│   ├── counter_servant.py
│   ├── shared_counter_servant.py
│   ├── dedicated_locator.py
│   └── config.server
├── client/
│   ├── main.cpp
│   └── CMakeLists.txt
├── generated/{python,cpp}/
├── build/
├── generate.sh
└── README.md
```

## Quick start (Docker — rekomendowane)

```bash
docker compose build
docker compose up -d server
docker compose logs -f server
docker compose run --rm client
docker compose down
```

Klient w kontenerze ma `ICE_HOST=server`. Lokalnie (bez Dockera) klient czyta `localhost`.

## Instalacja lokalna (alternatywa)

**Debian/Ubuntu:**
```bash
wget -O /etc/apt/trusted.gpg.d/zeroc.asc \
    https://download.zeroc.com/GPG-KEY-zeroc-release-B6391CB2CFBA643D
echo "deb https://download.zeroc.com/ice/3.7/ubuntu22.04 stable main" \
    | sudo tee /etc/apt/sources.list.d/zeroc-ice.list
sudo apt update
sudo apt install zeroc-ice-all-dev python3-zeroc-ice
```

## Build (lokalnie)

```bash
./generate.sh
cmake -S client -B build
cmake --build build
```

## Uruchomienie

Terminal 1 — serwer:
```bash
python3 server/server.py --Ice.Config=server/config.server
```

Terminal 2 — klient:
```bash
./build/client
```

## Menu klienta

```
[s] select proxy (category + name)
[c] checkedCast on the current base proxy
[u] uncheckedCast on the current base proxy
[g] getValue
[v] setValue
[i] increment
[r] reset
[n] getName
[q] quit
```

## Scenariusz demo

### A) Strategia dedykowana — lazy init + REUSE

1. Klient `[s] dedicated Alice` → `[c]`
   → serwer: `[LAZY INIT] dedicated/Alice (added to ASM)`
2. Klient: `[i]` → `[CALL] dedicated/Alice.increment() -> 1`
3. Klient: `[i]` ponownie — brak `[LAZY INIT]` (REUSE), tylko `[CALL] ... -> 2`
4. Klient: `[s] dedicated Bob` → `[c]` → `[i]`
   → `[LAZY INIT] dedicated/Bob`, własne `_value` = 1

### B) Strategia współdzielona — default servant

1. Klient: `[s] shared Foo` → `[u]` → `[i]`
   → serwer: `[SHARED] request for shared/Foo.increment() -> 1`
2. Klient: `[s] shared Bar` → `[u]` → `[i]`
   → serwer: `[SHARED] request for shared/Bar.increment() -> 2`
   → ten sam serwant, wspólny stan, różne `current.id.name`

### C) checkedCast vs uncheckedCast

- `[s] dedicated Charlie` → `[c]` → serwer woła locator (`[LAZY INIT]`)
- `[s] dedicated Dave` → `[u]` → cisza w logu, dopiero `[i]`/`[g]` wywołuje locator

## Mapowanie do wymagań I2

| Wymaganie | Realizacja |
|---|---|
| Aplikacja klient-serwer w Ice | Python serwer + C++ klient |
| Dwa typy obiektów (dedykowany + współdzielony serwant) | `dedicated` / `shared` |
| Indywidualny stan każdego obiektu | `_value` w `CounterI` |
| Lazy init dedykowanego serwanta | `DedicatedServantLocator.locate()` |
| Natywna ASM Ice | `adapter.add` |
| Default servant dla współdzielonego | `adapter.addDefaultServant(shared, "shared")` |
| Identity z parametru klienta | menu, `stringToProxy` |
| Różne kategorie Identity dla różnych strategii | `dedicated/*` vs `shared/*` |
| Logi pokazujące obiekt+serwant+czas instancjonowania | `[INIT]`, `[LAZY INIT]`, `[SHARED]`, `[CALL]` |
| checkedCast vs uncheckedCast | klawisze `c` i `u` |
| Klient interaktywny tekstowy | `client/main.cpp` |
| Pliki generowane oddzielnie od źródeł | `generated/{python,cpp}` |
| Pliki kompilacji oddzielnie od źródeł | `build/` |
| Dwa różne języki | Python + C++ |

## Punktacja
**8 pkt** (baza I2)
