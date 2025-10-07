-- Importowanie standardowych bibliotek IEEE.
library IEEE;
-- Uzycie pakietu STD_LOGIC_1164, ktory definiuje typ std_logic i std_logic_vector
-- oraz powiazane z nimi operacje.
use IEEE.STD_LOGIC_1164.ALL;
-- Uzycie pakietu NUMERIC_STD, ktory dostarcza typy arytmetyczne (signed, unsigned)
-- i operacje na nich dla std_logic_vector.
use IEEE.NUMERIC_STD.ALL;

-- Definicja encji (interfejsu) modulu o nazwie menu_selector.
-- Encja okresla porty wejsciowe i wyjsciowe modulu.
entity menu_selector is
    Port (
        clk        : in  STD_LOGIC;                     -- Wejscie sygnalu zegarowego.
        rst        : in  STD_LOGIC;                     -- Wejscie sygnalu resetu (asynchronicznego lub synchronicznego, zalezy od uzycia).
        btn_left   : in  STD_LOGIC;                     -- Wejscie dla lewego przycisku.
        btn_right  : in  STD_LOGIC;                     -- Wejscie dla prawego przycisku.
        seg_left   : out STD_LOGIC_VECTOR (6 downto 0); -- Wyjscie sterujace segmentami lewego wyswietlacza 7-segmentowego.
        seg_right  : out STD_LOGIC_VECTOR (6 downto 0)  -- Wyjscie sterujace segmentami prawego wyswietlacza 7-segmentowego.
    );
end menu_selector;

-- Definicja architektury (implementacji) Behavioral dla encji menu_selector.
architecture Behavioral of menu_selector is

    -- Deklaracje sygnalow wewnetrznych uzywanych w architekturze.

    -- Sygnal przechowujacy aktualna wartosc liczbowa dla lewego wyswietlacza (0-9).
    -- Inicjalizowany na 0 ("0000"). 4 bity, bo 2^4 = 16, wystarczajaco na 0-9.
    signal value_left    : unsigned(3 downto 0) := (others => '0');
    -- Sygnal przechowujacy aktualna wartosc liczbowa dla prawego wyswietlacza (0-9).
    -- Inicjalizowany na 0 ("0000").
    signal value_right   : unsigned(3 downto 0) := (others => '0');

    -- Sygnaly do debouncingu (eliminacji drgan stykow) i detekcji zbocza dla lewego przycisku.
    signal btn_left_db   : STD_LOGIC := '0'; -- Stan lewego przycisku po debouncingu. '1' jesli przycisk jest stabilnie wcisniety.
    signal btn_left_prev : STD_LOGIC := '0'; -- Poprzedni stan zdebouncowanego lewego przycisku (do detekcji zbocza narastajacego).

    -- Sygnaly do debouncingu i detekcji zbocza dla prawego przycisku.
    signal btn_right_db  : STD_LOGIC := '0'; -- Stan prawego przycisku po debouncingu.
    signal btn_right_prev: STD_LOGIC := '0'; -- Poprzedni stan zdebouncowanego prawego przycisku.

    -- Liczniki uzywane w procesie debouncingu. Zliczaja, jak dlugo przycisk jest wcisniety.
    -- 20 bitow pozwala na zliczenie do 2^20 - 1 cykli zegara.
    signal cnt_left   : unsigned(19 downto 0) := (others => '0'); -- Licznik dla lewego przycisku.
    signal cnt_right  : unsigned(19 downto 0) := (others => '0'); -- Licznik dla prawego przycisku.

    -- Stala okreslajaca czas (w cyklach zegara) potrzebny do uznania przycisku za stabilnie wcisniety.
    -- TODO: zmien to_unsigned(3, 20) na to_unsigned(DUZA_WARTOSC, 20), np. 500000 dla zegara 50MHz i debounce 10ms.
    -- W symulacji EDA Playground zegar jest "szybki" w stosunku do krokow czasowych symulacji,
    -- wiec mala wartosc (3) jest wystarczajaca do testow. W realnym ukladzie FPGA musi byc znacznie wieksza.
    constant DEBOUNCE_TIME : unsigned(19 downto 0) := to_unsigned(250000, 20); -- ok. 10 ms dla 25 MHz

    -- Funkcja konwertujaca 4-bitowa liczbe bez znaku (0-9) na 7-bitowy wektor
    -- sterujacy wyswietlaczem 7-segmentowym.
    -- Zaklada sie, ze '0' zapala segment, a '1' gasi (lub odwrotnie, zalezy od typu wyswietlacza - wspolna anoda/katoda).
    -- Te wzory sa dla wspolnej anody (0 = segment ON) lub dla logiki gdzie '1' to segment OFF.
    -- Standardowo dla 7-segmentow: a,b,c,d,e,f,g. "1000000" dla 0 -> a,b,c,d,e,f ON, g OFF.
    -- (segmenty: g f e d c b a)
    -- 0: "1000000" (a,b,c,d,e,f)
    -- 1: "1111001" (b,c)
    -- 2: "0100100" (a,b,g,e,d)
    -- ...itd.
    function to_7seg(val : unsigned(3 downto 0)) return STD_LOGIC_VECTOR is
        variable seg : STD_LOGIC_VECTOR(6 downto 0); -- Zmienna lokalna do przechowywania wzoru segmentow.
    begin
        case to_integer(val) is -- Konwersja wartosci 'unsigned' na 'integer' do uzycia w case.
            when 0      => seg := "1000000"; -- Wzor dla cyfry 0
            when 1      => seg := "1111001"; -- Wzor dla cyfry 1
            when 2      => seg := "0100100"; -- Wzor dla cyfry 2
            when 3      => seg := "0110000"; -- Wzor dla cyfry 3
            when 4      => seg := "0011001"; -- Wzor dla cyfry 4
            when 5      => seg := "0010010"; -- Wzor dla cyfry 5
            when 6      => seg := "0000010"; -- Wzor dla cyfry 6
            when 7      => seg := "1111000"; -- Wzor dla cyfry 7
            when 8      => seg := "0000000"; -- Wzor dla cyfry 8
            when 9      => seg := "0010000"; -- Wzor dla cyfry 9
            when others => seg := "1111111"; -- Dla wszystkich innych wartosci (np. >9), wyswietl pusty ekran lub blad (wszystkie segmenty wylaczone).
        end case;
        return seg; -- Zwrocenie obliczonego wzoru segmentow.
    end function;

begin -- Rozpoczecie ciala architektury (procesy i przypisania wspolbiezne).

    -- Proces odpowiedzialny za debouncing lewego przycisku.
    -- Proces jest czuly na zmiany sygnalu zegarowego 'clk'.
    process(clk)
    begin
        -- Akcja wykonywana tylko na narastajacym zboczu sygnalu zegarowego.
        if rising_edge(clk) then
            -- Jesli fizyczny przycisk 'btn_left' jest wcisniety ('1').
            if btn_left = '1' then
                -- Jesli licznik 'cnt_left' nie osiagnal jeszcze wartosci 'DEBOUNCE_TIME'.
                if cnt_left < DEBOUNCE_TIME then
                    -- Inkrementuj licznik. Oznacza to, ze przycisk jest nadal wcisniety, ale czas stabilizacji jeszcze nie minal.
                    cnt_left <= cnt_left + 1;
                else
                    -- Licznik osiagnal 'DEBOUNCE_TIME'. Oznacza to, ze przycisk jest stabilnie wcisniety.
                    -- Ustaw zdebouncowany sygnal 'btn_left_db' na '1'.
                    btn_left_db <= '1';
                end if;
            else -- Jesli fizyczny przycisk 'btn_left' nie jest wcisniety ('0').
                -- Zresetuj licznik 'cnt_left' do zera.
                cnt_left <= (others => '0');
                -- Ustaw zdebouncowany sygnal 'btn_left_db' na '0'.
                btn_left_db <= '0';
            end if;
        end if;
    end process;

    -- Proces odpowiedzialny za debouncing prawego przycisku.
    -- Dziala analogicznie do procesu debouncingu dla lewego przycisku.
    process(clk)
    begin
        if rising_edge(clk) then
            if btn_right = '1' then
                if cnt_right < DEBOUNCE_TIME then
                    cnt_right <= cnt_right + 1;
                else
                    btn_right_db <= '1';
                end if;
            else
                cnt_right <= (others => '0');
                btn_right_db <= '0';
            end if;
        end if;
    end process;

    -- Glowny proces logiki sterujacej.
    -- Jest czuly na zmiany sygnalu zegarowego 'clk' oraz sygnalu resetu 'rst'.
    process(clk, rst)
    begin
        -- Obsluga resetu: jesli 'rst' jest aktywne ('1').
        -- Reset jest tutaj asynchroniczny, poniewaz jest sprawdzany przed 'rising_edge(clk)'.
        -- Jesli mialby byc synchroniczny, warunek 'rst = '1'' bylby wewnatrz 'elsif rising_edge(clk)'.
        if rst = '1' then
            -- Zresetuj wartosci na obu wyswietlaczach do 0.
            value_left  <= (others => '0');
            value_right <= (others => '0');
            -- Opcjonalnie: zresetuj takze btn_left_prev i btn_right_prev, chociaz dla logiki detekcji zbocza
            -- po resecie i tak pierwsze zbocze bedzie poprawnie wykryte.
            -- btn_left_prev <= '0';
            -- btn_right_prev <= '0';
        -- Akcje wykonywane na narastajacym zboczu zegara, jesli reset nie jest aktywny.
        elsif rising_edge(clk) then

            -- Obsluga lewego przycisku (inkrementacja wartosci na lewym wyswietlaczu).
            -- Sprawdz, czy nastapilo narastajace zbocze na zdebouncowanym sygnale lewego przycisku.
            -- Oznacza to, ze przycisk zostal wlasnie wcisniety (przeszedl ze stanu '0' do '1').
            if btn_left_db = '1' and btn_left_prev = '0' then
                -- Jesli aktualna wartosc na lewym wyswietlaczu to 9 ("1001" binarnie).
                if value_left = "1001" then
                    -- Zmien wartosc na 0 (zawiniecie licznika).
                    value_left <= (others => '0');
                else
                    -- W przeciwnym razie, zwieksz wartosc o 1.
                    value_left <= value_left + 1;
                end if;
            end if;
            -- Zapisz aktualny stan zdebouncowanego lewego przycisku do 'btn_left_prev'.
            -- Bedzie on uzyty w nastepnym cyklu zegara do wykrycia kolejnego narastajacego zbocza.
            btn_left_prev <= btn_left_db;

            -- Obsluga prawego przycisku (kopiowanie wartosci z lewego wyswietlacza na prawy).
            -- Sprawdz, czy nastapilo narastajace zbocze na zdebouncowanym sygnale prawego przycisku.
            if btn_right_db = '1' and btn_right_prev = '0' then
                -- Skopiuj wartosc z 'value_left' do 'value_right'.
                value_right <= value_left;
            end if;
            -- Zapisz aktualny stan zdebouncowanego prawego przycisku do 'btn_right_prev'.
            btn_right_prev <= btn_right_db;
        end if;
    end process;

    -- Wspolbiezne przypisania sygnalow sterujacych wyswietlaczami 7-segmentowymi.
    -- Te przypisania sa aktywne caly czas i aktualizuja wyjscia 'seg_left'/'seg_right'
    -- za kazdym razem, gdy zmienia sie wartosc 'value_left'/'value_right'.
    -- Funkcja 'to_7seg' jest wywolywana automatycznie przy kazdej zmianie jej argumentu.
    seg_left  <= to_7seg(value_left);
    seg_right <= to_7seg(value_right);

end Behavioral;
