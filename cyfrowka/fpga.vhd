library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity menu_selector is
    Port (
        clk        : in  STD_LOGIC;
        rst        : in  STD_LOGIC;
        btn_left   : in  STD_LOGIC;
        btn_right  : in  STD_LOGIC;
        seg_left   : out STD_LOGIC_VECTOR (6 downto 0);
        seg_right  : out STD_LOGIC_VECTOR (6 downto 0)
    );
end menu_selector;

architecture Behavioral of menu_selector is

    signal value_left    : unsigned(3 downto 0) := (others => '0');
    signal value_right   : unsigned(3 downto 0) := (others => '0');

    signal btn_left_db,  btn_left_prev  : STD_LOGIC := '0';
    signal btn_right_db, btn_right_prev : STD_LOGIC := '0';

    signal cnt_left   : unsigned(19 downto 0) := (others => '0');
    signal cnt_right  : unsigned(19 downto 0) := (others => '0');

	-- TODO: change to_unsigned(3, 20) to to_unsigned(1000, 20), on test playground the clock cycle is very slow, but in real scenario it should be 10000
    constant DEBOUNCE_TIME : unsigned(19 downto 0) := to_unsigned(3, 20); -- skrócony do testów

    function to_7seg(val : unsigned(3 downto 0)) return STD_LOGIC_VECTOR is
        variable seg : STD_LOGIC_VECTOR(6 downto 0);
    begin
        case to_integer(val) is
            when 0 => seg := "1000000";
            when 1 => seg := "1111001";
            when 2 => seg := "0100100";
            when 3 => seg := "0110000";
            when 4 => seg := "0011001";
            when 5 => seg := "0010010";
            when 6 => seg := "0000010";
            when 7 => seg := "1111000";
            when 8 => seg := "0000000";
            when 9 => seg := "0010000";
            when others => seg := "1111111";
        end case;
        return seg;
    end function;

begin

    -- Debounce LEFT
    process(clk)
    begin
        if rising_edge(clk) then
            if btn_left = '1' then
                if cnt_left < DEBOUNCE_TIME then
                    cnt_left <= cnt_left + 1;
                else
                    btn_left_db <= '1';
                end if;
            else
                cnt_left <= (others => '0');
                btn_left_db <= '0';
            end if;
        end if;
    end process;

    -- Debounce RIGHT
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

    -- Glowna logika
    process(clk, rst)
    begin
        if rst = '1' then
            value_left  <= (others => '0');
            value_right <= (others => '0');
        elsif rising_edge(clk) then

            if btn_left_db = '1' and btn_left_prev = '0' then
                if value_left = "1001" then
                    value_left <= (others => '0');
                else
                    value_left <= value_left + 1;
                end if;
            end if;
            btn_left_prev <= btn_left_db;

            if btn_right_db = '1' and btn_right_prev = '0' then
                value_right <= value_left;
            end if;
            btn_right_prev <= btn_right_db;
        end if;
    end process;

    seg_left  <= to_7seg(value_left);
    seg_right <= to_7seg(value_right);

end Behavioral;
