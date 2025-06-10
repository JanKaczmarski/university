-- testbench.vhd

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity tb_menu_selector is
end tb_menu_selector;

architecture sim of tb_menu_selector is
    signal clk        : std_logic := '0';
    signal rst        : std_logic := '1';
    signal btn_left   : std_logic := '0';
    signal btn_right  : std_logic := '0';
    signal seg_left   : std_logic_vector(6 downto 0);
    signal seg_right  : std_logic_vector(6 downto 0);

    constant clk_period : time := 20 ns;

begin
    uut: entity work.menu_selector
        port map (
            clk        => clk,
            rst        => rst,
            btn_left   => btn_left,
            btn_right  => btn_right,
            seg_left   => seg_left,
            seg_right  => seg_right
        );

    -- Clock
    clk_process : process
    begin
        while true loop
            clk <= '0'; wait for clk_period / 2;
            clk <= '1'; wait for clk_period / 2;
        end loop;
    end process;

    -- Test
    stim_proc: process
    begin
        wait for 100 ns;
        rst <= '0';

        -- 1. Nacisnij lewy (powinno pokazac 1)
        btn_left <= '1'; wait for 2 us; btn_left <= '0'; wait for 10 us;

        -- 2. Nacisnij lewy (powinno pokazac 2)
        btn_left <= '1'; wait for 2 us; btn_left <= '0'; wait for 10 us;

        -- 3. Nacisnij prawy (powinien skopiowac 2)
        btn_right <= '1'; wait for 2 us; btn_right <= '0'; wait for 10 us;

        -- 4. Nacisnij lewy 8x (powinno wrocic do 0)
        for i in 1 to 8 loop
            btn_left <= '1'; wait for 2 us; btn_left <= '0'; wait for 10 us;
        end loop;

        -- 5. Nacisnij prawy (powinien skopiowac 0)
        btn_right <= '1'; wait for 2 us; btn_right <= '0';
        
        wait for 1 us; -- Poczekaj, az ostatnia akcja sie rozpropaguje

        report "Stimulus finished. Stopping simulation.";
        assert false report "Simulation stop requested by testbench" severity failure;
        
        wait; -- Proces czeka w nieskonczonosc po zazadaniu zatrzymania
    end process;

end sim;
