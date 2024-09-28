library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.runtime.all;


entity Interface_accelerometer is
  port (signal clk : in std_logic;
        signal reset : in std_logic;
        signal start : in std_logic;

        signal rdy : out std_logic_vector(0 to 0);
        -- Accelerometer
      
        -- signal GSENSOR_CS_N : out std_logic;
      
        -- signal GSENSOR_INT : in std_logic_vector(2 downto 1);
      
        -- signal GSENSOR_SCLK : out std_logic;
        
        -- signal GSENSOR_SDO_i : in std_logic;
        -- signal GSENSOR_SDO_o : out std_logic;
      
        -- signal GSENSOR_SDI_i : in std_logic;
        -- signal GSENSOR_SDI_o : out std_logic;
        
        -- signal ACCEL_X : out unsigned(15 downto 0);
        -- signal ACCEL_Y : out unsigned(15 downto 0);
        -- signal ACCEL_Z : out unsigned(15 downto 0)

        signal argument : in std_logic_vector(0 to 3); -- GSENSOR_INT(2) + GSENSOR_SDO_i(1) + GSENSOR_SDI_i(1)
        signal result : out std_logic_vector(0 to 51) -- ACCEL_X(16) + ACCEL_Y(16) + ACCEL_Z(16) + GSENSOR_CS_N(1) + GSENSOR_SCLK(1) + GSENSOR_SDO_o(1) + GSENSOR_SDI_o(1)
  );
end entity;

architecture rtl of Interface_accelerometer is
    signal dly_rst : std_logic := '0';
    signal spi_clk : std_logic;
    signal spi_clk_out : std_logic;

    signal ACCEL_X : std_logic_vector(15 downto 0);
    signal ACCEL_Y : std_logic_vector(15 downto 0);
    signal ACCEL_Z : std_logic_vector(15 downto 0);
    signal GSENSOR_CS_N : std_logic;
    signal GSENSOR_SCLK : std_logic;
    signal GSENSOR_SDO_o : std_logic;
    signal GSENSOR_SDI_o : std_logic;
    begin
        -- process (clk)
            --begin
           -- if (rising_edge(clk)) then
			--	null;
        --    end if;
        -- end process;

    -- PLL
    u_spi_pll : entity work.spi_pll port map (
        areset => dly_rst,
        inclk0 => clk,
        c0 => spi_clk,      -- 2MHz
        locked => spi_clk_out   -- 2MHz phase shift 
    );

    -- Initial Setting and Data Read Back
    u_spi_ee_config : entity work.spi_ee_config port map (
        iRSTN => not dly_rst,
        iSPI_CLK => spi_clk,
        iSPI_CLK_OUT => spi_clk_out,
        -- iG_INT2 => GSENSOR_INT(1),
        iG_INT2 => argument(1),
        std_logic_vector(data_x) => ACCEL_X,
        std_logic_vector(data_y) => ACCEL_Y,
        std_logic_vector(data_z) => ACCEL_Z,
        -- SPI_SDIO_i => GSENSOR_SDI_i,
        SPI_SDIO_i => argument(3),
        SPI_SDIO_o => GSENSOR_SDI_o,
        oSPI_CSN => GSENSOR_CS_N,
        oSPI_CLK => GSENSOR_SCLK
    );
-- GSENSOR_SDI <= SPI_SDIO_o;
					 result <= ACCEL_X & ACCEL_Y & ACCEL_Z & GSENSOR_CS_N & GSENSOR_SCLK & GSENSOR_SDO_o & GSENSOR_SDI_o;
					 rdy <= "1";
end architecture;


-- library IEEE;
-- use IEEE.std_logic_1164.all;
-- use IEEE.numeric_std.all;
-- 
-- use work.runtime.all;
-- 
-- 
-- entity Interface_accelerometer is
--   port (signal clk : in std_logic;
--         signal reset : in std_logic;
--         signal start : in std_logic;
-- 		  
-- 		  signal GSENSOR_INT : in std_logic_vector(2 downto 1);
-- 		  signal GSENSOR_SDO : inout std_logic;
-- 		  signal GSENSOR_SDIO_i : in std_logic;
-- 		  signal GSENSOR_SDIO_o : out std_logic;
--         -- signal argument : in std_logic_vector(0 to 3); -- 2 + 1 + 1 (GSENSOR_INT, GSENSOR_SDO, GSENSOR_SDI)
--           signal GSENSOR_CS_N : out std_logic;
--           signal GSENSOR_SCLK : out std_logic;
--         
-- 		  signal ACCEL_X : out unsigned(15 downto 0);
-- 		  signal ACCEL_Y : out unsigned(15 downto 0);
-- 		  signal ACCEL_Z : out unsigned(15 downto 0);
-- 		  -- signal result : out std_logic_vector(0 to 51); -- 16 + 16 + 16 + 1 + 1 + 1 + 1 (X,Y,Z,GSENSOR_CS_N,GSENSOR_SCLK,GSENSOR_SDO,GSENSOR_SDI)
--         signal rdy : out std_logic_vector(0 to 0)
--   );
-- end entity;
-- 
-- architecture rtl of Interface_accelerometer is
--     signal dly_rst : std_logic := '0';
--     signal spi_clk : std_logic;
--     signal spi_clk_out : std_logic;
--     begin
--     -- PLL
--     u_spi_pll : entity work.spi_pll port map (
--         areset => dly_rst,
--         inclk0 => clk,
--         c0 => spi_clk,      -- 2MHz
--         locked => spi_clk_out   -- 2MHz phase shift 
--     );
-- 
--     -- Initial Setting and Data Read Back
--     u_spi_ee_config : entity work.spi_ee_config port map (
--         iRSTN => not dly_rst,
--         iSPI_CLK => spi_clk,
--         iSPI_CLK_OUT => spi_clk_out,
--         -- iG_INT2 => argument(1), -- ou 1
--         iG_INT2 => GSENSOR_INT(1), -- ou 1
--         std_logic_vector(data_x) => ACCEL_X,
--         std_logic_vector(data_y) => ACCEL_Y,
--         std_logic_vector(data_z) => ACCEL_Z,
--         SPI_SDIO_i => GSENSOR_SDIO_i, -- io -> i & o
--         SPI_SDIO_o => GSENSOR_SDIO_o,
--         oSPI_CSN => GSENSOR_CS_N,
--         oSPI_CLK => GSENSOR_SCLK
--     );
-- 
--     -- result <= ACCEL_X & ACCEL_Y & ACCEL_Z & GSENSOR_CS_N & GSENSOR_SCLK & argument(2) & SPI_SDIO_o;
-- 	rdy <= "1";
-- 
-- end architecture;
