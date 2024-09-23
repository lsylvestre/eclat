  LIBRARY ieee;
  USE ieee.std_logic_1164.all;
  
  LIBRARY altera_mf;
  USE altera_mf.all;
  
  ENTITY spi_pll IS
      PORT
      (
          areset		: IN STD_LOGIC;
          inclk0		: IN STD_LOGIC;
          c0				: OUT STD_LOGIC ;
          locked		: OUT STD_LOGIC 
      );
  END spi_pll;
  
  
  ARCHITECTURE SYN OF spi_pll IS
      signal sub_wire0 : std_logic_vector(4 downto 0);
      SIGNAL sub_wire5	: STD_LOGIC;
      SIGNAL sub_wire2	: STD_LOGIC;
      SIGNAL sub_wire1	: STD_LOGIC;
      SIGNAL sub_wire3	: STD_LOGIC;
      SIGNAL sub_wire4	: STD_LOGIC_vector(1 downto 0);
      signal poub : std_logic;
  
  
  
      COMPONENT altpll
      GENERIC (
          bandwidth_type		: STRING;
          clk0_divide_by		: NATURAL;
          clk0_duty_cycle		: NATURAL;
          clk0_multiply_by		: NATURAL;
          clk0_phase_shift		: STRING;
          clk1_divide_by		: NATURAL;
          clk1_duty_cycle		: NATURAL;
          clk1_multiply_by		: NATURAL;
          clk1_phase_shift		: STRING;
          compensate_clock		: STRING;
          inclk0_input_frequency		: NATURAL;
          intended_device_family		: STRING;
          lpm_hint		: STRING;
          lpm_type		: STRING;
          operation_mode		: STRING;
          pll_type		: STRING;
          port_activeclock		: STRING;
          port_areset		: STRING;
          port_clkbad0		: STRING;
          port_clkbad1		: STRING;
          port_clkloss		: STRING;
          port_clkswitch		: STRING;
          port_configupdate		: STRING;
          port_fbin		: STRING;
          port_inclk0		: STRING;
          port_inclk1		: STRING;
          port_locked		: STRING;
          port_pfdena		: STRING;
          port_phasecounterselect		: STRING;
          port_phasedone		: STRING;
          port_phasestep		: STRING;
          port_phaseupdown		: STRING;
          port_pllena		: STRING;
          port_scanaclr		: STRING;
          port_scanclk		: STRING;
          port_scanclkena		: STRING;
          port_scandata		: STRING;
          port_scandataout		: STRING;
          port_scandone		: STRING;
          port_scanread		: STRING;
          port_scanwrite		: STRING;
          port_clk0		: STRING;
          port_clk1		: STRING;
          port_clk2		: STRING;
          port_clk3		: STRING;
          port_clk4		: STRING;
          port_clk5		: STRING;
          port_clkena0		: STRING;
          port_clkena1		: STRING;
          port_clkena2		: STRING;
          port_clkena3		: STRING;
          port_clkena4		: STRING;
          port_clkena5		: STRING;
          port_extclk0		: STRING;
          port_extclk1		: STRING;
          port_extclk2		: STRING;
          port_extclk3		: STRING;
          self_reset_on_loss_lock		: STRING;
          width_clock		: NATURAL
      );
      PORT (
              areset	: IN STD_LOGIC ;
              inclk	: IN STD_LOGIC_VECTOR (1 DOWNTO 0);
              clk	: OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
              locked	: OUT STD_LOGIC 
      );
      END COMPONENT;
  
  BEGIN
      sub_wire5 <= '0';
      sub_wire2 <= sub_wire0(1);
      sub_wire1 <= sub_wire0(0);
      c0 <= sub_wire1;
      locked <= sub_wire2;
      sub_wire3 <= inclk0;
      sub_wire4(1 downto 0) <= sub_wire5 & sub_wire3;
      
      -- sub_wire2_bv(0 DOWNTO 0) <= "0";
      -- sub_wire2    <= To_stdlogicvector(sub_wire2_bv);
      -- sub_wire0    <= inclk0;
      -- sub_wire1    <= sub_wire2(0 DOWNTO 0) & sub_wire0;
      -- sub_wire4    <= sub_wire3(0);
      -- c0    <= sub_wire4;
      -- locked    <= sub_wire5;
  
      altpll_component : altpll
      GENERIC MAP (
          bandwidth_type => "AUTO",
          clk0_divide_by => 25,
          clk0_duty_cycle => 50,
          clk0_multiply_by => 1,
          clk0_phase_shift => "277778",
          clk1_divide_by => 25,
          clk1_duty_cycle => 50,
          clk1_multiply_by => 1,
          clk1_phase_shift => "166667",
          compensate_clock => "CLK0",
          inclk0_input_frequency => 20000,
          intended_device_family => "MAX 10",
          lpm_hint => "CBX_MODULE_PREFIX=spi_pll",
          lpm_type => "altpll",
          operation_mode => "NORMAL",
          pll_type => "AUTO",
          port_activeclock => "PORT_UNUSED",
          port_areset => "PORT_USED",
          port_clkbad0 => "PORT_UNUSED",
          port_clkbad1 => "PORT_UNUSED",
          port_clkloss => "PORT_UNUSED",
          port_clkswitch => "PORT_UNUSED",
          port_configupdate => "PORT_UNUSED",
          port_fbin => "PORT_UNUSED",
          port_inclk0 => "PORT_USED",
          port_inclk1 => "PORT_UNUSED",
          port_locked => "PORT_USED",
          port_pfdena => "PORT_UNUSED",
          port_phasecounterselect => "PORT_UNUSED",
          port_phasedone => "PORT_UNUSED",
          port_phasestep => "PORT_UNUSED",
          port_phaseupdown => "PORT_UNUSED",
          port_pllena => "PORT_UNUSED",
          port_scanaclr => "PORT_UNUSED",
          port_scanclk => "PORT_UNUSED",
          port_scanclkena => "PORT_UNUSED",
          port_scandata => "PORT_UNUSED",
          port_scandataout => "PORT_UNUSED",
          port_scandone => "PORT_UNUSED",
          port_scanread => "PORT_UNUSED",
          port_scanwrite => "PORT_UNUSED",
          port_clk0 => "PORT_USED",
          port_clk1 => "PORT_USED",
          port_clk2 => "PORT_UNUSED",
          port_clk3 => "PORT_UNUSED",
          port_clk4 => "PORT_UNUSED",
          port_clk5 => "PORT_UNUSED",
          port_clkena0 => "PORT_UNUSED",
          port_clkena1 => "PORT_UNUSED",
          port_clkena2 => "PORT_UNUSED",
          port_clkena3 => "PORT_UNUSED",
          port_clkena4 => "PORT_UNUSED",
          port_clkena5 => "PORT_UNUSED",
          port_extclk0 => "PORT_UNUSED",
          port_extclk1 => "PORT_UNUSED",
          port_extclk2 => "PORT_UNUSED",
          port_extclk3 => "PORT_UNUSED",
          self_reset_on_loss_lock => "OFF",
          width_clock => 5
      )
      PORT MAP (
          areset => areset,
          inclk => sub_wire4,
          clk => sub_wire0,
          locked => poub
      );
  
  
  
  END SYN;

-- This VHDL was converted from Verilog using the
-- Icarus Verilog VHDL Code Generator 11.0 (stable) ()
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_ee_config is
  port (
    -- SPI_SDIO : inout std_logic;
    SPI_SDIO_i : in std_logic;
    SPI_SDIO_o : out std_logic;
    iG_INT2 : in std_logic;
    iRSTN : in std_logic;
    iSPI_CLK : in std_logic;
    iSPI_CLK_OUT : in std_logic;
    data_x : out unsigned(15 downto 0);
    data_y : out unsigned(15 downto 0);
    data_z : out unsigned(15 downto 0);
    oSPI_CLK : out std_logic;
    oSPI_CSN : out std_logic;
	 rdy : out std_logic_vector(0 to 0)
  );
end entity; 

architecture from_verilog of spi_ee_config is
  signal clear_status : std_logic;  -- Declared at spi_ee_config.v:42
  signal clear_status_d : unsigned(3 downto 0);  -- Declared at spi_ee_config.v:43
  signal high_byte : std_logic;  -- Declared at spi_ee_config.v:40
  signal high_byte_d : std_logic;  -- Declared at spi_ee_config.v:44
  signal ini_index : unsigned(3 downto 0);  -- Declared at spi_ee_config.v:32
  signal low_byte_data : unsigned(7 downto 0);  -- Declared at spi_ee_config.v:38
  signal p2s_data : unsigned(15 downto 0);  -- Declared at spi_ee_config.v:34
  signal read_back : std_logic;  -- Declared at spi_ee_config.v:41
  signal read_back_d : std_logic;  -- Declared at spi_ee_config.v:44
  signal read_idle_count : unsigned(14 downto 0);  -- Declared at spi_ee_config.v:45
  signal read_ready : std_logic;  -- Declared at spi_ee_config.v:42
  signal s2p_data : unsigned(7 downto 0);  -- Declared at spi_ee_config.v:37
  signal spi_end : std_logic;  -- Declared at spi_ee_config.v:36
  signal spi_go : std_logic;  -- Declared at spi_ee_config.v:35
  signal spi_state : std_logic;  -- Declared at spi_ee_config.v:39
  signal write_data : unsigned(13 downto 0);  -- Declared at spi_ee_config.v:33
  -- ajout
  signal read_idx : integer range 0 to 2; -- 0 -> x; 1 -> y; 2 -> z

  component spi_controller is
    port (
      SPI_SDIO_i : in std_logic;
      SPI_SDIO_o : out std_logic;
      iP2S_DATA : in unsigned(15 downto 0);
      iRSTN : in std_logic;
      iSPI_CLK : in std_logic;
      iSPI_CLK_OUT : in std_logic;
      iSPI_GO : in std_logic;
      oS2P_DATA : out unsigned(7 downto 0);
      oSPI_CLK : out std_logic;
      oSPI_CSN : out std_logic;
      oSPI_END : buffer std_logic
    );
  end component;
  signal oSPI_CLK_Readable : std_logic;  -- Needed to connect outputs
  signal oSPI_CSN_Readable : std_logic;  -- Needed to connect outputs
begin
  oSPI_CLK <= oSPI_CLK_Readable;
  oSPI_CSN <= oSPI_CSN_Readable;
  
  -- Generated from instantiation at spi_ee_config.v:50
  u_spi_controller: spi_controller
    port map (
      SPI_SDIO_i => SPI_SDIO_i,
      SPI_SDIO_o => SPI_SDIO_o,
      iP2S_DATA => p2s_data,
      iRSTN => iRSTN,
      iSPI_CLK => iSPI_CLK,
      iSPI_CLK_OUT => iSPI_CLK_OUT,
      iSPI_GO => spi_go,
      oS2P_DATA => s2p_data,
      oSPI_CLK => oSPI_CLK_Readable,
      oSPI_CSN => oSPI_CSN_Readable,
      oSPI_END => spi_end
    );
  
  -- Generated from always process in spi_ee_config (spi_ee_config.v:66)
  process (ini_index) is
  begin
    case ini_index is
      when X"0" =>
        write_data <= "10010000100000";
      when X"1" =>
        write_data <= "10010100000011";
      when X"2" =>
        write_data <= "10011000000001";
      when X"3" =>
        write_data <= "10011101111111";
      when X"4" =>
        write_data <= "10100000001001";
      when X"5" =>
        write_data <= "10100101000110";
      when X"6" =>
        write_data <= "10110000001001";
      when X"7" =>
        write_data <= "10111000010000";
      when X"8" =>
        write_data <= "10111100010000";
      when X"9" =>
        write_data <= "11000101000000";
      when others =>
        write_data <= "10110100001000";
    end case;
  end process;
  
  -- Generated from always process in spi_ee_config (spi_ee_config.v:81)
  process (iRSTN, iSPI_CLK) is
  begin
    if (not iRSTN) = '1' then
      ini_index <= X"0";
      spi_go <= '0';
      spi_state <= '0';
      read_idle_count <= "000000000000000";
      high_byte <= '0';
      read_idx <= 1;
      read_back <= '0';
      clear_status <= '0';
    elsif rising_edge(iSPI_CLK) then
      if ini_index < X"b" then
        case spi_state is
          when '0' =>
            p2s_data <= "00" & write_data;
            spi_go <= '1';
            spi_state <= '1';
          when '1' =>
            if spi_end = '1' then
              ini_index <= ini_index + X"1";
              spi_go <= '0';
              spi_state <= '0';
            end if;
          when others =>
            null;
        end case;
      else
        case spi_state is
          when '0' =>
            read_idle_count <= read_idle_count + "000000000000001";
            if high_byte = '1' then
              case read_idx is
                when 0 => p2s_data(8 + 7 downto 8) <= "10110011"; -- READMODE(2) & X_HB(6)
                when 1 => p2s_data(8 + 7 downto 8) <= "10110101"; -- READMODE(2) & Y_HB(6)
                when 2 => p2s_data(8 + 7 downto 8) <= "10110111"; -- READMODE(2) & Z_HB(6)
                when others => null;
              end case;
              read_back <= '1';
            else
              if read_ready = '1' then
                case read_idx is
                  when 0 => p2s_data(8 + 7 downto 8) <= "10110010"; -- READMODE(2) & X_LB(6)
                  when 1 => p2s_data(8 + 7 downto 8) <= "10110100"; -- READMODE(2) & Y_LB(6)
                  when 2 => p2s_data(8 + 7 downto 8) <= "10110110"; -- READMODE(2) & Z_LB(6)
                  when others => null;
                end case;
                read_back <= '1';
              else
                if (((not clear_status_d(3)) = '1') and (iG_INT2 = '1')) or (read_idle_count(14) = '1') then
                  p2s_data(8 + 7 downto 8) <= X"b0";
                  clear_status <= '1';
                end if;
              end if;
            end if;
            if (((high_byte = '1') or (read_ready = '1')) or (read_idle_count(14) = '1')) or (((not clear_status_d(3)) = '1') and (iG_INT2 = '1')) then
              spi_go <= '1';
              spi_state <= '1';
            end if;
            if read_back_d = '1' then
              if high_byte_d = '1' then
                case read_idx is
                  when 0 => data_z <= s2p_data & low_byte_data;
                  when 1 => data_x <= s2p_data & low_byte_data;
                  when 2 => data_y <= s2p_data & low_byte_data;
                  when others => null;
                end case;
					 rdy <= "1";
              else
                low_byte_data <= s2p_data;
					 rdy <= "0";
              end if;
            end if;
          when '1' =>
            if spi_end = '1' then
              spi_go <= '0';
              spi_state <= '0';
              if read_back = '1' then
                read_back <= '0';
                if high_byte = '1' then
                  if read_idx = 2 then
                    read_idx <= 0;
                  else
                    read_idx <= read_idx + 1;
                  end if;
                end if;
                high_byte <= not high_byte;
                read_ready <= '0';
              else
                clear_status <= '0';
                read_ready <= s2p_data(6);
                read_idle_count <= "000000000000000";
              end if;
            end if;
          when others =>
            null;
        end case;
      end if;
    end if;
  end process;
  
  -- Generated from always process in spi_ee_config (spi_ee_config.v:170)
  process (iRSTN, iSPI_CLK) is
  begin
    if (not iRSTN) = '1' then
      high_byte_d <= '0';
      read_back_d <= '0';
      clear_status_d <= X"0";
    elsif rising_edge(iSPI_CLK) then
      high_byte_d <= high_byte;
      read_back_d <= read_back;
      clear_status_d <= clear_status_d(0 + 2 downto 0) & clear_status;
    end if;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_controller is
  port (
    SPI_SDIO_i : in std_logic;
    SPI_SDIO_o : out std_logic;
    iP2S_DATA : in unsigned(15 downto 0);
    iRSTN : in std_logic;
    iSPI_CLK : in std_logic;
    iSPI_CLK_OUT : in std_logic;
    iSPI_GO : in std_logic;
    oS2P_DATA : out unsigned(7 downto 0);
    oSPI_CLK : out std_logic;
    oSPI_CSN : out std_logic;
    oSPI_END : buffer std_logic
  );
end entity;

architecture from_verilog of spi_controller is
  signal oS2P_DATA_Reg : unsigned(7 downto 0);
  signal tmp_ivl_13 : std_logic;  -- Temporary created at spi_controller.v:46
  signal tmp_ivl_15 : std_logic;  -- Temporary created at spi_controller.v:46
  signal tmp_ivl_17 : std_logic;  -- Temporary created at spi_controller.v:46
  signal tmp_ivl_19 : std_logic;  -- Temporary created at spi_controller.v:46
  signal read_mode : std_logic;  -- Declared at spi_controller.v:34
  signal spi_count : unsigned(3 downto 0);  -- Declared at spi_controller.v:36
  signal spi_count_en : std_logic;  -- Declared at spi_controller.v:35
  signal write_address : std_logic;  -- Declared at spi_controller.v:34
  
  function Reduce_OR(X : std_logic_vector) return std_logic is
    variable R : std_logic := '0';
  begin
    for I in X'Range loop
      R := X(I) or R;
    end loop;
    return R;
  end function;
begin
  oS2P_DATA <= oS2P_DATA_Reg;
  oSPI_CSN <= not iSPI_GO;
  tmp_ivl_15 <= tmp_ivl_13 or write_address;
  tmp_ivl_17 <= spi_count_en and tmp_ivl_15;
  read_mode <= iP2S_DATA(15);
  write_address <= spi_count(3);
  oSPI_END <= not Reduce_OR(std_logic_vector(spi_count));
  oSPI_CLK <= iSPI_CLK_OUT when spi_count_en = '1' else '1';
  tmp_ivl_13 <= not read_mode;
  tmp_ivl_19 <= iP2S_DATA(To_Integer(spi_count));
  SPI_SDIO_o <= tmp_ivl_19 when tmp_ivl_17 = '1' else 'Z';
  
  -- Generated from always process in spi_controller (spi_controller.v:48)
  process (iRSTN, iSPI_CLK) is
  begin
    if (not iRSTN) = '1' then
      spi_count_en <= '0';
      spi_count <= X"f";
    elsif rising_edge(iSPI_CLK) then
      if oSPI_END = '1' then
        spi_count_en <= '0';
      else
        if iSPI_GO = '1' then
          spi_count_en <= '1';
        end if;
      end if;
      if (not spi_count_en) = '1' then
        spi_count <= X"f";
      else
        spi_count <= spi_count - X"1";
      end if;
      if (read_mode = '1') and ((not write_address) = '1') then
        oS2P_DATA_Reg <= oS2P_DATA_Reg(0 + 6 downto 0) & SPI_SDIO_i;
      end if;
    end if;
  end process;
end architecture;

