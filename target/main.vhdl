-- code generated from the following source code:
--   ../ocaml-vm/vm/mlvalue.ecl
--   ../ocaml-vm/vm/fail.ecl
--   ../ocaml-vm/vm/ram.ecl
--   ../ocaml-vm/vm/runtime.ecl
--   ../ocaml-vm/vm/debug.ecl
--   ../ocaml-vm/vm/alloc.ecl
--   ../ocaml-vm/vm/prims.ecl
--   ../ocaml-vm/bytecode.ecl
--   ../ocaml-vm/vm/vm.ecl
--   ../ocaml-vm/vm/target-specific/intel-max10/IOs.ecl
--   ../ocaml-vm/vm/target-specific/intel-max10/main.ecl
--
-- with the following command:
--
--    ./eclat -arg ((true,true,true,true,true,true,true,true,true,true),(true,false)) ../ocaml-vm/vm/mlvalue.ecl ../ocaml-vm/vm/fail.ecl ../ocaml-vm/vm/ram.ecl ../ocaml-vm/vm/runtime.ecl ../ocaml-vm/vm/debug.ecl ../ocaml-vm/vm/alloc.ecl ../ocaml-vm/vm/prims.ecl ../ocaml-vm/bytecode.ecl ../ocaml-vm/vm/vm.ecl ../ocaml-vm/vm/target-specific/intel-max10/IOs.ecl ../ocaml-vm/vm/target-specific/intel-max10/main.ecl

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal run    : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 11);
       signal result : out value(0 to 57));
       
end entity;
architecture rtl of main is

  type t_state is (compute5620);
  signal state: t_state;
  type t_state_var6953 is (compute6940);
  signal state_var6953: t_state_var6953;
  type t_state_var6952 is (compute5634, pause_setI5635, pause_setI5639, pause_setI5643, pause_setI5647, pause_setI5651, pause_setI5655, pause_setI5659, pause_setI5663, pause_setI5667, pause_setI5671, pause_setI5675, pause_setI5679, pause_setI5683, pause_setI5687, pause_setI5691, pause_setI5695, pause_setI5699, pause_setI5703, pause_setI5707, pause_setI5711, pause_setI5715, pause_setI5719, pause_setI5723, pause_setI5727, pause_setI5731, pause_setI5735, pause_setI5739, pause_setI5743, pause_setI5747, pause_setI5751, pause_setI5755, pause_setI5759, pause_setI5763, pause_setI5767, pause_setI5771, pause_setI5775, pause_setI5779, pause_setI5783, pause_setI5787, pause_setI5791, pause_setI5795, pause_setI5799, pause_setI5803, pause_setI5807, pause_setI5811, pause_setI5815, pause_setI5819, pause_setI5823, pause_setI5827, pause_setI5831, pause_setI5835, pause_setI5839, pause_setI5843, pause_setI5847, pause_setI5851, pause_setI5855, pause_setI5859, pause_setI5863, pause_setI5867, pause_setI5871, pause_setI5875, pause_setI5879, pause_setI5883, pause_setI5887, pause_setI5891, pause_setI5895, pause_setI5899, pause_setI5903, pause_setI5907, pause_setI5911, pause_setI5915, pause_setI5919, pause_setI5923, pause_setI5927, pause_setI5931, pause_setI5935, pause_setI5939, pause_setI5943, pause_setI5947, pause_setI5951, pause_setI5955, pause_setI5959, pause_setI5963, pause_setI5967, pause_setI5971, pause_setI5975, pause_setI5979, pause_setII5636, pause_setII5640, pause_setII5644, pause_setII5648, pause_setII5652, pause_setII5656, pause_setII5660, pause_setII5664, pause_setII5668, pause_setII5672, pause_setII5676, pause_setII5680, pause_setII5684, pause_setII5688, pause_setII5692, pause_setII5696, pause_setII5700, pause_setII5704, pause_setII5708, pause_setII5712, pause_setII5716, pause_setII5720, pause_setII5724, pause_setII5728, pause_setII5732, pause_setII5736, pause_setII5740, pause_setII5744, pause_setII5748, pause_setII5752, pause_setII5756, pause_setII5760, pause_setII5764, pause_setII5768, pause_setII5772, pause_setII5776, pause_setII5780, pause_setII5784, pause_setII5788, pause_setII5792, pause_setII5796, pause_setII5800, pause_setII5804, pause_setII5808, pause_setII5812, pause_setII5816, pause_setII5820, pause_setII5824, pause_setII5828, pause_setII5832, pause_setII5836, pause_setII5840, pause_setII5844, pause_setII5848, pause_setII5852, pause_setII5856, pause_setII5860, pause_setII5864, pause_setII5868, pause_setII5872, pause_setII5876, pause_setII5880, pause_setII5884, pause_setII5888, pause_setII5892, pause_setII5896, pause_setII5900, pause_setII5904, pause_setII5908, pause_setII5912, pause_setII5916, pause_setII5920, pause_setII5924, pause_setII5928, pause_setII5932, pause_setII5936, pause_setII5940, pause_setII5944, pause_setII5948, pause_setII5952, pause_setII5956, pause_setII5960, pause_setII5964, pause_setII5968, pause_setII5972, pause_setII5976, pause_setII5980, q_wait5637, q_wait5641, q_wait5645, q_wait5649, q_wait5653, q_wait5657, q_wait5661, q_wait5665, q_wait5669, q_wait5673, q_wait5677, q_wait5681, q_wait5685, q_wait5689, q_wait5693, q_wait5697, q_wait5701, q_wait5705, q_wait5709, q_wait5713, q_wait5717, q_wait5721, q_wait5725, q_wait5729, q_wait5733, q_wait5737, q_wait5741, q_wait5745, q_wait5749, q_wait5753, q_wait5757, q_wait5761, q_wait5765, q_wait5769, q_wait5773, q_wait5777, q_wait5781, q_wait5785, q_wait5789, q_wait5793, q_wait5797, q_wait5801, q_wait5805, q_wait5809, q_wait5813, q_wait5817, q_wait5821, q_wait5825, q_wait5829, q_wait5833, q_wait5837, q_wait5841, q_wait5845, q_wait5849, q_wait5853, q_wait5857, q_wait5861, q_wait5865, q_wait5869, q_wait5873, q_wait5877, q_wait5881, q_wait5885, q_wait5889, q_wait5893, q_wait5897, q_wait5901, q_wait5905, q_wait5909, q_wait5913, q_wait5917, q_wait5921, q_wait5925, q_wait5929, q_wait5933, q_wait5937, q_wait5941, q_wait5945, q_wait5949, q_wait5953, q_wait5957, q_wait5961, q_wait5965, q_wait5969, q_wait5973, q_wait5977, q_wait5981);
  signal state_var6952: t_state_var6952;
  type t_state_var6951 is (compute5629);
  signal state_var6951: t_state_var6951;
  type t_state_var6950 is (compute5999, \$14365_copy_root_in_ram610\, \$14372_aux611\, \$14382_forever617\, \$14408_loop612\, \$14475_loop613\, \$14602_loop613\, \$14698_loop613\, \$14794_loop613\, pause_getI6008, pause_getI6025, pause_getI6030, pause_getI6035, pause_getI6048, pause_getI6065, pause_getI6070, pause_getI6075, pause_getI6080, pause_getI6086, pause_getI6094, pause_getI6111, pause_getI6116, pause_getI6125, pause_getI6142, pause_getI6147, pause_getII6009, pause_getII6026, pause_getII6031, pause_getII6036, pause_getII6049, pause_getII6066, pause_getII6071, pause_getII6076, pause_getII6081, pause_getII6087, pause_getII6095, pause_getII6112, pause_getII6117, pause_getII6126, pause_getII6143, pause_getII6148, pause_setI6000, pause_setI6004, pause_setI6013, pause_setI6017, pause_setI6021, pause_setI6040, pause_setI6044, pause_setI6053, pause_setI6057, pause_setI6061, pause_setI6090, pause_setI6099, pause_setI6103, pause_setI6107, pause_setI6121, pause_setI6130, pause_setI6134, pause_setI6138, pause_setII6001, pause_setII6005, pause_setII6014, pause_setII6018, pause_setII6022, pause_setII6041, pause_setII6045, pause_setII6054, pause_setII6058, pause_setII6062, pause_setII6091, pause_setII6100, pause_setII6104, pause_setII6108, pause_setII6122, pause_setII6131, pause_setII6135, pause_setII6139, q_wait6002, q_wait6006, q_wait6010, q_wait6015, q_wait6019, q_wait6023, q_wait6027, q_wait6032, q_wait6037, q_wait6042, q_wait6046, q_wait6050, q_wait6055, q_wait6059, q_wait6063, q_wait6067, q_wait6072, q_wait6077, q_wait6082, q_wait6088, q_wait6092, q_wait6096, q_wait6101, q_wait6105, q_wait6109, q_wait6113, q_wait6118, q_wait6123, q_wait6127, q_wait6132, q_wait6136, q_wait6140, q_wait6144, q_wait6149);
  signal state_var6950: t_state_var6950;
  type t_state_var6949 is (compute5996);
  signal state_var6949: t_state_var6949;
  type t_state_var6948 is (compute5989, \$12221_make_block531\, \$12257_apply585\, \$12258_offsetclosure_n586\, \$12259_binop_int590\, \$12260_compare591\, \$12261_binop_compare592\, \$12262_make_block_n593\, \$12263_branch_if595\, \$12267_compbranch596\, \$12287_w0597\, \$12289_w1598\, \$12292_w3599\, \$12443_w600\, \$12496_fill601\, \$12586_fill602\, \$12804_w603\, \$12931_forever617\, \$12968_forever617\, \$13283_forever617\, \$13561_loop_push604\, \$13950_forever617\, \$13957_forever617\, \$13964_forever617\, \$14112_modulo615\, \$14127_modulo615\, \$14296_wait609\, pause_getI6157, pause_getI6189, pause_getI6194, pause_getI6199, pause_getI6210, pause_getI6215, pause_getI6223, pause_getI6232, pause_getI6242, pause_getI6247, pause_getI6251, pause_getI6255, pause_getI6259, pause_getI6263, pause_getI6267, pause_getI6271, pause_getI6275, pause_getI6287, pause_getI6295, pause_getI6303, pause_getI6311, pause_getI6319, pause_getI6327, pause_getI6335, pause_getI6343, pause_getI6347, pause_getI6351, pause_getI6355, pause_getI6359, pause_getI6367, pause_getI6375, pause_getI6383, pause_getI6395, pause_getI6400, pause_getI6404, pause_getI6424, pause_getI6428, pause_getI6432, pause_getI6436, pause_getI6444, pause_getI6452, pause_getI6460, pause_getI6468, pause_getI6472, pause_getI6476, pause_getI6480, pause_getI6488, pause_getI6492, pause_getI6496, pause_getI6500, pause_getI6508, pause_getI6512, pause_getI6516, pause_getI6520, pause_getI6524, pause_getI6528, pause_getI6532, pause_getI6552, pause_getI6556, pause_getI6568, pause_getI6572, pause_getI6592, pause_getI6596, pause_getI6600, pause_getI6604, pause_getI6608, pause_getI6617, pause_getI6622, pause_getI6626, pause_getI6630, pause_getI6647, pause_getI6651, pause_getI6667, pause_getI6675, pause_getI6679, pause_getI6683, pause_getI6704, pause_getI6713, pause_getI6722, pause_getI6726, pause_getI6735, pause_getI6739, pause_getI6743, pause_getI6752, pause_getI6756, pause_getI6760, pause_getI6764, pause_getI6773, pause_getI6777, pause_getI6781, pause_getI6785, pause_getI6797, pause_getI6805, pause_getI6810, pause_getI6818, pause_getI6832, pause_getI6836, pause_getI6840, pause_getI6844, pause_getI6856, pause_getI6869, pause_getI6878, pause_getI6905, pause_getI6910, pause_getI6915, pause_getI6920, pause_getI6925, pause_getII6158, pause_getII6190, pause_getII6195, pause_getII6200, pause_getII6211, pause_getII6216, pause_getII6224, pause_getII6233, pause_getII6243, pause_getII6248, pause_getII6252, pause_getII6256, pause_getII6260, pause_getII6264, pause_getII6268, pause_getII6272, pause_getII6276, pause_getII6288, pause_getII6296, pause_getII6304, pause_getII6312, pause_getII6320, pause_getII6328, pause_getII6336, pause_getII6344, pause_getII6348, pause_getII6352, pause_getII6356, pause_getII6360, pause_getII6368, pause_getII6376, pause_getII6384, pause_getII6396, pause_getII6401, pause_getII6405, pause_getII6425, pause_getII6429, pause_getII6433, pause_getII6437, pause_getII6445, pause_getII6453, pause_getII6461, pause_getII6469, pause_getII6473, pause_getII6477, pause_getII6481, pause_getII6489, pause_getII6493, pause_getII6497, pause_getII6501, pause_getII6509, pause_getII6513, pause_getII6517, pause_getII6521, pause_getII6525, pause_getII6529, pause_getII6533, pause_getII6553, pause_getII6557, pause_getII6569, pause_getII6573, pause_getII6593, pause_getII6597, pause_getII6601, pause_getII6605, pause_getII6609, pause_getII6618, pause_getII6623, pause_getII6627, pause_getII6631, pause_getII6648, pause_getII6652, pause_getII6668, pause_getII6676, pause_getII6680, pause_getII6684, pause_getII6705, pause_getII6714, pause_getII6723, pause_getII6727, pause_getII6736, pause_getII6740, pause_getII6744, pause_getII6753, pause_getII6757, pause_getII6761, pause_getII6765, pause_getII6774, pause_getII6778, pause_getII6782, pause_getII6786, pause_getII6798, pause_getII6806, pause_getII6811, pause_getII6819, pause_getII6833, pause_getII6837, pause_getII6841, pause_getII6845, pause_getII6857, pause_getII6870, pause_getII6879, pause_getII6906, pause_getII6911, pause_getII6916, pause_getII6921, pause_getII6926, pause_setI5990, pause_setI6161, pause_setI6166, pause_setI6171, pause_setI6176, pause_setI6180, pause_setI6184, pause_setI6219, pause_setI6228, pause_setI6237, pause_setI6279, pause_setI6283, pause_setI6291, pause_setI6299, pause_setI6307, pause_setI6315, pause_setI6323, pause_setI6331, pause_setI6339, pause_setI6363, pause_setI6371, pause_setI6379, pause_setI6387, pause_setI6391, pause_setI6408, pause_setI6412, pause_setI6416, pause_setI6420, pause_setI6440, pause_setI6448, pause_setI6456, pause_setI6464, pause_setI6484, pause_setI6504, pause_setI6536, pause_setI6540, pause_setI6544, pause_setI6548, pause_setI6560, pause_setI6564, pause_setI6576, pause_setI6580, pause_setI6584, pause_setI6588, pause_setI6613, pause_setI6634, pause_setI6638, pause_setI6643, pause_setI6655, pause_setI6659, pause_setI6663, pause_setI6671, pause_setI6688, pause_setI6692, pause_setI6696, pause_setI6700, pause_setI6709, pause_setI6718, pause_setI6731, pause_setI6748, pause_setI6769, pause_setI6789, pause_setI6793, pause_setI6801, pause_setI6814, pause_setI6823, pause_setI6827, pause_setI6848, pause_setI6852, pause_setI6861, pause_setI6865, pause_setI6874, pause_setI6882, pause_setI6887, pause_setI6892, pause_setI6896, pause_setI6900, pause_setII5991, pause_setII6162, pause_setII6167, pause_setII6172, pause_setII6177, pause_setII6181, pause_setII6185, pause_setII6220, pause_setII6229, pause_setII6238, pause_setII6280, pause_setII6284, pause_setII6292, pause_setII6300, pause_setII6308, pause_setII6316, pause_setII6324, pause_setII6332, pause_setII6340, pause_setII6364, pause_setII6372, pause_setII6380, pause_setII6388, pause_setII6392, pause_setII6409, pause_setII6413, pause_setII6417, pause_setII6421, pause_setII6441, pause_setII6449, pause_setII6457, pause_setII6465, pause_setII6485, pause_setII6505, pause_setII6537, pause_setII6541, pause_setII6545, pause_setII6549, pause_setII6561, pause_setII6565, pause_setII6577, pause_setII6581, pause_setII6585, pause_setII6589, pause_setII6614, pause_setII6635, pause_setII6639, pause_setII6644, pause_setII6656, pause_setII6660, pause_setII6664, pause_setII6672, pause_setII6689, pause_setII6693, pause_setII6697, pause_setII6701, pause_setII6710, pause_setII6719, pause_setII6732, pause_setII6749, pause_setII6770, pause_setII6790, pause_setII6794, pause_setII6802, pause_setII6815, pause_setII6824, pause_setII6828, pause_setII6849, pause_setII6853, pause_setII6862, pause_setII6866, pause_setII6875, pause_setII6883, pause_setII6888, pause_setII6893, pause_setII6897, pause_setII6901, q_wait5992, q_wait6159, q_wait6163, q_wait6168, q_wait6173, q_wait6178, q_wait6182, q_wait6186, q_wait6191, q_wait6196, q_wait6201, q_wait6212, q_wait6217, q_wait6221, q_wait6225, q_wait6230, q_wait6234, q_wait6239, q_wait6244, q_wait6249, q_wait6253, q_wait6257, q_wait6261, q_wait6265, q_wait6269, q_wait6273, q_wait6277, q_wait6281, q_wait6285, q_wait6289, q_wait6293, q_wait6297, q_wait6301, q_wait6305, q_wait6309, q_wait6313, q_wait6317, q_wait6321, q_wait6325, q_wait6329, q_wait6333, q_wait6337, q_wait6341, q_wait6345, q_wait6349, q_wait6353, q_wait6357, q_wait6361, q_wait6365, q_wait6369, q_wait6373, q_wait6377, q_wait6381, q_wait6385, q_wait6389, q_wait6393, q_wait6397, q_wait6402, q_wait6406, q_wait6410, q_wait6414, q_wait6418, q_wait6422, q_wait6426, q_wait6430, q_wait6434, q_wait6438, q_wait6442, q_wait6446, q_wait6450, q_wait6454, q_wait6458, q_wait6462, q_wait6466, q_wait6470, q_wait6474, q_wait6478, q_wait6482, q_wait6486, q_wait6490, q_wait6494, q_wait6498, q_wait6502, q_wait6506, q_wait6510, q_wait6514, q_wait6518, q_wait6522, q_wait6526, q_wait6530, q_wait6534, q_wait6538, q_wait6542, q_wait6546, q_wait6550, q_wait6554, q_wait6558, q_wait6562, q_wait6566, q_wait6570, q_wait6574, q_wait6578, q_wait6582, q_wait6586, q_wait6590, q_wait6594, q_wait6598, q_wait6602, q_wait6606, q_wait6610, q_wait6615, q_wait6619, q_wait6624, q_wait6628, q_wait6632, q_wait6636, q_wait6640, q_wait6645, q_wait6649, q_wait6653, q_wait6657, q_wait6661, q_wait6665, q_wait6669, q_wait6673, q_wait6677, q_wait6681, q_wait6685, q_wait6690, q_wait6694, q_wait6698, q_wait6702, q_wait6706, q_wait6711, q_wait6715, q_wait6720, q_wait6724, q_wait6728, q_wait6733, q_wait6737, q_wait6741, q_wait6745, q_wait6750, q_wait6754, q_wait6758, q_wait6762, q_wait6766, q_wait6771, q_wait6775, q_wait6779, q_wait6783, q_wait6787, q_wait6791, q_wait6795, q_wait6799, q_wait6803, q_wait6807, q_wait6812, q_wait6816, q_wait6820, q_wait6825, q_wait6829, q_wait6834, q_wait6838, q_wait6842, q_wait6846, q_wait6850, q_wait6854, q_wait6858, q_wait6863, q_wait6867, q_wait6871, q_wait6876, q_wait6880, q_wait6884, q_wait6889, q_wait6894, q_wait6898, q_wait6902, q_wait6907, q_wait6912, q_wait6917, q_wait6922, q_wait6927);
  signal state_var6948: t_state_var6948;
  type t_state_var6947 is (compute5986);
  signal state_var6947: t_state_var6947;
  type t_state_var6946 is (compute5627);
  signal state_var6946: t_state_var6946;
  type t_state_var6945 is (compute5622);
  signal state_var6945: t_state_var6945;
  type t_state_var6944 is (compute5622);
  signal state_var6944: t_state_var6944;
  type array_value_16 is array (natural range <>) of value(0 to 15);
  type array_value_31 is array (natural range <>) of value(0 to 30);
  type array_value_32 is array (natural range <>) of value(0 to 31);
  signal ram : array_value_32(0 to 16383) := (others => "000"& X"000000" & X"0" & eclat_true);
  signal \$ram_value\ : value(0 to 31);
  signal \$ram_ptr\ : natural range 0 to 16383;
  signal \$ram_ptr_write\ : natural range 0 to 16383;
  signal \$ram_write\ : value(0 to 31);
  signal \$ram_write_request\ : std_logic := '0';
  signal global_end : array_value_16(0 to 0) := (others => X"000" & X"0");
  signal \$global_end_value\ : value(0 to 15);
  signal \$global_end_ptr\ : natural range 0 to 0;
  signal \$global_end_ptr_write\ : natural range 0 to 0;
  signal \$global_end_write\ : value(0 to 15);
  signal \$global_end_write_request\ : std_logic := '0';
  signal code : array_value_31(0 to 83) := (others => "000"& X"000000" & X"0");
  signal \$code_value\ : value(0 to 30);
  signal \$code_ptr\ : natural range 0 to 83;
  signal \$code_ptr_write\ : natural range 0 to 83;
  signal \$code_write\ : value(0 to 30);
  signal \$code_write_request\ : std_logic := '0';
  
  begin
    process (clk)
            begin
            if (rising_edge(clk)) then
                  if \$ram_write_request\ = '1' then
                    ram(\$ram_ptr_write\) <= \$ram_write\;
                  else
                   \$ram_value\ <= ram(\$ram_ptr\);
                  end if;
            end if;
        end process;
    
    process (clk)
            begin
            if (rising_edge(clk)) then
                  if \$global_end_write_request\ = '1' then
                    global_end(\$global_end_ptr_write\) <= \$global_end_write\;
                  else
                   \$global_end_value\ <= global_end(\$global_end_ptr\);
                  end if;
            end if;
        end process;
    
    process (clk)
            begin
            if (rising_edge(clk)) then
                  if \$code_write_request\ = '1' then
                    code(\$code_ptr_write\) <= \$code_write\;
                  else
                   \$code_value\ <= code(\$code_ptr\);
                  end if;
            end if;
        end process;
    
    process(clk)
      variable \$14877\ : value(0 to 1) := (others => '0');
      variable \$14408_loop612_arg\, \$12792\, \$12221_make_block531_result\, 
               \$12578\, \$12488\, \$12287_w0597_arg\, \$14018\, \$12279\ : value(0 to 95) := (others => '0');
      variable \$12260_compare591_arg\ : value(0 to 93) := (others => '0');
      variable \$14208\, \$14345\, \$14202\, \$14205\, \$14444\, \$14362\, 
               \$14569\ : value(0 to 47) := (others => '0');
      variable \$12804_w603_arg\, \$14372_aux611_arg\, \$14602_loop613_arg\, 
               \$14475_loop613_arg\, \$14698_loop613_arg\, 
               \$13561_loop_push604_arg\, \$14794_loop613_arg\, 
               \$12443_w600_arg\ : value(0 to 63) := (others => '0');
      variable \$v6919\, \$v6909\, \$v6914\, \$v6924\ : value(0 to 7) := (others => '0');
      variable \$12212\, \$12218\, \$12198\, \$12263_branch_if595_arg\ : value(0 to 122) := (others => '0');
      variable \$14023_sp\, \$12278_sp\, \$12288_sp\, 
               \$13561_loop_push604_result\, \$14408_loop612_result\, 
               \$12804_w603_result\, \$14024_sp\, \$12496_fill601_result\, 
               \$14409_next\, \$14372_aux611_result\, \$14367\, \$14213_sp\, 
               \$12293_sp\, \$14212_sp\, \$12497_sp\, \$14368_next\, 
               \$14211_sp\, \$14214_sp\, \$12287_w0597_result\, \$12975_ofs\, 
               \$12586_fill602_result\, \$14365_copy_root_in_ram610_result\, 
               \$12292_w3599_result\, \$12587_sp\, \$12805_sp\, \$13565_sp\, 
               \$14373_next\, \$12487_sp\, \$14366_next\ : value(0 to 15) := (others => '0');
      variable \$14127_modulo615_arg\, \$14112_modulo615_arg\ : value(0 to 61) := (others => '0');
      variable \$14296_wait609_arg\ : value(0 to 96) := (others => '0');
      variable result5618 : value(0 to 57) := (others => '0');
      variable \$12267_compbranch596_arg\ : value(0 to 215) := (others => '0');
      variable \$12165\, \$12190\ : value(0 to 3) := (others => '0');
      variable \$14365_copy_root_in_ram610_id\, \$12221_make_block531_id\, 
               \$12260_compare591_id\ : value(0 to 11) := (others => '0');
      variable \$12270_argument2\, \$12976\, \$v6708\, \$v6717\, 
               \$14108_res\, \$12275\, \$12343\, \$14112_modulo615_result\, 
               \$12268_argument1\, \$12272_argument3\, \$v6747\, 
               \$14127_modulo615_result\, \$14128_r\, \$12265\, \$14113_r\, 
               \$v6730\, \$v6768\, \$13994_arg\ : value(0 to 30) := (others => '0');
      variable \$12258_offsetclosure_n586_arg\ : value(0 to 137) := (others => '0');
      variable result5997 : value(0 to 127) := (others => '0');
      variable \$12221_make_block531_arg\ : value(0 to 103) := (others => '0');
      variable \$v6503\, \$v6443\, \$v6294\, \$v6354\, \$v6435\, \$v6843\, 
               \$v5806\, \$v5946\, \$v5770\, \$v5866\, \$v6203\, \$v6868\, 
               \$v6028\, \$14074_res\, \$v6908\, \$v6411\, \$v6358\, 
               \$v5642\, \$v5822\, \$v5954\, \$v6928\, \$v6011\, \$v6342\, 
               \$v6151\, \$v6682\, \$v5782\, rdy5619, \$v6932\, \$v6547\, 
               \$v6800\, \$v6523\, \$v5858\, \$v5714\, \$v6535\, \$v6583\, 
               \$v5738\, \$v5870\, \$v6826\, \$v6695\, \$v6093\, \$v6302\, 
               \$v6120\, \$v5810\, \$v5650\, \$v6241\, \$v5818\, \$v6885\, 
               \$v6254\, \$v5890\, \$14475_loop613_result\, \$v6707\, 
               \$v6555\, \$v6687\, \$v5698\, \$v6278\, \$v6374\, \$v6507\, 
               \$v6674\, \$v6923\, \$v5862\, \$v6188\, \$v6258\, \$v5962\, 
               \$v5970\, \$v6929\, \$v6603\, \$v6742\, \$v6084\, rdy6939, 
               \$v5894\, \$v6646\, \$14602_loop613_result\, \$v5666\, 
               \$v6612\, \$v6835\, \$v6686\, \$v6226\, \$v6729\, \$v6106\, 
               \$v6543\, \$v6804\, \$v5790\, \$v6270\, \$v6187\, \$v5774\, 
               \$v6029\, \$v6322\, \$v6156\, \$v6266\, \$v6641\, \$v5798\, 
               \$v5914\, \$v5886\, \$v6274\, \$v6551\, \$v6813\, \$v6483\, 
               \$v5786\, \$v5930\, \$v6060\, \$v6310\, \$v5854\, \$v6654\, 
               \$v6712\, \$v6455\, \$v5654\, \$v6038\, \$v6326\, \$v6366\, 
               \$v6860\, \$v6637\, \$v6611\, \$v5834\, \$v6621\, \$v6877\, 
               \$v6069\, rdy5988, \$v6202\, \$v6145\, \$v6918\, \$v5802\, 
               \$v6245\, \$v6141\, \$v6527\, \$v6839\, \$v6746\, rdy5995, 
               \$v5994\, rdy5626, \$v6851\, \$v6098\, \$v6658\, \$v6511\, 
               \$v5746\, \$v5624\, rdy5628, \$v6222\, \$v5670\, \$v6403\, 
               \$v6097\, \$v5850\, \$v6463\, \$v6796\, \$v6831\, \$v6386\, 
               \$v6154\, \$v6563\, \$v5966\, \$v5902\, \$v5958\, \$v6817\, 
               \$14881\, \$v6670\, \$v5910\, \$v6020\, \$v5842\, \$v6699\, 
               \$v6903\, \$v5658\, \$v5625\, \$v6235\, \$v5706\, \$v6759\, 
               \$v6633\, rdy5998, \$v6064\, \$v6207\, \$v6150\, \$v6024\, 
               \$v6282\, \$v6531\, \$v5826\, \$v6179\, \$v6110\, \$v5926\, 
               \$v5674\, \$v5702\, \$v6298\, \$v5762\, \$v6052\, \$v6650\, 
               \$v6579\, \$v6003\, \$v5974\, \$v6808\, \$v6039\, \$v6515\, 
               \$v6227\, \$v6936\, \$v6487\, \$v6809\, \$v5993\, \$v6784\, 
               \$v5984\, \$v6119\, \$v6591\, \$v6133\, 
               \$12260_compare591_result\, \$v5934\, \$v6886\, \$v6398\, 
               \$v6051\, \$14794_loop613_result\, \$v6250\, \$v6822\, 
               \$v5838\, \$v6218\, rdy5621, result5632, \$v5631\, \$v6780\, 
               \$v6074\, \$v6539\, \$v6198\, \$v6873\, \$v6338\, \$v6378\, 
               \$v5682\, \$v6169\, \$v6830\, \$v6394\, \$v6559\, \$v5898\, 
               \$v6089\, \$v6772\, \$v6475\, \$v6716\, \$v5690\, \$v6666\, 
               \$v6599\, \$v6286\, \$v6318\, \$v5938\, rdy5985, \$v5814\, 
               \$v6933\, \$v6390\, \$v5882\, \$v6847\, \$v5686\, \$v5758\, 
               \$v6872\, \$v6208\, \$v6859\, \$v6047\, \$v5906\, 
               \$14880_rdy\, \$v6792\, \$v5718\, \$v5694\, \$v6519\, 
               \$v6124\, \$v5646\, \$v6043\, \$v5662\, \$v6205\, \$v6170\, 
               \$v6146\, \$v6192\, \$v6073\, \$v6431\, \$v6629\, \$v6078\, 
               \$v6346\, \$v6213\, \$v6012\, \$v6160\, \$v5766\, \$v6115\, 
               \$v6595\, \$v6350\, \$v6890\, \$v6620\, \$v6314\, \$v6129\, 
               \$v6183\, \$v5710\, \$v5730\, \$v5878\, \$v6016\, \$v6721\, 
               \$v6362\, \$v6085\, \$v6370\, \$v6034\, \$v6855\, \$v6738\, 
               \$v6678\, \$v6942\, \$v6776\, \$v6165\, \$v5742\, \$v6751\, 
               \$v6407\, \$v6495\, \$v6290\, \$v6625\, \$v6306\, \$v6471\, 
               \$v5726\, \$v6204\, \$v5830\, \$14698_loop613_result\, 
               \$v6399\, \$v6330\, \$v5874\, \$v6236\, \$v6427\, \$v6895\, 
               \$v5918\, \$v6079\, \$v6164\, \$v6439\, \$v6128\, \$v5778\, 
               \$v6571\, \$12443_w600_result\, \$v6240\, \$v6904\, \$v6788\, 
               \$v6419\, \$v5734\, \$v6382\, \$v6152\, \$13344_b\, \$v6662\, 
               \$v6864\, \$v6479\, \$v6587\, \$v6767\, \$v6467\, \$v6575\, 
               \$v6881\, \$v6175\, \$v5950\, \$v6102\, \$v5978\, \$v6938\, 
               \$v6734\, \$v6703\, \$v6755\, \$v5922\, \$v5794\, \$v6821\, 
               \$v6206\, \$v6607\, \$v6083\, \$v6262\, rdy5633, \$v6334\, 
               \$12289_w1598_result\, \$v6930\, \$v6943\, \$v6068\, \$v6447\, 
               \$v6231\, \$v6459\, \$v6491\, \$v5942\, \$v5754\, \$v6913\, 
               \$v6567\, \$v6899\, \$v6423\, \$v6174\, \$v6246\, \$v6691\, 
               \$v5678\, \$v6451\, \$v6137\, \$v6725\, \$v5722\, \$v6033\, 
               \$v6114\, \$v6642\, \$v5846\, \$v6197\, \$v6935\, \$v5750\, 
               \$v6007\, \$v5982\, \$v6193\, \$v6891\, \$v6056\, \$v6499\, 
               \$v6415\, \$v6763\, \$v6616\, \$v5638\ : value(0 to 0) := (others => '0');
      variable \$12262_make_block_n593_arg\ : value(0 to 171) := (others => '0');
      variable \$12257_apply585_arg\ : value(0 to 165) := (others => '0');
      variable \$14296_wait609_result\, \$14272\, \$12292_w3599_arg\, 
               \$14351\, \$12289_w1598_arg\, 
               \$14365_copy_root_in_ram610_arg\, \$12586_fill602_arg\, 
               \$12496_fill601_arg\ : value(0 to 79) := (others => '0');
      variable \$14324\, \$14314\ : value(0 to 128) := (others => '0');
      variable \$13214\, \$12261_binop_compare592_arg\, \$13074\, 
               \$12259_binop_int590_arg\, \$13045\, \$13156\, \$13110\ : value(0 to 153) := (others => '0');
      variable \$14404\, \$13086_v\, \$14684_hd\, \$13122_v\, \$14104_v\, 
               \$12806_v\, \$14252_v\, \$13168_v\, \$12894_v\, \$13475\, 
               \$13721_v\, \$14509\, \$13846_v\, \$13226_v\, \$13350_v\, 
               \$13560_hd\, \$13776_hd\, \$13453_v\, \$12546\, \$14636\, 
               \$12171\, \$13408_v\, \$14583_w\, \$13836_next_acc\, \$12477\, 
               \$14025_v\, \$12811_v\, \$12951_v\, \$14828\, \$12550_v\, 
               \$13806_v\, \$13486\, \$13522\, \$14775_w\, \$13900_v\, 
               \$13668_v\, \$12736_v\, \$13569_next_env\, \$14780_hd\, 
               \$12842_v\, \$13380_v\, \$13535\, \$14732\, \$13360_v\, 
               \$12810_v\, \$12182\, \$12634_v\, \$14588_hd\, \$14255_v\, 
               \$12731_v\, \$12776\, \$13417_v\, \$13611\, \$13464\, 
               \$13497\, \$13747_v\, \$14036_v\, \$13683_v\, \$12670\, 
               \$13899_v\, \$14984\, \$12656\, \$12512_v\, \$13895_v\, 
               \$13435_v\, \$12566_v\, \$v6209\, \$13509\, \$12626_v\, 
               \$13300_f0\, \$12942_v\, \$12705\, \$13876_v\, \$13807_v\, 
               \$13057_v\, \$13370_v\, \$13444_v\, \$13796_v\, \$14070_v\, 
               \$14440\, \$13385_v\, \$12448\, \$13548\, \$13891_v\, 
               \$14679_w\, \$14218\, \$14456_w\, \$13399_v\, \$12900_v\, 
               \$12987_hd\, \$13789_v\, \$13698_v\, \$13847_v\, \$v6214\, 
               \$14249_v\, \$13713_v\, \$13734_v\, \$13375_v\, \$13829_v\, 
               \$12398_v\, \$14461_hd\, \$12562\, \$13365_v\, \$12735_v\, 
               \$14565\, \$13355_v\, \$12602_v\, \$13426_v\, \$13760_v\, 
               \$12164_cy\ : value(0 to 31) := (others => '0');
      variable \$12257_apply585_result\, \$12258_offsetclosure_n586_result\, 
               \$12261_binop_compare592_result\, 
               \$12263_branch_if595_result\, \$12262_make_block_n593_result\, 
               \$12267_compbranch596_result\, result5987, 
               \$12259_binop_int590_result\ : value(0 to 121) := (others => '0');
      variable \$ram_ptr_take\ : value(0 to 0) := "0";
      variable \$global_end_ptr_take\ : value(0 to 0) := "0";
      variable \$code_ptr_take\ : value(0 to 0) := "0";
      
    begin
      
      if rising_edge(clk) then
        if (reset = '1') then
          default_zero(\$v5638\); default_zero(\$12164_cy\); 
          default_zero(\$v6616\); default_zero(\$12443_w600_arg\); 
          default_zero(\$v6763\); default_zero(\$14366_next\); 
          default_zero(\$v6415\); default_zero(\$v6499\); 
          default_zero(\$13760_v\); default_zero(\$v6056\); 
          default_zero(\$v6891\); default_zero(\$v6193\); 
          default_zero(\$v5982\); default_zero(\$v6007\); 
          default_zero(\$14794_loop613_arg\); default_zero(\$13426_v\); 
          default_zero(\$v5750\); default_zero(\$v6935\); 
          default_zero(\$12602_v\); default_zero(\$v6197\); 
          default_zero(\$v5846\); default_zero(\$v6642\); 
          default_zero(\$v6114\); default_zero(\$13355_v\); 
          default_zero(\$14569\); default_zero(\$14565\); 
          default_zero(\$v6033\); default_zero(\$v5722\); 
          default_zero(\$12279\); default_zero(\$12735_v\); 
          default_zero(\$13365_v\); default_zero(\$12260_compare591_id\); 
          default_zero(\$13110\); default_zero(\$v6725\); 
          default_zero(\$v6137\); default_zero(\$v6451\); 
          default_zero(\$13994_arg\); default_zero(\$12487_sp\); 
          default_zero(\$14373_next\); default_zero(\$v5678\); 
          default_zero(\$v6691\); default_zero(\$12496_fill601_arg\); 
          default_zero(\$12562\); default_zero(\$v6246\); 
          default_zero(\$v6174\); default_zero(\$v6423\); 
          default_zero(\$14461_hd\); default_zero(\$v6899\); 
          default_zero(\$v6567\); default_zero(\$12398_v\); 
          default_zero(\$v6913\); default_zero(\$13829_v\); 
          default_zero(\$13375_v\); default_zero(\$v5754\); 
          default_zero(\$13734_v\); default_zero(\$13713_v\); 
          default_zero(\$14249_v\); default_zero(\$v6214\); 
          default_zero(\$v5942\); default_zero(\$v6491\); 
          default_zero(\$v6459\); default_zero(\$13847_v\); 
          default_zero(\$v6231\); default_zero(\$13698_v\); 
          default_zero(\$v6447\); default_zero(\$13789_v\); 
          default_zero(\$v6068\); default_zero(\$v6943\); 
          default_zero(\$v6930\); default_zero(\$v6768\); 
          default_zero(\$12289_w1598_result\); default_zero(\$13565_sp\); 
          default_zero(\$v6334\); default_zero(rdy5633); 
          default_zero(\$v6262\); default_zero(\$v6083\); 
          default_zero(\$v6607\); default_zero(\$v6206\); 
          default_zero(\$v6821\); default_zero(\$v5794\); 
          default_zero(\$12987_hd\); default_zero(\$12900_v\); 
          default_zero(\$v5922\); default_zero(\$v6755\); 
          default_zero(\$13561_loop_push604_arg\); default_zero(\$v6730\); 
          default_zero(\$v6703\); default_zero(\$v6734\); 
          default_zero(\$v6938\); default_zero(\$12586_fill602_arg\); 
          default_zero(\$v5978\); default_zero(\$13156\); 
          default_zero(\$14113_r\); default_zero(\$v6102\); 
          default_zero(\$v5950\); 
          default_zero(\$14365_copy_root_in_ram610_arg\); 
          default_zero(\$12190\); default_zero(\$v6175\); 
          default_zero(\$v6881\); default_zero(\$v6575\); 
          default_zero(\$v6467\); default_zero(\$v6767\); 
          default_zero(\$v6587\); default_zero(\$v6479\); 
          default_zero(\$v6864\); default_zero(\$v6662\); 
          default_zero(\$13344_b\); default_zero(\$12257_apply585_arg\); 
          default_zero(\$13399_v\); default_zero(\$v6152\); 
          default_zero(\$12805_sp\); default_zero(\$14698_loop613_arg\); 
          default_zero(\$v6382\); default_zero(\$14456_w\); 
          default_zero(\$v5734\); default_zero(\$v6419\); 
          default_zero(\$v6788\); default_zero(\$v6904\); 
          default_zero(\$13045\); default_zero(\$14018\); 
          default_zero(\$v6240\); default_zero(\$12587_sp\); 
          default_zero(\$12443_w600_result\); 
          default_zero(\$12259_binop_int590_result\); 
          default_zero(result5987); default_zero(\$v6571\); 
          default_zero(\$14218\); default_zero(\$v5778\); 
          default_zero(\$v6128\); default_zero(\$v6439\); 
          default_zero(\$v6164\); default_zero(\$v6079\); 
          default_zero(\$14679_w\); default_zero(\$v5918\); 
          default_zero(\$13891_v\); default_zero(\$13548\); 
          default_zero(\$v6895\); default_zero(\$v6427\); 
          default_zero(\$12448\); default_zero(\$v6236\); 
          default_zero(\$v5874\); default_zero(\$v6330\); 
          default_zero(\$12265\); default_zero(\$v6399\); 
          default_zero(\$12287_w0597_arg\); 
          default_zero(\$14698_loop613_result\); default_zero(\$v5830\); 
          default_zero(\$v6204\); default_zero(\$13385_v\); 
          default_zero(\$v5726\); default_zero(\$v6471\); 
          default_zero(\$14440\); default_zero(\$v6306\); 
          default_zero(\$v6625\); default_zero(\$v6290\); 
          default_zero(\$v6495\); default_zero(\$v6407\); 
          default_zero(\$14070_v\); default_zero(\$v6751\); 
          default_zero(\$v5742\); default_zero(\$12165\); 
          default_zero(\$13796_v\); default_zero(\$v6165\); 
          default_zero(\$v6776\); default_zero(\$v6942\); 
          default_zero(\$v6678\); default_zero(\$v6738\); 
          default_zero(\$v6855\); default_zero(\$13444_v\); 
          default_zero(\$12292_w3599_result\); default_zero(\$v6034\); 
          default_zero(\$v6370\); default_zero(\$13370_v\); 
          default_zero(\$v6085\); 
          default_zero(\$14365_copy_root_in_ram610_result\); 
          default_zero(\$14112_modulo615_arg\); default_zero(\$v6362\); 
          default_zero(\$13057_v\); default_zero(\$v6721\); 
          default_zero(\$13807_v\); default_zero(\$v6016\); 
          default_zero(\$v5878\); default_zero(\$v5730\); 
          default_zero(\$13876_v\); default_zero(\$v5710\); 
          default_zero(\$v6183\); default_zero(\$v6129\); 
          default_zero(\$v6314\); default_zero(\$v6620\); 
          default_zero(\$v6890\); default_zero(\$v6350\); 
          default_zero(\$12705\); default_zero(\$12221_make_block531_id\); 
          default_zero(\$v6595\); default_zero(\$12942_v\); 
          default_zero(\$v6115\); default_zero(\$v5766\); 
          default_zero(\$13300_f0\); default_zero(\$12626_v\); 
          default_zero(\$v6160\); default_zero(\$v6012\); 
          default_zero(\$14128_r\); default_zero(\$12289_w1598_arg\); 
          default_zero(\$v6213\); default_zero(\$v6346\); 
          default_zero(\$v6078\); default_zero(\$14351\); 
          default_zero(\$12586_fill602_result\); default_zero(\$v6629\); 
          default_zero(\$13509\); default_zero(\$v6431\); 
          default_zero(\$v6073\); default_zero(\$14362\); 
          default_zero(\$v6192\); default_zero(\$v6146\); 
          default_zero(\$v6170\); default_zero(\$v6205\); 
          default_zero(\$v5662\); default_zero(\$v6043\); 
          default_zero(\$v5646\); default_zero(\$v6209\); 
          default_zero(\$v6124\); default_zero(\$v6519\); 
          default_zero(\$v5694\); default_zero(\$v5718\); 
          default_zero(\$v6792\); default_zero(\$12566_v\); 
          default_zero(\$14127_modulo615_result\); default_zero(\$13435_v\); 
          default_zero(\$14880_rdy\); default_zero(\$v5906\); 
          default_zero(\$v6047\); default_zero(\$v6859\); 
          default_zero(\$v6208\); default_zero(\$v6872\); 
          default_zero(\$v5758\); default_zero(\$v5686\); 
          default_zero(\$v6847\); 
          default_zero(\$12267_compbranch596_result\); 
          default_zero(\$v5882\); default_zero(\$v6390\); 
          default_zero(\$12260_compare591_arg\); default_zero(\$v6933\); 
          default_zero(\$v5814\); default_zero(rdy5985); 
          default_zero(\$v5938\); default_zero(\$v6318\); 
          default_zero(\$v6286\); default_zero(\$v6599\); 
          default_zero(\$13895_v\); default_zero(\$v6666\); 
          default_zero(\$v5690\); default_zero(\$12512_v\); 
          default_zero(\$v6716\); default_zero(\$14475_loop613_arg\); 
          default_zero(\$12262_make_block_n593_result\); 
          default_zero(\$v6475\); default_zero(\$v6747\); 
          default_zero(\$12263_branch_if595_arg\); default_zero(\$v6772\); 
          default_zero(\$v6089\); default_zero(\$12488\); 
          default_zero(\$v5898\); default_zero(\$14127_modulo615_arg\); 
          default_zero(\$14444\); default_zero(\$12975_ofs\); 
          default_zero(\$12656\); 
          default_zero(\$14365_copy_root_in_ram610_id\); 
          default_zero(\$v6559\); default_zero(\$12287_w0597_result\); 
          default_zero(\$v6394\); default_zero(\$14214_sp\); 
          default_zero(\$v6830\); default_zero(\$v6169\); 
          default_zero(\$v5682\); default_zero(\$v6378\); 
          default_zero(\$v6338\); default_zero(\$v6873\); 
          default_zero(\$12221_make_block531_arg\); 
          default_zero(\$12259_binop_int590_arg\); default_zero(\$v6198\); 
          default_zero(\$v6539\); default_zero(\$v6074\); 
          default_zero(\$12292_w3599_arg\); default_zero(\$v6780\); 
          default_zero(\$12267_compbranch596_arg\); default_zero(\$v5631\); 
          default_zero(\$14272\); default_zero(result5632); 
          default_zero(\$14211_sp\); default_zero(rdy5621); 
          default_zero(\$v6218\); default_zero(\$v5838\); 
          default_zero(\$14984\); default_zero(\$v6822\); 
          default_zero(\$v6250\); default_zero(\$14794_loop613_result\); 
          default_zero(\$v6051\); default_zero(\$v6924\); 
          default_zero(\$v6398\); default_zero(\$v6886\); 
          default_zero(\$v5934\); default_zero(\$13899_v\); 
          default_zero(\$12670\); default_zero(\$13683_v\); 
          default_zero(\$14368_next\); 
          default_zero(\$12260_compare591_result\); default_zero(\$14205\); 
          default_zero(\$12497_sp\); default_zero(\$12198\); 
          default_zero(\$v6133\); default_zero(\$14602_loop613_arg\); 
          default_zero(\$12272_argument3\); default_zero(\$13074\); 
          default_zero(\$14036_v\); default_zero(\$v6591\); 
          default_zero(\$12268_argument1\); default_zero(\$13747_v\); 
          default_zero(\$v6119\); default_zero(\$v5984\); 
          default_zero(\$14372_aux611_arg\); default_zero(\$v6784\); 
          default_zero(\$v5993\); default_zero(\$v6809\); 
          default_zero(\$14212_sp\); default_zero(\$13497\); 
          default_zero(\$13464\); default_zero(\$v6487\); 
          default_zero(\$13611\); default_zero(\$13417_v\); 
          default_zero(\$v6936\); default_zero(\$12293_sp\); 
          default_zero(\$v6227\); default_zero(\$v6515\); 
          default_zero(\$v6039\); default_zero(\$v6808\); 
          default_zero(\$12804_w603_arg\); default_zero(\$12578\); 
          default_zero(\$14213_sp\); default_zero(\$v5974\); 
          default_zero(\$v6003\); default_zero(\$v6579\); 
          default_zero(\$v6650\); default_zero(\$v6052\); 
          default_zero(result5997); default_zero(\$v5762\); 
          default_zero(\$v6298\); default_zero(\$v5702\); 
          default_zero(\$v5674\); default_zero(\$v5926\); 
          default_zero(\$v6110\); default_zero(\$v6179\); 
          default_zero(\$12776\); default_zero(\$v5826\); 
          default_zero(\$v6531\); default_zero(\$v6282\); 
          default_zero(\$12731_v\); default_zero(\$v6024\); 
          default_zero(\$14367\); default_zero(\$v6150\); 
          default_zero(\$v6207\); default_zero(\$12263_branch_if595_result\); 
          default_zero(\$14255_v\); default_zero(\$v6064\); 
          default_zero(\$14588_hd\); default_zero(rdy5998); 
          default_zero(\$v6633\); default_zero(\$12634_v\); 
          default_zero(\$14372_aux611_result\); default_zero(\$v6759\); 
          default_zero(\$v5706\); default_zero(\$12182\); 
          default_zero(\$v6235\); default_zero(\$v5625\); 
          default_zero(\$12810_v\); default_zero(\$v5658\); 
          default_zero(\$v6903\); default_zero(\$v6699\); 
          default_zero(\$13360_v\); default_zero(\$14732\); 
          default_zero(\$14112_modulo615_result\); default_zero(\$v5842\); 
          default_zero(\$13535\); default_zero(\$13380_v\); 
          default_zero(\$v6020\); default_zero(\$12343\); 
          default_zero(\$12842_v\); default_zero(\$14296_wait609_arg\); 
          default_zero(\$v5910\); default_zero(\$v6670\); 
          default_zero(\$14881\); default_zero(\$v6817\); 
          default_zero(\$v5958\); default_zero(\$v5902\); 
          default_zero(\$v5966\); default_zero(\$v6563\); 
          default_zero(\$v6154\); default_zero(\$v6386\); 
          default_zero(\$v6831\); default_zero(\$v6796\); 
          default_zero(\$v6463\); default_zero(\$v5850\); 
          default_zero(\$14780_hd\); default_zero(\$v6097\); 
          default_zero(\$14296_wait609_result\); 
          default_zero(\$12221_make_block531_result\); 
          default_zero(\$13569_next_env\); default_zero(\$v6403\); 
          default_zero(\$v5670\); default_zero(\$v6222\); 
          default_zero(rdy5628); default_zero(\$12792\); 
          default_zero(\$v5624\); default_zero(\$v5746\); 
          default_zero(\$v6511\); default_zero(\$v6658\); 
          default_zero(\$14202\); default_zero(\$v6098\); 
          default_zero(\$v6851\); default_zero(rdy5626); 
          default_zero(\$12275\); default_zero(\$v5994\); 
          default_zero(rdy5995); default_zero(\$14877\); 
          default_zero(\$v6746\); default_zero(\$v6839\); 
          default_zero(\$12736_v\); default_zero(\$v6527\); 
          default_zero(\$v6141\); default_zero(\$v6245\); 
          default_zero(\$v5802\); default_zero(\$v6918\); 
          default_zero(\$v6145\); default_zero(\$v6202\); 
          default_zero(rdy5988); default_zero(\$v6069\); 
          default_zero(\$v6877\); default_zero(\$13668_v\); 
          default_zero(\$v6621\); default_zero(\$v5834\); 
          default_zero(\$v6611\); default_zero(\$13900_v\); 
          default_zero(\$14409_next\); default_zero(\$14775_w\); 
          default_zero(\$v6637\); default_zero(\$12496_fill601_result\); 
          default_zero(\$14024_sp\); default_zero(\$v6860\); 
          default_zero(\$v6366\); default_zero(\$13522\); 
          default_zero(\$v6326\); default_zero(\$v6038\); 
          default_zero(\$v5654\); default_zero(\$v6455\); 
          default_zero(\$v6712\); default_zero(\$14108_res\); 
          default_zero(\$12261_binop_compare592_result\); 
          default_zero(\$13486\); default_zero(\$v6654\); 
          default_zero(\$v5854\); default_zero(\$v6310\); 
          default_zero(\$13806_v\); default_zero(\$14314\); 
          default_zero(\$v6060\); default_zero(\$12550_v\); 
          default_zero(\$14828\); default_zero(\$v5930\); 
          default_zero(\$v5786\); default_zero(\$v6717\); 
          default_zero(\$v6483\); default_zero(\$v6813\); 
          default_zero(\$12218\); default_zero(\$v6551\); 
          default_zero(\$v6274\); default_zero(\$v5886\); 
          default_zero(\$v5914\); default_zero(\$v5798\); 
          default_zero(\$v6641\); default_zero(\$v6266\); 
          default_zero(\$v6156\); default_zero(\$12804_w603_result\); 
          default_zero(\$v6708\); default_zero(\$12951_v\); 
          default_zero(\$v6322\); default_zero(\$v6029\); 
          default_zero(\$12258_offsetclosure_n586_arg\); 
          default_zero(\$v5774\); default_zero(\$12811_v\); 
          default_zero(\$12212\); default_zero(\$14025_v\); 
          default_zero(\$12976\); default_zero(\$v6187\); 
          default_zero(\$v6270\); default_zero(\$12477\); 
          default_zero(\$13836_next_acc\); 
          default_zero(\$14408_loop612_result\); default_zero(\$v5790\); 
          default_zero(\$14345\); default_zero(\$v6804\); 
          default_zero(\$12258_offsetclosure_n586_result\); 
          default_zero(\$14583_w\); default_zero(\$v6543\); 
          default_zero(\$12261_binop_compare592_arg\); 
          default_zero(\$13561_loop_push604_result\); default_zero(\$v6106\); 
          default_zero(result5618); default_zero(\$12270_argument2\); 
          default_zero(\$v6729\); default_zero(\$v6914\); 
          default_zero(\$v6226\); default_zero(\$v6686\); 
          default_zero(\$13408_v\); default_zero(\$v6835\); 
          default_zero(\$12171\); default_zero(\$14636\); 
          default_zero(\$12288_sp\); default_zero(\$v6612\); 
          default_zero(\$v5666\); default_zero(\$12546\); 
          default_zero(\$13453_v\); default_zero(\$14602_loop613_result\); 
          default_zero(\$v6646\); default_zero(\$13776_hd\); 
          default_zero(\$13560_hd\); default_zero(\$13350_v\); 
          default_zero(\$v5894\); default_zero(rdy6939); 
          default_zero(\$v6084\); default_zero(\$v6742\); 
          default_zero(\$v6603\); default_zero(\$v6929\); 
          default_zero(\$v5970\); default_zero(\$v5962\); 
          default_zero(\$v6258\); default_zero(\$12278_sp\); 
          default_zero(\$13214\); default_zero(\$13226_v\); 
          default_zero(\$v6188\); default_zero(\$v5862\); 
          default_zero(\$v6923\); default_zero(\$v6674\); 
          default_zero(\$v6507\); default_zero(\$12262_make_block_n593_arg\); 
          default_zero(\$v6374\); default_zero(\$v6278\); 
          default_zero(\$13846_v\); default_zero(\$v5698\); 
          default_zero(\$14509\); default_zero(\$v6687\); 
          default_zero(\$v6555\); default_zero(\$v6707\); 
          default_zero(\$13721_v\); default_zero(\$14475_loop613_result\); 
          default_zero(\$v5890\); default_zero(\$v6254\); 
          default_zero(\$v6885\); default_zero(\$13475\); 
          default_zero(\$12894_v\); default_zero(\$v5818\); 
          default_zero(\$v6241\); default_zero(\$13168_v\); 
          default_zero(\$v5650\); default_zero(\$14408_loop612_arg\); 
          default_zero(\$v5810\); default_zero(\$v6120\); 
          default_zero(\$v6302\); default_zero(\$v6093\); 
          default_zero(\$v6695\); default_zero(\$v6826\); 
          default_zero(\$12257_apply585_result\); default_zero(\$v5870\); 
          default_zero(\$v5738\); default_zero(\$v6583\); 
          default_zero(\$v6535\); default_zero(\$v5714\); 
          default_zero(\$v5858\); default_zero(\$14252_v\); 
          default_zero(\$v6523\); default_zero(\$v6800\); 
          default_zero(\$v6547\); default_zero(\$v6932\); 
          default_zero(rdy5619); default_zero(\$v5782\); 
          default_zero(\$12806_v\); default_zero(\$v6682\); 
          default_zero(\$14104_v\); default_zero(\$v6151\); 
          default_zero(\$14023_sp\); default_zero(\$v6342\); 
          default_zero(\$v6011\); default_zero(\$v6928\); 
          default_zero(\$13122_v\); default_zero(\$v5954\); 
          default_zero(\$v5822\); default_zero(\$v5642\); 
          default_zero(\$v6909\); default_zero(\$v6358\); 
          default_zero(\$v6411\); default_zero(\$14684_hd\); 
          default_zero(\$14208\); default_zero(\$v6908\); 
          default_zero(\$14074_res\); default_zero(\$v6028\); 
          default_zero(\$v6919\); default_zero(\$v6868\); 
          default_zero(\$v6203\); default_zero(\$v5866\); 
          default_zero(\$v5770\); default_zero(\$13086_v\); 
          default_zero(\$v5946\); default_zero(\$v5806\); 
          default_zero(\$v6843\); default_zero(\$14404\); 
          default_zero(\$v6435\); default_zero(\$14324\); 
          default_zero(\$v6354\); default_zero(\$v6294\); 
          default_zero(\$v6443\); default_zero(\$v6503\); 
          rdy <= "1";
          rdy5619 := "0";
          state <= compute5620;
          state_var6953 <= compute6940;
          state_var6952 <= compute5634;
          state_var6951 <= compute5629;
          state_var6950 <= compute5999;
          state_var6949 <= compute5996;
          state_var6948 <= compute5989;
          state_var6947 <= compute5986;
          state_var6946 <= compute5627;
          state_var6945 <= compute5622;
          state_var6944 <= compute5622;
          
        else if run = '1' then
          case state is
          when compute5620 =>
            rdy5619 := eclat_false;
            \$v6943\ := eclat_not(""&argument(10));
            if \$v6943\(0) = '1' then
              result5618 := ""&argument(11) & ""&argument(11) & ""&argument(11) & ""&argument(11) & ""&argument(11) & ""&argument(11) & ""&argument(11) & ""&argument(11) & ""&argument(11) & ""&argument(11) & "01100011" & "00000011" & "01110001" & "01110001" & "01100001" & "01100001";
              rdy5619 := eclat_true;
              state <= compute5620;
            else
              \$v6942\ := eclat_not(rdy6939);
              if \$v6942\(0) = '1' then
                \$14984\ := X"0000000" & X"0";
              end if;
              case state_var6953 is
              when compute6940 =>
                rdy6939 := eclat_false;
                \$14984\ := eclat_if(""&argument(11) & X"0000000" & X"0" & eclat_add(\$14984\ & X"0000000" & X"1"));
                rdy6939 := eclat_true;
                state_var6953 <= compute6940;
              end case;
              \$14984\ := \$14984\;
              \$12164_cy\ := \$14984\;
              \$v6938\ := eclat_not(rdy5626);
              if \$v6938\(0) = '1' then
                \$12190\ := eclat_false & eclat_false & eclat_false & eclat_false;
              end if;
              case state_var6946 is
              when compute5627 =>
                rdy5626 := eclat_false;
                \$v6936\ := eclat_not(""&\$12190\(2));
                if \$v6936\(0) = '1' then
                  case state_var6952 is
                  when pause_setI5635 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5636;
                  when pause_setI5639 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5640;
                  when pause_setI5643 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5644;
                  when pause_setI5647 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5648;
                  when pause_setI5651 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5652;
                  when pause_setI5655 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5656;
                  when pause_setI5659 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5660;
                  when pause_setI5663 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5664;
                  when pause_setI5667 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5668;
                  when pause_setI5671 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5672;
                  when pause_setI5675 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5676;
                  when pause_setI5679 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5680;
                  when pause_setI5683 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5684;
                  when pause_setI5687 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5688;
                  when pause_setI5691 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5692;
                  when pause_setI5695 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5696;
                  when pause_setI5699 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5700;
                  when pause_setI5703 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5704;
                  when pause_setI5707 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5708;
                  when pause_setI5711 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5712;
                  when pause_setI5715 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5716;
                  when pause_setI5719 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5720;
                  when pause_setI5723 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5724;
                  when pause_setI5727 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5728;
                  when pause_setI5731 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5732;
                  when pause_setI5735 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5736;
                  when pause_setI5739 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5740;
                  when pause_setI5743 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5744;
                  when pause_setI5747 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5748;
                  when pause_setI5751 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5752;
                  when pause_setI5755 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5756;
                  when pause_setI5759 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5760;
                  when pause_setI5763 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5764;
                  when pause_setI5767 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5768;
                  when pause_setI5771 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5772;
                  when pause_setI5775 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5776;
                  when pause_setI5779 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5780;
                  when pause_setI5783 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5784;
                  when pause_setI5787 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5788;
                  when pause_setI5791 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5792;
                  when pause_setI5795 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5796;
                  when pause_setI5799 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5800;
                  when pause_setI5803 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5804;
                  when pause_setI5807 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5808;
                  when pause_setI5811 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5812;
                  when pause_setI5815 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5816;
                  when pause_setI5819 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5820;
                  when pause_setI5823 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5824;
                  when pause_setI5827 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5828;
                  when pause_setI5831 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5832;
                  when pause_setI5835 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5836;
                  when pause_setI5839 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5840;
                  when pause_setI5843 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5844;
                  when pause_setI5847 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5848;
                  when pause_setI5851 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5852;
                  when pause_setI5855 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5856;
                  when pause_setI5859 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5860;
                  when pause_setI5863 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5864;
                  when pause_setI5867 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5868;
                  when pause_setI5871 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5872;
                  when pause_setI5875 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5876;
                  when pause_setI5879 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5880;
                  when pause_setI5883 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5884;
                  when pause_setI5887 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5888;
                  when pause_setI5891 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5892;
                  when pause_setI5895 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5896;
                  when pause_setI5899 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5900;
                  when pause_setI5903 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5904;
                  when pause_setI5907 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5908;
                  when pause_setI5911 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5912;
                  when pause_setI5915 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5916;
                  when pause_setI5919 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5920;
                  when pause_setI5923 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5924;
                  when pause_setI5927 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5928;
                  when pause_setI5931 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5932;
                  when pause_setI5935 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5936;
                  when pause_setI5939 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5940;
                  when pause_setI5943 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5944;
                  when pause_setI5947 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5948;
                  when pause_setI5951 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5952;
                  when pause_setI5955 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5956;
                  when pause_setI5959 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5960;
                  when pause_setI5963 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5964;
                  when pause_setI5967 =>
                    \$code_write_request\ <= '0';
                    state_var6952 <= pause_setII5968;
                  when pause_setI5971 =>
                    \$global_end_write_request\ <= '0';
                    state_var6952 <= pause_setII5972;
                  when pause_setI5975 =>
                    \$ram_write_request\ <= '0';
                    state_var6952 <= pause_setII5976;
                  when pause_setI5979 =>
                    \$ram_write_request\ <= '0';
                    state_var6952 <= pause_setII5980;
                  when pause_setII5636 =>
                    \$code_ptr_take\(0) := '0';
                    result5632 := eclat_unit;
                    rdy5633 := eclat_true;
                    state_var6952 <= compute5634;
                  when pause_setII5640 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5638\ := \$code_ptr_take\;
                    if \$v5638\(0) = '1' then
                      state_var6952 <= q_wait5637;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 83;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"8f";
                      state_var6952 <= pause_setI5635;
                    end if;
                  when pause_setII5644 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5642\ := \$code_ptr_take\;
                    if \$v5642\(0) = '1' then
                      state_var6952 <= q_wait5641;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 82;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"4";
                      state_var6952 <= pause_setI5639;
                    end if;
                  when pause_setII5648 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5646\ := \$code_ptr_take\;
                    if \$v5646\(0) = '1' then
                      state_var6952 <= q_wait5645;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 81;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"13";
                      state_var6952 <= pause_setI5643;
                    end if;
                  when pause_setII5652 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5650\ := \$code_ptr_take\;
                    if \$v5650\(0) = '1' then
                      state_var6952 <= q_wait5649;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 80;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"2";
                      state_var6952 <= pause_setI5647;
                    end if;
                  when pause_setII5656 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5654\ := \$code_ptr_take\;
                    if \$v5654\(0) = '1' then
                      state_var6952 <= q_wait5653;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 79;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"13";
                      state_var6952 <= pause_setI5651;
                    end if;
                  when pause_setII5660 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5658\ := \$code_ptr_take\;
                    if \$v5658\(0) = '1' then
                      state_var6952 <= q_wait5657;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 78;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"12");
                      state_var6952 <= pause_setI5655;
                    end if;
                  when pause_setII5664 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5662\ := \$code_ptr_take\;
                    if \$v5662\(0) = '1' then
                      state_var6952 <= q_wait5661;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 77;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"55";
                      state_var6952 <= pause_setI5659;
                    end if;
                  when pause_setII5668 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5666\ := \$code_ptr_take\;
                    if \$v5666\(0) = '1' then
                      state_var6952 <= q_wait5665;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 76;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7a";
                      state_var6952 <= pause_setI5663;
                    end if;
                  when pause_setII5672 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5670\ := \$code_ptr_take\;
                    if \$v5670\(0) = '1' then
                      state_var6952 <= q_wait5669;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 75;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5667;
                    end if;
                  when pause_setII5676 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5674\ := \$code_ptr_take\;
                    if \$v5674\(0) = '1' then
                      state_var6952 <= q_wait5673;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 74;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"2";
                      state_var6952 <= pause_setI5671;
                    end if;
                  when pause_setII5680 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5678\ := \$code_ptr_take\;
                    if \$v5678\(0) = '1' then
                      state_var6952 <= q_wait5677;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 73;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"14";
                      state_var6952 <= pause_setI5675;
                    end if;
                  when pause_setII5684 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5682\ := \$code_ptr_take\;
                    if \$v5682\(0) = '1' then
                      state_var6952 <= q_wait5681;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 72;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5679;
                    end if;
                  when pause_setII5688 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5686\ := \$code_ptr_take\;
                    if \$v5686\(0) = '1' then
                      state_var6952 <= q_wait5685;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 71;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7f";
                      state_var6952 <= pause_setI5683;
                    end if;
                  when pause_setII5692 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5690\ := \$code_ptr_take\;
                    if \$v5690\(0) = '1' then
                      state_var6952 <= q_wait5689;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 70;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5687;
                    end if;
                  when pause_setII5696 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5694\ := \$code_ptr_take\;
                    if \$v5694\(0) = '1' then
                      state_var6952 <= q_wait5693;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 69;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5691;
                    end if;
                  when pause_setII5700 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5698\ := \$code_ptr_take\;
                    if \$v5698\(0) = '1' then
                      state_var6952 <= q_wait5697;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 68;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5695;
                    end if;
                  when pause_setII5704 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5702\ := \$code_ptr_take\;
                    if \$v5702\(0) = '1' then
                      state_var6952 <= q_wait5701;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 67;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"5d";
                      state_var6952 <= pause_setI5699;
                    end if;
                  when pause_setII5708 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5706\ := \$code_ptr_take\;
                    if \$v5706\(0) = '1' then
                      state_var6952 <= q_wait5705;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 66;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"23";
                      state_var6952 <= pause_setI5703;
                    end if;
                  when pause_setII5712 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5710\ := \$code_ptr_take\;
                    if \$v5710\(0) = '1' then
                      state_var6952 <= q_wait5709;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 65;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"8";
                      state_var6952 <= pause_setI5707;
                    end if;
                  when pause_setII5716 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5714\ := \$code_ptr_take\;
                    if \$v5714\(0) = '1' then
                      state_var6952 <= q_wait5713;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 64;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"12";
                      state_var6952 <= pause_setI5711;
                    end if;
                  when pause_setII5720 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5718\ := \$code_ptr_take\;
                    if \$v5718\(0) = '1' then
                      state_var6952 <= q_wait5717;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 63;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"10";
                      state_var6952 <= pause_setI5715;
                    end if;
                  when pause_setII5724 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5722\ := \$code_ptr_take\;
                    if \$v5722\(0) = '1' then
                      state_var6952 <= q_wait5721;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 62;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"e";
                      state_var6952 <= pause_setI5719;
                    end if;
                  when pause_setII5728 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5726\ := \$code_ptr_take\;
                    if \$v5726\(0) = '1' then
                      state_var6952 <= q_wait5725;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 61;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"64";
                      state_var6952 <= pause_setI5723;
                    end if;
                  when pause_setII5732 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5730\ := \$code_ptr_take\;
                    if \$v5730\(0) = '1' then
                      state_var6952 <= q_wait5729;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 60;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"5c";
                      state_var6952 <= pause_setI5727;
                    end if;
                  when pause_setII5736 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5734\ := \$code_ptr_take\;
                    if \$v5734\(0) = '1' then
                      state_var6952 <= q_wait5733;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 59;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"14";
                      state_var6952 <= pause_setI5731;
                    end if;
                  when pause_setII5740 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5738\ := \$code_ptr_take\;
                    if \$v5738\(0) = '1' then
                      state_var6952 <= q_wait5737;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 58;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"55";
                      state_var6952 <= pause_setI5735;
                    end if;
                  when pause_setII5744 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5742\ := \$code_ptr_take\;
                    if \$v5742\(0) = '1' then
                      state_var6952 <= q_wait5741;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 57;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7d";
                      state_var6952 <= pause_setI5739;
                    end if;
                  when pause_setII5748 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5746\ := \$code_ptr_take\;
                    if \$v5746\(0) = '1' then
                      state_var6952 <= q_wait5745;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 56;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5743;
                    end if;
                  when pause_setII5752 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5750\ := \$code_ptr_take\;
                    if \$v5750\(0) = '1' then
                      state_var6952 <= q_wait5749;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 55;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5747;
                    end if;
                  when pause_setII5756 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5754\ := \$code_ptr_take\;
                    if \$v5754\(0) = '1' then
                      state_var6952 <= q_wait5753;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 54;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"b";
                      state_var6952 <= pause_setI5751;
                    end if;
                  when pause_setII5760 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5758\ := \$code_ptr_take\;
                    if \$v5758\(0) = '1' then
                      state_var6952 <= q_wait5757;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 53;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"69";
                      state_var6952 <= pause_setI5755;
                    end if;
                  when pause_setII5764 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5762\ := \$code_ptr_take\;
                    if \$v5762\(0) = '1' then
                      state_var6952 <= q_wait5761;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 52;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"69";
                      state_var6952 <= pause_setI5759;
                    end if;
                  when pause_setII5768 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5766\ := \$code_ptr_take\;
                    if \$v5766\(0) = '1' then
                      state_var6952 <= q_wait5765;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 51;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"31");
                      state_var6952 <= pause_setI5763;
                    end if;
                  when pause_setII5772 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5770\ := \$code_ptr_take\;
                    if \$v5770\(0) = '1' then
                      state_var6952 <= q_wait5769;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 50;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5767;
                    end if;
                  when pause_setII5776 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5774\ := \$code_ptr_take\;
                    if \$v5774\(0) = '1' then
                      state_var6952 <= q_wait5773;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 49;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5771;
                    end if;
                  when pause_setII5780 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5778\ := \$code_ptr_take\;
                    if \$v5778\(0) = '1' then
                      state_var6952 <= q_wait5777;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 48;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5775;
                    end if;
                  when pause_setII5784 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5782\ := \$code_ptr_take\;
                    if \$v5782\(0) = '1' then
                      state_var6952 <= q_wait5781;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 47;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"28");
                      state_var6952 <= pause_setI5779;
                    end if;
                  when pause_setII5788 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5786\ := \$code_ptr_take\;
                    if \$v5786\(0) = '1' then
                      state_var6952 <= q_wait5785;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 46;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5783;
                    end if;
                  when pause_setII5792 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5790\ := \$code_ptr_take\;
                    if \$v5790\(0) = '1' then
                      state_var6952 <= q_wait5789;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 45;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5787;
                    end if;
                  when pause_setII5796 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5794\ := \$code_ptr_take\;
                    if \$v5794\(0) = '1' then
                      state_var6952 <= q_wait5793;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 44;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5791;
                    end if;
                  when pause_setII5800 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5798\ := \$code_ptr_take\;
                    if \$v5798\(0) = '1' then
                      state_var6952 <= q_wait5797;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 43;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"39";
                      state_var6952 <= pause_setI5795;
                    end if;
                  when pause_setII5804 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5802\ := \$code_ptr_take\;
                    if \$v5802\(0) = '1' then
                      state_var6952 <= q_wait5801;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 42;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"1c");
                      state_var6952 <= pause_setI5799;
                    end if;
                  when pause_setII5808 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5806\ := \$code_ptr_take\;
                    if \$v5806\(0) = '1' then
                      state_var6952 <= q_wait5805;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 41;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5803;
                    end if;
                  when pause_setII5812 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5810\ := \$code_ptr_take\;
                    if \$v5810\(0) = '1' then
                      state_var6952 <= q_wait5809;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 40;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5807;
                    end if;
                  when pause_setII5816 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5814\ := \$code_ptr_take\;
                    if \$v5814\(0) = '1' then
                      state_var6952 <= q_wait5813;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 39;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5811;
                    end if;
                  when pause_setII5820 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5818\ := \$code_ptr_take\;
                    if \$v5818\(0) = '1' then
                      state_var6952 <= q_wait5817;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 38;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5815;
                    end if;
                  when pause_setII5824 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5822\ := \$code_ptr_take\;
                    if \$v5822\(0) = '1' then
                      state_var6952 <= q_wait5821;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 37;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"13";
                      state_var6952 <= pause_setI5819;
                    end if;
                  when pause_setII5828 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5826\ := \$code_ptr_take\;
                    if \$v5826\(0) = '1' then
                      state_var6952 <= q_wait5825;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 36;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5823;
                    end if;
                  when pause_setII5832 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5830\ := \$code_ptr_take\;
                    if \$v5830\(0) = '1' then
                      state_var6952 <= q_wait5829;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 35;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"d";
                      state_var6952 <= pause_setI5827;
                    end if;
                  when pause_setII5836 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5834\ := \$code_ptr_take\;
                    if \$v5834\(0) = '1' then
                      state_var6952 <= q_wait5833;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 34;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"39";
                      state_var6952 <= pause_setI5831;
                    end if;
                  when pause_setII5840 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5838\ := \$code_ptr_take\;
                    if \$v5838\(0) = '1' then
                      state_var6952 <= q_wait5837;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 33;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5835;
                    end if;
                  when pause_setII5844 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5842\ := \$code_ptr_take\;
                    if \$v5842\(0) = '1' then
                      state_var6952 <= q_wait5841;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 32;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"000000" & X"a");
                      state_var6952 <= pause_setI5839;
                    end if;
                  when pause_setII5848 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5846\ := \$code_ptr_take\;
                    if \$v5846\(0) = '1' then
                      state_var6952 <= q_wait5845;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 31;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5843;
                    end if;
                  when pause_setII5852 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5850\ := \$code_ptr_take\;
                    if \$v5850\(0) = '1' then
                      state_var6952 <= q_wait5849;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 30;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5847;
                    end if;
                  when pause_setII5856 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5854\ := \$code_ptr_take\;
                    if \$v5854\(0) = '1' then
                      state_var6952 <= q_wait5853;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 29;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"3";
                      state_var6952 <= pause_setI5851;
                    end if;
                  when pause_setII5860 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5858\ := \$code_ptr_take\;
                    if \$v5858\(0) = '1' then
                      state_var6952 <= q_wait5857;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 28;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"25";
                      state_var6952 <= pause_setI5855;
                    end if;
                  when pause_setII5864 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5862\ := \$code_ptr_take\;
                    if \$v5862\(0) = '1' then
                      state_var6952 <= q_wait5861;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 27;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"b";
                      state_var6952 <= pause_setI5859;
                    end if;
                  when pause_setII5868 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5866\ := \$code_ptr_take\;
                    if \$v5866\(0) = '1' then
                      state_var6952 <= q_wait5865;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 26;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"21";
                      state_var6952 <= pause_setI5863;
                    end if;
                  when pause_setII5872 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5870\ := \$code_ptr_take\;
                    if \$v5870\(0) = '1' then
                      state_var6952 <= q_wait5869;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 25;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"b";
                      state_var6952 <= pause_setI5867;
                    end if;
                  when pause_setII5876 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5874\ := \$code_ptr_take\;
                    if \$v5874\(0) = '1' then
                      state_var6952 <= q_wait5873;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 24;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5871;
                    end if;
                  when pause_setII5880 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5878\ := \$code_ptr_take\;
                    if \$v5878\(0) = '1' then
                      state_var6952 <= q_wait5877;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 23;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5875;
                    end if;
                  when pause_setII5884 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5882\ := \$code_ptr_take\;
                    if \$v5882\(0) = '1' then
                      state_var6952 <= q_wait5881;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 22;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2a";
                      state_var6952 <= pause_setI5879;
                    end if;
                  when pause_setII5888 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5886\ := \$code_ptr_take\;
                    if \$v5886\(0) = '1' then
                      state_var6952 <= q_wait5885;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 21;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"29";
                      state_var6952 <= pause_setI5883;
                    end if;
                  when pause_setII5892 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5890\ := \$code_ptr_take\;
                    if \$v5890\(0) = '1' then
                      state_var6952 <= q_wait5889;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 20;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"3";
                      state_var6952 <= pause_setI5887;
                    end if;
                  when pause_setII5896 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5894\ := \$code_ptr_take\;
                    if \$v5894\(0) = '1' then
                      state_var6952 <= q_wait5893;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 19;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"26";
                      state_var6952 <= pause_setI5891;
                    end if;
                  when pause_setII5900 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5898\ := \$code_ptr_take\;
                    if \$v5898\(0) = '1' then
                      state_var6952 <= q_wait5897;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 18;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"d";
                      state_var6952 <= pause_setI5895;
                    end if;
                  when pause_setII5904 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5902\ := \$code_ptr_take\;
                    if \$v5902\(0) = '1' then
                      state_var6952 <= q_wait5901;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 17;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5899;
                    end if;
                  when pause_setII5908 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5906\ := \$code_ptr_take\;
                    if \$v5906\(0) = '1' then
                      state_var6952 <= q_wait5905;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 16;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"d";
                      state_var6952 <= pause_setI5903;
                    end if;
                  when pause_setII5912 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5910\ := \$code_ptr_take\;
                    if \$v5910\(0) = '1' then
                      state_var6952 <= q_wait5909;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 15;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5907;
                    end if;
                  when pause_setII5916 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5914\ := \$code_ptr_take\;
                    if \$v5914\(0) = '1' then
                      state_var6952 <= q_wait5913;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 14;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5911;
                    end if;
                  when pause_setII5920 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5918\ := \$code_ptr_take\;
                    if \$v5918\(0) = '1' then
                      state_var6952 <= q_wait5917;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 13;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"3";
                      state_var6952 <= pause_setI5915;
                    end if;
                  when pause_setII5924 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5922\ := \$code_ptr_take\;
                    if \$v5922\(0) = '1' then
                      state_var6952 <= q_wait5921;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 12;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"26";
                      state_var6952 <= pause_setI5919;
                    end if;
                  when pause_setII5928 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5926\ := \$code_ptr_take\;
                    if \$v5926\(0) = '1' then
                      state_var6952 <= q_wait5925;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 11;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5923;
                    end if;
                  when pause_setII5932 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5930\ := \$code_ptr_take\;
                    if \$v5930\(0) = '1' then
                      state_var6952 <= q_wait5929;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 10;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5927;
                    end if;
                  when pause_setII5936 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5934\ := \$code_ptr_take\;
                    if \$v5934\(0) = '1' then
                      state_var6952 <= q_wait5933;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 9;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5931;
                    end if;
                  when pause_setII5940 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5938\ := \$code_ptr_take\;
                    if \$v5938\(0) = '1' then
                      state_var6952 <= q_wait5937;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 8;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5935;
                    end if;
                  when pause_setII5944 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5942\ := \$code_ptr_take\;
                    if \$v5942\(0) = '1' then
                      state_var6952 <= q_wait5941;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 7;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5939;
                    end if;
                  when pause_setII5948 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5946\ := \$code_ptr_take\;
                    if \$v5946\(0) = '1' then
                      state_var6952 <= q_wait5945;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 6;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5943;
                    end if;
                  when pause_setII5952 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5950\ := \$code_ptr_take\;
                    if \$v5950\(0) = '1' then
                      state_var6952 <= q_wait5949;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 5;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"28";
                      state_var6952 <= pause_setI5947;
                    end if;
                  when pause_setII5956 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5954\ := \$code_ptr_take\;
                    if \$v5954\(0) = '1' then
                      state_var6952 <= q_wait5953;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 4;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5951;
                    end if;
                  when pause_setII5960 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5958\ := \$code_ptr_take\;
                    if \$v5958\(0) = '1' then
                      state_var6952 <= q_wait5957;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 3;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7f";
                      state_var6952 <= pause_setI5955;
                    end if;
                  when pause_setII5964 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5962\ := \$code_ptr_take\;
                    if \$v5962\(0) = '1' then
                      state_var6952 <= q_wait5961;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 2;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5959;
                    end if;
                  when pause_setII5968 =>
                    \$code_ptr_take\(0) := '0';
                    \$v5966\ := \$code_ptr_take\;
                    if \$v5966\(0) = '1' then
                      state_var6952 <= q_wait5965;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 1;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"1d";
                      state_var6952 <= pause_setI5963;
                    end if;
                  when pause_setII5972 =>
                    \$global_end_ptr_take\(0) := '0';
                    \$v5970\ := \$code_ptr_take\;
                    if \$v5970\(0) = '1' then
                      state_var6952 <= q_wait5969;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 0;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"54";
                      state_var6952 <= pause_setI5967;
                    end if;
                  when pause_setII5976 =>
                    \$ram_ptr_take\(0) := '0';
                    \$v5974\ := \$global_end_ptr_take\;
                    if \$v5974\(0) = '1' then
                      state_var6952 <= q_wait5973;
                    else
                      \$global_end_ptr_take\(0) := '1';
                      \$global_end_ptr_write\ <= 0;
                      \$global_end_write_request\ <= '1';
                      \$global_end_write\ <= eclat_add(X"3e80" & X"000" & X"e");
                      state_var6952 <= pause_setI5971;
                    end if;
                  when pause_setII5980 =>
                    \$ram_ptr_take\(0) := '0';
                    \$v5978\ := \$ram_ptr_take\;
                    if \$v5978\(0) = '1' then
                      state_var6952 <= q_wait5977;
                    else
                      \$ram_ptr_take\(0) := '1';
                      \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(X"3e80" & X"000" & X"d")));
                      \$ram_write_request\ <= '1';
                      \$ram_write\ <= "000"& X"000000" & X"0" & eclat_true;
                      state_var6952 <= pause_setI5975;
                    end if;
                  when q_wait5637 =>
                    \$v5638\ := \$code_ptr_take\;
                    if \$v5638\(0) = '1' then
                      state_var6952 <= q_wait5637;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 83;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"8f";
                      state_var6952 <= pause_setI5635;
                    end if;
                  when q_wait5641 =>
                    \$v5642\ := \$code_ptr_take\;
                    if \$v5642\(0) = '1' then
                      state_var6952 <= q_wait5641;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 82;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"4";
                      state_var6952 <= pause_setI5639;
                    end if;
                  when q_wait5645 =>
                    \$v5646\ := \$code_ptr_take\;
                    if \$v5646\(0) = '1' then
                      state_var6952 <= q_wait5645;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 81;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"13";
                      state_var6952 <= pause_setI5643;
                    end if;
                  when q_wait5649 =>
                    \$v5650\ := \$code_ptr_take\;
                    if \$v5650\(0) = '1' then
                      state_var6952 <= q_wait5649;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 80;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"2";
                      state_var6952 <= pause_setI5647;
                    end if;
                  when q_wait5653 =>
                    \$v5654\ := \$code_ptr_take\;
                    if \$v5654\(0) = '1' then
                      state_var6952 <= q_wait5653;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 79;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"13";
                      state_var6952 <= pause_setI5651;
                    end if;
                  when q_wait5657 =>
                    \$v5658\ := \$code_ptr_take\;
                    if \$v5658\(0) = '1' then
                      state_var6952 <= q_wait5657;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 78;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"12");
                      state_var6952 <= pause_setI5655;
                    end if;
                  when q_wait5661 =>
                    \$v5662\ := \$code_ptr_take\;
                    if \$v5662\(0) = '1' then
                      state_var6952 <= q_wait5661;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 77;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"55";
                      state_var6952 <= pause_setI5659;
                    end if;
                  when q_wait5665 =>
                    \$v5666\ := \$code_ptr_take\;
                    if \$v5666\(0) = '1' then
                      state_var6952 <= q_wait5665;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 76;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7a";
                      state_var6952 <= pause_setI5663;
                    end if;
                  when q_wait5669 =>
                    \$v5670\ := \$code_ptr_take\;
                    if \$v5670\(0) = '1' then
                      state_var6952 <= q_wait5669;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 75;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5667;
                    end if;
                  when q_wait5673 =>
                    \$v5674\ := \$code_ptr_take\;
                    if \$v5674\(0) = '1' then
                      state_var6952 <= q_wait5673;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 74;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"2";
                      state_var6952 <= pause_setI5671;
                    end if;
                  when q_wait5677 =>
                    \$v5678\ := \$code_ptr_take\;
                    if \$v5678\(0) = '1' then
                      state_var6952 <= q_wait5677;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 73;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"14";
                      state_var6952 <= pause_setI5675;
                    end if;
                  when q_wait5681 =>
                    \$v5682\ := \$code_ptr_take\;
                    if \$v5682\(0) = '1' then
                      state_var6952 <= q_wait5681;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 72;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5679;
                    end if;
                  when q_wait5685 =>
                    \$v5686\ := \$code_ptr_take\;
                    if \$v5686\(0) = '1' then
                      state_var6952 <= q_wait5685;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 71;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7f";
                      state_var6952 <= pause_setI5683;
                    end if;
                  when q_wait5689 =>
                    \$v5690\ := \$code_ptr_take\;
                    if \$v5690\(0) = '1' then
                      state_var6952 <= q_wait5689;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 70;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5687;
                    end if;
                  when q_wait5693 =>
                    \$v5694\ := \$code_ptr_take\;
                    if \$v5694\(0) = '1' then
                      state_var6952 <= q_wait5693;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 69;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5691;
                    end if;
                  when q_wait5697 =>
                    \$v5698\ := \$code_ptr_take\;
                    if \$v5698\(0) = '1' then
                      state_var6952 <= q_wait5697;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 68;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5695;
                    end if;
                  when q_wait5701 =>
                    \$v5702\ := \$code_ptr_take\;
                    if \$v5702\(0) = '1' then
                      state_var6952 <= q_wait5701;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 67;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"5d";
                      state_var6952 <= pause_setI5699;
                    end if;
                  when q_wait5705 =>
                    \$v5706\ := \$code_ptr_take\;
                    if \$v5706\(0) = '1' then
                      state_var6952 <= q_wait5705;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 66;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"23";
                      state_var6952 <= pause_setI5703;
                    end if;
                  when q_wait5709 =>
                    \$v5710\ := \$code_ptr_take\;
                    if \$v5710\(0) = '1' then
                      state_var6952 <= q_wait5709;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 65;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"8";
                      state_var6952 <= pause_setI5707;
                    end if;
                  when q_wait5713 =>
                    \$v5714\ := \$code_ptr_take\;
                    if \$v5714\(0) = '1' then
                      state_var6952 <= q_wait5713;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 64;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"12";
                      state_var6952 <= pause_setI5711;
                    end if;
                  when q_wait5717 =>
                    \$v5718\ := \$code_ptr_take\;
                    if \$v5718\(0) = '1' then
                      state_var6952 <= q_wait5717;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 63;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"10";
                      state_var6952 <= pause_setI5715;
                    end if;
                  when q_wait5721 =>
                    \$v5722\ := \$code_ptr_take\;
                    if \$v5722\(0) = '1' then
                      state_var6952 <= q_wait5721;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 62;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"e";
                      state_var6952 <= pause_setI5719;
                    end if;
                  when q_wait5725 =>
                    \$v5726\ := \$code_ptr_take\;
                    if \$v5726\(0) = '1' then
                      state_var6952 <= q_wait5725;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 61;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"64";
                      state_var6952 <= pause_setI5723;
                    end if;
                  when q_wait5729 =>
                    \$v5730\ := \$code_ptr_take\;
                    if \$v5730\(0) = '1' then
                      state_var6952 <= q_wait5729;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 60;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"5c";
                      state_var6952 <= pause_setI5727;
                    end if;
                  when q_wait5733 =>
                    \$v5734\ := \$code_ptr_take\;
                    if \$v5734\(0) = '1' then
                      state_var6952 <= q_wait5733;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 59;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"14";
                      state_var6952 <= pause_setI5731;
                    end if;
                  when q_wait5737 =>
                    \$v5738\ := \$code_ptr_take\;
                    if \$v5738\(0) = '1' then
                      state_var6952 <= q_wait5737;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 58;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"55";
                      state_var6952 <= pause_setI5735;
                    end if;
                  when q_wait5741 =>
                    \$v5742\ := \$code_ptr_take\;
                    if \$v5742\(0) = '1' then
                      state_var6952 <= q_wait5741;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 57;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7d";
                      state_var6952 <= pause_setI5739;
                    end if;
                  when q_wait5745 =>
                    \$v5746\ := \$code_ptr_take\;
                    if \$v5746\(0) = '1' then
                      state_var6952 <= q_wait5745;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 56;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5743;
                    end if;
                  when q_wait5749 =>
                    \$v5750\ := \$code_ptr_take\;
                    if \$v5750\(0) = '1' then
                      state_var6952 <= q_wait5749;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 55;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5747;
                    end if;
                  when q_wait5753 =>
                    \$v5754\ := \$code_ptr_take\;
                    if \$v5754\(0) = '1' then
                      state_var6952 <= q_wait5753;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 54;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"b";
                      state_var6952 <= pause_setI5751;
                    end if;
                  when q_wait5757 =>
                    \$v5758\ := \$code_ptr_take\;
                    if \$v5758\(0) = '1' then
                      state_var6952 <= q_wait5757;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 53;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"69";
                      state_var6952 <= pause_setI5755;
                    end if;
                  when q_wait5761 =>
                    \$v5762\ := \$code_ptr_take\;
                    if \$v5762\(0) = '1' then
                      state_var6952 <= q_wait5761;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 52;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"69";
                      state_var6952 <= pause_setI5759;
                    end if;
                  when q_wait5765 =>
                    \$v5766\ := \$code_ptr_take\;
                    if \$v5766\(0) = '1' then
                      state_var6952 <= q_wait5765;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 51;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"31");
                      state_var6952 <= pause_setI5763;
                    end if;
                  when q_wait5769 =>
                    \$v5770\ := \$code_ptr_take\;
                    if \$v5770\(0) = '1' then
                      state_var6952 <= q_wait5769;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 50;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5767;
                    end if;
                  when q_wait5773 =>
                    \$v5774\ := \$code_ptr_take\;
                    if \$v5774\(0) = '1' then
                      state_var6952 <= q_wait5773;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 49;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5771;
                    end if;
                  when q_wait5777 =>
                    \$v5778\ := \$code_ptr_take\;
                    if \$v5778\(0) = '1' then
                      state_var6952 <= q_wait5777;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 48;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5775;
                    end if;
                  when q_wait5781 =>
                    \$v5782\ := \$code_ptr_take\;
                    if \$v5782\(0) = '1' then
                      state_var6952 <= q_wait5781;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 47;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"28");
                      state_var6952 <= pause_setI5779;
                    end if;
                  when q_wait5785 =>
                    \$v5786\ := \$code_ptr_take\;
                    if \$v5786\(0) = '1' then
                      state_var6952 <= q_wait5785;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 46;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5783;
                    end if;
                  when q_wait5789 =>
                    \$v5790\ := \$code_ptr_take\;
                    if \$v5790\(0) = '1' then
                      state_var6952 <= q_wait5789;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 45;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5787;
                    end if;
                  when q_wait5793 =>
                    \$v5794\ := \$code_ptr_take\;
                    if \$v5794\(0) = '1' then
                      state_var6952 <= q_wait5793;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 44;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5791;
                    end if;
                  when q_wait5797 =>
                    \$v5798\ := \$code_ptr_take\;
                    if \$v5798\(0) = '1' then
                      state_var6952 <= q_wait5797;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 43;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"39";
                      state_var6952 <= pause_setI5795;
                    end if;
                  when q_wait5801 =>
                    \$v5802\ := \$code_ptr_take\;
                    if \$v5802\(0) = '1' then
                      state_var6952 <= q_wait5801;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 42;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"00000" & X"1c");
                      state_var6952 <= pause_setI5799;
                    end if;
                  when q_wait5805 =>
                    \$v5806\ := \$code_ptr_take\;
                    if \$v5806\(0) = '1' then
                      state_var6952 <= q_wait5805;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 41;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5803;
                    end if;
                  when q_wait5809 =>
                    \$v5810\ := \$code_ptr_take\;
                    if \$v5810\(0) = '1' then
                      state_var6952 <= q_wait5809;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 40;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5807;
                    end if;
                  when q_wait5813 =>
                    \$v5814\ := \$code_ptr_take\;
                    if \$v5814\(0) = '1' then
                      state_var6952 <= q_wait5813;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 39;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5811;
                    end if;
                  when q_wait5817 =>
                    \$v5818\ := \$code_ptr_take\;
                    if \$v5818\(0) = '1' then
                      state_var6952 <= q_wait5817;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 38;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5815;
                    end if;
                  when q_wait5821 =>
                    \$v5822\ := \$code_ptr_take\;
                    if \$v5822\(0) = '1' then
                      state_var6952 <= q_wait5821;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 37;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"13";
                      state_var6952 <= pause_setI5819;
                    end if;
                  when q_wait5825 =>
                    \$v5826\ := \$code_ptr_take\;
                    if \$v5826\(0) = '1' then
                      state_var6952 <= q_wait5825;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 36;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5823;
                    end if;
                  when q_wait5829 =>
                    \$v5830\ := \$code_ptr_take\;
                    if \$v5830\(0) = '1' then
                      state_var6952 <= q_wait5829;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 35;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"d";
                      state_var6952 <= pause_setI5827;
                    end if;
                  when q_wait5833 =>
                    \$v5834\ := \$code_ptr_take\;
                    if \$v5834\(0) = '1' then
                      state_var6952 <= q_wait5833;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 34;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"39";
                      state_var6952 <= pause_setI5831;
                    end if;
                  when q_wait5837 =>
                    \$v5838\ := \$code_ptr_take\;
                    if \$v5838\(0) = '1' then
                      state_var6952 <= q_wait5837;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 33;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"9";
                      state_var6952 <= pause_setI5835;
                    end if;
                  when q_wait5841 =>
                    \$v5842\ := \$code_ptr_take\;
                    if \$v5842\(0) = '1' then
                      state_var6952 <= q_wait5841;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 32;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= eclat_sub("000"& X"000000" & X"0" & "000"& X"000000" & X"a");
                      state_var6952 <= pause_setI5839;
                    end if;
                  when q_wait5845 =>
                    \$v5846\ := \$code_ptr_take\;
                    if \$v5846\(0) = '1' then
                      state_var6952 <= q_wait5845;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 31;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5843;
                    end if;
                  when q_wait5849 =>
                    \$v5850\ := \$code_ptr_take\;
                    if \$v5850\(0) = '1' then
                      state_var6952 <= q_wait5849;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 30;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2b";
                      state_var6952 <= pause_setI5847;
                    end if;
                  when q_wait5853 =>
                    \$v5854\ := \$code_ptr_take\;
                    if \$v5854\(0) = '1' then
                      state_var6952 <= q_wait5853;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 29;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"3";
                      state_var6952 <= pause_setI5851;
                    end if;
                  when q_wait5857 =>
                    \$v5858\ := \$code_ptr_take\;
                    if \$v5858\(0) = '1' then
                      state_var6952 <= q_wait5857;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 28;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"25";
                      state_var6952 <= pause_setI5855;
                    end if;
                  when q_wait5861 =>
                    \$v5862\ := \$code_ptr_take\;
                    if \$v5862\(0) = '1' then
                      state_var6952 <= q_wait5861;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 27;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"b";
                      state_var6952 <= pause_setI5859;
                    end if;
                  when q_wait5865 =>
                    \$v5866\ := \$code_ptr_take\;
                    if \$v5866\(0) = '1' then
                      state_var6952 <= q_wait5865;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 26;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"21";
                      state_var6952 <= pause_setI5863;
                    end if;
                  when q_wait5869 =>
                    \$v5870\ := \$code_ptr_take\;
                    if \$v5870\(0) = '1' then
                      state_var6952 <= q_wait5869;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 25;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"b";
                      state_var6952 <= pause_setI5867;
                    end if;
                  when q_wait5873 =>
                    \$v5874\ := \$code_ptr_take\;
                    if \$v5874\(0) = '1' then
                      state_var6952 <= q_wait5873;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 24;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5871;
                    end if;
                  when q_wait5877 =>
                    \$v5878\ := \$code_ptr_take\;
                    if \$v5878\(0) = '1' then
                      state_var6952 <= q_wait5877;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 23;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5875;
                    end if;
                  when q_wait5881 =>
                    \$v5882\ := \$code_ptr_take\;
                    if \$v5882\(0) = '1' then
                      state_var6952 <= q_wait5881;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 22;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"2a";
                      state_var6952 <= pause_setI5879;
                    end if;
                  when q_wait5885 =>
                    \$v5886\ := \$code_ptr_take\;
                    if \$v5886\(0) = '1' then
                      state_var6952 <= q_wait5885;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 21;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"29";
                      state_var6952 <= pause_setI5883;
                    end if;
                  when q_wait5889 =>
                    \$v5890\ := \$code_ptr_take\;
                    if \$v5890\(0) = '1' then
                      state_var6952 <= q_wait5889;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 20;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"3";
                      state_var6952 <= pause_setI5887;
                    end if;
                  when q_wait5893 =>
                    \$v5894\ := \$code_ptr_take\;
                    if \$v5894\(0) = '1' then
                      state_var6952 <= q_wait5893;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 19;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"26";
                      state_var6952 <= pause_setI5891;
                    end if;
                  when q_wait5897 =>
                    \$v5898\ := \$code_ptr_take\;
                    if \$v5898\(0) = '1' then
                      state_var6952 <= q_wait5897;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 18;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"d";
                      state_var6952 <= pause_setI5895;
                    end if;
                  when q_wait5901 =>
                    \$v5902\ := \$code_ptr_take\;
                    if \$v5902\(0) = '1' then
                      state_var6952 <= q_wait5901;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 17;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5899;
                    end if;
                  when q_wait5905 =>
                    \$v5906\ := \$code_ptr_take\;
                    if \$v5906\(0) = '1' then
                      state_var6952 <= q_wait5905;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 16;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"d";
                      state_var6952 <= pause_setI5903;
                    end if;
                  when q_wait5909 =>
                    \$v5910\ := \$code_ptr_take\;
                    if \$v5910\(0) = '1' then
                      state_var6952 <= q_wait5909;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 15;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5907;
                    end if;
                  when q_wait5913 =>
                    \$v5914\ := \$code_ptr_take\;
                    if \$v5914\(0) = '1' then
                      state_var6952 <= q_wait5913;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 14;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5911;
                    end if;
                  when q_wait5917 =>
                    \$v5918\ := \$code_ptr_take\;
                    if \$v5918\(0) = '1' then
                      state_var6952 <= q_wait5917;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 13;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"3";
                      state_var6952 <= pause_setI5915;
                    end if;
                  when q_wait5921 =>
                    \$v5922\ := \$code_ptr_take\;
                    if \$v5922\(0) = '1' then
                      state_var6952 <= q_wait5921;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 12;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"26";
                      state_var6952 <= pause_setI5919;
                    end if;
                  when q_wait5925 =>
                    \$v5926\ := \$code_ptr_take\;
                    if \$v5926\(0) = '1' then
                      state_var6952 <= q_wait5925;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 11;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5923;
                    end if;
                  when q_wait5929 =>
                    \$v5930\ := \$code_ptr_take\;
                    if \$v5930\(0) = '1' then
                      state_var6952 <= q_wait5929;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 10;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5927;
                    end if;
                  when q_wait5933 =>
                    \$v5934\ := \$code_ptr_take\;
                    if \$v5934\(0) = '1' then
                      state_var6952 <= q_wait5933;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 9;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"c";
                      state_var6952 <= pause_setI5931;
                    end if;
                  when q_wait5937 =>
                    \$v5938\ := \$code_ptr_take\;
                    if \$v5938\(0) = '1' then
                      state_var6952 <= q_wait5937;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 8;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"36";
                      state_var6952 <= pause_setI5935;
                    end if;
                  when q_wait5941 =>
                    \$v5942\ := \$code_ptr_take\;
                    if \$v5942\(0) = '1' then
                      state_var6952 <= q_wait5941;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 7;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5939;
                    end if;
                  when q_wait5945 =>
                    \$v5946\ := \$code_ptr_take\;
                    if \$v5946\(0) = '1' then
                      state_var6952 <= q_wait5945;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 6;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5943;
                    end if;
                  when q_wait5949 =>
                    \$v5950\ := \$code_ptr_take\;
                    if \$v5950\(0) = '1' then
                      state_var6952 <= q_wait5949;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 5;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"28";
                      state_var6952 <= pause_setI5947;
                    end if;
                  when q_wait5953 =>
                    \$v5954\ := \$code_ptr_take\;
                    if \$v5954\(0) = '1' then
                      state_var6952 <= q_wait5953;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 4;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"1";
                      state_var6952 <= pause_setI5951;
                    end if;
                  when q_wait5957 =>
                    \$v5958\ := \$code_ptr_take\;
                    if \$v5958\(0) = '1' then
                      state_var6952 <= q_wait5957;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 3;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"7f";
                      state_var6952 <= pause_setI5955;
                    end if;
                  when q_wait5961 =>
                    \$v5962\ := \$code_ptr_take\;
                    if \$v5962\(0) = '1' then
                      state_var6952 <= q_wait5961;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 2;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"000000" & X"0";
                      state_var6952 <= pause_setI5959;
                    end if;
                  when q_wait5965 =>
                    \$v5966\ := \$code_ptr_take\;
                    if \$v5966\(0) = '1' then
                      state_var6952 <= q_wait5965;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 1;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"1d";
                      state_var6952 <= pause_setI5963;
                    end if;
                  when q_wait5969 =>
                    \$v5970\ := \$code_ptr_take\;
                    if \$v5970\(0) = '1' then
                      state_var6952 <= q_wait5969;
                    else
                      \$code_ptr_take\(0) := '1';
                      \$code_ptr_write\ <= 0;
                      \$code_write_request\ <= '1';
                      \$code_write\ <= "000"& X"00000" & X"54";
                      state_var6952 <= pause_setI5967;
                    end if;
                  when q_wait5973 =>
                    \$v5974\ := \$global_end_ptr_take\;
                    if \$v5974\(0) = '1' then
                      state_var6952 <= q_wait5973;
                    else
                      \$global_end_ptr_take\(0) := '1';
                      \$global_end_ptr_write\ <= 0;
                      \$global_end_write_request\ <= '1';
                      \$global_end_write\ <= eclat_add(X"3e80" & X"000" & X"e");
                      state_var6952 <= pause_setI5971;
                    end if;
                  when q_wait5977 =>
                    \$v5978\ := \$ram_ptr_take\;
                    if \$v5978\(0) = '1' then
                      state_var6952 <= q_wait5977;
                    else
                      \$ram_ptr_take\(0) := '1';
                      \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(X"3e80" & X"000" & X"d")));
                      \$ram_write_request\ <= '1';
                      \$ram_write\ <= "000"& X"000000" & X"0" & eclat_true;
                      state_var6952 <= pause_setI5975;
                    end if;
                  when q_wait5981 =>
                    \$v5982\ := \$ram_ptr_take\;
                    if \$v5982\(0) = '1' then
                      state_var6952 <= q_wait5981;
                    else
                      \$ram_ptr_take\(0) := '1';
                      \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(X"3e80" & X"000" & X"c")));
                      \$ram_write_request\ <= '1';
                      \$ram_write\ <= "000"& X"000000" & X"0" & eclat_true;
                      state_var6952 <= pause_setI5979;
                    end if;
                  when compute5634 =>
                    rdy5633 := eclat_false;
                    \$v5982\ := \$ram_ptr_take\;
                    if \$v5982\(0) = '1' then
                      state_var6952 <= q_wait5981;
                    else
                      \$ram_ptr_take\(0) := '1';
                      \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(X"3e80" & X"000" & X"c")));
                      \$ram_write_request\ <= '1';
                      \$ram_write\ <= "000"& X"000000" & X"0" & eclat_true;
                      state_var6952 <= pause_setI5979;
                    end if;
                  end case;
                  \$v5984\ := eclat_not(rdy5633);
                  if \$v5984\(0) = '1' then
                    result5632 := eclat_unit;
                  end if;
                  \$14877\ := result5632 & rdy5633;
                  \$v5631\ := eclat_not(rdy5628);
                  if \$v5631\(0) = '1' then
                    \$14881\ := eclat_false;
                  end if;
                  case state_var6951 is
                  when compute5629 =>
                    rdy5628 := eclat_false;
                    \$14881\ := eclat_and(eclat_if(\$14881\ & eclat_true & ""&\$14877\(1)) & eclat_not(eclat_false));
                    rdy5628 := eclat_true;
                    state_var6951 <= compute5629;
                  end case;
                  \$14881\ := \$14881\;
                  \$14880_rdy\ := \$14881\;
                  \$12190\ := eclat_false & eclat_true & \$14880_rdy\ & ""&\$12190\(3);
                  rdy5626 := eclat_true;
                  state_var6946 <= compute5627;
                else
                  \$v6935\ := eclat_not(rdy5985);
                  if \$v6935\(0) = '1' then
                    \$12212\ := X"000" & X"0" & "000"& X"000000" & X"1" & eclat_true & X"0" & X"3e8" & "000"& X"000000" & X"1" & eclat_true & "00000000" & X"000" & X"0" & eclat_false & eclat_false & eclat_true;
                  end if;
                  case state_var6947 is
                  when compute5986 =>
                    rdy5985 := eclat_false;
                    \$v6933\ := eclat_not(""&\$12190\(2));
                    if \$v6933\(0) = '1' then
                      \$12212\ := \$12212\(0 to 121) & eclat_true;
                      rdy5985 := eclat_true;
                      state_var6947 <= compute5986;
                    else
                      case state_var6948 is
                      when \$12221_make_block531\ =>
                        eclat_print_string(of_string("GC-ALLOC:(size="));
                        
                        eclat_print_int(eclat_add(eclat_if(eclat_eq(\$12221_make_block531_arg\(88 to 103) & X"000" & X"0") & X"000" & X"1" & \$12221_make_block531_arg\(88 to 103)) & X"000" & X"1"));
                        
                        eclat_print_string(of_string(")"));
                        
                        eclat_print_newline(eclat_unit);
                        
                        \$14296_wait609_arg\ := eclat_unit & \$12221_make_block531_arg\(16 to 47) & \$12221_make_block531_arg\(48 to 79) & \$12221_make_block531_arg\(0 to 15) & eclat_add(
                        eclat_if(eclat_eq(\$12221_make_block531_arg\(88 to 103) & X"000" & X"0") & X"000" & X"1" & \$12221_make_block531_arg\(88 to 103)) & X"000" & X"1");
                        state_var6948 <= \$14296_wait609\;
                      when \$12257_apply585\ =>
                        eclat_print_string(of_string("ENV:"));
                        
                        eclat_print_int(\$12257_apply585_arg\(110 to 140));
                        
                        eclat_print_string(of_string("<"));
                        
                        \$v6204\ := ""&\$12257_apply585_arg\(141);
                        if \$v6204\(0) = '1' then
                          eclat_print_string(of_string("int"));
                          
                          eclat_print_string(of_string(">"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$v6203\ := ""&\$12257_apply585_arg\(0);
                          if \$v6203\(0) = '1' then
                            \$v6202\ := \$ram_ptr_take\;
                            if \$v6202\(0) = '1' then
                              state_var6948 <= q_wait6201;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12257_apply585_arg\(92 to 107) & X"000" & X"1")));
                              state_var6948 <= pause_getI6199;
                            end if;
                          else
                            \$14202\ := "000"& X"000000" & X"1" & eclat_true & \$12257_apply585_arg\(92 to 107);
                            \$v6198\ := ""&\$12257_apply585_arg\(1);
                            if \$v6198\(0) = '1' then
                              \$v6197\ := \$ram_ptr_take\;
                              if \$v6197\(0) = '1' then
                                state_var6948 <= q_wait6196;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14202\(32 to 47) & X"000" & X"1")));
                                state_var6948 <= pause_getI6194;
                              end if;
                            else
                              \$14205\ := "000"& X"000000" & X"1" & eclat_true & \$14202\(32 to 47);
                              \$v6193\ := ""&\$12257_apply585_arg\(2);
                              if \$v6193\(0) = '1' then
                                \$v6192\ := \$ram_ptr_take\;
                                if \$v6192\(0) = '1' then
                                  state_var6948 <= q_wait6191;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14205\(32 to 47) & X"000" & X"1")));
                                  state_var6948 <= pause_getI6189;
                                end if;
                              else
                                \$14208\ := "000"& X"000000" & X"1" & eclat_true & \$14205\(32 to 47);
                                \$v6188\ := ""&\$12257_apply585_arg\(11);
                                if \$v6188\(0) = '1' then
                                  \$14211_sp\ := eclat_add(eclat_sub(\$14208\(32 to 47) & \$12257_apply585_arg\(12 to 27)) & \$12257_apply585_arg\(28 to 43));
                                  \$v6175\ := ""&\$12257_apply585_arg\(2);
                                  if \$v6175\(0) = '1' then
                                    \$v6174\ := \$ram_ptr_take\;
                                    if \$v6174\(0) = '1' then
                                      state_var6948 <= q_wait6173;
                                    else
                                      \$ram_ptr_take\(0) := '1';
                                      \$ram_ptr_write\ <= to_integer(unsigned(\$14211_sp\));
                                      \$ram_write_request\ <= '1';
                                      \$ram_write\ <= \$14208\(0 to 31);
                                      state_var6948 <= pause_setI6171;
                                    end if;
                                  else
                                    \$14212_sp\ := \$14211_sp\;
                                    \$v6170\ := ""&\$12257_apply585_arg\(1);
                                    if \$v6170\(0) = '1' then
                                      \$v6169\ := \$ram_ptr_take\;
                                      if \$v6169\(0) = '1' then
                                        state_var6948 <= q_wait6168;
                                      else
                                        \$ram_ptr_take\(0) := '1';
                                        \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                                        \$ram_write_request\ <= '1';
                                        \$ram_write\ <= \$14205\(0 to 31);
                                        state_var6948 <= pause_setI6166;
                                      end if;
                                    else
                                      \$14213_sp\ := \$14212_sp\;
                                      \$v6165\ := ""&\$12257_apply585_arg\(0);
                                      if \$v6165\(0) = '1' then
                                        \$v6164\ := \$ram_ptr_take\;
                                        if \$v6164\(0) = '1' then
                                          state_var6948 <= q_wait6163;
                                        else
                                          \$ram_ptr_take\(0) := '1';
                                          \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                                          \$ram_write_request\ <= '1';
                                          \$ram_write\ <= \$14202\(0 to 31);
                                          state_var6948 <= pause_setI6161;
                                        end if;
                                      else
                                        \$14214_sp\ := \$14213_sp\;
                                        \$v6160\ := \$ram_ptr_take\;
                                        if \$v6160\(0) = '1' then
                                          state_var6948 <= q_wait6159;
                                        else
                                          \$ram_ptr_take\(0) := '1';
                                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                                          state_var6948 <= pause_getI6157;
                                        end if;
                                      end if;
                                    end if;
                                  end if;
                                else
                                  \$v6187\ := \$ram_ptr_take\;
                                  if \$v6187\(0) = '1' then
                                    state_var6948 <= q_wait6186;
                                  else
                                    \$ram_ptr_take\(0) := '1';
                                    \$ram_ptr_write\ <= to_integer(unsigned(\$14208\(32 to 47)));
                                    \$ram_write_request\ <= '1';
                                    \$ram_write\ <= eclat_resize(\$12257_apply585_arg\(142 to 149),31) & eclat_true;
                                    state_var6948 <= pause_setI6184;
                                  end if;
                                end if;
                              end if;
                            end if;
                          end if;
                        else
                          eclat_print_string(of_string("ptr"));
                          
                          eclat_print_string(of_string(">"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$v6203\ := ""&\$12257_apply585_arg\(0);
                          if \$v6203\(0) = '1' then
                            \$v6202\ := \$ram_ptr_take\;
                            if \$v6202\(0) = '1' then
                              state_var6948 <= q_wait6201;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12257_apply585_arg\(92 to 107) & X"000" & X"1")));
                              state_var6948 <= pause_getI6199;
                            end if;
                          else
                            \$14202\ := "000"& X"000000" & X"1" & eclat_true & \$12257_apply585_arg\(92 to 107);
                            \$v6198\ := ""&\$12257_apply585_arg\(1);
                            if \$v6198\(0) = '1' then
                              \$v6197\ := \$ram_ptr_take\;
                              if \$v6197\(0) = '1' then
                                state_var6948 <= q_wait6196;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14202\(32 to 47) & X"000" & X"1")));
                                state_var6948 <= pause_getI6194;
                              end if;
                            else
                              \$14205\ := "000"& X"000000" & X"1" & eclat_true & \$14202\(32 to 47);
                              \$v6193\ := ""&\$12257_apply585_arg\(2);
                              if \$v6193\(0) = '1' then
                                \$v6192\ := \$ram_ptr_take\;
                                if \$v6192\(0) = '1' then
                                  state_var6948 <= q_wait6191;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14205\(32 to 47) & X"000" & X"1")));
                                  state_var6948 <= pause_getI6189;
                                end if;
                              else
                                \$14208\ := "000"& X"000000" & X"1" & eclat_true & \$14205\(32 to 47);
                                \$v6188\ := ""&\$12257_apply585_arg\(11);
                                if \$v6188\(0) = '1' then
                                  \$14211_sp\ := eclat_add(eclat_sub(\$14208\(32 to 47) & \$12257_apply585_arg\(12 to 27)) & \$12257_apply585_arg\(28 to 43));
                                  \$v6175\ := ""&\$12257_apply585_arg\(2);
                                  if \$v6175\(0) = '1' then
                                    \$v6174\ := \$ram_ptr_take\;
                                    if \$v6174\(0) = '1' then
                                      state_var6948 <= q_wait6173;
                                    else
                                      \$ram_ptr_take\(0) := '1';
                                      \$ram_ptr_write\ <= to_integer(unsigned(\$14211_sp\));
                                      \$ram_write_request\ <= '1';
                                      \$ram_write\ <= \$14208\(0 to 31);
                                      state_var6948 <= pause_setI6171;
                                    end if;
                                  else
                                    \$14212_sp\ := \$14211_sp\;
                                    \$v6170\ := ""&\$12257_apply585_arg\(1);
                                    if \$v6170\(0) = '1' then
                                      \$v6169\ := \$ram_ptr_take\;
                                      if \$v6169\(0) = '1' then
                                        state_var6948 <= q_wait6168;
                                      else
                                        \$ram_ptr_take\(0) := '1';
                                        \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                                        \$ram_write_request\ <= '1';
                                        \$ram_write\ <= \$14205\(0 to 31);
                                        state_var6948 <= pause_setI6166;
                                      end if;
                                    else
                                      \$14213_sp\ := \$14212_sp\;
                                      \$v6165\ := ""&\$12257_apply585_arg\(0);
                                      if \$v6165\(0) = '1' then
                                        \$v6164\ := \$ram_ptr_take\;
                                        if \$v6164\(0) = '1' then
                                          state_var6948 <= q_wait6163;
                                        else
                                          \$ram_ptr_take\(0) := '1';
                                          \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                                          \$ram_write_request\ <= '1';
                                          \$ram_write\ <= \$14202\(0 to 31);
                                          state_var6948 <= pause_setI6161;
                                        end if;
                                      else
                                        \$14214_sp\ := \$14213_sp\;
                                        \$v6160\ := \$ram_ptr_take\;
                                        if \$v6160\(0) = '1' then
                                          state_var6948 <= q_wait6159;
                                        else
                                          \$ram_ptr_take\(0) := '1';
                                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                                          state_var6948 <= pause_getI6157;
                                        end if;
                                      end if;
                                    end if;
                                  end if;
                                else
                                  \$v6187\ := \$ram_ptr_take\;
                                  if \$v6187\(0) = '1' then
                                    state_var6948 <= q_wait6186;
                                  else
                                    \$ram_ptr_take\(0) := '1';
                                    \$ram_ptr_write\ <= to_integer(unsigned(\$14208\(32 to 47)));
                                    \$ram_write_request\ <= '1';
                                    \$ram_write\ <= eclat_resize(\$12257_apply585_arg\(142 to 149),31) & eclat_true;
                                    state_var6948 <= pause_setI6184;
                                  end if;
                                end if;
                              end if;
                            end if;
                          end if;
                        end if;
                      when \$12258_offsetclosure_n586\ =>
                        \$12258_offsetclosure_n586_result\ := \$12258_offsetclosure_n586_arg\(0 to 15) & eclat_resize(eclat_add(eclat_resize(\$12258_offsetclosure_n586_arg\(106 to 136),16) & \$12258_offsetclosure_n586_arg\(32 to 47)),31) & eclat_false & \$12258_offsetclosure_n586_arg\(16 to 31) & \$12258_offsetclosure_n586_arg\(48 to 103) & \$12258_offsetclosure_n586_arg\(104 to 105);
                        result5987 := \$12258_offsetclosure_n586_result\;
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when \$12259_binop_int590\ =>
                        \$v6213\ := \$ram_ptr_take\;
                        if \$v6213\(0) = '1' then
                          state_var6948 <= q_wait6212;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6210;
                        end if;
                      when \$12260_compare591\ =>
                        \$v6214\ := \$12260_compare591_arg\(0 to 31);
                        case \$v6214\ is
                        when X"0000000" & X"0" =>
                          \$12260_compare591_result\ := eclat_eq(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93));
                          case \$12260_compare591_id\ is
                          when "000000001101" =>
                            \$14074_res\ := \$12260_compare591_result\;
                            \$12261_binop_compare592_result\ := eclat_add(\$12261_binop_compare592_arg\(32 to 47) & X"000" & X"1") & 
                            eclat_if(\$14074_res\ & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1") & \$12261_binop_compare592_arg\(96 to 151) & \$12261_binop_compare592_arg\(152 to 153);
                            result5987 := \$12261_binop_compare592_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when "000000110011" =>
                            \$13344_b\ := \$12260_compare591_result\;
                            \$12267_compbranch596_result\ := eclat_if(\$13344_b\ & eclat_add(eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"2") & eclat_resize(\$12267_compbranch596_arg\(63 to 93),16)) & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215) & eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"3") & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215));
                            result5987 := \$12267_compbranch596_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when others =>
                            
                          end case;
                        when X"0000000" & X"1" =>
                          \$12260_compare591_result\ := eclat_not(eclat_eq(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93)));
                          case \$12260_compare591_id\ is
                          when "000000001101" =>
                            \$14074_res\ := \$12260_compare591_result\;
                            \$12261_binop_compare592_result\ := eclat_add(\$12261_binop_compare592_arg\(32 to 47) & X"000" & X"1") & 
                            eclat_if(\$14074_res\ & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1") & \$12261_binop_compare592_arg\(96 to 151) & \$12261_binop_compare592_arg\(152 to 153);
                            result5987 := \$12261_binop_compare592_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when "000000110011" =>
                            \$13344_b\ := \$12260_compare591_result\;
                            \$12267_compbranch596_result\ := eclat_if(\$13344_b\ & eclat_add(eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"2") & eclat_resize(\$12267_compbranch596_arg\(63 to 93),16)) & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215) & eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"3") & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215));
                            result5987 := \$12267_compbranch596_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when others =>
                            
                          end case;
                        when X"0000000" & X"2" =>
                          \$12260_compare591_result\ := eclat_lt(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93));
                          case \$12260_compare591_id\ is
                          when "000000001101" =>
                            \$14074_res\ := \$12260_compare591_result\;
                            \$12261_binop_compare592_result\ := eclat_add(\$12261_binop_compare592_arg\(32 to 47) & X"000" & X"1") & 
                            eclat_if(\$14074_res\ & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1") & \$12261_binop_compare592_arg\(96 to 151) & \$12261_binop_compare592_arg\(152 to 153);
                            result5987 := \$12261_binop_compare592_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when "000000110011" =>
                            \$13344_b\ := \$12260_compare591_result\;
                            \$12267_compbranch596_result\ := eclat_if(\$13344_b\ & eclat_add(eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"2") & eclat_resize(\$12267_compbranch596_arg\(63 to 93),16)) & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215) & eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"3") & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215));
                            result5987 := \$12267_compbranch596_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when others =>
                            
                          end case;
                        when X"0000000" & X"3" =>
                          \$12260_compare591_result\ := eclat_if(eclat_lt(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93)) & eclat_true & eclat_eq(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93)));
                          case \$12260_compare591_id\ is
                          when "000000001101" =>
                            \$14074_res\ := \$12260_compare591_result\;
                            \$12261_binop_compare592_result\ := eclat_add(\$12261_binop_compare592_arg\(32 to 47) & X"000" & X"1") & 
                            eclat_if(\$14074_res\ & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1") & \$12261_binop_compare592_arg\(96 to 151) & \$12261_binop_compare592_arg\(152 to 153);
                            result5987 := \$12261_binop_compare592_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when "000000110011" =>
                            \$13344_b\ := \$12260_compare591_result\;
                            \$12267_compbranch596_result\ := eclat_if(\$13344_b\ & eclat_add(eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"2") & eclat_resize(\$12267_compbranch596_arg\(63 to 93),16)) & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215) & eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"3") & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215));
                            result5987 := \$12267_compbranch596_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when others =>
                            
                          end case;
                        when X"0000000" & X"4" =>
                          \$12260_compare591_result\ := eclat_not(eclat_if(eclat_lt(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93)) & eclat_true & eclat_eq(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93))));
                          case \$12260_compare591_id\ is
                          when "000000001101" =>
                            \$14074_res\ := \$12260_compare591_result\;
                            \$12261_binop_compare592_result\ := eclat_add(\$12261_binop_compare592_arg\(32 to 47) & X"000" & X"1") & 
                            eclat_if(\$14074_res\ & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1") & \$12261_binop_compare592_arg\(96 to 151) & \$12261_binop_compare592_arg\(152 to 153);
                            result5987 := \$12261_binop_compare592_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when "000000110011" =>
                            \$13344_b\ := \$12260_compare591_result\;
                            \$12267_compbranch596_result\ := eclat_if(\$13344_b\ & eclat_add(eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"2") & eclat_resize(\$12267_compbranch596_arg\(63 to 93),16)) & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215) & eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"3") & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215));
                            result5987 := \$12267_compbranch596_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when others =>
                            
                          end case;
                        when X"0000000" & X"5" =>
                          \$12260_compare591_result\ := eclat_not(eclat_lt(\$12260_compare591_arg\(32 to 62) & \$12260_compare591_arg\(63 to 93)));
                          case \$12260_compare591_id\ is
                          when "000000001101" =>
                            \$14074_res\ := \$12260_compare591_result\;
                            \$12261_binop_compare592_result\ := eclat_add(\$12261_binop_compare592_arg\(32 to 47) & X"000" & X"1") & 
                            eclat_if(\$14074_res\ & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1") & \$12261_binop_compare592_arg\(96 to 151) & \$12261_binop_compare592_arg\(152 to 153);
                            result5987 := \$12261_binop_compare592_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when "000000110011" =>
                            \$13344_b\ := \$12260_compare591_result\;
                            \$12267_compbranch596_result\ := eclat_if(\$13344_b\ & eclat_add(eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"2") & eclat_resize(\$12267_compbranch596_arg\(63 to 93),16)) & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215) & eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"3") & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215));
                            result5987 := \$12267_compbranch596_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when others =>
                            
                          end case;
                        when others =>
                          \$12260_compare591_result\ := eclat_false;
                          case \$12260_compare591_id\ is
                          when "000000001101" =>
                            \$14074_res\ := \$12260_compare591_result\;
                            \$12261_binop_compare592_result\ := eclat_add(\$12261_binop_compare592_arg\(32 to 47) & X"000" & X"1") & 
                            eclat_if(\$14074_res\ & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1") & \$12261_binop_compare592_arg\(96 to 151) & \$12261_binop_compare592_arg\(152 to 153);
                            result5987 := \$12261_binop_compare592_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when "000000110011" =>
                            \$13344_b\ := \$12260_compare591_result\;
                            \$12267_compbranch596_result\ := eclat_if(\$13344_b\ & eclat_add(eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"2") & eclat_resize(\$12267_compbranch596_arg\(63 to 93),16)) & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215) & eclat_add(\$12267_compbranch596_arg\(94 to 109) & X"000" & X"3") & \$12267_compbranch596_arg\(110 to 141) & \$12267_compbranch596_arg\(142 to 157) & \$12267_compbranch596_arg\(158 to 213) & \$12267_compbranch596_arg\(214 to 215));
                            result5987 := \$12267_compbranch596_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          when others =>
                            
                          end case;
                        end case;
                      when \$12261_binop_compare592\ =>
                        \$v6218\ := \$ram_ptr_take\;
                        if \$v6218\(0) = '1' then
                          state_var6948 <= q_wait6217;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6215;
                        end if;
                      when \$12262_make_block_n593\ =>
                        \$12221_make_block531_id\ := "000000001110";
                        \$12221_make_block531_arg\ := \$12262_make_block_n593_arg\(16 to 31) & \$12262_make_block_n593_arg\(82 to 113) & \$12262_make_block_n593_arg\(116 to 147) & eclat_resize(\$12262_make_block_n593_arg\(35 to 65),8) & \$12262_make_block_n593_arg\(66 to 81);
                        state_var6948 <= \$12221_make_block531\;
                      when \$12263_branch_if595\ =>
                        \$v6246\ := eclat_if(""&\$12263_branch_if595_arg\(0) & eclat_not(eclat_neq(\$12263_branch_if595_arg\(17 to 47) & "000"& X"000000" & X"0")) & eclat_neq(\$12263_branch_if595_arg\(17 to 47) & "000"& X"000000" & X"0"));
                        if \$v6246\(0) = '1' then
                          \$v6245\ := \$code_ptr_take\;
                          if \$v6245\(0) = '1' then
                            state_var6948 <= q_wait6244;
                          else
                            \$code_ptr_take\(0) := '1';
                            \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12263_branch_if595_arg\(1 to 16) & X"000" & X"1")));
                            state_var6948 <= pause_getI6242;
                          end if;
                        else
                          \$12263_branch_if595_result\ := eclat_add(\$12263_branch_if595_arg\(1 to 16) & X"000" & X"2") & \$12263_branch_if595_arg\(17 to 48) & \$12263_branch_if595_arg\(49 to 64) & \$12263_branch_if595_arg\(65 to 120) & \$12263_branch_if595_arg\(121 to 122);
                          result5987 := \$12263_branch_if595_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        end if;
                      when \$12267_compbranch596\ =>
                        \$12260_compare591_id\ := "000000110011";
                        \$12260_compare591_arg\ := \$12267_compbranch596_arg\(0 to 31) & \$12267_compbranch596_arg\(32 to 62) & \$12267_compbranch596_arg\(110 to 140);
                        state_var6948 <= \$12260_compare591\;
                      when \$12287_w0597\ =>
                        \$v6873\ := eclat_ge(\$12287_w0597_arg\(0 to 15) & \$12287_w0597_arg\(48 to 63));
                        if \$v6873\(0) = '1' then
                          \$12287_w0597_result\ := \$12287_w0597_arg\(16 to 31);
                          \$12288_sp\ := \$12287_w0597_result\;
                          \$12289_w1598_arg\ := X"000" & X"1" & \$12212\(0 to 15) & eclat_resize(\$12268_argument1\,16) & \$12279\(64 to 95);
                          state_var6948 <= \$12289_w1598\;
                        else
                          \$v6872\ := \$ram_ptr_take\;
                          if \$v6872\(0) = '1' then
                            state_var6948 <= q_wait6871;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12287_w0597_arg\(16 to 31) & X"000" & X"1")));
                            state_var6948 <= pause_getI6869;
                          end if;
                        end if;
                      when \$12289_w1598\ =>
                        \$v6886\ := eclat_ge(\$12289_w1598_arg\(0 to 15) & \$12289_w1598_arg\(32 to 47));
                        if \$v6886\(0) = '1' then
                          \$12289_w1598_result\ := eclat_unit;
                          \$v6895\ := \$ram_ptr_take\;
                          if \$v6895\(0) = '1' then
                            state_var6948 <= q_wait6894;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12288_sp\));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12279\(64 to 95);
                            state_var6948 <= pause_setI6892;
                          end if;
                        else
                          \$v6885\ := \$ram_ptr_take\;
                          if \$v6885\(0) = '1' then
                            state_var6948 <= q_wait6884;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12289_w1598_arg\(48 to 78),16) & eclat_sub(eclat_mult(X"000" & X"2" & \$12289_w1598_arg\(0 to 15)) & X"000" & X"1")) & X"000" & X"1")));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_lor(eclat_lsl(eclat_resize("11111001",31) & "000"& X"00000" & X"18") & eclat_lsl(eclat_resize(eclat_mult(X"000" & X"2" & \$12289_w1598_arg\(0 to 15)),31) & "000"& X"000000" & X"2")) & eclat_true;
                            state_var6948 <= pause_setI6882;
                          end if;
                        end if;
                      when \$12292_w3599\ =>
                        \$v6891\ := eclat_ge(\$12292_w3599_arg\(0 to 15) & \$12292_w3599_arg\(32 to 47));
                        if \$v6891\(0) = '1' then
                          \$12292_w3599_result\ := \$12292_w3599_arg\(16 to 31);
                          \$12293_sp\ := \$12292_w3599_result\;
                          result5987 := eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"3") & eclat_resize(\$12268_argument1\,16)) & \$12279\(64 to 95) & \$12293_sp\ & \$12279\(32 to 63) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        else
                          \$v6890\ := \$ram_ptr_take\;
                          if \$v6890\(0) = '1' then
                            state_var6948 <= q_wait6889;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12292_w3599_arg\(16 to 31)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_resize(eclat_add(eclat_resize(\$12292_w3599_arg\(48 to 78),16) & eclat_mult(X"000" & X"2" & \$12292_w3599_arg\(0 to 15))),31) & eclat_true;
                            state_var6948 <= pause_setI6887;
                          end if;
                        end if;
                      when \$12443_w600\ =>
                        \$v6809\ := eclat_gt(\$12443_w600_arg\(0 to 15) & \$12443_w600_arg\(32 to 47));
                        if \$v6809\(0) = '1' then
                          \$12443_w600_result\ := eclat_unit;
                          \$v6813\ := \$ram_ptr_take\;
                          if \$v6813\(0) = '1' then
                            state_var6948 <= q_wait6812;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                            state_var6948 <= pause_getI6810;
                          end if;
                        else
                          \$v6808\ := \$ram_ptr_take\;
                          if \$v6808\(0) = '1' then
                            state_var6948 <= q_wait6807;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12443_w600_arg\(16 to 31) & \$12443_w600_arg\(0 to 15))));
                            state_var6948 <= pause_getI6805;
                          end if;
                        end if;
                      when \$12496_fill601\ =>
                        \$v6822\ := eclat_gt(\$12496_fill601_arg\(0 to 15) & \$12496_fill601_arg\(32 to 47));
                        if \$v6822\(0) = '1' then
                          \$12496_fill601_result\ := \$12496_fill601_arg\(16 to 31);
                          \$12497_sp\ := \$12496_fill601_result\;
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"3") & \$12488\(64 to 95) & \$12497_sp\ & \$12488\(32 to 63) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        else
                          \$v6821\ := \$ram_ptr_take\;
                          if \$v6821\(0) = '1' then
                            state_var6948 <= q_wait6820;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12496_fill601_arg\(16 to 31) & X"000" & X"1")));
                            state_var6948 <= pause_getI6818;
                          end if;
                        end if;
                      when \$12586_fill602\ =>
                        \$v6860\ := eclat_ge(\$12586_fill602_arg\(0 to 15) & \$12586_fill602_arg\(32 to 47));
                        if \$v6860\(0) = '1' then
                          \$12586_fill602_result\ := \$12586_fill602_arg\(16 to 31);
                          \$12587_sp\ := \$12586_fill602_result\;
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"3") & \$12578\(64 to 95) & \$12587_sp\ & \$12578\(32 to 63) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        else
                          \$v6859\ := \$ram_ptr_take\;
                          if \$v6859\(0) = '1' then
                            state_var6948 <= q_wait6858;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12586_fill602_arg\(16 to 31) & X"000" & X"1")));
                            state_var6948 <= pause_getI6856;
                          end if;
                        end if;
                      when \$12804_w603\ =>
                        \$v6621\ := eclat_gt(\$12804_w603_arg\(0 to 7) & \$12804_w603_arg\(24 to 31));
                        if \$v6621\(0) = '1' then
                          \$12804_w603_result\ := \$12804_w603_arg\(8 to 23);
                          \$12805_sp\ := \$12804_w603_result\;
                          \$v6633\ := \$ram_ptr_take\;
                          if \$v6633\(0) = '1' then
                            state_var6948 <= q_wait6632;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12805_sp\ & X"000" & X"1")));
                            state_var6948 <= pause_getI6630;
                          end if;
                        else
                          \$v6620\ := \$ram_ptr_take\;
                          if \$v6620\(0) = '1' then
                            state_var6948 <= q_wait6619;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12804_w603_arg\(8 to 23) & X"000" & X"1")));
                            state_var6948 <= pause_getI6617;
                          end if;
                        end if;
                      when \$12931_forever617\ =>
                        state_var6948 <= \$12931_forever617\;
                      when \$12968_forever617\ =>
                        state_var6948 <= \$12968_forever617\;
                      when \$13283_forever617\ =>
                        state_var6948 <= \$13283_forever617\;
                      when \$13561_loop_push604\ =>
                        \$v6399\ := eclat_ge(\$13561_loop_push604_arg\(16 to 23) & eclat_sub(\$13561_loop_push604_arg\(56 to 63) & "00000010"));
                        if \$v6399\(0) = '1' then
                          \$13561_loop_push604_result\ := \$13561_loop_push604_arg\(0 to 15);
                          \$13565_sp\ := \$13561_loop_push604_result\;
                          \$v6403\ := \$ram_ptr_take\;
                          if \$v6403\(0) = '1' then
                            state_var6948 <= q_wait6402;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & X"000" & X"1") & X"000" & X"1")));
                            state_var6948 <= pause_getI6400;
                          end if;
                        else
                          \$v6398\ := \$ram_ptr_take\;
                          if \$v6398\(0) = '1' then
                            state_var6948 <= q_wait6397;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$13561_loop_push604_arg\(24 to 54),16) & eclat_resize(eclat_add(\$13561_loop_push604_arg\(16 to 23) & "00000010"),16)) & X"000" & X"1")));
                            state_var6948 <= pause_getI6395;
                          end if;
                        end if;
                      when \$13950_forever617\ =>
                        state_var6948 <= \$13950_forever617\;
                      when \$13957_forever617\ =>
                        state_var6948 <= \$13957_forever617\;
                      when \$13964_forever617\ =>
                        state_var6948 <= \$13964_forever617\;
                      when \$14112_modulo615\ =>
                        \$v6205\ := eclat_lt(\$14112_modulo615_arg\(0 to 30) & \$14112_modulo615_arg\(31 to 61));
                        if \$v6205\(0) = '1' then
                          \$14112_modulo615_result\ := \$14112_modulo615_arg\(0 to 30);
                          \$14113_r\ := \$14112_modulo615_result\;
                          \$14108_res\ := eclat_if(eclat_lt(\$12259_binop_int590_arg\(48 to 78) & "000"& X"000000" & X"0") & eclat_sub("000"& X"000000" & X"0" & \$14113_r\) & \$14113_r\);
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        else
                          \$14112_modulo615_arg\ := eclat_sub(\$14112_modulo615_arg\(0 to 30) & \$14112_modulo615_arg\(31 to 61)) & \$14112_modulo615_arg\(31 to 61);
                          state_var6948 <= \$14112_modulo615\;
                        end if;
                      when \$14127_modulo615\ =>
                        \$v6207\ := eclat_lt(\$14127_modulo615_arg\(0 to 30) & \$14127_modulo615_arg\(31 to 61));
                        if \$v6207\(0) = '1' then
                          \$14127_modulo615_result\ := \$14127_modulo615_arg\(0 to 30);
                          \$14128_r\ := \$14127_modulo615_result\;
                          \$14108_res\ := eclat_if(eclat_lt(\$12259_binop_int590_arg\(48 to 78) & "000"& X"000000" & X"0") & eclat_sub("000"& X"000000" & X"0" & \$14128_r\) & \$14128_r\);
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        else
                          \$14127_modulo615_arg\ := eclat_sub(\$14127_modulo615_arg\(0 to 30) & \$14127_modulo615_arg\(31 to 61)) & \$14127_modulo615_arg\(31 to 61);
                          state_var6948 <= \$14127_modulo615\;
                        end if;
                      when \$14296_wait609\ =>
                        \$v6156\ := eclat_not(rdy5995);
                        if \$v6156\(0) = '1' then
                          \$14324\ := \$14296_wait609_arg\(1 to 32) & \$14296_wait609_arg\(33 to 64) & X"0" & X"fa0" & X"0" & X"fa0" & X"0" & X"fa0" & eclat_add(X"0" & X"fa0" & X"1770") & eclat_false;
                        end if;
                        case state_var6949 is
                        when compute5996 =>
                          rdy5995 := eclat_false;
                          case state_var6950 is
                          when \$14365_copy_root_in_ram610\ =>
                            \$v6039\ := eclat_ge(\$14365_copy_root_in_ram610_arg\(0 to 15) & \$14365_copy_root_in_ram610_arg\(16 to 31));
                            if \$v6039\(0) = '1' then
                              \$14365_copy_root_in_ram610_result\ := \$14365_copy_root_in_ram610_arg\(32 to 47);
                              case \$14365_copy_root_in_ram610_id\ is
                              when "000000000110" =>
                                \$14368_next\ := \$14365_copy_root_in_ram610_result\;
                                eclat_print_string(of_string("======================================="));
                                
                                eclat_print_newline(eclat_unit);
                                
                                \$14372_aux611_arg\ := \$14324\(112 to 127) & \$14368_next\ & \$14324\(96 to 111) & \$14324\(112 to 127);
                                state_var6950 <= \$14372_aux611\;
                              when "000000000111" =>
                                \$14366_next\ := \$14365_copy_root_in_ram610_result\;
                                \$v6089\ := \$global_end_ptr_take\;
                                if \$v6089\(0) = '1' then
                                  state_var6950 <= q_wait6088;
                                else
                                  \$global_end_ptr_take\(0) := '1';
                                  \$global_end_ptr\ <= 0;
                                  state_var6950 <= pause_getI6086;
                                end if;
                              when others =>
                                
                              end case;
                            else
                              eclat_print_string(of_string("racine:"));
                              
                              eclat_print_int(\$14365_copy_root_in_ram610_arg\(0 to 15));
                              
                              eclat_print_newline(eclat_unit);
                              
                              \$v6038\ := \$ram_ptr_take\;
                              if \$v6038\(0) = '1' then
                                state_var6950 <= q_wait6037;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(0 to 15)));
                                state_var6950 <= pause_getI6035;
                              end if;
                            end if;
                          when \$14372_aux611\ =>
                            eclat_print_string(of_string("     scan="));
                            
                            eclat_print_int(\$14372_aux611_arg\(0 to 15));
                            
                            eclat_print_string(of_string(" | next="));
                            
                            eclat_print_int(\$14372_aux611_arg\(16 to 31));
                            
                            eclat_print_newline(eclat_unit);
                            
                            \$v6084\ := eclat_ge(\$14372_aux611_arg\(0 to 15) & \$14372_aux611_arg\(16 to 31));
                            if \$v6084\(0) = '1' then
                              \$14372_aux611_result\ := \$14372_aux611_arg\(16 to 31);
                              \$14373_next\ := \$14372_aux611_result\;
                              eclat_print_string(of_string("memory copied in to_space : "));
                              
                              eclat_print_int(eclat_sub(\$14373_next\ & \$14324\(112 to 127)));
                              
                              eclat_print_string(of_string(" words"));
                              
                              eclat_print_newline(eclat_unit);
                              
                              \$v6085\ := eclat_gt(eclat_sub(\$14373_next\ & \$14324\(112 to 127)) & X"1770");
                              if \$v6085\(0) = '1' then
                                eclat_print_string(of_string("fatal error: "));
                                
                                eclat_print_string(of_string("Out of memory"));
                                
                                eclat_print_newline(eclat_unit);
                                
                                state_var6950 <= \$14382_forever617\;
                              else
                                \$14351\ := \$14345\(0 to 31) & \$14362\(0 to 31) & \$14373_next\;
                                eclat_print_newline(eclat_unit);
                                
                                eclat_print_newline(eclat_unit);
                                
                                eclat_print_string(of_string("[================= GC END ======================]"));
                                
                                eclat_print_newline(eclat_unit);
                                
                                eclat_print_newline(eclat_unit);
                                
                                result5997 := \$14351\(0 to 31) & \$14351\(32 to 63) & \$14351\(64 to 79) & eclat_add(\$14351\(64 to 79) & \$14296_wait609_arg\(81 to 96)) & \$14324\(112 to 127) & \$14324\(96 to 111);
                                rdy5998 := eclat_true;
                                state_var6950 <= compute5999;
                              end if;
                            else
                              \$v6083\ := \$ram_ptr_take\;
                              if \$v6083\(0) = '1' then
                                state_var6950 <= q_wait6082;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(\$14372_aux611_arg\(0 to 15)));
                                state_var6950 <= pause_getI6080;
                              end if;
                            end if;
                          when \$14382_forever617\ =>
                            state_var6950 <= \$14382_forever617\;
                          when \$14408_loop612\ =>
                            \$v6079\ := eclat_ge(\$14408_loop612_arg\(0 to 15) & eclat_add(\$14408_loop612_arg\(80 to 95) & X"000" & X"1"));
                            if \$v6079\(0) = '1' then
                              \$14408_loop612_result\ := \$14408_loop612_arg\(16 to 31);
                              \$14409_next\ := \$14408_loop612_result\;
                              \$14372_aux611_arg\ := eclat_add(\$14372_aux611_arg\(0 to 15) & eclat_add(eclat_resize(eclat_lsr(eclat_resize(eclat_resize(\$14404\(0 to 30),16),31) & "000"& X"000000" & X"2"),16) & X"000" & X"1")) & \$14409_next\ & \$14372_aux611_arg\(32 to 47) & \$14372_aux611_arg\(48 to 63);
                              state_var6950 <= \$14372_aux611\;
                            else
                              \$v6078\ := \$ram_ptr_take\;
                              if \$v6078\(0) = '1' then
                                state_var6950 <= q_wait6077;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14408_loop612_arg\(64 to 79) & \$14408_loop612_arg\(0 to 15))));
                                state_var6950 <= pause_getI6075;
                              end if;
                            end if;
                          when \$14475_loop613\ =>
                            \$v6052\ := eclat_ge(\$14475_loop613_arg\(0 to 15) & eclat_add(\$14475_loop613_arg\(48 to 63) & X"000" & X"1"));
                            if \$v6052\(0) = '1' then
                              \$14475_loop613_result\ := eclat_unit;
                              \$v6060\ := \$ram_ptr_take\;
                              if \$v6060\(0) = '1' then
                                state_var6950 <= q_wait6059;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14440\(0 to 30),16)));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= eclat_resize(\$14408_loop612_arg\(16 to 31),31) & eclat_false;
                                state_var6950 <= pause_setI6057;
                              end if;
                            else
                              \$v6051\ := \$ram_ptr_take\;
                              if \$v6051\(0) = '1' then
                                state_var6950 <= q_wait6050;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14475_loop613_arg\(32 to 47) & \$14475_loop613_arg\(0 to 15))));
                                state_var6950 <= pause_getI6048;
                              end if;
                            end if;
                          when \$14602_loop613\ =>
                            \$v6012\ := eclat_ge(\$14602_loop613_arg\(0 to 15) & eclat_add(\$14602_loop613_arg\(48 to 63) & X"000" & X"1"));
                            if \$v6012\(0) = '1' then
                              \$14602_loop613_result\ := eclat_unit;
                              \$v6020\ := \$ram_ptr_take\;
                              if \$v6020\(0) = '1' then
                                state_var6950 <= q_wait6019;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14565\(0 to 30),16)));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= eclat_resize(\$14365_copy_root_in_ram610_arg\(32 to 47),31) & eclat_false;
                                state_var6950 <= pause_setI6017;
                              end if;
                            else
                              \$v6011\ := \$ram_ptr_take\;
                              if \$v6011\(0) = '1' then
                                state_var6950 <= q_wait6010;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14602_loop613_arg\(32 to 47) & \$14602_loop613_arg\(0 to 15))));
                                state_var6950 <= pause_getI6008;
                              end if;
                            end if;
                          when \$14698_loop613\ =>
                            \$v6098\ := eclat_ge(\$14698_loop613_arg\(0 to 15) & eclat_add(\$14698_loop613_arg\(48 to 63) & X"000" & X"1"));
                            if \$v6098\(0) = '1' then
                              \$14698_loop613_result\ := eclat_unit;
                              \$v6106\ := \$ram_ptr_take\;
                              if \$v6106\(0) = '1' then
                                state_var6950 <= q_wait6105;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(33 to 63),16)));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= eclat_resize(\$14345\(32 to 47),31) & eclat_false;
                                state_var6950 <= pause_setI6103;
                              end if;
                            else
                              \$v6097\ := \$ram_ptr_take\;
                              if \$v6097\(0) = '1' then
                                state_var6950 <= q_wait6096;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14698_loop613_arg\(32 to 47) & \$14698_loop613_arg\(0 to 15))));
                                state_var6950 <= pause_getI6094;
                              end if;
                            end if;
                          when \$14794_loop613\ =>
                            \$v6129\ := eclat_ge(\$14794_loop613_arg\(0 to 15) & eclat_add(\$14794_loop613_arg\(48 to 63) & X"000" & X"1"));
                            if \$v6129\(0) = '1' then
                              \$14794_loop613_result\ := eclat_unit;
                              \$v6137\ := \$ram_ptr_take\;
                              if \$v6137\(0) = '1' then
                                state_var6950 <= q_wait6136;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(1 to 31),16)));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= eclat_resize(\$14324\(112 to 127),31) & eclat_false;
                                state_var6950 <= pause_setI6134;
                              end if;
                            else
                              \$v6128\ := \$ram_ptr_take\;
                              if \$v6128\(0) = '1' then
                                state_var6950 <= q_wait6127;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14794_loop613_arg\(32 to 47) & \$14794_loop613_arg\(0 to 15))));
                                state_var6950 <= pause_getI6125;
                              end if;
                            end if;
                          when pause_getI6008 =>
                            state_var6950 <= pause_getII6009;
                          when pause_getI6025 =>
                            state_var6950 <= pause_getII6026;
                          when pause_getI6030 =>
                            state_var6950 <= pause_getII6031;
                          when pause_getI6035 =>
                            state_var6950 <= pause_getII6036;
                          when pause_getI6048 =>
                            state_var6950 <= pause_getII6049;
                          when pause_getI6065 =>
                            state_var6950 <= pause_getII6066;
                          when pause_getI6070 =>
                            state_var6950 <= pause_getII6071;
                          when pause_getI6075 =>
                            state_var6950 <= pause_getII6076;
                          when pause_getI6080 =>
                            state_var6950 <= pause_getII6081;
                          when pause_getI6086 =>
                            state_var6950 <= pause_getII6087;
                          when pause_getI6094 =>
                            state_var6950 <= pause_getII6095;
                          when pause_getI6111 =>
                            state_var6950 <= pause_getII6112;
                          when pause_getI6116 =>
                            state_var6950 <= pause_getII6117;
                          when pause_getI6125 =>
                            state_var6950 <= pause_getII6126;
                          when pause_getI6142 =>
                            state_var6950 <= pause_getII6143;
                          when pause_getI6147 =>
                            state_var6950 <= pause_getII6148;
                          when pause_getII6009 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14636\ := \$ram_value\;
                            \$v6007\ := \$ram_ptr_take\;
                            if \$v6007\(0) = '1' then
                              state_var6950 <= q_wait6006;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14602_loop613_arg\(16 to 31) & \$14602_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14636\;
                              state_var6950 <= pause_setI6004;
                            end if;
                          when pause_getII6026 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14588_hd\ := \$ram_value\;
                            eclat_print_string(of_string("bloc "));
                            
                            eclat_print_int(eclat_resize(\$14565\(0 to 30),16));
                            
                            eclat_print_string(of_string(" of size "));
                            
                            eclat_print_int(eclat_add(eclat_resize(eclat_lsr(\$14588_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            
                            eclat_print_string(of_string(" from "));
                            
                            eclat_print_int(eclat_resize(\$14565\(0 to 30),16));
                            
                            eclat_print_string(of_string(" to "));
                            
                            eclat_print_int(\$14365_copy_root_in_ram610_arg\(32 to 47));
                            
                            eclat_print_newline(eclat_unit);
                            
                            \$v6024\ := \$ram_ptr_take\;
                            if \$v6024\(0) = '1' then
                              state_var6950 <= q_wait6023;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(32 to 47)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14588_hd\;
                              state_var6950 <= pause_setI6021;
                            end if;
                          when pause_getII6031 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14583_w\ := \$ram_value\;
                            \$v6029\ := eclat_if(eclat_not(""&\$14583_w\(31)) & 
                                        eclat_if(eclat_le(\$14365_copy_root_in_ram610_arg\(64 to 79) & eclat_resize(\$14583_w\(0 to 30),16)) & eclat_lt(eclat_resize(\$14583_w\(0 to 30),16) & eclat_add(\$14365_copy_root_in_ram610_arg\(64 to 79) & X"1770")) & eclat_false) & eclat_false);
                            if \$v6029\(0) = '1' then
                              \$14569\ := \$14583_w\ & \$14365_copy_root_in_ram610_arg\(32 to 47);
                              \$v6003\ := \$ram_ptr_take\;
                              if \$v6003\(0) = '1' then
                                state_var6950 <= q_wait6002;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(0 to 15)));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= \$14569\(0 to 31);
                                state_var6950 <= pause_setI6000;
                              end if;
                            else
                              \$v6028\ := \$ram_ptr_take\;
                              if \$v6028\(0) = '1' then
                                state_var6950 <= q_wait6027;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14565\(0 to 30),16)));
                                state_var6950 <= pause_getI6025;
                              end if;
                            end if;
                          when pause_getII6036 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14565\ := \$ram_value\;
                            \$v6034\ := eclat_not(eclat_if(eclat_not(""&\$14565\(31)) & 
                                                  eclat_if(eclat_le(\$14365_copy_root_in_ram610_arg\(48 to 63) & eclat_resize(\$14565\(0 to 30),16)) & eclat_lt(eclat_resize(\$14565\(0 to 30),16) & eclat_add(\$14365_copy_root_in_ram610_arg\(48 to 63) & X"1770")) & eclat_false) & eclat_false));
                            if \$v6034\(0) = '1' then
                              \$14569\ := \$14565\ & \$14365_copy_root_in_ram610_arg\(32 to 47);
                              \$v6003\ := \$ram_ptr_take\;
                              if \$v6003\(0) = '1' then
                                state_var6950 <= q_wait6002;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(0 to 15)));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= \$14569\(0 to 31);
                                state_var6950 <= pause_setI6000;
                              end if;
                            else
                              \$v6033\ := \$ram_ptr_take\;
                              if \$v6033\(0) = '1' then
                                state_var6950 <= q_wait6032;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14565\(0 to 30),16) & X"000" & X"1")));
                                state_var6950 <= pause_getI6030;
                              end if;
                            end if;
                          when pause_getII6049 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14509\ := \$ram_value\;
                            \$v6047\ := \$ram_ptr_take\;
                            if \$v6047\(0) = '1' then
                              state_var6950 <= q_wait6046;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14475_loop613_arg\(16 to 31) & \$14475_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14509\;
                              state_var6950 <= pause_setI6044;
                            end if;
                          when pause_getII6066 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14461_hd\ := \$ram_value\;
                            eclat_print_string(of_string("bloc "));
                            
                            eclat_print_int(eclat_resize(\$14440\(0 to 30),16));
                            
                            eclat_print_string(of_string(" of size "));
                            
                            eclat_print_int(eclat_add(eclat_resize(eclat_lsr(\$14461_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            
                            eclat_print_string(of_string(" from "));
                            
                            eclat_print_int(eclat_resize(\$14440\(0 to 30),16));
                            
                            eclat_print_string(of_string(" to "));
                            
                            eclat_print_int(\$14408_loop612_arg\(16 to 31));
                            
                            eclat_print_newline(eclat_unit);
                            
                            \$v6064\ := \$ram_ptr_take\;
                            if \$v6064\(0) = '1' then
                              state_var6950 <= q_wait6063;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14408_loop612_arg\(16 to 31)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14461_hd\;
                              state_var6950 <= pause_setI6061;
                            end if;
                          when pause_getII6071 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14456_w\ := \$ram_value\;
                            \$v6069\ := eclat_if(eclat_not(""&\$14456_w\(31)) & 
                                        eclat_if(eclat_le(\$14408_loop612_arg\(48 to 63) & eclat_resize(\$14456_w\(0 to 30),16)) & eclat_lt(eclat_resize(\$14456_w\(0 to 30),16) & eclat_add(\$14408_loop612_arg\(48 to 63) & X"1770")) & eclat_false) & eclat_false);
                            if \$v6069\(0) = '1' then
                              \$14444\ := \$14456_w\ & \$14408_loop612_arg\(16 to 31);
                              \$v6043\ := \$ram_ptr_take\;
                              if \$v6043\(0) = '1' then
                                state_var6950 <= q_wait6042;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14408_loop612_arg\(64 to 79) & \$14408_loop612_arg\(0 to 15))));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= \$14444\(0 to 31);
                                state_var6950 <= pause_setI6040;
                              end if;
                            else
                              \$v6068\ := \$ram_ptr_take\;
                              if \$v6068\(0) = '1' then
                                state_var6950 <= q_wait6067;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14440\(0 to 30),16)));
                                state_var6950 <= pause_getI6065;
                              end if;
                            end if;
                          when pause_getII6076 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14440\ := \$ram_value\;
                            \$v6074\ := eclat_not(eclat_if(eclat_not(""&\$14440\(31)) & 
                                                  eclat_if(eclat_le(\$14408_loop612_arg\(32 to 47) & eclat_resize(\$14440\(0 to 30),16)) & eclat_lt(eclat_resize(\$14440\(0 to 30),16) & eclat_add(\$14408_loop612_arg\(32 to 47) & X"1770")) & eclat_false) & eclat_false));
                            if \$v6074\(0) = '1' then
                              \$14444\ := \$14440\ & \$14408_loop612_arg\(16 to 31);
                              \$v6043\ := \$ram_ptr_take\;
                              if \$v6043\(0) = '1' then
                                state_var6950 <= q_wait6042;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14408_loop612_arg\(64 to 79) & \$14408_loop612_arg\(0 to 15))));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= \$14444\(0 to 31);
                                state_var6950 <= pause_setI6040;
                              end if;
                            else
                              \$v6073\ := \$ram_ptr_take\;
                              if \$v6073\(0) = '1' then
                                state_var6950 <= q_wait6072;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14440\(0 to 30),16) & X"000" & X"1")));
                                state_var6950 <= pause_getI6070;
                              end if;
                            end if;
                          when pause_getII6081 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14404\ := \$ram_value\;
                            \$14408_loop612_arg\ := X"000" & X"1" & \$14372_aux611_arg\(16 to 31) & \$14372_aux611_arg\(32 to 47) & \$14372_aux611_arg\(48 to 63) & \$14372_aux611_arg\(0 to 15) & eclat_resize(eclat_lsr(eclat_resize(eclat_resize(\$14404\(0 to 30),16),31) & "000"& X"000000" & X"2"),16);
                            state_var6950 <= \$14408_loop612\;
                          when pause_getII6087 =>
                            \$global_end_ptr_take\(0) := '0';
                            \$14367\ := \$global_end_value\;
                            \$14365_copy_root_in_ram610_id\ := "000000000110";
                            \$14365_copy_root_in_ram610_arg\ := X"3e80" & \$14367\ & \$14366_next\ & \$14324\(96 to 111) & \$14324\(112 to 127);
                            state_var6950 <= \$14365_copy_root_in_ram610\;
                          when pause_getII6095 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14732\ := \$ram_value\;
                            \$v6093\ := \$ram_ptr_take\;
                            if \$v6093\(0) = '1' then
                              state_var6950 <= q_wait6092;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14698_loop613_arg\(16 to 31) & \$14698_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14732\;
                              state_var6950 <= pause_setI6090;
                            end if;
                          when pause_getII6112 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14684_hd\ := \$ram_value\;
                            eclat_print_string(of_string("bloc "));
                            
                            eclat_print_int(eclat_resize(\$14296_wait609_arg\(33 to 63),16));
                            
                            eclat_print_string(of_string(" of size "));
                            
                            eclat_print_int(eclat_add(eclat_resize(eclat_lsr(\$14684_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            
                            eclat_print_string(of_string(" from "));
                            
                            eclat_print_int(eclat_resize(\$14296_wait609_arg\(33 to 63),16));
                            
                            eclat_print_string(of_string(" to "));
                            
                            eclat_print_int(\$14345\(32 to 47));
                            
                            eclat_print_newline(eclat_unit);
                            
                            \$v6110\ := \$ram_ptr_take\;
                            if \$v6110\(0) = '1' then
                              state_var6950 <= q_wait6109;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14345\(32 to 47)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14684_hd\;
                              state_var6950 <= pause_setI6107;
                            end if;
                          when pause_getII6117 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14679_w\ := \$ram_value\;
                            \$v6115\ := eclat_if(eclat_not(""&\$14679_w\(31)) & 
                                        eclat_if(eclat_le(\$14324\(112 to 127) & eclat_resize(\$14679_w\(0 to 30),16)) & eclat_lt(eclat_resize(\$14679_w\(0 to 30),16) & eclat_add(\$14324\(112 to 127) & X"1770")) & eclat_false) & eclat_false);
                            if \$v6115\(0) = '1' then
                              \$14362\ := \$14679_w\ & \$14345\(32 to 47);
                              \$14365_copy_root_in_ram610_id\ := "000000000111";
                              \$14365_copy_root_in_ram610_arg\ := X"0" & X"3e8" & \$14296_wait609_arg\(65 to 80) & \$14362\(32 to 47) & \$14324\(96 to 111) & \$14324\(112 to 127);
                              state_var6950 <= \$14365_copy_root_in_ram610\;
                            else
                              \$v6114\ := \$ram_ptr_take\;
                              if \$v6114\(0) = '1' then
                                state_var6950 <= q_wait6113;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(33 to 63),16)));
                                state_var6950 <= pause_getI6111;
                              end if;
                            end if;
                          when pause_getII6126 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14828\ := \$ram_value\;
                            \$v6124\ := \$ram_ptr_take\;
                            if \$v6124\(0) = '1' then
                              state_var6950 <= q_wait6123;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14794_loop613_arg\(16 to 31) & \$14794_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14828\;
                              state_var6950 <= pause_setI6121;
                            end if;
                          when pause_getII6143 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14780_hd\ := \$ram_value\;
                            eclat_print_string(of_string("bloc "));
                            
                            eclat_print_int(eclat_resize(\$14296_wait609_arg\(1 to 31),16));
                            
                            eclat_print_string(of_string(" of size "));
                            
                            eclat_print_int(eclat_add(eclat_resize(eclat_lsr(\$14780_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            
                            eclat_print_string(of_string(" from "));
                            
                            eclat_print_int(eclat_resize(\$14296_wait609_arg\(1 to 31),16));
                            
                            eclat_print_string(of_string(" to "));
                            
                            eclat_print_int(\$14324\(112 to 127));
                            
                            eclat_print_newline(eclat_unit);
                            
                            \$v6141\ := \$ram_ptr_take\;
                            if \$v6141\(0) = '1' then
                              state_var6950 <= q_wait6140;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14324\(112 to 127)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14780_hd\;
                              state_var6950 <= pause_setI6138;
                            end if;
                          when pause_getII6148 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14775_w\ := \$ram_value\;
                            \$v6146\ := eclat_if(eclat_not(""&\$14775_w\(31)) & 
                                        eclat_if(eclat_le(\$14324\(112 to 127) & eclat_resize(\$14775_w\(0 to 30),16)) & eclat_lt(eclat_resize(\$14775_w\(0 to 30),16) & eclat_add(\$14324\(112 to 127) & X"1770")) & eclat_false) & eclat_false);
                            if \$v6146\(0) = '1' then
                              \$14345\ := \$14775_w\ & \$14324\(112 to 127);
                              \$v6120\ := eclat_not(eclat_if(eclat_not(""&\$14296_wait609_arg\(64)) & 
                                                    eclat_if(eclat_le(\$14324\(96 to 111) & eclat_resize(\$14296_wait609_arg\(33 to 63),16)) & eclat_lt(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & eclat_add(\$14324\(96 to 111) & X"1770")) & eclat_false) & eclat_false));
                              if \$v6120\(0) = '1' then
                                \$14362\ := \$14296_wait609_arg\(33 to 64) & \$14345\(32 to 47);
                                \$14365_copy_root_in_ram610_id\ := "000000000111";
                                \$14365_copy_root_in_ram610_arg\ := X"0" & X"3e8" & \$14296_wait609_arg\(65 to 80) & \$14362\(32 to 47) & \$14324\(96 to 111) & \$14324\(112 to 127);
                                state_var6950 <= \$14365_copy_root_in_ram610\;
                              else
                                \$v6119\ := \$ram_ptr_take\;
                                if \$v6119\(0) = '1' then
                                  state_var6950 <= q_wait6118;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & X"000" & X"1")));
                                  state_var6950 <= pause_getI6116;
                                end if;
                              end if;
                            else
                              \$v6145\ := \$ram_ptr_take\;
                              if \$v6145\(0) = '1' then
                                state_var6950 <= q_wait6144;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(1 to 31),16)));
                                state_var6950 <= pause_getI6142;
                              end if;
                            end if;
                          when pause_setI6000 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6001;
                          when pause_setI6004 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6005;
                          when pause_setI6013 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6014;
                          when pause_setI6017 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6018;
                          when pause_setI6021 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6022;
                          when pause_setI6040 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6041;
                          when pause_setI6044 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6045;
                          when pause_setI6053 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6054;
                          when pause_setI6057 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6058;
                          when pause_setI6061 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6062;
                          when pause_setI6090 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6091;
                          when pause_setI6099 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6100;
                          when pause_setI6103 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6104;
                          when pause_setI6107 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6108;
                          when pause_setI6121 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6122;
                          when pause_setI6130 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6131;
                          when pause_setI6134 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6135;
                          when pause_setI6138 =>
                            \$ram_write_request\ <= '0';
                            state_var6950 <= pause_setII6139;
                          when pause_setII6001 =>
                            \$ram_ptr_take\(0) := '0';
                            eclat_print_string(of_string(" next="));
                            
                            eclat_print_int(\$14569\(32 to 47));
                            
                            eclat_print_newline(eclat_unit);
                            
                            \$14365_copy_root_in_ram610_arg\ := eclat_add(\$14365_copy_root_in_ram610_arg\(0 to 15) & X"000" & X"1") & \$14365_copy_root_in_ram610_arg\(16 to 31) & \$14569\(32 to 47) & \$14365_copy_root_in_ram610_arg\(48 to 63) & \$14365_copy_root_in_ram610_arg\(64 to 79);
                            state_var6950 <= \$14365_copy_root_in_ram610\;
                          when pause_setII6005 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14602_loop613_arg\ := eclat_add(\$14602_loop613_arg\(0 to 15) & X"000" & X"1") & \$14602_loop613_arg\(16 to 31) & \$14602_loop613_arg\(32 to 47) & \$14602_loop613_arg\(48 to 63);
                            state_var6950 <= \$14602_loop613\;
                          when pause_setII6014 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14569\ := eclat_resize(\$14365_copy_root_in_ram610_arg\(32 to 47),31) & eclat_false & eclat_add(\$14365_copy_root_in_ram610_arg\(32 to 47) & eclat_add(eclat_resize(eclat_lsr(\$14588_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            \$v6003\ := \$ram_ptr_take\;
                            if \$v6003\(0) = '1' then
                              state_var6950 <= q_wait6002;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(0 to 15)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14569\(0 to 31);
                              state_var6950 <= pause_setI6000;
                            end if;
                          when pause_setII6018 =>
                            \$ram_ptr_take\(0) := '0';
                            \$v6016\ := \$ram_ptr_take\;
                            if \$v6016\(0) = '1' then
                              state_var6950 <= q_wait6015;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14565\(0 to 30),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14365_copy_root_in_ram610_arg\(32 to 47),31) & eclat_false;
                              state_var6950 <= pause_setI6013;
                            end if;
                          when pause_setII6022 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14602_loop613_arg\ := X"000" & X"1" & \$14365_copy_root_in_ram610_arg\(32 to 47) & eclat_resize(\$14565\(0 to 30),16) & eclat_resize(eclat_lsr(\$14588_hd\(0 to 30) & "000"& X"000000" & X"2"),16);
                            state_var6950 <= \$14602_loop613\;
                          when pause_setII6041 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14408_loop612_arg\ := eclat_add(\$14408_loop612_arg\(0 to 15) & X"000" & X"1") & \$14444\(32 to 47) & \$14408_loop612_arg\(32 to 47) & \$14408_loop612_arg\(48 to 63) & \$14408_loop612_arg\(64 to 79) & \$14408_loop612_arg\(80 to 95);
                            state_var6950 <= \$14408_loop612\;
                          when pause_setII6045 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14475_loop613_arg\ := eclat_add(\$14475_loop613_arg\(0 to 15) & X"000" & X"1") & \$14475_loop613_arg\(16 to 31) & \$14475_loop613_arg\(32 to 47) & \$14475_loop613_arg\(48 to 63);
                            state_var6950 <= \$14475_loop613\;
                          when pause_setII6054 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14444\ := eclat_resize(\$14408_loop612_arg\(16 to 31),31) & eclat_false & eclat_add(\$14408_loop612_arg\(16 to 31) & eclat_add(eclat_resize(eclat_lsr(\$14461_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            \$v6043\ := \$ram_ptr_take\;
                            if \$v6043\(0) = '1' then
                              state_var6950 <= q_wait6042;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14408_loop612_arg\(64 to 79) & \$14408_loop612_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14444\(0 to 31);
                              state_var6950 <= pause_setI6040;
                            end if;
                          when pause_setII6058 =>
                            \$ram_ptr_take\(0) := '0';
                            \$v6056\ := \$ram_ptr_take\;
                            if \$v6056\(0) = '1' then
                              state_var6950 <= q_wait6055;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14440\(0 to 30),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14408_loop612_arg\(16 to 31),31) & eclat_false;
                              state_var6950 <= pause_setI6053;
                            end if;
                          when pause_setII6062 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14475_loop613_arg\ := X"000" & X"1" & \$14408_loop612_arg\(16 to 31) & eclat_resize(\$14440\(0 to 30),16) & eclat_resize(eclat_lsr(\$14461_hd\(0 to 30) & "000"& X"000000" & X"2"),16);
                            state_var6950 <= \$14475_loop613\;
                          when pause_setII6091 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14698_loop613_arg\ := eclat_add(\$14698_loop613_arg\(0 to 15) & X"000" & X"1") & \$14698_loop613_arg\(16 to 31) & \$14698_loop613_arg\(32 to 47) & \$14698_loop613_arg\(48 to 63);
                            state_var6950 <= \$14698_loop613\;
                          when pause_setII6100 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14362\ := eclat_resize(\$14345\(32 to 47),31) & eclat_false & eclat_add(\$14345\(32 to 47) & eclat_add(eclat_resize(eclat_lsr(\$14684_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            \$14365_copy_root_in_ram610_id\ := "000000000111";
                            \$14365_copy_root_in_ram610_arg\ := X"0" & X"3e8" & \$14296_wait609_arg\(65 to 80) & \$14362\(32 to 47) & \$14324\(96 to 111) & \$14324\(112 to 127);
                            state_var6950 <= \$14365_copy_root_in_ram610\;
                          when pause_setII6104 =>
                            \$ram_ptr_take\(0) := '0';
                            \$v6102\ := \$ram_ptr_take\;
                            if \$v6102\(0) = '1' then
                              state_var6950 <= q_wait6101;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14345\(32 to 47),31) & eclat_false;
                              state_var6950 <= pause_setI6099;
                            end if;
                          when pause_setII6108 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14698_loop613_arg\ := X"000" & X"1" & \$14345\(32 to 47) & eclat_resize(\$14296_wait609_arg\(33 to 63),16) & eclat_resize(eclat_lsr(\$14684_hd\(0 to 30) & "000"& X"000000" & X"2"),16);
                            state_var6950 <= \$14698_loop613\;
                          when pause_setII6122 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14794_loop613_arg\ := eclat_add(\$14794_loop613_arg\(0 to 15) & X"000" & X"1") & \$14794_loop613_arg\(16 to 31) & \$14794_loop613_arg\(32 to 47) & \$14794_loop613_arg\(48 to 63);
                            state_var6950 <= \$14794_loop613\;
                          when pause_setII6131 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14345\ := eclat_resize(\$14324\(112 to 127),31) & eclat_false & eclat_add(\$14324\(112 to 127) & eclat_add(eclat_resize(eclat_lsr(\$14780_hd\(0 to 30) & "000"& X"000000" & X"2"),16) & X"000" & X"1"));
                            \$v6120\ := eclat_not(eclat_if(eclat_not(""&\$14296_wait609_arg\(64)) & 
                                                  eclat_if(eclat_le(\$14324\(96 to 111) & eclat_resize(\$14296_wait609_arg\(33 to 63),16)) & eclat_lt(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & eclat_add(\$14324\(96 to 111) & X"1770")) & eclat_false) & eclat_false));
                            if \$v6120\(0) = '1' then
                              \$14362\ := \$14296_wait609_arg\(33 to 64) & \$14345\(32 to 47);
                              \$14365_copy_root_in_ram610_id\ := "000000000111";
                              \$14365_copy_root_in_ram610_arg\ := X"0" & X"3e8" & \$14296_wait609_arg\(65 to 80) & \$14362\(32 to 47) & \$14324\(96 to 111) & \$14324\(112 to 127);
                              state_var6950 <= \$14365_copy_root_in_ram610\;
                            else
                              \$v6119\ := \$ram_ptr_take\;
                              if \$v6119\(0) = '1' then
                                state_var6950 <= q_wait6118;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & X"000" & X"1")));
                                state_var6950 <= pause_getI6116;
                              end if;
                            end if;
                          when pause_setII6135 =>
                            \$ram_ptr_take\(0) := '0';
                            \$v6133\ := \$ram_ptr_take\;
                            if \$v6133\(0) = '1' then
                              state_var6950 <= q_wait6132;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(1 to 31),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14324\(112 to 127),31) & eclat_false;
                              state_var6950 <= pause_setI6130;
                            end if;
                          when pause_setII6139 =>
                            \$ram_ptr_take\(0) := '0';
                            \$14794_loop613_arg\ := X"000" & X"1" & \$14324\(112 to 127) & eclat_resize(\$14296_wait609_arg\(1 to 31),16) & eclat_resize(eclat_lsr(\$14780_hd\(0 to 30) & "000"& X"000000" & X"2"),16);
                            state_var6950 <= \$14794_loop613\;
                          when q_wait6002 =>
                            \$v6003\ := \$ram_ptr_take\;
                            if \$v6003\(0) = '1' then
                              state_var6950 <= q_wait6002;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(0 to 15)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14569\(0 to 31);
                              state_var6950 <= pause_setI6000;
                            end if;
                          when q_wait6006 =>
                            \$v6007\ := \$ram_ptr_take\;
                            if \$v6007\(0) = '1' then
                              state_var6950 <= q_wait6006;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14602_loop613_arg\(16 to 31) & \$14602_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14636\;
                              state_var6950 <= pause_setI6004;
                            end if;
                          when q_wait6010 =>
                            \$v6011\ := \$ram_ptr_take\;
                            if \$v6011\(0) = '1' then
                              state_var6950 <= q_wait6010;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14602_loop613_arg\(32 to 47) & \$14602_loop613_arg\(0 to 15))));
                              state_var6950 <= pause_getI6008;
                            end if;
                          when q_wait6015 =>
                            \$v6016\ := \$ram_ptr_take\;
                            if \$v6016\(0) = '1' then
                              state_var6950 <= q_wait6015;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14565\(0 to 30),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14365_copy_root_in_ram610_arg\(32 to 47),31) & eclat_false;
                              state_var6950 <= pause_setI6013;
                            end if;
                          when q_wait6019 =>
                            \$v6020\ := \$ram_ptr_take\;
                            if \$v6020\(0) = '1' then
                              state_var6950 <= q_wait6019;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14565\(0 to 30),16)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14365_copy_root_in_ram610_arg\(32 to 47),31) & eclat_false;
                              state_var6950 <= pause_setI6017;
                            end if;
                          when q_wait6023 =>
                            \$v6024\ := \$ram_ptr_take\;
                            if \$v6024\(0) = '1' then
                              state_var6950 <= q_wait6023;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(32 to 47)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14588_hd\;
                              state_var6950 <= pause_setI6021;
                            end if;
                          when q_wait6027 =>
                            \$v6028\ := \$ram_ptr_take\;
                            if \$v6028\(0) = '1' then
                              state_var6950 <= q_wait6027;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14565\(0 to 30),16)));
                              state_var6950 <= pause_getI6025;
                            end if;
                          when q_wait6032 =>
                            \$v6033\ := \$ram_ptr_take\;
                            if \$v6033\(0) = '1' then
                              state_var6950 <= q_wait6032;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14565\(0 to 30),16) & X"000" & X"1")));
                              state_var6950 <= pause_getI6030;
                            end if;
                          when q_wait6037 =>
                            \$v6038\ := \$ram_ptr_take\;
                            if \$v6038\(0) = '1' then
                              state_var6950 <= q_wait6037;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(\$14365_copy_root_in_ram610_arg\(0 to 15)));
                              state_var6950 <= pause_getI6035;
                            end if;
                          when q_wait6042 =>
                            \$v6043\ := \$ram_ptr_take\;
                            if \$v6043\(0) = '1' then
                              state_var6950 <= q_wait6042;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14408_loop612_arg\(64 to 79) & \$14408_loop612_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14444\(0 to 31);
                              state_var6950 <= pause_setI6040;
                            end if;
                          when q_wait6046 =>
                            \$v6047\ := \$ram_ptr_take\;
                            if \$v6047\(0) = '1' then
                              state_var6950 <= q_wait6046;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14475_loop613_arg\(16 to 31) & \$14475_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14509\;
                              state_var6950 <= pause_setI6044;
                            end if;
                          when q_wait6050 =>
                            \$v6051\ := \$ram_ptr_take\;
                            if \$v6051\(0) = '1' then
                              state_var6950 <= q_wait6050;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14475_loop613_arg\(32 to 47) & \$14475_loop613_arg\(0 to 15))));
                              state_var6950 <= pause_getI6048;
                            end if;
                          when q_wait6055 =>
                            \$v6056\ := \$ram_ptr_take\;
                            if \$v6056\(0) = '1' then
                              state_var6950 <= q_wait6055;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14440\(0 to 30),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14408_loop612_arg\(16 to 31),31) & eclat_false;
                              state_var6950 <= pause_setI6053;
                            end if;
                          when q_wait6059 =>
                            \$v6060\ := \$ram_ptr_take\;
                            if \$v6060\(0) = '1' then
                              state_var6950 <= q_wait6059;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14440\(0 to 30),16)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14408_loop612_arg\(16 to 31),31) & eclat_false;
                              state_var6950 <= pause_setI6057;
                            end if;
                          when q_wait6063 =>
                            \$v6064\ := \$ram_ptr_take\;
                            if \$v6064\(0) = '1' then
                              state_var6950 <= q_wait6063;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14408_loop612_arg\(16 to 31)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14461_hd\;
                              state_var6950 <= pause_setI6061;
                            end if;
                          when q_wait6067 =>
                            \$v6068\ := \$ram_ptr_take\;
                            if \$v6068\(0) = '1' then
                              state_var6950 <= q_wait6067;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14440\(0 to 30),16)));
                              state_var6950 <= pause_getI6065;
                            end if;
                          when q_wait6072 =>
                            \$v6073\ := \$ram_ptr_take\;
                            if \$v6073\(0) = '1' then
                              state_var6950 <= q_wait6072;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14440\(0 to 30),16) & X"000" & X"1")));
                              state_var6950 <= pause_getI6070;
                            end if;
                          when q_wait6077 =>
                            \$v6078\ := \$ram_ptr_take\;
                            if \$v6078\(0) = '1' then
                              state_var6950 <= q_wait6077;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14408_loop612_arg\(64 to 79) & \$14408_loop612_arg\(0 to 15))));
                              state_var6950 <= pause_getI6075;
                            end if;
                          when q_wait6082 =>
                            \$v6083\ := \$ram_ptr_take\;
                            if \$v6083\(0) = '1' then
                              state_var6950 <= q_wait6082;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(\$14372_aux611_arg\(0 to 15)));
                              state_var6950 <= pause_getI6080;
                            end if;
                          when q_wait6088 =>
                            \$v6089\ := \$global_end_ptr_take\;
                            if \$v6089\(0) = '1' then
                              state_var6950 <= q_wait6088;
                            else
                              \$global_end_ptr_take\(0) := '1';
                              \$global_end_ptr\ <= 0;
                              state_var6950 <= pause_getI6086;
                            end if;
                          when q_wait6092 =>
                            \$v6093\ := \$ram_ptr_take\;
                            if \$v6093\(0) = '1' then
                              state_var6950 <= q_wait6092;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14698_loop613_arg\(16 to 31) & \$14698_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14732\;
                              state_var6950 <= pause_setI6090;
                            end if;
                          when q_wait6096 =>
                            \$v6097\ := \$ram_ptr_take\;
                            if \$v6097\(0) = '1' then
                              state_var6950 <= q_wait6096;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14698_loop613_arg\(32 to 47) & \$14698_loop613_arg\(0 to 15))));
                              state_var6950 <= pause_getI6094;
                            end if;
                          when q_wait6101 =>
                            \$v6102\ := \$ram_ptr_take\;
                            if \$v6102\(0) = '1' then
                              state_var6950 <= q_wait6101;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14345\(32 to 47),31) & eclat_false;
                              state_var6950 <= pause_setI6099;
                            end if;
                          when q_wait6105 =>
                            \$v6106\ := \$ram_ptr_take\;
                            if \$v6106\(0) = '1' then
                              state_var6950 <= q_wait6105;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(33 to 63),16)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14345\(32 to 47),31) & eclat_false;
                              state_var6950 <= pause_setI6103;
                            end if;
                          when q_wait6109 =>
                            \$v6110\ := \$ram_ptr_take\;
                            if \$v6110\(0) = '1' then
                              state_var6950 <= q_wait6109;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14345\(32 to 47)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14684_hd\;
                              state_var6950 <= pause_setI6107;
                            end if;
                          when q_wait6113 =>
                            \$v6114\ := \$ram_ptr_take\;
                            if \$v6114\(0) = '1' then
                              state_var6950 <= q_wait6113;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(33 to 63),16)));
                              state_var6950 <= pause_getI6111;
                            end if;
                          when q_wait6118 =>
                            \$v6119\ := \$ram_ptr_take\;
                            if \$v6119\(0) = '1' then
                              state_var6950 <= q_wait6118;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & X"000" & X"1")));
                              state_var6950 <= pause_getI6116;
                            end if;
                          when q_wait6123 =>
                            \$v6124\ := \$ram_ptr_take\;
                            if \$v6124\(0) = '1' then
                              state_var6950 <= q_wait6123;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14794_loop613_arg\(16 to 31) & \$14794_loop613_arg\(0 to 15))));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14828\;
                              state_var6950 <= pause_setI6121;
                            end if;
                          when q_wait6127 =>
                            \$v6128\ := \$ram_ptr_take\;
                            if \$v6128\(0) = '1' then
                              state_var6950 <= q_wait6127;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(\$14794_loop613_arg\(32 to 47) & \$14794_loop613_arg\(0 to 15))));
                              state_var6950 <= pause_getI6125;
                            end if;
                          when q_wait6132 =>
                            \$v6133\ := \$ram_ptr_take\;
                            if \$v6133\(0) = '1' then
                              state_var6950 <= q_wait6132;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(1 to 31),16) & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14324\(112 to 127),31) & eclat_false;
                              state_var6950 <= pause_setI6130;
                            end if;
                          when q_wait6136 =>
                            \$v6137\ := \$ram_ptr_take\;
                            if \$v6137\(0) = '1' then
                              state_var6950 <= q_wait6136;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(1 to 31),16)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$14324\(112 to 127),31) & eclat_false;
                              state_var6950 <= pause_setI6134;
                            end if;
                          when q_wait6140 =>
                            \$v6141\ := \$ram_ptr_take\;
                            if \$v6141\(0) = '1' then
                              state_var6950 <= q_wait6140;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14324\(112 to 127)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14780_hd\;
                              state_var6950 <= pause_setI6138;
                            end if;
                          when q_wait6144 =>
                            \$v6145\ := \$ram_ptr_take\;
                            if \$v6145\(0) = '1' then
                              state_var6950 <= q_wait6144;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$14296_wait609_arg\(1 to 31),16)));
                              state_var6950 <= pause_getI6142;
                            end if;
                          when q_wait6149 =>
                            \$v6150\ := \$ram_ptr_take\;
                            if \$v6150\(0) = '1' then
                              state_var6950 <= q_wait6149;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(1 to 31),16) & X"000" & X"1")));
                              state_var6950 <= pause_getI6147;
                            end if;
                          when compute5999 =>
                            rdy5998 := eclat_false;
                            \$v6152\ := eclat_gt(eclat_add(\$14324\(80 to 95) & \$14296_wait609_arg\(81 to 96)) & eclat_add(\$14324\(96 to 111) & X"1770"));
                            if \$v6152\(0) = '1' then
                              eclat_print_newline(eclat_unit);
                              
                              eclat_print_newline(eclat_unit);
                              
                              eclat_print_string(of_string("[================= GC START ======================]"));
                              
                              eclat_print_newline(eclat_unit);
                              
                              eclat_print_newline(eclat_unit);
                              
                              \$v6151\ := eclat_not(eclat_if(eclat_not(""&\$14296_wait609_arg\(32)) & 
                                                    eclat_if(eclat_le(\$14324\(96 to 111) & eclat_resize(\$14296_wait609_arg\(1 to 31),16)) & eclat_lt(eclat_resize(\$14296_wait609_arg\(1 to 31),16) & eclat_add(\$14324\(96 to 111) & X"1770")) & eclat_false) & eclat_false));
                              if \$v6151\(0) = '1' then
                                \$14345\ := \$14296_wait609_arg\(1 to 32) & \$14324\(112 to 127);
                                \$v6120\ := eclat_not(eclat_if(eclat_not(""&\$14296_wait609_arg\(64)) & 
                                                      eclat_if(eclat_le(\$14324\(96 to 111) & eclat_resize(\$14296_wait609_arg\(33 to 63),16)) & eclat_lt(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & eclat_add(\$14324\(96 to 111) & X"1770")) & eclat_false) & eclat_false));
                                if \$v6120\(0) = '1' then
                                  \$14362\ := \$14296_wait609_arg\(33 to 64) & \$14345\(32 to 47);
                                  \$14365_copy_root_in_ram610_id\ := "000000000111";
                                  \$14365_copy_root_in_ram610_arg\ := X"0" & X"3e8" & \$14296_wait609_arg\(65 to 80) & \$14362\(32 to 47) & \$14324\(96 to 111) & \$14324\(112 to 127);
                                  state_var6950 <= \$14365_copy_root_in_ram610\;
                                else
                                  \$v6119\ := \$ram_ptr_take\;
                                  if \$v6119\(0) = '1' then
                                    state_var6950 <= q_wait6118;
                                  else
                                    \$ram_ptr_take\(0) := '1';
                                    \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(33 to 63),16) & X"000" & X"1")));
                                    state_var6950 <= pause_getI6116;
                                  end if;
                                end if;
                              else
                                \$v6150\ := \$ram_ptr_take\;
                                if \$v6150\(0) = '1' then
                                  state_var6950 <= q_wait6149;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_resize(\$14296_wait609_arg\(1 to 31),16) & X"000" & X"1")));
                                  state_var6950 <= pause_getI6147;
                                end if;
                              end if;
                            else
                              result5997 := \$14296_wait609_arg\(1 to 32) & \$14296_wait609_arg\(33 to 64) & \$14324\(80 to 95) & eclat_add(\$14324\(80 to 95) & \$14296_wait609_arg\(81 to 96)) & \$14324\(96 to 111) & \$14324\(112 to 127);
                              rdy5998 := eclat_true;
                              state_var6950 <= compute5999;
                            end if;
                          end case;
                          \$v6154\ := eclat_not(rdy5998);
                          if \$v6154\(0) = '1' then
                            result5997 := \$14324\(0 to 31) & \$14324\(32 to 63) & \$14324\(64 to 79) & \$14324\(80 to 95) & \$14324\(96 to 111) & \$14324\(112 to 127);
                          end if;
                          \$14324\ := result5997 & rdy5998;
                          rdy5995 := eclat_true;
                          state_var6949 <= compute5996;
                        end case;
                        \$14324\ := \$14324\;
                        \$14314\ := \$14324\;
                        \$v5994\ := ""&\$14314\(128);
                        if \$v5994\(0) = '1' then
                          \$14296_wait609_result\ := \$14314\(0 to 31) & \$14314\(32 to 63) & \$14314\(64 to 79);
                          \$14272\ := \$14296_wait609_result\;
                          eclat_print_string(of_string("size:"));
                          
                          eclat_print_int(eclat_if(eclat_eq(\$12221_make_block531_arg\(88 to 103) & X"000" & X"0") & X"000" & X"1" & \$12221_make_block531_arg\(88 to 103)));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$v5993\ := \$ram_ptr_take\;
                          if \$v5993\(0) = '1' then
                            state_var6948 <= q_wait5992;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$14272\(64 to 79)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_lor(eclat_lsl(eclat_resize(\$12221_make_block531_arg\(80 to 87),31) & "000"& X"00000" & X"18") & eclat_lsl(eclat_resize(
                            eclat_if(eclat_eq(\$12221_make_block531_arg\(88 to 103) & X"000" & X"0") & X"000" & X"1" & \$12221_make_block531_arg\(88 to 103)),31) & "000"& X"000000" & X"2")) & eclat_true;
                            state_var6948 <= pause_setI5990;
                          end if;
                        else
                          \$14296_wait609_arg\ := eclat_unit & \$14296_wait609_arg\(1 to 32) & \$14296_wait609_arg\(33 to 64) & \$14296_wait609_arg\(65 to 80) & \$14296_wait609_arg\(81 to 96);
                          state_var6948 <= \$14296_wait609\;
                        end if;
                      when pause_getI6157 =>
                        state_var6948 <= pause_getII6158;
                      when pause_getI6189 =>
                        state_var6948 <= pause_getII6190;
                      when pause_getI6194 =>
                        state_var6948 <= pause_getII6195;
                      when pause_getI6199 =>
                        state_var6948 <= pause_getII6200;
                      when pause_getI6210 =>
                        state_var6948 <= pause_getII6211;
                      when pause_getI6215 =>
                        state_var6948 <= pause_getII6216;
                      when pause_getI6223 =>
                        state_var6948 <= pause_getII6224;
                      when pause_getI6232 =>
                        state_var6948 <= pause_getII6233;
                      when pause_getI6242 =>
                        state_var6948 <= pause_getII6243;
                      when pause_getI6247 =>
                        state_var6948 <= pause_getII6248;
                      when pause_getI6251 =>
                        state_var6948 <= pause_getII6252;
                      when pause_getI6255 =>
                        state_var6948 <= pause_getII6256;
                      when pause_getI6259 =>
                        state_var6948 <= pause_getII6260;
                      when pause_getI6263 =>
                        state_var6948 <= pause_getII6264;
                      when pause_getI6267 =>
                        state_var6948 <= pause_getII6268;
                      when pause_getI6271 =>
                        state_var6948 <= pause_getII6272;
                      when pause_getI6275 =>
                        state_var6948 <= pause_getII6276;
                      when pause_getI6287 =>
                        state_var6948 <= pause_getII6288;
                      when pause_getI6295 =>
                        state_var6948 <= pause_getII6296;
                      when pause_getI6303 =>
                        state_var6948 <= pause_getII6304;
                      when pause_getI6311 =>
                        state_var6948 <= pause_getII6312;
                      when pause_getI6319 =>
                        state_var6948 <= pause_getII6320;
                      when pause_getI6327 =>
                        state_var6948 <= pause_getII6328;
                      when pause_getI6335 =>
                        state_var6948 <= pause_getII6336;
                      when pause_getI6343 =>
                        state_var6948 <= pause_getII6344;
                      when pause_getI6347 =>
                        state_var6948 <= pause_getII6348;
                      when pause_getI6351 =>
                        state_var6948 <= pause_getII6352;
                      when pause_getI6355 =>
                        state_var6948 <= pause_getII6356;
                      when pause_getI6359 =>
                        state_var6948 <= pause_getII6360;
                      when pause_getI6367 =>
                        state_var6948 <= pause_getII6368;
                      when pause_getI6375 =>
                        state_var6948 <= pause_getII6376;
                      when pause_getI6383 =>
                        state_var6948 <= pause_getII6384;
                      when pause_getI6395 =>
                        state_var6948 <= pause_getII6396;
                      when pause_getI6400 =>
                        state_var6948 <= pause_getII6401;
                      when pause_getI6404 =>
                        state_var6948 <= pause_getII6405;
                      when pause_getI6424 =>
                        state_var6948 <= pause_getII6425;
                      when pause_getI6428 =>
                        state_var6948 <= pause_getII6429;
                      when pause_getI6432 =>
                        state_var6948 <= pause_getII6433;
                      when pause_getI6436 =>
                        state_var6948 <= pause_getII6437;
                      when pause_getI6444 =>
                        state_var6948 <= pause_getII6445;
                      when pause_getI6452 =>
                        state_var6948 <= pause_getII6453;
                      when pause_getI6460 =>
                        state_var6948 <= pause_getII6461;
                      when pause_getI6468 =>
                        state_var6948 <= pause_getII6469;
                      when pause_getI6472 =>
                        state_var6948 <= pause_getII6473;
                      when pause_getI6476 =>
                        state_var6948 <= pause_getII6477;
                      when pause_getI6480 =>
                        state_var6948 <= pause_getII6481;
                      when pause_getI6488 =>
                        state_var6948 <= pause_getII6489;
                      when pause_getI6492 =>
                        state_var6948 <= pause_getII6493;
                      when pause_getI6496 =>
                        state_var6948 <= pause_getII6497;
                      when pause_getI6500 =>
                        state_var6948 <= pause_getII6501;
                      when pause_getI6508 =>
                        state_var6948 <= pause_getII6509;
                      when pause_getI6512 =>
                        state_var6948 <= pause_getII6513;
                      when pause_getI6516 =>
                        state_var6948 <= pause_getII6517;
                      when pause_getI6520 =>
                        state_var6948 <= pause_getII6521;
                      when pause_getI6524 =>
                        state_var6948 <= pause_getII6525;
                      when pause_getI6528 =>
                        state_var6948 <= pause_getII6529;
                      when pause_getI6532 =>
                        state_var6948 <= pause_getII6533;
                      when pause_getI6552 =>
                        state_var6948 <= pause_getII6553;
                      when pause_getI6556 =>
                        state_var6948 <= pause_getII6557;
                      when pause_getI6568 =>
                        state_var6948 <= pause_getII6569;
                      when pause_getI6572 =>
                        state_var6948 <= pause_getII6573;
                      when pause_getI6592 =>
                        state_var6948 <= pause_getII6593;
                      when pause_getI6596 =>
                        state_var6948 <= pause_getII6597;
                      when pause_getI6600 =>
                        state_var6948 <= pause_getII6601;
                      when pause_getI6604 =>
                        state_var6948 <= pause_getII6605;
                      when pause_getI6608 =>
                        state_var6948 <= pause_getII6609;
                      when pause_getI6617 =>
                        state_var6948 <= pause_getII6618;
                      when pause_getI6622 =>
                        state_var6948 <= pause_getII6623;
                      when pause_getI6626 =>
                        state_var6948 <= pause_getII6627;
                      when pause_getI6630 =>
                        state_var6948 <= pause_getII6631;
                      when pause_getI6647 =>
                        state_var6948 <= pause_getII6648;
                      when pause_getI6651 =>
                        state_var6948 <= pause_getII6652;
                      when pause_getI6667 =>
                        state_var6948 <= pause_getII6668;
                      when pause_getI6675 =>
                        state_var6948 <= pause_getII6676;
                      when pause_getI6679 =>
                        state_var6948 <= pause_getII6680;
                      when pause_getI6683 =>
                        state_var6948 <= pause_getII6684;
                      when pause_getI6704 =>
                        state_var6948 <= pause_getII6705;
                      when pause_getI6713 =>
                        state_var6948 <= pause_getII6714;
                      when pause_getI6722 =>
                        state_var6948 <= pause_getII6723;
                      when pause_getI6726 =>
                        state_var6948 <= pause_getII6727;
                      when pause_getI6735 =>
                        state_var6948 <= pause_getII6736;
                      when pause_getI6739 =>
                        state_var6948 <= pause_getII6740;
                      when pause_getI6743 =>
                        state_var6948 <= pause_getII6744;
                      when pause_getI6752 =>
                        state_var6948 <= pause_getII6753;
                      when pause_getI6756 =>
                        state_var6948 <= pause_getII6757;
                      when pause_getI6760 =>
                        state_var6948 <= pause_getII6761;
                      when pause_getI6764 =>
                        state_var6948 <= pause_getII6765;
                      when pause_getI6773 =>
                        state_var6948 <= pause_getII6774;
                      when pause_getI6777 =>
                        state_var6948 <= pause_getII6778;
                      when pause_getI6781 =>
                        state_var6948 <= pause_getII6782;
                      when pause_getI6785 =>
                        state_var6948 <= pause_getII6786;
                      when pause_getI6797 =>
                        state_var6948 <= pause_getII6798;
                      when pause_getI6805 =>
                        state_var6948 <= pause_getII6806;
                      when pause_getI6810 =>
                        state_var6948 <= pause_getII6811;
                      when pause_getI6818 =>
                        state_var6948 <= pause_getII6819;
                      when pause_getI6832 =>
                        state_var6948 <= pause_getII6833;
                      when pause_getI6836 =>
                        state_var6948 <= pause_getII6837;
                      when pause_getI6840 =>
                        state_var6948 <= pause_getII6841;
                      when pause_getI6844 =>
                        state_var6948 <= pause_getII6845;
                      when pause_getI6856 =>
                        state_var6948 <= pause_getII6857;
                      when pause_getI6869 =>
                        state_var6948 <= pause_getII6870;
                      when pause_getI6878 =>
                        state_var6948 <= pause_getII6879;
                      when pause_getI6905 =>
                        state_var6948 <= pause_getII6906;
                      when pause_getI6910 =>
                        state_var6948 <= pause_getII6911;
                      when pause_getI6915 =>
                        state_var6948 <= pause_getII6916;
                      when pause_getI6920 =>
                        state_var6948 <= pause_getII6921;
                      when pause_getI6925 =>
                        state_var6948 <= pause_getII6926;
                      when pause_getII6158 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14218\ := \$ram_value\;
                        \$12257_apply585_result\ := eclat_resize(\$14218\(0 to 30),16) & \$12257_apply585_arg\(60 to 91) & \$14214_sp\ & \$12257_apply585_arg\(60 to 91) & \$12257_apply585_arg\(3 to 10) & \$12257_apply585_arg\(150 to 165) & \$12257_apply585_arg\(108 to 109);
                        result5987 := \$12257_apply585_result\;
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6190 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14249_v\ := \$ram_value\;
                        \$14208\ := \$14249_v\ & eclat_sub(\$14205\(32 to 47) & X"000" & X"1");
                        \$v6188\ := ""&\$12257_apply585_arg\(11);
                        if \$v6188\(0) = '1' then
                          \$14211_sp\ := eclat_add(eclat_sub(\$14208\(32 to 47) & \$12257_apply585_arg\(12 to 27)) & \$12257_apply585_arg\(28 to 43));
                          \$v6175\ := ""&\$12257_apply585_arg\(2);
                          if \$v6175\(0) = '1' then
                            \$v6174\ := \$ram_ptr_take\;
                            if \$v6174\(0) = '1' then
                              state_var6948 <= q_wait6173;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14211_sp\));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14208\(0 to 31);
                              state_var6948 <= pause_setI6171;
                            end if;
                          else
                            \$14212_sp\ := \$14211_sp\;
                            \$v6170\ := ""&\$12257_apply585_arg\(1);
                            if \$v6170\(0) = '1' then
                              \$v6169\ := \$ram_ptr_take\;
                              if \$v6169\(0) = '1' then
                                state_var6948 <= q_wait6168;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= \$14205\(0 to 31);
                                state_var6948 <= pause_setI6166;
                              end if;
                            else
                              \$14213_sp\ := \$14212_sp\;
                              \$v6165\ := ""&\$12257_apply585_arg\(0);
                              if \$v6165\(0) = '1' then
                                \$v6164\ := \$ram_ptr_take\;
                                if \$v6164\(0) = '1' then
                                  state_var6948 <= q_wait6163;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                                  \$ram_write_request\ <= '1';
                                  \$ram_write\ <= \$14202\(0 to 31);
                                  state_var6948 <= pause_setI6161;
                                end if;
                              else
                                \$14214_sp\ := \$14213_sp\;
                                \$v6160\ := \$ram_ptr_take\;
                                if \$v6160\(0) = '1' then
                                  state_var6948 <= q_wait6159;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                                  state_var6948 <= pause_getI6157;
                                end if;
                              end if;
                            end if;
                          end if;
                        else
                          \$v6187\ := \$ram_ptr_take\;
                          if \$v6187\(0) = '1' then
                            state_var6948 <= q_wait6186;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$14208\(32 to 47)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_resize(\$12257_apply585_arg\(142 to 149),31) & eclat_true;
                            state_var6948 <= pause_setI6184;
                          end if;
                        end if;
                      when pause_getII6195 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14252_v\ := \$ram_value\;
                        \$14205\ := \$14252_v\ & eclat_sub(\$14202\(32 to 47) & X"000" & X"1");
                        \$v6193\ := ""&\$12257_apply585_arg\(2);
                        if \$v6193\(0) = '1' then
                          \$v6192\ := \$ram_ptr_take\;
                          if \$v6192\(0) = '1' then
                            state_var6948 <= q_wait6191;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14205\(32 to 47) & X"000" & X"1")));
                            state_var6948 <= pause_getI6189;
                          end if;
                        else
                          \$14208\ := "000"& X"000000" & X"1" & eclat_true & \$14205\(32 to 47);
                          \$v6188\ := ""&\$12257_apply585_arg\(11);
                          if \$v6188\(0) = '1' then
                            \$14211_sp\ := eclat_add(eclat_sub(\$14208\(32 to 47) & \$12257_apply585_arg\(12 to 27)) & \$12257_apply585_arg\(28 to 43));
                            \$v6175\ := ""&\$12257_apply585_arg\(2);
                            if \$v6175\(0) = '1' then
                              \$v6174\ := \$ram_ptr_take\;
                              if \$v6174\(0) = '1' then
                                state_var6948 <= q_wait6173;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(\$14211_sp\));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= \$14208\(0 to 31);
                                state_var6948 <= pause_setI6171;
                              end if;
                            else
                              \$14212_sp\ := \$14211_sp\;
                              \$v6170\ := ""&\$12257_apply585_arg\(1);
                              if \$v6170\(0) = '1' then
                                \$v6169\ := \$ram_ptr_take\;
                                if \$v6169\(0) = '1' then
                                  state_var6948 <= q_wait6168;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                                  \$ram_write_request\ <= '1';
                                  \$ram_write\ <= \$14205\(0 to 31);
                                  state_var6948 <= pause_setI6166;
                                end if;
                              else
                                \$14213_sp\ := \$14212_sp\;
                                \$v6165\ := ""&\$12257_apply585_arg\(0);
                                if \$v6165\(0) = '1' then
                                  \$v6164\ := \$ram_ptr_take\;
                                  if \$v6164\(0) = '1' then
                                    state_var6948 <= q_wait6163;
                                  else
                                    \$ram_ptr_take\(0) := '1';
                                    \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                                    \$ram_write_request\ <= '1';
                                    \$ram_write\ <= \$14202\(0 to 31);
                                    state_var6948 <= pause_setI6161;
                                  end if;
                                else
                                  \$14214_sp\ := \$14213_sp\;
                                  \$v6160\ := \$ram_ptr_take\;
                                  if \$v6160\(0) = '1' then
                                    state_var6948 <= q_wait6159;
                                  else
                                    \$ram_ptr_take\(0) := '1';
                                    \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                                    state_var6948 <= pause_getI6157;
                                  end if;
                                end if;
                              end if;
                            end if;
                          else
                            \$v6187\ := \$ram_ptr_take\;
                            if \$v6187\(0) = '1' then
                              state_var6948 <= q_wait6186;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14208\(32 to 47)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= eclat_resize(\$12257_apply585_arg\(142 to 149),31) & eclat_true;
                              state_var6948 <= pause_setI6184;
                            end if;
                          end if;
                        end if;
                      when pause_getII6200 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14255_v\ := \$ram_value\;
                        \$14202\ := \$14255_v\ & eclat_sub(\$12257_apply585_arg\(92 to 107) & X"000" & X"1");
                        \$v6198\ := ""&\$12257_apply585_arg\(1);
                        if \$v6198\(0) = '1' then
                          \$v6197\ := \$ram_ptr_take\;
                          if \$v6197\(0) = '1' then
                            state_var6948 <= q_wait6196;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14202\(32 to 47) & X"000" & X"1")));
                            state_var6948 <= pause_getI6194;
                          end if;
                        else
                          \$14205\ := "000"& X"000000" & X"1" & eclat_true & \$14202\(32 to 47);
                          \$v6193\ := ""&\$12257_apply585_arg\(2);
                          if \$v6193\(0) = '1' then
                            \$v6192\ := \$ram_ptr_take\;
                            if \$v6192\(0) = '1' then
                              state_var6948 <= q_wait6191;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14205\(32 to 47) & X"000" & X"1")));
                              state_var6948 <= pause_getI6189;
                            end if;
                          else
                            \$14208\ := "000"& X"000000" & X"1" & eclat_true & \$14205\(32 to 47);
                            \$v6188\ := ""&\$12257_apply585_arg\(11);
                            if \$v6188\(0) = '1' then
                              \$14211_sp\ := eclat_add(eclat_sub(\$14208\(32 to 47) & \$12257_apply585_arg\(12 to 27)) & \$12257_apply585_arg\(28 to 43));
                              \$v6175\ := ""&\$12257_apply585_arg\(2);
                              if \$v6175\(0) = '1' then
                                \$v6174\ := \$ram_ptr_take\;
                                if \$v6174\(0) = '1' then
                                  state_var6948 <= q_wait6173;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr_write\ <= to_integer(unsigned(\$14211_sp\));
                                  \$ram_write_request\ <= '1';
                                  \$ram_write\ <= \$14208\(0 to 31);
                                  state_var6948 <= pause_setI6171;
                                end if;
                              else
                                \$14212_sp\ := \$14211_sp\;
                                \$v6170\ := ""&\$12257_apply585_arg\(1);
                                if \$v6170\(0) = '1' then
                                  \$v6169\ := \$ram_ptr_take\;
                                  if \$v6169\(0) = '1' then
                                    state_var6948 <= q_wait6168;
                                  else
                                    \$ram_ptr_take\(0) := '1';
                                    \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                                    \$ram_write_request\ <= '1';
                                    \$ram_write\ <= \$14205\(0 to 31);
                                    state_var6948 <= pause_setI6166;
                                  end if;
                                else
                                  \$14213_sp\ := \$14212_sp\;
                                  \$v6165\ := ""&\$12257_apply585_arg\(0);
                                  if \$v6165\(0) = '1' then
                                    \$v6164\ := \$ram_ptr_take\;
                                    if \$v6164\(0) = '1' then
                                      state_var6948 <= q_wait6163;
                                    else
                                      \$ram_ptr_take\(0) := '1';
                                      \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                                      \$ram_write_request\ <= '1';
                                      \$ram_write\ <= \$14202\(0 to 31);
                                      state_var6948 <= pause_setI6161;
                                    end if;
                                  else
                                    \$14214_sp\ := \$14213_sp\;
                                    \$v6160\ := \$ram_ptr_take\;
                                    if \$v6160\(0) = '1' then
                                      state_var6948 <= q_wait6159;
                                    else
                                      \$ram_ptr_take\(0) := '1';
                                      \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                                      state_var6948 <= pause_getI6157;
                                    end if;
                                  end if;
                                end if;
                              end if;
                            else
                              \$v6187\ := \$ram_ptr_take\;
                              if \$v6187\(0) = '1' then
                                state_var6948 <= q_wait6186;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(\$14208\(32 to 47)));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= eclat_resize(\$12257_apply585_arg\(142 to 149),31) & eclat_true;
                                state_var6948 <= pause_setI6184;
                              end if;
                            end if;
                          end if;
                        end if;
                      when pause_getII6211 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14104_v\ := \$ram_value\;
                        \$v6209\ := \$12259_binop_int590_arg\(0 to 31);
                        case \$v6209\ is
                        when X"0000000" & X"0" =>
                          \$14108_res\ := eclat_add(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"1" =>
                          \$14108_res\ := eclat_sub(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"2" =>
                          \$14108_res\ := eclat_mult(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"3" =>
                          \$v6206\ := eclat_eq(\$14104_v\(0 to 30) & "000"& X"000000" & X"0");
                          if \$v6206\(0) = '1' then
                            \$14108_res\ := "000"& X"000000" & X"0";
                            \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                            result5987 := \$12259_binop_int590_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          else
                            \$14112_modulo615_arg\ := eclat_abs(\$12259_binop_int590_arg\(48 to 78)) & eclat_abs(\$14104_v\(0 to 30));
                            state_var6948 <= \$14112_modulo615\;
                          end if;
                        when X"0000000" & X"4" =>
                          \$v6208\ := eclat_eq(\$14104_v\(0 to 30) & "000"& X"000000" & X"0");
                          if \$v6208\(0) = '1' then
                            \$14108_res\ := "000"& X"000000" & X"0";
                            \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                            result5987 := \$12259_binop_int590_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          else
                            \$14127_modulo615_arg\ := eclat_abs(\$12259_binop_int590_arg\(48 to 78)) & eclat_abs(\$14104_v\(0 to 30));
                            state_var6948 <= \$14127_modulo615\;
                          end if;
                        when X"0000000" & X"5" =>
                          \$14108_res\ := eclat_land(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"6" =>
                          \$14108_res\ := eclat_lor(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"7" =>
                          \$14108_res\ := eclat_lxor(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"8" =>
                          \$14108_res\ := eclat_lsl(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"9" =>
                          \$14108_res\ := eclat_lsr(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"a" =>
                          \$14108_res\ := eclat_asr(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"b" =>
                          \$14108_res\ := eclat_if(eclat_lt(\$12259_binop_int590_arg\(48 to 78) & "000"& X"000000" & X"0") & 
                                          eclat_if(eclat_lt(\$14104_v\(0 to 30) & "000"& X"000000" & X"0") & 
                                          eclat_if(eclat_gt(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30)) & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & "000"& X"000000" & X"0") & 
                                          eclat_if(eclat_lt(\$14104_v\(0 to 30) & "000"& X"000000" & X"0") & "000"& X"000000" & X"0" & 
                                          eclat_if(eclat_lt(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30)) & "000"& X"000000" & X"1" & "000"& X"000000" & X"0")));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when X"0000000" & X"c" =>
                          \$14108_res\ := eclat_if(eclat_lt(\$12259_binop_int590_arg\(48 to 78) & "000"& X"000000" & X"0") & 
                                          eclat_if(eclat_lt(\$14104_v\(0 to 30) & "000"& X"000000" & X"0") & 
                                          eclat_if(eclat_le(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30)) & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & "000"& X"000000" & X"1") & 
                                          eclat_if(eclat_lt(\$14104_v\(0 to 30) & "000"& X"000000" & X"0") & "000"& X"000000" & X"1" & 
                                          eclat_if(eclat_ge(\$12259_binop_int590_arg\(48 to 78) & \$14104_v\(0 to 30)) & "000"& X"000000" & X"1" & "000"& X"000000" & X"0")));
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when others =>
                          \$14108_res\ := "000"& X"000000" & X"0";
                          \$12259_binop_int590_result\ := eclat_add(\$12259_binop_int590_arg\(32 to 47) & X"000" & X"1") & \$14108_res\ & eclat_true & eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1") & \$12259_binop_int590_arg\(96 to 151) & \$12259_binop_int590_arg\(152 to 153);
                          result5987 := \$12259_binop_int590_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        end case;
                      when pause_getII6216 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14070_v\ := \$ram_value\;
                        \$12260_compare591_id\ := "000000001101";
                        \$12260_compare591_arg\ := \$12261_binop_compare592_arg\(0 to 31) & \$12261_binop_compare592_arg\(48 to 78) & \$14070_v\(0 to 30);
                        state_var6948 <= \$12260_compare591\;
                      when pause_getII6224 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14025_v\ := \$ram_value\;
                        \$v6222\ := \$ram_ptr_take\;
                        if \$v6222\(0) = '1' then
                          state_var6948 <= q_wait6221;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$14018\(64 to 94),16) & X"000" & X"2") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14025_v\;
                          state_var6948 <= pause_setI6219;
                        end if;
                      when pause_getII6233 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14036_v\ := \$ram_value\;
                        \$v6231\ := \$ram_ptr_take\;
                        if \$v6231\(0) = '1' then
                          state_var6948 <= q_wait6230;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$14018\(64 to 94),16) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14036_v\;
                          state_var6948 <= pause_setI6228;
                        end if;
                      when pause_getII6243 =>
                        \$code_ptr_take\(0) := '0';
                        \$13994_arg\ := \$code_value\;
                        \$12263_branch_if595_result\ := eclat_add(eclat_add(\$12263_branch_if595_arg\(1 to 16) & X"000" & X"1") & eclat_resize(\$13994_arg\,16)) & \$12263_branch_if595_arg\(17 to 48) & \$12263_branch_if595_arg\(49 to 64) & \$12263_branch_if595_arg\(65 to 120) & \$12263_branch_if595_arg\(121 to 122);
                        result5987 := \$12263_branch_if595_result\;
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6248 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13350_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13350_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6252 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13355_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13355_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6256 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13360_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13360_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6260 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13365_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13365_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6264 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13370_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13370_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6268 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13375_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13375_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6272 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13380_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13380_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6276 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13385_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13385_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6288 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13399_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13399_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6296 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13408_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13408_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6304 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13417_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13417_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6312 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13426_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13426_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6320 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13435_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13435_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6328 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13444_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13444_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6336 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13453_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13453_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6344 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13464\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13464\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6348 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13475\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13475\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6352 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13486\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13486\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6356 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13497\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13497\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6360 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13509\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13509\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6368 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13522\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13522\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6376 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13535\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13535\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6384 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13548\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13548\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6396 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13611\ := \$ram_value\;
                        \$v6394\ := \$ram_ptr_take\;
                        if \$v6394\(0) = '1' then
                          state_var6948 <= q_wait6393;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$13561_loop_push604_arg\(0 to 15)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13611\;
                          state_var6948 <= pause_setI6391;
                        end if;
                      when pause_getII6401 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13569_next_env\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(16 to 47) & \$13565_sp\ & \$13569_next_env\ & eclat_add(\$12212\(96 to 103) & eclat_sub(eclat_resize(eclat_resize(eclat_lsr(eclat_resize(eclat_resize(\$13560_hd\(0 to 30),16),31) & "000"& X"000000" & X"2"),16),8) & "00000010")) & \$12212\(104 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6405 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13560_hd\ := \$ram_value\;
                        \$13561_loop_push604_arg\ := \$12212\(48 to 63) & "00000000" & \$12212\(64 to 95) & eclat_resize(eclat_resize(eclat_lsr(eclat_resize(eclat_resize(\$13560_hd\(0 to 30),16),31) & "000"& X"000000" & X"2"),16),8);
                        state_var6948 <= \$13561_loop_push604\;
                      when pause_getII6425 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13668_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13668_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6429 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13683_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13683_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6433 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13698_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13698_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6437 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13713_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13713_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6445 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13721_v\ := \$ram_value\;
                        \$v6443\ := \$ram_ptr_take\;
                        if \$v6443\(0) = '1' then
                          state_var6948 <= q_wait6442;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13721_v\;
                          state_var6948 <= pause_setI6440;
                        end if;
                      when pause_getII6453 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13734_v\ := \$ram_value\;
                        \$v6451\ := \$ram_ptr_take\;
                        if \$v6451\(0) = '1' then
                          state_var6948 <= q_wait6450;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13734_v\;
                          state_var6948 <= pause_setI6448;
                        end if;
                      when pause_getII6461 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13747_v\ := \$ram_value\;
                        \$v6459\ := \$ram_ptr_take\;
                        if \$v6459\(0) = '1' then
                          state_var6948 <= q_wait6458;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"2") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13747_v\;
                          state_var6948 <= pause_setI6456;
                        end if;
                      when pause_getII6469 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13760_v\ := \$ram_value\;
                        \$v6467\ := \$ram_ptr_take\;
                        if \$v6467\(0) = '1' then
                          state_var6948 <= q_wait6466;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"3") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13760_v\;
                          state_var6948 <= pause_setI6464;
                        end if;
                      when pause_getII6473 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13776_hd\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_resize(eclat_resize(eclat_lsr(eclat_resize(eclat_resize(\$13776_hd\(0 to 30),16),31) & "000"& X"000000" & X"2"),16),31) & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6477 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13796_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13796_v\ & eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6481 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13789_v\ := \$ram_value\;
                        \$v6479\ := \$ram_ptr_take\;
                        if \$v6479\(0) = '1' then
                          state_var6948 <= q_wait6478;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13789_v\(0 to 30),16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6476;
                        end if;
                      when pause_getII6489 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13807_v\ := \$ram_value\;
                        \$v6487\ := \$ram_ptr_take\;
                        if \$v6487\(0) = '1' then
                          state_var6948 <= q_wait6486;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13806_v\(0 to 30),16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13807_v\;
                          state_var6948 <= pause_setI6484;
                        end if;
                      when pause_getII6493 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13806_v\ := \$ram_value\;
                        \$v6491\ := \$ram_ptr_take\;
                        if \$v6491\(0) = '1' then
                          state_var6948 <= q_wait6490;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6488;
                        end if;
                      when pause_getII6497 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13836_next_acc\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$13836_next_acc\ & eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6501 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13829_v\ := \$ram_value\;
                        \$v6499\ := \$ram_ptr_take\;
                        if \$v6499\(0) = '1' then
                          state_var6948 <= q_wait6498;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13829_v\(0 to 30),16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6496;
                        end if;
                      when pause_getII6509 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13847_v\ := \$ram_value\;
                        \$v6507\ := \$ram_ptr_take\;
                        if \$v6507\(0) = '1' then
                          state_var6948 <= q_wait6506;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13846_v\(0 to 30),16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13847_v\;
                          state_var6948 <= pause_setI6504;
                        end if;
                      when pause_getII6513 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13846_v\ := \$ram_value\;
                        \$v6511\ := \$ram_ptr_take\;
                        if \$v6511\(0) = '1' then
                          state_var6948 <= q_wait6510;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6508;
                        end if;
                      when pause_getII6517 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13876_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(16 to 47) & eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"2") & \$12212\(64 to 95) & \$12212\(96 to 103) & eclat_resize(\$13876_v\(0 to 30),16) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6521 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13900_v\ := \$ram_value\;
                        result5987 := eclat_resize(\$13891_v\(0 to 30),16) & \$12212\(16 to 47) & eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(104 to 119) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$13899_v\ & eclat_resize(\$13900_v\(0 to 30),8) & eclat_resize(\$13895_v\(0 to 30),16) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6525 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13899_v\ := \$ram_value\;
                        \$v6523\ := \$ram_ptr_take\;
                        if \$v6523\(0) = '1' then
                          state_var6948 <= q_wait6522;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(104 to 119) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6520;
                        end if;
                      when pause_getII6529 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13895_v\ := \$ram_value\;
                        \$v6527\ := \$ram_ptr_take\;
                        if \$v6527\(0) = '1' then
                          state_var6948 <= q_wait6526;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(104 to 119) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6524;
                        end if;
                      when pause_getII6533 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13891_v\ := \$ram_value\;
                        \$v6531\ := \$ram_ptr_take\;
                        if \$v6531\(0) = '1' then
                          state_var6948 <= q_wait6530;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(104 to 119) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6528;
                        end if;
                      when pause_getII6553 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12626_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12626_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6557 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12634_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12634_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6569 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12656\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12656\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6573 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12670\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12670\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6593 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12705\ := \$ram_value\;
                        result5987 := eclat_resize(\$12705\(0 to 30),16) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(16 to 47) & eclat_sub(eclat_resize(\$12268_argument1\,8) & "00000001") & \$12212\(104 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6597 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12776\ := \$ram_value\;
                        result5987 := eclat_resize(\$12776\(0 to 30),16) & \$12212\(16 to 47) & eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & \$12212\(16 to 47) & eclat_sub(\$12212\(96 to 103) & "00000001") & \$12212\(104 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6601 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12736_v\ := \$ram_value\;
                        result5987 := eclat_resize(\$12731_v\(0 to 30),16) & \$12212\(16 to 47) & eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12735_v\ & eclat_resize(\$12736_v\(0 to 30),8) & \$12212\(104 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6605 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12735_v\ := \$ram_value\;
                        \$v6603\ := \$ram_ptr_take\;
                        if \$v6603\(0) = '1' then
                          state_var6948 <= q_wait6602;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6600;
                        end if;
                      when pause_getII6609 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12731_v\ := \$ram_value\;
                        \$v6607\ := \$ram_ptr_take\;
                        if \$v6607\(0) = '1' then
                          state_var6948 <= q_wait6606;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6604;
                        end if;
                      when pause_getII6618 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12842_v\ := \$ram_value\;
                        \$v6616\ := \$ram_ptr_take\;
                        if \$v6616\(0) = '1' then
                          state_var6948 <= q_wait6615;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12804_w603_arg\(32 to 62),16) & eclat_resize(eclat_add(\$12804_w603_arg\(0 to 7) & "00000010"),16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12842_v\;
                          state_var6948 <= pause_setI6613;
                        end if;
                      when pause_getII6623 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12811_v\ := \$ram_value\;
                        result5987 := eclat_resize(\$12806_v\(0 to 30),16) & \$12792\(64 to 95) & eclat_sub(eclat_sub(eclat_sub(\$12805_sp\ & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12810_v\ & eclat_resize(\$12811_v\(0 to 30),8) & \$12212\(104 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6627 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12810_v\ := \$ram_value\;
                        \$v6625\ := \$ram_ptr_take\;
                        if \$v6625\(0) = '1' then
                          state_var6948 <= q_wait6624;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12805_sp\ & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6622;
                        end if;
                      when pause_getII6631 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12806_v\ := \$ram_value\;
                        \$v6629\ := \$ram_ptr_take\;
                        if \$v6629\(0) = '1' then
                          state_var6948 <= q_wait6628;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12805_sp\ & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6626;
                        end if;
                      when pause_getII6648 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12894_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12894_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6652 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12900_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12900_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6668 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12942_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12942_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6676 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12951_v\ := \$ram_value\;
                        \$v6674\ := \$ram_ptr_take\;
                        if \$v6674\(0) = '1' then
                          state_var6948 <= q_wait6673;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12951_v\;
                          state_var6948 <= pause_setI6671;
                        end if;
                      when pause_getII6680 =>
                        \$code_ptr_take\(0) := '0';
                        \$12976\ := \$code_value\;
                        result5987 := eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & eclat_resize(\$12976\,16)) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6684 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12987_hd\ := \$ram_value\;
                        \$12975_ofs\ := eclat_add(eclat_resize(\$12268_argument1\,16) & eclat_lsr(eclat_resize(\$12987_hd\(0 to 30),16) & X"00" & X"18"));
                        \$v6682\ := \$code_ptr_take\;
                        if \$v6682\(0) = '1' then
                          state_var6948 <= q_wait6681;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12975_ofs\)));
                          state_var6948 <= pause_getI6679;
                        end if;
                      when pause_getII6705 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13057_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$13045\(0 to 31) & eclat_sub(\$13045\(80 to 95) & X"000" & X"1") & \$13057_v\ & \$13045\(128 to 135) & \$13045\(136 to 151) & \$13045\(152 to 153);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6714 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13086_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$13074\(0 to 31) & eclat_sub(\$13074\(80 to 95) & X"000" & X"1") & \$13086_v\ & \$13074\(128 to 135) & \$13074\(136 to 151) & \$13074\(152 to 153);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6723 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6721\ := \$ram_ptr_take\;
                        if \$v6721\(0) = '1' then
                          state_var6948 <= q_wait6720;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6718;
                        end if;
                      when pause_getII6727 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13122_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$13110\(0 to 31) & eclat_sub(\$13110\(80 to 95) & X"000" & X"1") & \$13122_v\ & \$13110\(128 to 135) & \$13110\(136 to 151) & \$13110\(152 to 153);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6736 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6734\ := \$ram_ptr_take\;
                        if \$v6734\(0) = '1' then
                          state_var6948 <= q_wait6733;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6731;
                        end if;
                      when pause_getII6740 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6738\ := \$ram_ptr_take\;
                        if \$v6738\(0) = '1' then
                          state_var6948 <= q_wait6737;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6735;
                        end if;
                      when pause_getII6744 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13168_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$13156\(0 to 31) & eclat_sub(\$13156\(80 to 95) & X"000" & X"1") & \$13168_v\ & \$13156\(128 to 135) & \$13156\(136 to 151) & \$13156\(152 to 153);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6753 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6751\ := \$ram_ptr_take\;
                        if \$v6751\(0) = '1' then
                          state_var6948 <= q_wait6750;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6748;
                        end if;
                      when pause_getII6757 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6755\ := \$ram_ptr_take\;
                        if \$v6755\(0) = '1' then
                          state_var6948 <= q_wait6754;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6752;
                        end if;
                      when pause_getII6761 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6759\ := \$ram_ptr_take\;
                        if \$v6759\(0) = '1' then
                          state_var6948 <= q_wait6758;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6756;
                        end if;
                      when pause_getII6765 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13226_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$13214\(0 to 31) & eclat_sub(\$13214\(80 to 95) & X"000" & X"1") & \$13226_v\ & \$13214\(128 to 135) & \$13214\(136 to 151) & \$13214\(152 to 153);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6774 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6772\ := \$ram_ptr_take\;
                        if \$v6772\(0) = '1' then
                          state_var6948 <= q_wait6771;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6769;
                        end if;
                      when pause_getII6778 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6776\ := \$ram_ptr_take\;
                        if \$v6776\(0) = '1' then
                          state_var6948 <= q_wait6775;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6773;
                        end if;
                      when pause_getII6782 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6780\ := \$ram_ptr_take\;
                        if \$v6780\(0) = '1' then
                          state_var6948 <= q_wait6779;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6777;
                        end if;
                      when pause_getII6786 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6784\ := \$ram_ptr_take\;
                        if \$v6784\(0) = '1' then
                          state_var6948 <= q_wait6783;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6781;
                        end if;
                      when pause_getII6798 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13300_f0\ := \$ram_value\;
                        \$v6796\ := \$ram_ptr_take\;
                        if \$v6796\(0) = '1' then
                          state_var6948 <= q_wait6795;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_add(\$13300_f0\(0 to 30) & \$12268_argument1\) & eclat_true;
                          state_var6948 <= pause_setI6793;
                        end if;
                      when pause_getII6806 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12477\ := \$ram_value\;
                        \$v6804\ := \$ram_ptr_take\;
                        if \$v6804\(0) = '1' then
                          state_var6948 <= q_wait6803;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12443_w600_arg\(16 to 31) & \$12443_w600_arg\(32 to 47)) & \$12443_w600_arg\(48 to 63)) & \$12443_w600_arg\(0 to 15))));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12477\;
                          state_var6948 <= pause_setI6801;
                        end if;
                      when pause_getII6811 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12448\ := \$ram_value\;
                        result5987 := eclat_resize(\$12448\(0 to 30),16) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(16 to 47) & eclat_sub(eclat_add(\$12212\(96 to 103) & eclat_resize(\$12268_argument1\,8)) & "00000001") & \$12212\(104 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6819 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12512_v\ := \$ram_value\;
                        \$v6817\ := \$ram_ptr_take\;
                        if \$v6817\(0) = '1' then
                          state_var6948 <= q_wait6816;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12496_fill601_arg\(48 to 78),16) & \$12496_fill601_arg\(0 to 15)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12512_v\;
                          state_var6948 <= pause_setI6814;
                        end if;
                      when pause_getII6833 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12550_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"3") & \$12550_v\ & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6837 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12546\ := \$ram_value\;
                        \$v6835\ := \$ram_ptr_take\;
                        if \$v6835\(0) = '1' then
                          state_var6948 <= q_wait6834;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12546\(0 to 30),16) & eclat_resize(\$12270_argument2\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6832;
                        end if;
                      when pause_getII6841 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12566_v\ := \$ram_value\;
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"3") & \$12566_v\ & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6845 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12562\ := \$ram_value\;
                        \$v6843\ := \$ram_ptr_take\;
                        if \$v6843\(0) = '1' then
                          state_var6948 <= q_wait6842;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12562\(0 to 30),16) & eclat_resize(\$12270_argument2\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6840;
                        end if;
                      when pause_getII6857 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12602_v\ := \$ram_value\;
                        \$v6855\ := \$ram_ptr_take\;
                        if \$v6855\(0) = '1' then
                          state_var6948 <= q_wait6854;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12586_fill602_arg\(48 to 78),16) & \$12586_fill602_arg\(0 to 15)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12602_v\;
                          state_var6948 <= pause_setI6852;
                        end if;
                      when pause_getII6870 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12398_v\ := \$ram_value\;
                        \$v6868\ := \$ram_ptr_take\;
                        if \$v6868\(0) = '1' then
                          state_var6948 <= q_wait6867;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12287_w0597_arg\(64 to 94),16) & eclat_sub(eclat_add(\$12287_w0597_arg\(0 to 15) & eclat_mult(X"000" & X"2" & \$12287_w0597_arg\(32 to 47))) & X"000" & X"1")) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12398_v\;
                          state_var6948 <= pause_setI6865;
                        end if;
                      when pause_getII6879 =>
                        \$code_ptr_take\(0) := '0';
                        \$12343\ := \$code_value\;
                        \$v6877\ := \$ram_ptr_take\;
                        if \$v6877\(0) = '1' then
                          state_var6948 <= q_wait6876;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12289_w1598_arg\(48 to 78),16) & eclat_mult(X"000" & X"2" & \$12289_w1598_arg\(0 to 15))) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12289_w1598_arg\(16 to 31) & X"000" & X"2") & eclat_resize(\$12343\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6874;
                        end if;
                      when pause_getII6906 =>
                        \$code_ptr_take\(0) := '0';
                        \$12275\ := \$code_value\;
                        eclat_print_int(\$12275\);
                        
                        eclat_print_newline(eclat_unit);
                        
                        result5987 := \$12212\(0 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_getII6911 =>
                        \$code_ptr_take\(0) := '0';
                        \$12272_argument3\ := \$code_value\;
                        \$v6909\ := eclat_resize(\$12265\,8);
                        case \$v6909\ is
                        when "00101100" =>
                          \$v6904\ := eclat_gt(eclat_resize(\$12270_argument2\,16) & X"000" & X"0");
                          if \$v6904\(0) = '1' then
                            \$v6903\ := \$ram_ptr_take\;
                            if \$v6903\(0) = '1' then
                              state_var6948 <= q_wait6902;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$12212\(16 to 47);
                              state_var6948 <= pause_setI6900;
                            end if;
                          else
                            \$12278_sp\ := \$12212\(48 to 63);
                            \$12221_make_block531_id\ := "000001010011";
                            \$12221_make_block531_arg\ := \$12278_sp\ & \$12212\(16 to 47) & \$12212\(64 to 95) & "11110111" & eclat_add(eclat_sub(eclat_mult(X"000" & X"2" & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1") & eclat_resize(\$12270_argument2\,16));
                            state_var6948 <= \$12221_make_block531\;
                          end if;
                        when others =>
                          eclat_print_string(of_string("unknown opcode : "));
                          
                          \$v6908\ := \$code_ptr_take\;
                          if \$v6908\(0) = '1' then
                            state_var6948 <= q_wait6907;
                          else
                            \$code_ptr_take\(0) := '1';
                            \$code_ptr\ <= to_integer(unsigned(\$12212\(0 to 15)));
                            state_var6948 <= pause_getI6905;
                          end if;
                        end case;
                      when pause_getII6916 =>
                        \$code_ptr_take\(0) := '0';
                        \$12270_argument2\ := \$code_value\;
                        \$v6914\ := eclat_resize(\$12265\,8);
                        case \$v6914\ is
                        when "00100100" =>
                          \$12443_w600_arg\ := X"000" & X"1" & \$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16) & eclat_resize(\$12270_argument2\,16);
                          state_var6948 <= \$12443_w600\;
                        when "00101011" =>
                          \$v6831\ := eclat_gt(eclat_resize(\$12268_argument1\,16) & X"000" & X"0");
                          if \$v6831\(0) = '1' then
                            \$v6830\ := \$ram_ptr_take\;
                            if \$v6830\(0) = '1' then
                              state_var6948 <= q_wait6829;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$12212\(16 to 47);
                              state_var6948 <= pause_setI6827;
                            end if;
                          else
                            \$12487_sp\ := \$12212\(48 to 63);
                            \$12221_make_block531_id\ := "000001000111";
                            \$12221_make_block531_arg\ := \$12487_sp\ & \$12212\(16 to 47) & \$12212\(64 to 95) & "11110111" & eclat_add(eclat_resize(\$12268_argument1\,16) & X"000" & X"1");
                            state_var6948 <= \$12221_make_block531\;
                          end if;
                        when "00110111" =>
                          \$v6839\ := \$ram_ptr_take\;
                          if \$v6839\(0) = '1' then
                            state_var6948 <= q_wait6838;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                            state_var6948 <= pause_getI6836;
                          end if;
                        when "00111000" =>
                          \$v6851\ := \$ram_ptr_take\;
                          if \$v6851\(0) = '1' then
                            state_var6948 <= q_wait6850;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6848;
                          end if;
                        when "00111110" =>
                          \$12221_make_block531_id\ := "000001001001";
                          \$12221_make_block531_arg\ := \$12212\(48 to 63) & \$12212\(16 to 47) & \$12212\(64 to 95) & eclat_resize(\$12270_argument2\,8) & eclat_resize(\$12268_argument1\,16);
                          state_var6948 <= \$12221_make_block531\;
                        when "10000011" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"0" & \$12268_argument1\ & \$12270_argument2\ & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when "10000100" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"1" & \$12268_argument1\ & \$12270_argument2\ & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when "10000101" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"2" & \$12268_argument1\ & \$12270_argument2\ & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when "10000110" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"3" & \$12268_argument1\ & \$12270_argument2\ & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when "10000111" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"4" & \$12268_argument1\ & \$12270_argument2\ & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when "10001000" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"5" & \$12268_argument1\ & \$12270_argument2\ & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when others =>
                          \$v6913\ := \$code_ptr_take\;
                          if \$v6913\(0) = '1' then
                            state_var6948 <= q_wait6912;
                          else
                            \$code_ptr_take\(0) := '1';
                            \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12212\(0 to 15) & X"000" & X"3")));
                            state_var6948 <= pause_getI6910;
                          end if;
                        end case;
                      when pause_getII6921 =>
                        \$code_ptr_take\(0) := '0';
                        \$12268_argument1\ := \$code_value\;
                        \$v6919\ := eclat_resize(\$12265\,8);
                        case \$v6919\ is
                        when "00001000" =>
                          \$v6555\ := \$ram_ptr_take\;
                          if \$v6555\(0) = '1' then
                            state_var6948 <= q_wait6554;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                            state_var6948 <= pause_getI6552;
                          end if;
                        when "00010010" =>
                          \$v6563\ := \$ram_ptr_take\;
                          if \$v6563\(0) = '1' then
                            state_var6948 <= q_wait6562;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6560;
                          end if;
                        when "00010011" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(16 to 47) & eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "00010100" =>
                          \$v6567\ := \$ram_ptr_take\;
                          if \$v6567\(0) = '1' then
                            state_var6948 <= q_wait6566;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16))));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6564;
                          end if;
                        when "00011001" =>
                          \$v6571\ := \$ram_ptr_take\;
                          if \$v6571\(0) = '1' then
                            state_var6948 <= q_wait6570;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(eclat_resize(\$12268_argument1\,16) & X"000" & X"1")) & X"000" & X"1")));
                            state_var6948 <= pause_getI6568;
                          end if;
                        when "00011110" =>
                          \$v6579\ := \$ram_ptr_take\;
                          if \$v6579\(0) = '1' then
                            state_var6948 <= q_wait6578;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6576;
                          end if;
                        when "00011111" =>
                          \$v6591\ := \$ram_ptr_take\;
                          if \$v6591\(0) = '1' then
                            state_var6948 <= q_wait6590;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_resize(\$12212\(96 to 103),31) & eclat_true;
                            state_var6948 <= pause_setI6588;
                          end if;
                        when "00100000" =>
                          \$v6595\ := \$ram_ptr_take\;
                          if \$v6595\(0) = '1' then
                            state_var6948 <= q_wait6594;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                            state_var6948 <= pause_getI6592;
                          end if;
                        when "00100101" =>
                          \$12257_apply585_arg\ := eclat_true & eclat_false & eclat_false & \$12212\(96 to 103) & eclat_true & eclat_resize(\$12268_argument1\,16) & X"000" & X"1" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12257_apply585\;
                        when "00100110" =>
                          \$12257_apply585_arg\ := eclat_true & eclat_true & eclat_false & eclat_add(\$12212\(96 to 103) & "00000001") & eclat_true & eclat_resize(\$12268_argument1\,16) & X"000" & X"2" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12257_apply585\;
                        when "00100111" =>
                          \$12257_apply585_arg\ := eclat_true & eclat_true & eclat_true & eclat_add(\$12212\(96 to 103) & "00000010") & eclat_true & eclat_resize(\$12268_argument1\,16) & X"000" & X"3" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12257_apply585\;
                        when "00101000" =>
                          \$v6612\ := eclat_gt(\$12212\(96 to 103) & "00000000");
                          if \$v6612\(0) = '1' then
                            \$v6599\ := \$ram_ptr_take\;
                            if \$v6599\(0) = '1' then
                              state_var6948 <= q_wait6598;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                              state_var6948 <= pause_getI6596;
                            end if;
                          else
                            \$v6611\ := \$ram_ptr_take\;
                            if \$v6611\(0) = '1' then
                              state_var6948 <= q_wait6610;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                              state_var6948 <= pause_getI6608;
                            end if;
                          end if;
                        when "00101010" =>
                          \$v6642\ := eclat_ge(\$12212\(96 to 103) & eclat_resize(\$12268_argument1\,8));
                          if \$v6642\(0) = '1' then
                            result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 95) & eclat_sub(\$12212\(96 to 103) & eclat_resize(\$12268_argument1\,8)) & \$12212\(104 to 119) & \$12212\(120 to 121);
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          else
                            \$12221_make_block531_id\ := "000000111000";
                            \$12221_make_block531_arg\ := \$12212\(48 to 63) & \$12212\(16 to 47) & \$12212\(64 to 95) & "11110111" & eclat_resize(eclat_add(\$12212\(96 to 103) & "00000011"),16);
                            state_var6948 <= \$12221_make_block531\;
                          end if;
                        when "00110000" =>
                          \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16) & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                          state_var6948 <= \$12258_offsetclosure_n586\;
                        when "00110100" =>
                          \$v6646\ := \$ram_ptr_take\;
                          if \$v6646\(0) = '1' then
                            state_var6948 <= q_wait6645;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6643;
                          end if;
                        when "00110101" =>
                          \$v6650\ := \$ram_ptr_take\;
                          if \$v6650\(0) = '1' then
                            state_var6948 <= q_wait6649;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                            state_var6948 <= pause_getI6647;
                          end if;
                        when "00110110" =>
                          \$v6658\ := \$ram_ptr_take\;
                          if \$v6658\(0) = '1' then
                            state_var6948 <= q_wait6657;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6655;
                          end if;
                        when "00111001" =>
                          \$v6662\ := \$ram_ptr_take\;
                          if \$v6662\(0) = '1' then
                            state_var6948 <= q_wait6661;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6659;
                          end if;
                        when "00111011" =>
                          \$12262_make_block_n593_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(48 to 63) & eclat_false & eclat_false & eclat_false & \$12268_argument1\ & X"000" & X"0" & \$12212\(16 to 47) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12262_make_block_n593\;
                        when "00111101" =>
                          \$v6666\ := \$ram_ptr_take\;
                          if \$v6666\(0) = '1' then
                            state_var6948 <= q_wait6665;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6663;
                          end if;
                        when "00111111" =>
                          \$12262_make_block_n593_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(48 to 63) & eclat_true & eclat_false & eclat_false & \$12268_argument1\ & X"000" & X"1" & \$12212\(16 to 47) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12262_make_block_n593\;
                        when "01000000" =>
                          \$12262_make_block_n593_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(48 to 63) & eclat_true & eclat_true & eclat_false & \$12268_argument1\ & X"000" & X"2" & \$12212\(16 to 47) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12262_make_block_n593\;
                        when "01000001" =>
                          \$12262_make_block_n593_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(48 to 63) & eclat_true & eclat_true & eclat_true & \$12268_argument1\ & X"000" & X"3" & \$12212\(16 to 47) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12262_make_block_n593\;
                        when "01000010" =>
                          eclat_print_string(of_string("fatal error: "));
                          
                          eclat_print_string(of_string("unsupported instruction SETFLOATFIELD"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          state_var6948 <= \$12931_forever617\;
                        when "01000111" =>
                          assert eclat_not(""&\$12212\(47)) = eclat_true report "assertion failed" severity error;
                          
                          \$v6670\ := \$ram_ptr_take\;
                          if \$v6670\(0) = '1' then
                            state_var6948 <= q_wait6669;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                            state_var6948 <= pause_getI6667;
                          end if;
                        when "01001101" =>
                          \$v6678\ := \$ram_ptr_take\;
                          if \$v6678\(0) = '1' then
                            state_var6948 <= q_wait6677;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6675;
                          end if;
                        when "01001110" =>
                          eclat_print_string(of_string("fatal error: "));
                          
                          eclat_print_string(of_string("unsupported instruction SETFLOATFIELD"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          state_var6948 <= \$12968_forever617\;
                        when "01010111" =>
                          \$v6687\ := ""&\$12212\(47);
                          if \$v6687\(0) = '1' then
                            \$12975_ofs\ := eclat_resize(\$12212\(16 to 46),16);
                            \$v6682\ := \$code_ptr_take\;
                            if \$v6682\(0) = '1' then
                              state_var6948 <= q_wait6681;
                            else
                              \$code_ptr_take\(0) := '1';
                              \$code_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12975_ofs\)));
                              state_var6948 <= pause_getI6679;
                            end if;
                          else
                            \$v6686\ := \$ram_ptr_take\;
                            if \$v6686\(0) = '1' then
                              state_var6948 <= q_wait6685;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$12212\(16 to 46),16)));
                              state_var6948 <= pause_getI6683;
                            end if;
                          end if;
                        when "01010100" =>
                          result5987 := eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16)) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01011001" =>
                          \$v6703\ := \$ram_ptr_take\;
                          if \$v6703\(0) = '1' then
                            state_var6948 <= q_wait6702;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_resize(\$12212\(96 to 103),31) & eclat_true;
                            state_var6948 <= pause_setI6700;
                          end if;
                        when "01011100" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01011101" =>
                          \$v6712\ := \$ram_ptr_take\;
                          if \$v6712\(0) = '1' then
                            state_var6948 <= q_wait6711;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(64 to 95);
                            state_var6948 <= pause_setI6709;
                          end if;
                        when "01011110" =>
                          \$v6725\ := \$ram_ptr_take\;
                          if \$v6725\(0) = '1' then
                            state_var6948 <= q_wait6724;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6722;
                          end if;
                        when "01011111" =>
                          \$v6742\ := \$ram_ptr_take\;
                          if \$v6742\(0) = '1' then
                            state_var6948 <= q_wait6741;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6739;
                          end if;
                        when "01100000" =>
                          \$v6763\ := \$ram_ptr_take\;
                          if \$v6763\(0) = '1' then
                            state_var6948 <= q_wait6762;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6760;
                          end if;
                        when "01100001" =>
                          \$v6788\ := \$ram_ptr_take\;
                          if \$v6788\(0) = '1' then
                            state_var6948 <= q_wait6787;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6785;
                          end if;
                        when "01100010" =>
                          eclat_print_string(of_string("fatal error: "));
                          
                          eclat_print_string(of_string("unsupported instruction CALLN"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          state_var6948 <= \$13283_forever617\;
                        when "01100111" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12268_argument1\ & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01101100" =>
                          \$v6792\ := \$ram_ptr_take\;
                          if \$v6792\(0) = '1' then
                            state_var6948 <= q_wait6791;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6789;
                          end if;
                        when "01111111" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & eclat_add(\$12212\(16 to 46) & \$12268_argument1\) & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "10000000" =>
                          \$v6800\ := \$ram_ptr_take\;
                          if \$v6800\(0) = '1' then
                            state_var6948 <= q_wait6799;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                            state_var6948 <= pause_getI6797;
                          end if;
                        when "10001011" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"2" & \$12268_argument1\ & \$12212\(16 to 46) & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when "10001100" =>
                          \$12267_compbranch596_arg\ := X"0000000" & X"5" & \$12268_argument1\ & \$12212\(16 to 46) & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12267_compbranch596\;
                        when others =>
                          \$v6918\ := \$code_ptr_take\;
                          if \$v6918\(0) = '1' then
                            state_var6948 <= q_wait6917;
                          else
                            \$code_ptr_take\(0) := '1';
                            \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12212\(0 to 15) & X"000" & X"2")));
                            state_var6948 <= pause_getI6915;
                          end if;
                        end case;
                      when pause_getII6926 =>
                        \$code_ptr_take\(0) := '0';
                        \$12265\ := \$code_value\;
                        \$v6924\ := eclat_resize(\$12265\,8);
                        case \$v6924\ is
                        when "00000000" =>
                          \$v6250\ := \$ram_ptr_take\;
                          if \$v6250\(0) = '1' then
                            state_var6948 <= q_wait6249;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"0") & X"000" & X"1")));
                            state_var6948 <= pause_getI6247;
                          end if;
                        when "00000001" =>
                          \$v6254\ := \$ram_ptr_take\;
                          if \$v6254\(0) = '1' then
                            state_var6948 <= q_wait6253;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                            state_var6948 <= pause_getI6251;
                          end if;
                        when "00000010" =>
                          \$v6258\ := \$ram_ptr_take\;
                          if \$v6258\(0) = '1' then
                            state_var6948 <= q_wait6257;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"2") & X"000" & X"1")));
                            state_var6948 <= pause_getI6255;
                          end if;
                        when "00000011" =>
                          \$v6262\ := \$ram_ptr_take\;
                          if \$v6262\(0) = '1' then
                            state_var6948 <= q_wait6261;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"3") & X"000" & X"1")));
                            state_var6948 <= pause_getI6259;
                          end if;
                        when "00000100" =>
                          \$v6266\ := \$ram_ptr_take\;
                          if \$v6266\(0) = '1' then
                            state_var6948 <= q_wait6265;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"4") & X"000" & X"1")));
                            state_var6948 <= pause_getI6263;
                          end if;
                        when "00000101" =>
                          \$v6270\ := \$ram_ptr_take\;
                          if \$v6270\(0) = '1' then
                            state_var6948 <= q_wait6269;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"5") & X"000" & X"1")));
                            state_var6948 <= pause_getI6267;
                          end if;
                        when "00000110" =>
                          \$v6274\ := \$ram_ptr_take\;
                          if \$v6274\(0) = '1' then
                            state_var6948 <= q_wait6273;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"6") & X"000" & X"1")));
                            state_var6948 <= pause_getI6271;
                          end if;
                        when "00000111" =>
                          \$v6278\ := \$ram_ptr_take\;
                          if \$v6278\(0) = '1' then
                            state_var6948 <= q_wait6277;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"7") & X"000" & X"1")));
                            state_var6948 <= pause_getI6275;
                          end if;
                        when "00001001" =>
                          \$v6282\ := \$ram_ptr_take\;
                          if \$v6282\(0) = '1' then
                            state_var6948 <= q_wait6281;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6279;
                          end if;
                        when "00001010" =>
                          \$v6286\ := \$ram_ptr_take\;
                          if \$v6286\(0) = '1' then
                            state_var6948 <= q_wait6285;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6283;
                          end if;
                        when "00001011" =>
                          \$v6294\ := \$ram_ptr_take\;
                          if \$v6294\(0) = '1' then
                            state_var6948 <= q_wait6293;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6291;
                          end if;
                        when "00001100" =>
                          \$v6302\ := \$ram_ptr_take\;
                          if \$v6302\(0) = '1' then
                            state_var6948 <= q_wait6301;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6299;
                          end if;
                        when "00001101" =>
                          \$v6310\ := \$ram_ptr_take\;
                          if \$v6310\(0) = '1' then
                            state_var6948 <= q_wait6309;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6307;
                          end if;
                        when "00001110" =>
                          \$v6318\ := \$ram_ptr_take\;
                          if \$v6318\(0) = '1' then
                            state_var6948 <= q_wait6317;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6315;
                          end if;
                        when "00001111" =>
                          \$v6326\ := \$ram_ptr_take\;
                          if \$v6326\(0) = '1' then
                            state_var6948 <= q_wait6325;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6323;
                          end if;
                        when "00010000" =>
                          \$v6334\ := \$ram_ptr_take\;
                          if \$v6334\(0) = '1' then
                            state_var6948 <= q_wait6333;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6331;
                          end if;
                        when "00010001" =>
                          \$v6342\ := \$ram_ptr_take\;
                          if \$v6342\(0) = '1' then
                            state_var6948 <= q_wait6341;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6339;
                          end if;
                        when "00010101" =>
                          \$v6346\ := \$ram_ptr_take\;
                          if \$v6346\(0) = '1' then
                            state_var6948 <= q_wait6345;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"1" & X"000" & X"1")) & X"000" & X"1")));
                            state_var6948 <= pause_getI6343;
                          end if;
                        when "00010110" =>
                          \$v6350\ := \$ram_ptr_take\;
                          if \$v6350\(0) = '1' then
                            state_var6948 <= q_wait6349;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"2" & X"000" & X"1")) & X"000" & X"1")));
                            state_var6948 <= pause_getI6347;
                          end if;
                        when "00010111" =>
                          \$v6354\ := \$ram_ptr_take\;
                          if \$v6354\(0) = '1' then
                            state_var6948 <= q_wait6353;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"3" & X"000" & X"1")) & X"000" & X"1")));
                            state_var6948 <= pause_getI6351;
                          end if;
                        when "00011000" =>
                          \$v6358\ := \$ram_ptr_take\;
                          if \$v6358\(0) = '1' then
                            state_var6948 <= q_wait6357;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"4" & X"000" & X"1")) & X"000" & X"1")));
                            state_var6948 <= pause_getI6355;
                          end if;
                        when "00011010" =>
                          \$v6366\ := \$ram_ptr_take\;
                          if \$v6366\(0) = '1' then
                            state_var6948 <= q_wait6365;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6363;
                          end if;
                        when "00011011" =>
                          \$v6374\ := \$ram_ptr_take\;
                          if \$v6374\(0) = '1' then
                            state_var6948 <= q_wait6373;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6371;
                          end if;
                        when "00011100" =>
                          \$v6382\ := \$ram_ptr_take\;
                          if \$v6382\(0) = '1' then
                            state_var6948 <= q_wait6381;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6379;
                          end if;
                        when "00011101" =>
                          \$v6390\ := \$ram_ptr_take\;
                          if \$v6390\(0) = '1' then
                            state_var6948 <= q_wait6389;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6387;
                          end if;
                        when "00100001" =>
                          \$12257_apply585_arg\ := eclat_true & eclat_false & eclat_false & "00000000" & eclat_false & X"000" & X"0" & X"000" & X"0" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12257_apply585\;
                        when "00100010" =>
                          \$12257_apply585_arg\ := eclat_true & eclat_true & eclat_false & "00000001" & eclat_false & X"000" & X"0" & X"000" & X"0" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12257_apply585\;
                        when "00100011" =>
                          \$12257_apply585_arg\ := eclat_true & eclat_true & eclat_true & "00000010" & eclat_false & X"000" & X"0" & X"000" & X"0" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12257_apply585\;
                        when "00101001" =>
                          \$v6407\ := \$ram_ptr_take\;
                          if \$v6407\(0) = '1' then
                            state_var6948 <= q_wait6406;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$12212\(64 to 94),16)));
                            state_var6948 <= pause_getI6404;
                          end if;
                        when "00101101" =>
                          \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(48 to 63) & eclat_sub(X"000" & X"0" & X"000" & X"2") & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                          state_var6948 <= \$12258_offsetclosure_n586\;
                        when "00101110" =>
                          \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(48 to 63) & X"000" & X"0" & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                          state_var6948 <= \$12258_offsetclosure_n586\;
                        when "00101111" =>
                          \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(48 to 63) & X"000" & X"2" & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                          state_var6948 <= \$12258_offsetclosure_n586\;
                        when "00110001" =>
                          \$v6411\ := \$ram_ptr_take\;
                          if \$v6411\(0) = '1' then
                            state_var6948 <= q_wait6410;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6408;
                          end if;
                        when "00110010" =>
                          \$v6415\ := \$ram_ptr_take\;
                          if \$v6415\(0) = '1' then
                            state_var6948 <= q_wait6414;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6412;
                          end if;
                        when "00110011" =>
                          \$v6419\ := \$ram_ptr_take\;
                          if \$v6419\(0) = '1' then
                            state_var6948 <= q_wait6418;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6416;
                          end if;
                        when "00111010" =>
                          \$12262_make_block_n593_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(48 to 63) & eclat_false & eclat_false & eclat_false & "000"& X"000000" & X"0" & X"000" & X"0" & \$12212\(16 to 47) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                          state_var6948 <= \$12262_make_block_n593\;
                        when "00111100" =>
                          \$v6423\ := \$ram_ptr_take\;
                          if \$v6423\(0) = '1' then
                            state_var6948 <= q_wait6422;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6420;
                          end if;
                        when "01000011" =>
                          assert eclat_not(""&\$12212\(47)) = eclat_true report "assertion failed" severity error;
                          
                          \$v6427\ := \$ram_ptr_take\;
                          if \$v6427\(0) = '1' then
                            state_var6948 <= q_wait6426;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                            state_var6948 <= pause_getI6424;
                          end if;
                        when "01000100" =>
                          assert eclat_not(""&\$12212\(47)) = eclat_true report "assertion failed" severity error;
                          
                          \$v6431\ := \$ram_ptr_take\;
                          if \$v6431\(0) = '1' then
                            state_var6948 <= q_wait6430;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"1") & X"000" & X"1")));
                            state_var6948 <= pause_getI6428;
                          end if;
                        when "01000101" =>
                          assert eclat_not(""&\$12212\(47)) = eclat_true report "assertion failed" severity error;
                          
                          \$v6435\ := \$ram_ptr_take\;
                          if \$v6435\(0) = '1' then
                            state_var6948 <= q_wait6434;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"2") & X"000" & X"1")));
                            state_var6948 <= pause_getI6432;
                          end if;
                        when "01000110" =>
                          assert eclat_not(""&\$12212\(47)) = eclat_true report "assertion failed" severity error;
                          
                          \$v6439\ := \$ram_ptr_take\;
                          if \$v6439\(0) = '1' then
                            state_var6948 <= q_wait6438;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"3") & X"000" & X"1")));
                            state_var6948 <= pause_getI6436;
                          end if;
                        when "01001001" =>
                          \$v6447\ := \$ram_ptr_take\;
                          if \$v6447\(0) = '1' then
                            state_var6948 <= q_wait6446;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6444;
                          end if;
                        when "01001010" =>
                          \$v6455\ := \$ram_ptr_take\;
                          if \$v6455\(0) = '1' then
                            state_var6948 <= q_wait6454;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6452;
                          end if;
                        when "01001011" =>
                          \$v6463\ := \$ram_ptr_take\;
                          if \$v6463\(0) = '1' then
                            state_var6948 <= q_wait6462;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6460;
                          end if;
                        when "01001100" =>
                          \$v6471\ := \$ram_ptr_take\;
                          if \$v6471\(0) = '1' then
                            state_var6948 <= q_wait6470;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6468;
                          end if;
                        when "01001111" =>
                          \$v6475\ := \$ram_ptr_take\;
                          if \$v6475\(0) = '1' then
                            state_var6948 <= q_wait6474;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$12212\(16 to 46),16)));
                            state_var6948 <= pause_getI6472;
                          end if;
                        when "01010000" =>
                          \$v6483\ := \$ram_ptr_take\;
                          if \$v6483\(0) = '1' then
                            state_var6948 <= q_wait6482;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6480;
                          end if;
                        when "01010001" =>
                          \$v6495\ := \$ram_ptr_take\;
                          if \$v6495\(0) = '1' then
                            state_var6948 <= q_wait6494;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6492;
                          end if;
                        when "01010010" =>
                          \$v6503\ := \$ram_ptr_take\;
                          if \$v6503\(0) = '1' then
                            state_var6948 <= q_wait6502;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6500;
                          end if;
                        when "01010011" =>
                          \$v6515\ := \$ram_ptr_take\;
                          if \$v6515\(0) = '1' then
                            state_var6948 <= q_wait6514;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                            state_var6948 <= pause_getI6512;
                          end if;
                        when "01010101" =>
                          \$12263_branch_if595_arg\ := eclat_false & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12263_branch_if595\;
                        when "01010110" =>
                          \$12263_branch_if595_arg\ := eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12263_branch_if595\;
                        when "01011000" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & 
                          eclat_if(eclat_eq(\$12212\(16 to 46) & "000"& X"000000" & X"0") & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01011010" =>
                          \$v6519\ := \$ram_ptr_take\;
                          if \$v6519\(0) = '1' then
                            state_var6948 <= q_wait6518;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                            state_var6948 <= pause_getI6516;
                          end if;
                        when "01011011" =>
                          \$v6535\ := \$ram_ptr_take\;
                          if \$v6535\(0) = '1' then
                            state_var6948 <= q_wait6534;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(104 to 119) & X"000" & X"1")));
                            state_var6948 <= pause_getI6532;
                          end if;
                        when "01100011" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"0" & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01100100" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01100101" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"2" & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01100110" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"3" & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "01101000" =>
                          \$v6539\ := \$ram_ptr_take\;
                          if \$v6539\(0) = '1' then
                            state_var6948 <= q_wait6538;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6536;
                          end if;
                        when "01101001" =>
                          \$v6543\ := \$ram_ptr_take\;
                          if \$v6543\(0) = '1' then
                            state_var6948 <= q_wait6542;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6540;
                          end if;
                        when "01101010" =>
                          \$v6547\ := \$ram_ptr_take\;
                          if \$v6547\(0) = '1' then
                            state_var6948 <= q_wait6546;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6544;
                          end if;
                        when "01101011" =>
                          \$v6551\ := \$ram_ptr_take\;
                          if \$v6551\(0) = '1' then
                            state_var6948 <= q_wait6550;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12212\(16 to 47);
                            state_var6948 <= pause_setI6548;
                          end if;
                        when "01101110" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"0" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01101111" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"1" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110000" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"2" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110001" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"3" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110010" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"4" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110011" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"5" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110100" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"6" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110101" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"7" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110110" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"8" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01110111" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"9" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "01111000" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"a" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "10000010" =>
                          eclat_print_string(of_string("fatal error: "));
                          
                          eclat_print_string(of_string("unsupported instruction GETMETHOD"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          state_var6948 <= \$13950_forever617\;
                        when "10001001" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"b" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "10001010" =>
                          \$12259_binop_int590_arg\ := X"0000000" & X"c" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12259_binop_int590\;
                        when "10001101" =>
                          eclat_print_string(of_string("fatal error: "));
                          
                          eclat_print_string(of_string("unsupported instruction GETPUBMET"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          state_var6948 <= \$13957_forever617\;
                        when "10001110" =>
                          eclat_print_string(of_string("fatal error: "));
                          
                          eclat_print_string(of_string("unsupported instruction GETDYNMET"));
                          
                          eclat_print_newline(eclat_unit);
                          
                          state_var6948 <= \$13964_forever617\;
                        when "01111001" =>
                          \$12261_binop_compare592_arg\ := X"0000000" & X"0" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12261_binop_compare592\;
                        when "01111010" =>
                          \$12261_binop_compare592_arg\ := X"0000000" & X"1" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12261_binop_compare592\;
                        when "01111011" =>
                          \$12261_binop_compare592_arg\ := X"0000000" & X"2" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12261_binop_compare592\;
                        when "01111100" =>
                          \$12261_binop_compare592_arg\ := X"0000000" & X"3" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12261_binop_compare592\;
                        when "01111101" =>
                          \$12261_binop_compare592_arg\ := X"0000000" & X"4" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12261_binop_compare592\;
                        when "01111110" =>
                          \$12261_binop_compare592_arg\ := X"0000000" & X"5" & \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          state_var6948 <= \$12261_binop_compare592\;
                        when "10000001" =>
                          result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & 
                          eclat_if(""&\$12212\(47) & "000"& X"000000" & X"1" & "000"& X"000000" & X"0") & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when "10001111" =>
                          eclat_print_string(of_string("STOP : "));
                          
                          result5987 := \$12212\(0 to 15) & \$12212\(16 to 47) & \$12212\(48 to 63) & \$12212\(64 to 119) & eclat_true & ""&\$12212\(121);
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        when others =>
                          \$v6923\ := \$code_ptr_take\;
                          if \$v6923\(0) = '1' then
                            state_var6948 <= q_wait6922;
                          else
                            \$code_ptr_take\(0) := '1';
                            \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12212\(0 to 15) & X"000" & X"1")));
                            state_var6948 <= pause_getI6920;
                          end if;
                        end case;
                      when pause_setI5990 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII5991;
                      when pause_setI6161 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6162;
                      when pause_setI6166 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6167;
                      when pause_setI6171 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6172;
                      when pause_setI6176 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6177;
                      when pause_setI6180 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6181;
                      when pause_setI6184 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6185;
                      when pause_setI6219 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6220;
                      when pause_setI6228 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6229;
                      when pause_setI6237 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6238;
                      when pause_setI6279 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6280;
                      when pause_setI6283 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6284;
                      when pause_setI6291 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6292;
                      when pause_setI6299 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6300;
                      when pause_setI6307 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6308;
                      when pause_setI6315 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6316;
                      when pause_setI6323 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6324;
                      when pause_setI6331 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6332;
                      when pause_setI6339 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6340;
                      when pause_setI6363 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6364;
                      when pause_setI6371 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6372;
                      when pause_setI6379 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6380;
                      when pause_setI6387 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6388;
                      when pause_setI6391 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6392;
                      when pause_setI6408 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6409;
                      when pause_setI6412 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6413;
                      when pause_setI6416 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6417;
                      when pause_setI6420 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6421;
                      when pause_setI6440 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6441;
                      when pause_setI6448 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6449;
                      when pause_setI6456 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6457;
                      when pause_setI6464 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6465;
                      when pause_setI6484 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6485;
                      when pause_setI6504 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6505;
                      when pause_setI6536 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6537;
                      when pause_setI6540 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6541;
                      when pause_setI6544 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6545;
                      when pause_setI6548 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6549;
                      when pause_setI6560 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6561;
                      when pause_setI6564 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6565;
                      when pause_setI6576 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6577;
                      when pause_setI6580 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6581;
                      when pause_setI6584 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6585;
                      when pause_setI6588 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6589;
                      when pause_setI6613 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6614;
                      when pause_setI6634 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6635;
                      when pause_setI6638 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6639;
                      when pause_setI6643 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6644;
                      when pause_setI6655 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6656;
                      when pause_setI6659 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6660;
                      when pause_setI6663 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6664;
                      when pause_setI6671 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6672;
                      when pause_setI6688 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6689;
                      when pause_setI6692 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6693;
                      when pause_setI6696 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6697;
                      when pause_setI6700 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6701;
                      when pause_setI6709 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6710;
                      when pause_setI6718 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6719;
                      when pause_setI6731 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6732;
                      when pause_setI6748 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6749;
                      when pause_setI6769 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6770;
                      when pause_setI6789 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6790;
                      when pause_setI6793 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6794;
                      when pause_setI6801 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6802;
                      when pause_setI6814 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6815;
                      when pause_setI6823 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6824;
                      when pause_setI6827 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6828;
                      when pause_setI6848 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6849;
                      when pause_setI6852 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6853;
                      when pause_setI6861 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6862;
                      when pause_setI6865 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6866;
                      when pause_setI6874 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6875;
                      when pause_setI6882 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6883;
                      when pause_setI6887 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6888;
                      when pause_setI6892 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6893;
                      when pause_setI6896 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6897;
                      when pause_setI6900 =>
                        \$ram_write_request\ <= '0';
                        state_var6948 <= pause_setII6901;
                      when pause_setII5991 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12221_make_block531_result\ := \$14272\(0 to 31) & \$14272\(32 to 63) & eclat_resize(\$14272\(64 to 79),31) & eclat_false;
                        case \$12221_make_block531_id\ is
                        when "000000001110" =>
                          \$14018\ := \$12221_make_block531_result\;
                          \$v6241\ := ""&\$12262_make_block_n593_arg\(32);
                          if \$v6241\(0) = '1' then
                            \$v6240\ := \$ram_ptr_take\;
                            if \$v6240\(0) = '1' then
                              state_var6948 <= q_wait6239;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$14018\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14018\(0 to 31);
                              state_var6948 <= pause_setI6237;
                            end if;
                          else
                            \$v6236\ := ""&\$12262_make_block_n593_arg\(33);
                            if \$v6236\(0) = '1' then
                              \$v6235\ := \$ram_ptr_take\;
                              if \$v6235\(0) = '1' then
                                state_var6948 <= q_wait6234;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12262_make_block_n593_arg\(16 to 31) & X"000" & X"1")));
                                state_var6948 <= pause_getI6232;
                              end if;
                            else
                              \$14023_sp\ := \$12262_make_block_n593_arg\(16 to 31);
                              \$v6227\ := ""&\$12262_make_block_n593_arg\(34);
                              if \$v6227\(0) = '1' then
                                \$v6226\ := \$ram_ptr_take\;
                                if \$v6226\(0) = '1' then
                                  state_var6948 <= q_wait6225;
                                else
                                  \$ram_ptr_take\(0) := '1';
                                  \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14023_sp\ & X"000" & X"1")));
                                  state_var6948 <= pause_getI6223;
                                end if;
                              else
                                \$14024_sp\ := \$14023_sp\;
                                \$12262_make_block_n593_result\ := \$12262_make_block_n593_arg\(0 to 15) & \$14018\(64 to 95) & \$14024_sp\ & \$14018\(32 to 63) & \$12262_make_block_n593_arg\(148 to 155) & \$12262_make_block_n593_arg\(156 to 171) & \$12262_make_block_n593_arg\(114 to 115);
                                result5987 := \$12262_make_block_n593_result\;
                                rdy5988 := eclat_true;
                                state_var6948 <= compute5989;
                              end if;
                            end if;
                          end if;
                        when "000000111000" =>
                          \$12792\ := \$12221_make_block531_result\;
                          \$v6641\ := \$ram_ptr_take\;
                          if \$v6641\(0) = '1' then
                            state_var6948 <= q_wait6640;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12792\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_resize(eclat_sub(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & X"000" & X"3"),31) & eclat_true;
                            state_var6948 <= pause_setI6638;
                          end if;
                        when "000001000111" =>
                          \$12488\ := \$12221_make_block531_result\;
                          \$v6826\ := \$ram_ptr_take\;
                          if \$v6826\(0) = '1' then
                            state_var6948 <= q_wait6825;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12488\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & eclat_resize(\$12270_argument2\,16)),31) & eclat_true;
                            state_var6948 <= pause_setI6823;
                          end if;
                        when "000001001001" =>
                          \$12578\ := \$12221_make_block531_result\;
                          \$v6864\ := \$ram_ptr_take\;
                          if \$v6864\(0) = '1' then
                            state_var6948 <= q_wait6863;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12578\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$12578\(0 to 31);
                            state_var6948 <= pause_setI6861;
                          end if;
                        when "000001010011" =>
                          \$12279\ := \$12221_make_block531_result\;
                          \$v6899\ := \$ram_ptr_take\;
                          if \$v6899\(0) = '1' then
                            state_var6948 <= q_wait6898;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12279\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"3") & eclat_resize(\$12272_argument3\,16)),31) & eclat_true;
                            state_var6948 <= pause_setI6896;
                          end if;
                        when others =>
                          
                        end case;
                      when pause_setII6162 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14214_sp\ := eclat_add(\$14213_sp\ & X"000" & X"1");
                        \$v6160\ := \$ram_ptr_take\;
                        if \$v6160\(0) = '1' then
                          state_var6948 <= q_wait6159;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6157;
                        end if;
                      when pause_setII6167 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14213_sp\ := eclat_add(\$14212_sp\ & X"000" & X"1");
                        \$v6165\ := ""&\$12257_apply585_arg\(0);
                        if \$v6165\(0) = '1' then
                          \$v6164\ := \$ram_ptr_take\;
                          if \$v6164\(0) = '1' then
                            state_var6948 <= q_wait6163;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$14202\(0 to 31);
                            state_var6948 <= pause_setI6161;
                          end if;
                        else
                          \$14214_sp\ := \$14213_sp\;
                          \$v6160\ := \$ram_ptr_take\;
                          if \$v6160\(0) = '1' then
                            state_var6948 <= q_wait6159;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                            state_var6948 <= pause_getI6157;
                          end if;
                        end if;
                      when pause_setII6172 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14212_sp\ := eclat_add(\$14211_sp\ & X"000" & X"1");
                        \$v6170\ := ""&\$12257_apply585_arg\(1);
                        if \$v6170\(0) = '1' then
                          \$v6169\ := \$ram_ptr_take\;
                          if \$v6169\(0) = '1' then
                            state_var6948 <= q_wait6168;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$14205\(0 to 31);
                            state_var6948 <= pause_setI6166;
                          end if;
                        else
                          \$14213_sp\ := \$14212_sp\;
                          \$v6165\ := ""&\$12257_apply585_arg\(0);
                          if \$v6165\(0) = '1' then
                            \$v6164\ := \$ram_ptr_take\;
                            if \$v6164\(0) = '1' then
                              state_var6948 <= q_wait6163;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14202\(0 to 31);
                              state_var6948 <= pause_setI6161;
                            end if;
                          else
                            \$14214_sp\ := \$14213_sp\;
                            \$v6160\ := \$ram_ptr_take\;
                            if \$v6160\(0) = '1' then
                              state_var6948 <= q_wait6159;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                              state_var6948 <= pause_getI6157;
                            end if;
                          end if;
                        end if;
                      when pause_setII6177 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14211_sp\ := eclat_add(eclat_add(eclat_add(\$14208\(32 to 47) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1");
                        \$v6175\ := ""&\$12257_apply585_arg\(2);
                        if \$v6175\(0) = '1' then
                          \$v6174\ := \$ram_ptr_take\;
                          if \$v6174\(0) = '1' then
                            state_var6948 <= q_wait6173;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr_write\ <= to_integer(unsigned(\$14211_sp\));
                            \$ram_write_request\ <= '1';
                            \$ram_write\ <= \$14208\(0 to 31);
                            state_var6948 <= pause_setI6171;
                          end if;
                        else
                          \$14212_sp\ := \$14211_sp\;
                          \$v6170\ := ""&\$12257_apply585_arg\(1);
                          if \$v6170\(0) = '1' then
                            \$v6169\ := \$ram_ptr_take\;
                            if \$v6169\(0) = '1' then
                              state_var6948 <= q_wait6168;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                              \$ram_write_request\ <= '1';
                              \$ram_write\ <= \$14205\(0 to 31);
                              state_var6948 <= pause_setI6166;
                            end if;
                          else
                            \$14213_sp\ := \$14212_sp\;
                            \$v6165\ := ""&\$12257_apply585_arg\(0);
                            if \$v6165\(0) = '1' then
                              \$v6164\ := \$ram_ptr_take\;
                              if \$v6164\(0) = '1' then
                                state_var6948 <= q_wait6163;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                                \$ram_write_request\ <= '1';
                                \$ram_write\ <= \$14202\(0 to 31);
                                state_var6948 <= pause_setI6161;
                              end if;
                            else
                              \$14214_sp\ := \$14213_sp\;
                              \$v6160\ := \$ram_ptr_take\;
                              if \$v6160\(0) = '1' then
                                state_var6948 <= q_wait6159;
                              else
                                \$ram_ptr_take\(0) := '1';
                                \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                                state_var6948 <= pause_getI6157;
                              end if;
                            end if;
                          end if;
                        end if;
                      when pause_setII6181 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6179\ := \$ram_ptr_take\;
                        if \$v6179\(0) = '1' then
                          state_var6948 <= q_wait6178;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(\$14208\(32 to 47) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(\$12257_apply585_arg\(44 to 59) & X"000" & X"1"),31) & eclat_true;
                          state_var6948 <= pause_setI6176;
                        end if;
                      when pause_setII6185 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6183\ := \$ram_ptr_take\;
                        if \$v6183\(0) = '1' then
                          state_var6948 <= q_wait6182;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14208\(32 to 47) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12257_apply585_arg\(110 to 141);
                          state_var6948 <= pause_setI6180;
                        end if;
                      when pause_setII6220 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14024_sp\ := eclat_sub(\$14023_sp\ & X"000" & X"1");
                        \$12262_make_block_n593_result\ := \$12262_make_block_n593_arg\(0 to 15) & \$14018\(64 to 95) & \$14024_sp\ & \$14018\(32 to 63) & \$12262_make_block_n593_arg\(148 to 155) & \$12262_make_block_n593_arg\(156 to 171) & \$12262_make_block_n593_arg\(114 to 115);
                        result5987 := \$12262_make_block_n593_result\;
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6229 =>
                        \$ram_ptr_take\(0) := '0';
                        \$14023_sp\ := eclat_sub(\$12262_make_block_n593_arg\(16 to 31) & X"000" & X"1");
                        \$v6227\ := ""&\$12262_make_block_n593_arg\(34);
                        if \$v6227\(0) = '1' then
                          \$v6226\ := \$ram_ptr_take\;
                          if \$v6226\(0) = '1' then
                            state_var6948 <= q_wait6225;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14023_sp\ & X"000" & X"1")));
                            state_var6948 <= pause_getI6223;
                          end if;
                        else
                          \$14024_sp\ := \$14023_sp\;
                          \$12262_make_block_n593_result\ := \$12262_make_block_n593_arg\(0 to 15) & \$14018\(64 to 95) & \$14024_sp\ & \$14018\(32 to 63) & \$12262_make_block_n593_arg\(148 to 155) & \$12262_make_block_n593_arg\(156 to 171) & \$12262_make_block_n593_arg\(114 to 115);
                          result5987 := \$12262_make_block_n593_result\;
                          rdy5988 := eclat_true;
                          state_var6948 <= compute5989;
                        end if;
                      when pause_setII6238 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6236\ := ""&\$12262_make_block_n593_arg\(33);
                        if \$v6236\(0) = '1' then
                          \$v6235\ := \$ram_ptr_take\;
                          if \$v6235\(0) = '1' then
                            state_var6948 <= q_wait6234;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12262_make_block_n593_arg\(16 to 31) & X"000" & X"1")));
                            state_var6948 <= pause_getI6232;
                          end if;
                        else
                          \$14023_sp\ := \$12262_make_block_n593_arg\(16 to 31);
                          \$v6227\ := ""&\$12262_make_block_n593_arg\(34);
                          if \$v6227\(0) = '1' then
                            \$v6226\ := \$ram_ptr_take\;
                            if \$v6226\(0) = '1' then
                              state_var6948 <= q_wait6225;
                            else
                              \$ram_ptr_take\(0) := '1';
                              \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14023_sp\ & X"000" & X"1")));
                              state_var6948 <= pause_getI6223;
                            end if;
                          else
                            \$14024_sp\ := \$14023_sp\;
                            \$12262_make_block_n593_result\ := \$12262_make_block_n593_arg\(0 to 15) & \$14018\(64 to 95) & \$14024_sp\ & \$14018\(32 to 63) & \$12262_make_block_n593_arg\(148 to 155) & \$12262_make_block_n593_arg\(156 to 171) & \$12262_make_block_n593_arg\(114 to 115);
                            result5987 := \$12262_make_block_n593_result\;
                            rdy5988 := eclat_true;
                            state_var6948 <= compute5989;
                          end if;
                        end if;
                      when pause_setII6280 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(16 to 47) & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6284 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & \$12212\(16 to 47) & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6292 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6290\ := \$ram_ptr_take\;
                        if \$v6290\(0) = '1' then
                          state_var6948 <= q_wait6289;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6287;
                        end if;
                      when pause_setII6300 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6298\ := \$ram_ptr_take\;
                        if \$v6298\(0) = '1' then
                          state_var6948 <= q_wait6297;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"2") & X"000" & X"1")));
                          state_var6948 <= pause_getI6295;
                        end if;
                      when pause_setII6308 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6306\ := \$ram_ptr_take\;
                        if \$v6306\(0) = '1' then
                          state_var6948 <= q_wait6305;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"3") & X"000" & X"1")));
                          state_var6948 <= pause_getI6303;
                        end if;
                      when pause_setII6316 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6314\ := \$ram_ptr_take\;
                        if \$v6314\(0) = '1' then
                          state_var6948 <= q_wait6313;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"4") & X"000" & X"1")));
                          state_var6948 <= pause_getI6311;
                        end if;
                      when pause_setII6324 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6322\ := \$ram_ptr_take\;
                        if \$v6322\(0) = '1' then
                          state_var6948 <= q_wait6321;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"5") & X"000" & X"1")));
                          state_var6948 <= pause_getI6319;
                        end if;
                      when pause_setII6332 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6330\ := \$ram_ptr_take\;
                        if \$v6330\(0) = '1' then
                          state_var6948 <= q_wait6329;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"6") & X"000" & X"1")));
                          state_var6948 <= pause_getI6327;
                        end if;
                      when pause_setII6340 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6338\ := \$ram_ptr_take\;
                        if \$v6338\(0) = '1' then
                          state_var6948 <= q_wait6337;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"7") & X"000" & X"1")));
                          state_var6948 <= pause_getI6335;
                        end if;
                      when pause_setII6364 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6362\ := \$ram_ptr_take\;
                        if \$v6362\(0) = '1' then
                          state_var6948 <= q_wait6361;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"1" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6359;
                        end if;
                      when pause_setII6372 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6370\ := \$ram_ptr_take\;
                        if \$v6370\(0) = '1' then
                          state_var6948 <= q_wait6369;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"2" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6367;
                        end if;
                      when pause_setII6380 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6378\ := \$ram_ptr_take\;
                        if \$v6378\(0) = '1' then
                          state_var6948 <= q_wait6377;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"3" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6375;
                        end if;
                      when pause_setII6388 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6386\ := \$ram_ptr_take\;
                        if \$v6386\(0) = '1' then
                          state_var6948 <= q_wait6385;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"4" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6383;
                        end if;
                      when pause_setII6392 =>
                        \$ram_ptr_take\(0) := '0';
                        \$13561_loop_push604_arg\ := eclat_add(\$13561_loop_push604_arg\(0 to 15) & X"000" & X"1") & eclat_add(\$13561_loop_push604_arg\(16 to 23) & "00000001") & \$13561_loop_push604_arg\(24 to 55) & \$13561_loop_push604_arg\(56 to 63);
                        state_var6948 <= \$13561_loop_push604\;
                      when pause_setII6409 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & eclat_sub(X"000" & X"0" & X"000" & X"2") & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                        state_var6948 <= \$12258_offsetclosure_n586\;
                      when pause_setII6413 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"0" & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                        state_var6948 <= \$12258_offsetclosure_n586\;
                      when pause_setII6417 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"2" & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                        state_var6948 <= \$12258_offsetclosure_n586\;
                      when pause_setII6421 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12262_make_block_n593_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & eclat_false & eclat_false & eclat_false & "000"& X"000000" & X"0" & X"000" & X"0" & \$12212\(16 to 47) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                        state_var6948 <= \$12262_make_block_n593\;
                      when pause_setII6441 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6449 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6457 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6465 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6485 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6505 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6537 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"0" & eclat_true & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6541 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"1" & eclat_true & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6545 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"2" & eclat_true & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6549 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"1") & "000"& X"000000" & X"3" & eclat_true & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6561 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6559\ := \$ram_ptr_take\;
                        if \$v6559\(0) = '1' then
                          state_var6948 <= q_wait6558;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6556;
                        end if;
                      when pause_setII6565 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & "000"& X"000000" & X"1" & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6577 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6575\ := \$ram_ptr_take\;
                        if \$v6575\(0) = '1' then
                          state_var6948 <= q_wait6574;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(eclat_resize(\$12268_argument1\,16) & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6572;
                        end if;
                      when pause_setII6581 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(16 to 47) & eclat_add(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6585 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6583\ := \$ram_ptr_take\;
                        if \$v6583\(0) = '1' then
                          state_var6948 <= q_wait6582;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6580;
                        end if;
                      when pause_setII6589 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6587\ := \$ram_ptr_take\;
                        if \$v6587\(0) = '1' then
                          state_var6948 <= q_wait6586;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$12212\(48 to 63) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6584;
                        end if;
                      when pause_setII6614 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12804_w603_arg\ := eclat_add(\$12804_w603_arg\(0 to 7) & "00000001") & eclat_sub(\$12804_w603_arg\(8 to 23) & X"000" & X"1") & \$12804_w603_arg\(24 to 31) & \$12804_w603_arg\(32 to 63);
                        state_var6948 <= \$12804_w603\;
                      when pause_setII6635 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12804_w603_arg\ := "00000000" & \$12212\(48 to 63) & \$12212\(96 to 103) & \$12792\(64 to 95);
                        state_var6948 <= \$12804_w603\;
                      when pause_setII6639 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6637\ := \$ram_ptr_take\;
                        if \$v6637\(0) = '1' then
                          state_var6948 <= q_wait6636;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12792\(64 to 94),16) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12792\(32 to 63);
                          state_var6948 <= pause_setI6634;
                        end if;
                      when pause_setII6644 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12258_offsetclosure_n586_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16) & \$12212\(64 to 119) & \$12212\(120 to 121) & \$12212\(64 to 95);
                        state_var6948 <= \$12258_offsetclosure_n586\;
                      when pause_setII6656 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6654\ := \$ram_ptr_take\;
                        if \$v6654\(0) = '1' then
                          state_var6948 <= q_wait6653;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                          state_var6948 <= pause_getI6651;
                        end if;
                      when pause_setII6660 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & "000"& X"000000" & X"1" & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6664 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12262_make_block_n593_arg\ := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & eclat_false & eclat_false & eclat_false & \$12268_argument1\ & X"000" & X"0" & \$12212\(16 to 47) & \$12212\(120 to 121) & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119);
                        state_var6948 <= \$12262_make_block_n593\;
                      when pause_setII6672 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & "000"& X"000000" & X"1" & eclat_true & eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6689 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12212\(16 to 47) & eclat_add(eclat_add(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & eclat_add(eclat_add(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6693 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6691\ := \$ram_ptr_take\;
                        if \$v6691\(0) = '1' then
                          state_var6948 <= q_wait6690;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6688;
                        end if;
                      when pause_setII6697 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6695\ := \$ram_ptr_take\;
                        if \$v6695\(0) = '1' then
                          state_var6948 <= q_wait6694;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(\$12212\(104 to 119),31) & eclat_true;
                          state_var6948 <= pause_setI6692;
                        end if;
                      when pause_setII6701 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6699\ := \$ram_ptr_take\;
                        if \$v6699\(0) = '1' then
                          state_var6948 <= q_wait6698;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$12212\(48 to 63) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6696;
                        end if;
                      when pause_setII6710 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6708\ := \$12268_argument1\;
                        case \$v6708\ is
                        when "000"& X"000000" & X"0" =>
                          eclat_print_string(of_string("======> "));
                          
                          eclat_print_int(\$12212\(16 to 46));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$13045\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6707\ := \$ram_ptr_take\;
                          if \$v6707\(0) = '1' then
                            state_var6948 <= q_wait6706;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13045\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6704;
                          end if;
                        when others =>
                          eclat_print_string(of_string("unknown primitive"));
                          
                          \$13045\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6707\ := \$ram_ptr_take\;
                          if \$v6707\(0) = '1' then
                            state_var6948 <= q_wait6706;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13045\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6704;
                          end if;
                        end case;
                      when pause_setII6719 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6717\ := \$12268_argument1\;
                        case \$v6717\ is
                        when "000"& X"000000" & X"0" =>
                          eclat_print_string(of_string("======> "));
                          
                          eclat_print_int(\$12212\(16 to 46));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$13074\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6716\ := \$ram_ptr_take\;
                          if \$v6716\(0) = '1' then
                            state_var6948 <= q_wait6715;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13074\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6713;
                          end if;
                        when others =>
                          eclat_print_string(of_string("unknown primitive"));
                          
                          \$13074\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6716\ := \$ram_ptr_take\;
                          if \$v6716\(0) = '1' then
                            state_var6948 <= q_wait6715;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13074\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6713;
                          end if;
                        end case;
                      when pause_setII6732 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6730\ := \$12268_argument1\;
                        case \$v6730\ is
                        when "000"& X"000000" & X"0" =>
                          eclat_print_string(of_string("======> "));
                          
                          eclat_print_int(\$12212\(16 to 46));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$13110\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6729\ := \$ram_ptr_take\;
                          if \$v6729\(0) = '1' then
                            state_var6948 <= q_wait6728;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13110\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6726;
                          end if;
                        when others =>
                          eclat_print_string(of_string("unknown primitive"));
                          
                          \$13110\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6729\ := \$ram_ptr_take\;
                          if \$v6729\(0) = '1' then
                            state_var6948 <= q_wait6728;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13110\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6726;
                          end if;
                        end case;
                      when pause_setII6749 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6747\ := \$12268_argument1\;
                        case \$v6747\ is
                        when "000"& X"000000" & X"0" =>
                          eclat_print_string(of_string("======> "));
                          
                          eclat_print_int(\$12212\(16 to 46));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$13156\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6746\ := \$ram_ptr_take\;
                          if \$v6746\(0) = '1' then
                            state_var6948 <= q_wait6745;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13156\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6743;
                          end if;
                        when others =>
                          eclat_print_string(of_string("unknown primitive"));
                          
                          \$13156\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6746\ := \$ram_ptr_take\;
                          if \$v6746\(0) = '1' then
                            state_var6948 <= q_wait6745;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13156\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6743;
                          end if;
                        end case;
                      when pause_setII6770 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6768\ := \$12268_argument1\;
                        case \$v6768\ is
                        when "000"& X"000000" & X"0" =>
                          eclat_print_string(of_string("======> "));
                          
                          eclat_print_int(\$12212\(16 to 46));
                          
                          eclat_print_newline(eclat_unit);
                          
                          \$13214\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6767\ := \$ram_ptr_take\;
                          if \$v6767\(0) = '1' then
                            state_var6948 <= q_wait6766;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13214\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6764;
                          end if;
                        when others =>
                          eclat_print_string(of_string("unknown primitive"));
                          
                          \$13214\ := "000"& X"000000" & X"1" & eclat_true & \$12212\(0 to 15) & \$12212\(16 to 47) & eclat_add(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & \$12212\(64 to 95) & \$12212\(96 to 103) & \$12212\(104 to 119) & \$12212\(120 to 121);
                          \$v6767\ := \$ram_ptr_take\;
                          if \$v6767\(0) = '1' then
                            state_var6948 <= q_wait6766;
                          else
                            \$ram_ptr_take\(0) := '1';
                            \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13214\(80 to 95) & X"000" & X"1")));
                            state_var6948 <= pause_getI6764;
                          end if;
                        end case;
                      when pause_setII6790 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12268_argument1\ & eclat_true & eclat_add(\$12212\(48 to 63) & X"000" & X"1") & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6794 =>
                        \$ram_ptr_take\(0) := '0';
                        result5987 := eclat_add(\$12212\(0 to 15) & X"000" & X"2") & "000"& X"000000" & X"1" & eclat_true & \$12212\(48 to 63) & \$12212\(64 to 119) & \$12212\(120 to 121);
                        rdy5988 := eclat_true;
                        state_var6948 <= compute5989;
                      when pause_setII6802 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12443_w600_arg\ := eclat_add(\$12443_w600_arg\(0 to 15) & X"000" & X"1") & \$12443_w600_arg\(16 to 31) & \$12443_w600_arg\(32 to 47) & \$12443_w600_arg\(48 to 63);
                        state_var6948 <= \$12443_w600\;
                      when pause_setII6815 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12496_fill601_arg\ := eclat_add(\$12496_fill601_arg\(0 to 15) & X"000" & X"1") & eclat_sub(\$12496_fill601_arg\(16 to 31) & X"000" & X"1") & \$12496_fill601_arg\(32 to 47) & \$12496_fill601_arg\(48 to 79);
                        state_var6948 <= \$12496_fill601\;
                      when pause_setII6824 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12496_fill601_arg\ := X"000" & X"1" & \$12487_sp\ & eclat_resize(\$12268_argument1\,16) & \$12488\(64 to 95);
                        state_var6948 <= \$12496_fill601\;
                      when pause_setII6828 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12487_sp\ := eclat_add(\$12212\(48 to 63) & X"000" & X"1");
                        \$12221_make_block531_id\ := "000001000111";
                        \$12221_make_block531_arg\ := \$12487_sp\ & \$12212\(16 to 47) & \$12212\(64 to 95) & "11110111" & eclat_add(eclat_resize(\$12268_argument1\,16) & X"000" & X"1");
                        state_var6948 <= \$12221_make_block531\;
                      when pause_setII6849 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6847\ := \$ram_ptr_take\;
                        if \$v6847\(0) = '1' then
                          state_var6948 <= q_wait6846;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                          state_var6948 <= pause_getI6844;
                        end if;
                      when pause_setII6853 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12586_fill602_arg\ := eclat_add(\$12586_fill602_arg\(0 to 15) & X"000" & X"1") & eclat_sub(\$12586_fill602_arg\(16 to 31) & X"000" & X"1") & \$12586_fill602_arg\(32 to 47) & \$12586_fill602_arg\(48 to 79);
                        state_var6948 <= \$12586_fill602\;
                      when pause_setII6862 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12586_fill602_arg\ := X"000" & X"1" & \$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16) & \$12578\(64 to 95);
                        state_var6948 <= \$12586_fill602\;
                      when pause_setII6866 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12287_w0597_arg\ := eclat_add(\$12287_w0597_arg\(0 to 15) & X"000" & X"1") & eclat_sub(\$12287_w0597_arg\(16 to 31) & X"000" & X"1") & \$12287_w0597_arg\(32 to 47) & \$12287_w0597_arg\(48 to 63) & \$12287_w0597_arg\(64 to 95);
                        state_var6948 <= \$12287_w0597\;
                      when pause_setII6875 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12289_w1598_arg\ := eclat_add(\$12289_w1598_arg\(0 to 15) & X"000" & X"1") & \$12289_w1598_arg\(16 to 31) & \$12289_w1598_arg\(32 to 47) & \$12289_w1598_arg\(48 to 79);
                        state_var6948 <= \$12289_w1598\;
                      when pause_setII6883 =>
                        \$ram_ptr_take\(0) := '0';
                        \$v6881\ := \$code_ptr_take\;
                        if \$v6881\(0) = '1' then
                          state_var6948 <= q_wait6880;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(\$12289_w1598_arg\(16 to 31) & X"000" & X"3") & \$12289_w1598_arg\(0 to 15))));
                          state_var6948 <= pause_getI6878;
                        end if;
                      when pause_setII6888 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12292_w3599_arg\ := eclat_add(\$12292_w3599_arg\(0 to 15) & X"000" & X"1") & eclat_add(\$12292_w3599_arg\(16 to 31) & X"000" & X"1") & \$12292_w3599_arg\(32 to 47) & \$12292_w3599_arg\(48 to 79);
                        state_var6948 <= \$12292_w3599\;
                      when pause_setII6893 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12292_w3599_arg\ := X"000" & X"1" & eclat_add(\$12288_sp\ & X"000" & X"1") & eclat_resize(\$12268_argument1\,16) & \$12279\(64 to 95);
                        state_var6948 <= \$12292_w3599\;
                      when pause_setII6897 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12287_w0597_arg\ := X"000" & X"0" & \$12278_sp\ & eclat_resize(\$12268_argument1\,16) & eclat_resize(\$12270_argument2\,16) & \$12279\(64 to 95);
                        state_var6948 <= \$12287_w0597\;
                      when pause_setII6901 =>
                        \$ram_ptr_take\(0) := '0';
                        \$12278_sp\ := eclat_add(\$12212\(48 to 63) & X"000" & X"1");
                        \$12221_make_block531_id\ := "000001010011";
                        \$12221_make_block531_arg\ := \$12278_sp\ & \$12212\(16 to 47) & \$12212\(64 to 95) & "11110111" & eclat_add(eclat_sub(eclat_mult(X"000" & X"2" & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1") & eclat_resize(\$12270_argument2\,16));
                        state_var6948 <= \$12221_make_block531\;
                      when q_wait5992 =>
                        \$v5993\ := \$ram_ptr_take\;
                        if \$v5993\(0) = '1' then
                          state_var6948 <= q_wait5992;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$14272\(64 to 79)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_lor(eclat_lsl(eclat_resize(\$12221_make_block531_arg\(80 to 87),31) & "000"& X"00000" & X"18") & eclat_lsl(eclat_resize(
                          eclat_if(eclat_eq(\$12221_make_block531_arg\(88 to 103) & X"000" & X"0") & X"000" & X"1" & \$12221_make_block531_arg\(88 to 103)),31) & "000"& X"000000" & X"2")) & eclat_true;
                          state_var6948 <= pause_setI5990;
                        end if;
                      when q_wait6159 =>
                        \$v6160\ := \$ram_ptr_take\;
                        if \$v6160\(0) = '1' then
                          state_var6948 <= q_wait6159;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12257_apply585_arg\(60 to 90),16) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6157;
                        end if;
                      when q_wait6163 =>
                        \$v6164\ := \$ram_ptr_take\;
                        if \$v6164\(0) = '1' then
                          state_var6948 <= q_wait6163;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$14213_sp\));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14202\(0 to 31);
                          state_var6948 <= pause_setI6161;
                        end if;
                      when q_wait6168 =>
                        \$v6169\ := \$ram_ptr_take\;
                        if \$v6169\(0) = '1' then
                          state_var6948 <= q_wait6168;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$14212_sp\));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14205\(0 to 31);
                          state_var6948 <= pause_setI6166;
                        end if;
                      when q_wait6173 =>
                        \$v6174\ := \$ram_ptr_take\;
                        if \$v6174\(0) = '1' then
                          state_var6948 <= q_wait6173;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$14211_sp\));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14208\(0 to 31);
                          state_var6948 <= pause_setI6171;
                        end if;
                      when q_wait6178 =>
                        \$v6179\ := \$ram_ptr_take\;
                        if \$v6179\(0) = '1' then
                          state_var6948 <= q_wait6178;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(\$14208\(32 to 47) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(\$12257_apply585_arg\(44 to 59) & X"000" & X"1"),31) & eclat_true;
                          state_var6948 <= pause_setI6176;
                        end if;
                      when q_wait6182 =>
                        \$v6183\ := \$ram_ptr_take\;
                        if \$v6183\(0) = '1' then
                          state_var6948 <= q_wait6182;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$14208\(32 to 47) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12257_apply585_arg\(110 to 141);
                          state_var6948 <= pause_setI6180;
                        end if;
                      when q_wait6186 =>
                        \$v6187\ := \$ram_ptr_take\;
                        if \$v6187\(0) = '1' then
                          state_var6948 <= q_wait6186;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$14208\(32 to 47)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(\$12257_apply585_arg\(142 to 149),31) & eclat_true;
                          state_var6948 <= pause_setI6184;
                        end if;
                      when q_wait6191 =>
                        \$v6192\ := \$ram_ptr_take\;
                        if \$v6192\(0) = '1' then
                          state_var6948 <= q_wait6191;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14205\(32 to 47) & X"000" & X"1")));
                          state_var6948 <= pause_getI6189;
                        end if;
                      when q_wait6196 =>
                        \$v6197\ := \$ram_ptr_take\;
                        if \$v6197\(0) = '1' then
                          state_var6948 <= q_wait6196;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14202\(32 to 47) & X"000" & X"1")));
                          state_var6948 <= pause_getI6194;
                        end if;
                      when q_wait6201 =>
                        \$v6202\ := \$ram_ptr_take\;
                        if \$v6202\(0) = '1' then
                          state_var6948 <= q_wait6201;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12257_apply585_arg\(92 to 107) & X"000" & X"1")));
                          state_var6948 <= pause_getI6199;
                        end if;
                      when q_wait6212 =>
                        \$v6213\ := \$ram_ptr_take\;
                        if \$v6213\(0) = '1' then
                          state_var6948 <= q_wait6212;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12259_binop_int590_arg\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6210;
                        end if;
                      when q_wait6217 =>
                        \$v6218\ := \$ram_ptr_take\;
                        if \$v6218\(0) = '1' then
                          state_var6948 <= q_wait6217;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12261_binop_compare592_arg\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6215;
                        end if;
                      when q_wait6221 =>
                        \$v6222\ := \$ram_ptr_take\;
                        if \$v6222\(0) = '1' then
                          state_var6948 <= q_wait6221;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$14018\(64 to 94),16) & X"000" & X"2") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14025_v\;
                          state_var6948 <= pause_setI6219;
                        end if;
                      when q_wait6225 =>
                        \$v6226\ := \$ram_ptr_take\;
                        if \$v6226\(0) = '1' then
                          state_var6948 <= q_wait6225;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$14023_sp\ & X"000" & X"1")));
                          state_var6948 <= pause_getI6223;
                        end if;
                      when q_wait6230 =>
                        \$v6231\ := \$ram_ptr_take\;
                        if \$v6231\(0) = '1' then
                          state_var6948 <= q_wait6230;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$14018\(64 to 94),16) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14036_v\;
                          state_var6948 <= pause_setI6228;
                        end if;
                      when q_wait6234 =>
                        \$v6235\ := \$ram_ptr_take\;
                        if \$v6235\(0) = '1' then
                          state_var6948 <= q_wait6234;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12262_make_block_n593_arg\(16 to 31) & X"000" & X"1")));
                          state_var6948 <= pause_getI6232;
                        end if;
                      when q_wait6239 =>
                        \$v6240\ := \$ram_ptr_take\;
                        if \$v6240\(0) = '1' then
                          state_var6948 <= q_wait6239;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$14018\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$14018\(0 to 31);
                          state_var6948 <= pause_setI6237;
                        end if;
                      when q_wait6244 =>
                        \$v6245\ := \$code_ptr_take\;
                        if \$v6245\(0) = '1' then
                          state_var6948 <= q_wait6244;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12263_branch_if595_arg\(1 to 16) & X"000" & X"1")));
                          state_var6948 <= pause_getI6242;
                        end if;
                      when q_wait6249 =>
                        \$v6250\ := \$ram_ptr_take\;
                        if \$v6250\(0) = '1' then
                          state_var6948 <= q_wait6249;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6247;
                        end if;
                      when q_wait6253 =>
                        \$v6254\ := \$ram_ptr_take\;
                        if \$v6254\(0) = '1' then
                          state_var6948 <= q_wait6253;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6251;
                        end if;
                      when q_wait6257 =>
                        \$v6258\ := \$ram_ptr_take\;
                        if \$v6258\(0) = '1' then
                          state_var6948 <= q_wait6257;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"2") & X"000" & X"1")));
                          state_var6948 <= pause_getI6255;
                        end if;
                      when q_wait6261 =>
                        \$v6262\ := \$ram_ptr_take\;
                        if \$v6262\(0) = '1' then
                          state_var6948 <= q_wait6261;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"3") & X"000" & X"1")));
                          state_var6948 <= pause_getI6259;
                        end if;
                      when q_wait6265 =>
                        \$v6266\ := \$ram_ptr_take\;
                        if \$v6266\(0) = '1' then
                          state_var6948 <= q_wait6265;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"4") & X"000" & X"1")));
                          state_var6948 <= pause_getI6263;
                        end if;
                      when q_wait6269 =>
                        \$v6270\ := \$ram_ptr_take\;
                        if \$v6270\(0) = '1' then
                          state_var6948 <= q_wait6269;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"5") & X"000" & X"1")));
                          state_var6948 <= pause_getI6267;
                        end if;
                      when q_wait6273 =>
                        \$v6274\ := \$ram_ptr_take\;
                        if \$v6274\(0) = '1' then
                          state_var6948 <= q_wait6273;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"6") & X"000" & X"1")));
                          state_var6948 <= pause_getI6271;
                        end if;
                      when q_wait6277 =>
                        \$v6278\ := \$ram_ptr_take\;
                        if \$v6278\(0) = '1' then
                          state_var6948 <= q_wait6277;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"7") & X"000" & X"1")));
                          state_var6948 <= pause_getI6275;
                        end if;
                      when q_wait6281 =>
                        \$v6282\ := \$ram_ptr_take\;
                        if \$v6282\(0) = '1' then
                          state_var6948 <= q_wait6281;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6279;
                        end if;
                      when q_wait6285 =>
                        \$v6286\ := \$ram_ptr_take\;
                        if \$v6286\(0) = '1' then
                          state_var6948 <= q_wait6285;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6283;
                        end if;
                      when q_wait6289 =>
                        \$v6290\ := \$ram_ptr_take\;
                        if \$v6290\(0) = '1' then
                          state_var6948 <= q_wait6289;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6287;
                        end if;
                      when q_wait6293 =>
                        \$v6294\ := \$ram_ptr_take\;
                        if \$v6294\(0) = '1' then
                          state_var6948 <= q_wait6293;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6291;
                        end if;
                      when q_wait6297 =>
                        \$v6298\ := \$ram_ptr_take\;
                        if \$v6298\(0) = '1' then
                          state_var6948 <= q_wait6297;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"2") & X"000" & X"1")));
                          state_var6948 <= pause_getI6295;
                        end if;
                      when q_wait6301 =>
                        \$v6302\ := \$ram_ptr_take\;
                        if \$v6302\(0) = '1' then
                          state_var6948 <= q_wait6301;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6299;
                        end if;
                      when q_wait6305 =>
                        \$v6306\ := \$ram_ptr_take\;
                        if \$v6306\(0) = '1' then
                          state_var6948 <= q_wait6305;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"3") & X"000" & X"1")));
                          state_var6948 <= pause_getI6303;
                        end if;
                      when q_wait6309 =>
                        \$v6310\ := \$ram_ptr_take\;
                        if \$v6310\(0) = '1' then
                          state_var6948 <= q_wait6309;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6307;
                        end if;
                      when q_wait6313 =>
                        \$v6314\ := \$ram_ptr_take\;
                        if \$v6314\(0) = '1' then
                          state_var6948 <= q_wait6313;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"4") & X"000" & X"1")));
                          state_var6948 <= pause_getI6311;
                        end if;
                      when q_wait6317 =>
                        \$v6318\ := \$ram_ptr_take\;
                        if \$v6318\(0) = '1' then
                          state_var6948 <= q_wait6317;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6315;
                        end if;
                      when q_wait6321 =>
                        \$v6322\ := \$ram_ptr_take\;
                        if \$v6322\(0) = '1' then
                          state_var6948 <= q_wait6321;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"5") & X"000" & X"1")));
                          state_var6948 <= pause_getI6319;
                        end if;
                      when q_wait6325 =>
                        \$v6326\ := \$ram_ptr_take\;
                        if \$v6326\(0) = '1' then
                          state_var6948 <= q_wait6325;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6323;
                        end if;
                      when q_wait6329 =>
                        \$v6330\ := \$ram_ptr_take\;
                        if \$v6330\(0) = '1' then
                          state_var6948 <= q_wait6329;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"6") & X"000" & X"1")));
                          state_var6948 <= pause_getI6327;
                        end if;
                      when q_wait6333 =>
                        \$v6334\ := \$ram_ptr_take\;
                        if \$v6334\(0) = '1' then
                          state_var6948 <= q_wait6333;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6331;
                        end if;
                      when q_wait6337 =>
                        \$v6338\ := \$ram_ptr_take\;
                        if \$v6338\(0) = '1' then
                          state_var6948 <= q_wait6337;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"7") & X"000" & X"1")));
                          state_var6948 <= pause_getI6335;
                        end if;
                      when q_wait6341 =>
                        \$v6342\ := \$ram_ptr_take\;
                        if \$v6342\(0) = '1' then
                          state_var6948 <= q_wait6341;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6339;
                        end if;
                      when q_wait6345 =>
                        \$v6346\ := \$ram_ptr_take\;
                        if \$v6346\(0) = '1' then
                          state_var6948 <= q_wait6345;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"1" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6343;
                        end if;
                      when q_wait6349 =>
                        \$v6350\ := \$ram_ptr_take\;
                        if \$v6350\(0) = '1' then
                          state_var6948 <= q_wait6349;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"2" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6347;
                        end if;
                      when q_wait6353 =>
                        \$v6354\ := \$ram_ptr_take\;
                        if \$v6354\(0) = '1' then
                          state_var6948 <= q_wait6353;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"3" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6351;
                        end if;
                      when q_wait6357 =>
                        \$v6358\ := \$ram_ptr_take\;
                        if \$v6358\(0) = '1' then
                          state_var6948 <= q_wait6357;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"4" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6355;
                        end if;
                      when q_wait6361 =>
                        \$v6362\ := \$ram_ptr_take\;
                        if \$v6362\(0) = '1' then
                          state_var6948 <= q_wait6361;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"1" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6359;
                        end if;
                      when q_wait6365 =>
                        \$v6366\ := \$ram_ptr_take\;
                        if \$v6366\(0) = '1' then
                          state_var6948 <= q_wait6365;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6363;
                        end if;
                      when q_wait6369 =>
                        \$v6370\ := \$ram_ptr_take\;
                        if \$v6370\(0) = '1' then
                          state_var6948 <= q_wait6369;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"2" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6367;
                        end if;
                      when q_wait6373 =>
                        \$v6374\ := \$ram_ptr_take\;
                        if \$v6374\(0) = '1' then
                          state_var6948 <= q_wait6373;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6371;
                        end if;
                      when q_wait6377 =>
                        \$v6378\ := \$ram_ptr_take\;
                        if \$v6378\(0) = '1' then
                          state_var6948 <= q_wait6377;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"3" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6375;
                        end if;
                      when q_wait6381 =>
                        \$v6382\ := \$ram_ptr_take\;
                        if \$v6382\(0) = '1' then
                          state_var6948 <= q_wait6381;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6379;
                        end if;
                      when q_wait6385 =>
                        \$v6386\ := \$ram_ptr_take\;
                        if \$v6386\(0) = '1' then
                          state_var6948 <= q_wait6385;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(X"000" & X"4" & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6383;
                        end if;
                      when q_wait6389 =>
                        \$v6390\ := \$ram_ptr_take\;
                        if \$v6390\(0) = '1' then
                          state_var6948 <= q_wait6389;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6387;
                        end if;
                      when q_wait6393 =>
                        \$v6394\ := \$ram_ptr_take\;
                        if \$v6394\(0) = '1' then
                          state_var6948 <= q_wait6393;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$13561_loop_push604_arg\(0 to 15)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13611\;
                          state_var6948 <= pause_setI6391;
                        end if;
                      when q_wait6397 =>
                        \$v6398\ := \$ram_ptr_take\;
                        if \$v6398\(0) = '1' then
                          state_var6948 <= q_wait6397;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$13561_loop_push604_arg\(24 to 54),16) & eclat_resize(eclat_add(\$13561_loop_push604_arg\(16 to 23) & "00000010"),16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6395;
                        end if;
                      when q_wait6402 =>
                        \$v6403\ := \$ram_ptr_take\;
                        if \$v6403\(0) = '1' then
                          state_var6948 <= q_wait6402;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6400;
                        end if;
                      when q_wait6406 =>
                        \$v6407\ := \$ram_ptr_take\;
                        if \$v6407\(0) = '1' then
                          state_var6948 <= q_wait6406;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$12212\(64 to 94),16)));
                          state_var6948 <= pause_getI6404;
                        end if;
                      when q_wait6410 =>
                        \$v6411\ := \$ram_ptr_take\;
                        if \$v6411\(0) = '1' then
                          state_var6948 <= q_wait6410;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6408;
                        end if;
                      when q_wait6414 =>
                        \$v6415\ := \$ram_ptr_take\;
                        if \$v6415\(0) = '1' then
                          state_var6948 <= q_wait6414;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6412;
                        end if;
                      when q_wait6418 =>
                        \$v6419\ := \$ram_ptr_take\;
                        if \$v6419\(0) = '1' then
                          state_var6948 <= q_wait6418;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6416;
                        end if;
                      when q_wait6422 =>
                        \$v6423\ := \$ram_ptr_take\;
                        if \$v6423\(0) = '1' then
                          state_var6948 <= q_wait6422;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6420;
                        end if;
                      when q_wait6426 =>
                        \$v6427\ := \$ram_ptr_take\;
                        if \$v6427\(0) = '1' then
                          state_var6948 <= q_wait6426;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6424;
                        end if;
                      when q_wait6430 =>
                        \$v6431\ := \$ram_ptr_take\;
                        if \$v6431\(0) = '1' then
                          state_var6948 <= q_wait6430;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6428;
                        end if;
                      when q_wait6434 =>
                        \$v6435\ := \$ram_ptr_take\;
                        if \$v6435\(0) = '1' then
                          state_var6948 <= q_wait6434;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"2") & X"000" & X"1")));
                          state_var6948 <= pause_getI6432;
                        end if;
                      when q_wait6438 =>
                        \$v6439\ := \$ram_ptr_take\;
                        if \$v6439\(0) = '1' then
                          state_var6948 <= q_wait6438;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"3") & X"000" & X"1")));
                          state_var6948 <= pause_getI6436;
                        end if;
                      when q_wait6442 =>
                        \$v6443\ := \$ram_ptr_take\;
                        if \$v6443\(0) = '1' then
                          state_var6948 <= q_wait6442;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13721_v\;
                          state_var6948 <= pause_setI6440;
                        end if;
                      when q_wait6446 =>
                        \$v6447\ := \$ram_ptr_take\;
                        if \$v6447\(0) = '1' then
                          state_var6948 <= q_wait6446;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6444;
                        end if;
                      when q_wait6450 =>
                        \$v6451\ := \$ram_ptr_take\;
                        if \$v6451\(0) = '1' then
                          state_var6948 <= q_wait6450;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13734_v\;
                          state_var6948 <= pause_setI6448;
                        end if;
                      when q_wait6454 =>
                        \$v6455\ := \$ram_ptr_take\;
                        if \$v6455\(0) = '1' then
                          state_var6948 <= q_wait6454;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6452;
                        end if;
                      when q_wait6458 =>
                        \$v6459\ := \$ram_ptr_take\;
                        if \$v6459\(0) = '1' then
                          state_var6948 <= q_wait6458;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"2") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13747_v\;
                          state_var6948 <= pause_setI6456;
                        end if;
                      when q_wait6462 =>
                        \$v6463\ := \$ram_ptr_take\;
                        if \$v6463\(0) = '1' then
                          state_var6948 <= q_wait6462;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6460;
                        end if;
                      when q_wait6466 =>
                        \$v6467\ := \$ram_ptr_take\;
                        if \$v6467\(0) = '1' then
                          state_var6948 <= q_wait6466;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"3") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13760_v\;
                          state_var6948 <= pause_setI6464;
                        end if;
                      when q_wait6470 =>
                        \$v6471\ := \$ram_ptr_take\;
                        if \$v6471\(0) = '1' then
                          state_var6948 <= q_wait6470;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6468;
                        end if;
                      when q_wait6474 =>
                        \$v6475\ := \$ram_ptr_take\;
                        if \$v6475\(0) = '1' then
                          state_var6948 <= q_wait6474;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$12212\(16 to 46),16)));
                          state_var6948 <= pause_getI6472;
                        end if;
                      when q_wait6478 =>
                        \$v6479\ := \$ram_ptr_take\;
                        if \$v6479\(0) = '1' then
                          state_var6948 <= q_wait6478;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13789_v\(0 to 30),16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6476;
                        end if;
                      when q_wait6482 =>
                        \$v6483\ := \$ram_ptr_take\;
                        if \$v6483\(0) = '1' then
                          state_var6948 <= q_wait6482;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6480;
                        end if;
                      when q_wait6486 =>
                        \$v6487\ := \$ram_ptr_take\;
                        if \$v6487\(0) = '1' then
                          state_var6948 <= q_wait6486;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13806_v\(0 to 30),16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13807_v\;
                          state_var6948 <= pause_setI6484;
                        end if;
                      when q_wait6490 =>
                        \$v6491\ := \$ram_ptr_take\;
                        if \$v6491\(0) = '1' then
                          state_var6948 <= q_wait6490;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6488;
                        end if;
                      when q_wait6494 =>
                        \$v6495\ := \$ram_ptr_take\;
                        if \$v6495\(0) = '1' then
                          state_var6948 <= q_wait6494;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6492;
                        end if;
                      when q_wait6498 =>
                        \$v6499\ := \$ram_ptr_take\;
                        if \$v6499\(0) = '1' then
                          state_var6948 <= q_wait6498;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13829_v\(0 to 30),16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6496;
                        end if;
                      when q_wait6502 =>
                        \$v6503\ := \$ram_ptr_take\;
                        if \$v6503\(0) = '1' then
                          state_var6948 <= q_wait6502;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6500;
                        end if;
                      when q_wait6506 =>
                        \$v6507\ := \$ram_ptr_take\;
                        if \$v6507\(0) = '1' then
                          state_var6948 <= q_wait6506;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$13846_v\(0 to 30),16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$13847_v\;
                          state_var6948 <= pause_setI6504;
                        end if;
                      when q_wait6510 =>
                        \$v6511\ := \$ram_ptr_take\;
                        if \$v6511\(0) = '1' then
                          state_var6948 <= q_wait6510;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6508;
                        end if;
                      when q_wait6514 =>
                        \$v6515\ := \$ram_ptr_take\;
                        if \$v6515\(0) = '1' then
                          state_var6948 <= q_wait6514;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6512;
                        end if;
                      when q_wait6518 =>
                        \$v6519\ := \$ram_ptr_take\;
                        if \$v6519\(0) = '1' then
                          state_var6948 <= q_wait6518;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6516;
                        end if;
                      when q_wait6522 =>
                        \$v6523\ := \$ram_ptr_take\;
                        if \$v6523\(0) = '1' then
                          state_var6948 <= q_wait6522;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(104 to 119) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6520;
                        end if;
                      when q_wait6526 =>
                        \$v6527\ := \$ram_ptr_take\;
                        if \$v6527\(0) = '1' then
                          state_var6948 <= q_wait6526;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(104 to 119) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6524;
                        end if;
                      when q_wait6530 =>
                        \$v6531\ := \$ram_ptr_take\;
                        if \$v6531\(0) = '1' then
                          state_var6948 <= q_wait6530;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(104 to 119) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6528;
                        end if;
                      when q_wait6534 =>
                        \$v6535\ := \$ram_ptr_take\;
                        if \$v6535\(0) = '1' then
                          state_var6948 <= q_wait6534;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(104 to 119) & X"000" & X"1")));
                          state_var6948 <= pause_getI6532;
                        end if;
                      when q_wait6538 =>
                        \$v6539\ := \$ram_ptr_take\;
                        if \$v6539\(0) = '1' then
                          state_var6948 <= q_wait6538;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6536;
                        end if;
                      when q_wait6542 =>
                        \$v6543\ := \$ram_ptr_take\;
                        if \$v6543\(0) = '1' then
                          state_var6948 <= q_wait6542;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6540;
                        end if;
                      when q_wait6546 =>
                        \$v6547\ := \$ram_ptr_take\;
                        if \$v6547\(0) = '1' then
                          state_var6948 <= q_wait6546;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6544;
                        end if;
                      when q_wait6550 =>
                        \$v6551\ := \$ram_ptr_take\;
                        if \$v6551\(0) = '1' then
                          state_var6948 <= q_wait6550;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6548;
                        end if;
                      when q_wait6554 =>
                        \$v6555\ := \$ram_ptr_take\;
                        if \$v6555\(0) = '1' then
                          state_var6948 <= q_wait6554;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6552;
                        end if;
                      when q_wait6558 =>
                        \$v6559\ := \$ram_ptr_take\;
                        if \$v6559\(0) = '1' then
                          state_var6948 <= q_wait6558;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6556;
                        end if;
                      when q_wait6562 =>
                        \$v6563\ := \$ram_ptr_take\;
                        if \$v6563\(0) = '1' then
                          state_var6948 <= q_wait6562;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6560;
                        end if;
                      when q_wait6566 =>
                        \$v6567\ := \$ram_ptr_take\;
                        if \$v6567\(0) = '1' then
                          state_var6948 <= q_wait6566;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16))));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6564;
                        end if;
                      when q_wait6570 =>
                        \$v6571\ := \$ram_ptr_take\;
                        if \$v6571\(0) = '1' then
                          state_var6948 <= q_wait6570;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(eclat_resize(\$12268_argument1\,16) & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6568;
                        end if;
                      when q_wait6574 =>
                        \$v6575\ := \$ram_ptr_take\;
                        if \$v6575\(0) = '1' then
                          state_var6948 <= q_wait6574;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(64 to 94),16) & eclat_sub(eclat_resize(\$12268_argument1\,16) & X"000" & X"1")) & X"000" & X"1")));
                          state_var6948 <= pause_getI6572;
                        end if;
                      when q_wait6578 =>
                        \$v6579\ := \$ram_ptr_take\;
                        if \$v6579\(0) = '1' then
                          state_var6948 <= q_wait6578;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6576;
                        end if;
                      when q_wait6582 =>
                        \$v6583\ := \$ram_ptr_take\;
                        if \$v6583\(0) = '1' then
                          state_var6948 <= q_wait6582;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6580;
                        end if;
                      when q_wait6586 =>
                        \$v6587\ := \$ram_ptr_take\;
                        if \$v6587\(0) = '1' then
                          state_var6948 <= q_wait6586;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$12212\(48 to 63) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6584;
                        end if;
                      when q_wait6590 =>
                        \$v6591\ := \$ram_ptr_take\;
                        if \$v6591\(0) = '1' then
                          state_var6948 <= q_wait6590;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(\$12212\(96 to 103),31) & eclat_true;
                          state_var6948 <= pause_setI6588;
                        end if;
                      when q_wait6594 =>
                        \$v6595\ := \$ram_ptr_take\;
                        if \$v6595\(0) = '1' then
                          state_var6948 <= q_wait6594;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6592;
                        end if;
                      when q_wait6598 =>
                        \$v6599\ := \$ram_ptr_take\;
                        if \$v6599\(0) = '1' then
                          state_var6948 <= q_wait6598;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6596;
                        end if;
                      when q_wait6602 =>
                        \$v6603\ := \$ram_ptr_take\;
                        if \$v6603\(0) = '1' then
                          state_var6948 <= q_wait6602;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6600;
                        end if;
                      when q_wait6606 =>
                        \$v6607\ := \$ram_ptr_take\;
                        if \$v6607\(0) = '1' then
                          state_var6948 <= q_wait6606;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6604;
                        end if;
                      when q_wait6610 =>
                        \$v6611\ := \$ram_ptr_take\;
                        if \$v6611\(0) = '1' then
                          state_var6948 <= q_wait6610;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6608;
                        end if;
                      when q_wait6615 =>
                        \$v6616\ := \$ram_ptr_take\;
                        if \$v6616\(0) = '1' then
                          state_var6948 <= q_wait6615;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12804_w603_arg\(32 to 62),16) & eclat_resize(eclat_add(\$12804_w603_arg\(0 to 7) & "00000010"),16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12842_v\;
                          state_var6948 <= pause_setI6613;
                        end if;
                      when q_wait6619 =>
                        \$v6620\ := \$ram_ptr_take\;
                        if \$v6620\(0) = '1' then
                          state_var6948 <= q_wait6619;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12804_w603_arg\(8 to 23) & X"000" & X"1")));
                          state_var6948 <= pause_getI6617;
                        end if;
                      when q_wait6624 =>
                        \$v6625\ := \$ram_ptr_take\;
                        if \$v6625\(0) = '1' then
                          state_var6948 <= q_wait6624;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12805_sp\ & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6622;
                        end if;
                      when q_wait6628 =>
                        \$v6629\ := \$ram_ptr_take\;
                        if \$v6629\(0) = '1' then
                          state_var6948 <= q_wait6628;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12805_sp\ & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6626;
                        end if;
                      when q_wait6632 =>
                        \$v6633\ := \$ram_ptr_take\;
                        if \$v6633\(0) = '1' then
                          state_var6948 <= q_wait6632;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12805_sp\ & X"000" & X"1")));
                          state_var6948 <= pause_getI6630;
                        end if;
                      when q_wait6636 =>
                        \$v6637\ := \$ram_ptr_take\;
                        if \$v6637\(0) = '1' then
                          state_var6948 <= q_wait6636;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12792\(64 to 94),16) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12792\(32 to 63);
                          state_var6948 <= pause_setI6634;
                        end if;
                      when q_wait6640 =>
                        \$v6641\ := \$ram_ptr_take\;
                        if \$v6641\(0) = '1' then
                          state_var6948 <= q_wait6640;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12792\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_sub(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & X"000" & X"3"),31) & eclat_true;
                          state_var6948 <= pause_setI6638;
                        end if;
                      when q_wait6645 =>
                        \$v6646\ := \$ram_ptr_take\;
                        if \$v6646\(0) = '1' then
                          state_var6948 <= q_wait6645;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6643;
                        end if;
                      when q_wait6649 =>
                        \$v6650\ := \$ram_ptr_take\;
                        if \$v6650\(0) = '1' then
                          state_var6948 <= q_wait6649;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                          state_var6948 <= pause_getI6647;
                        end if;
                      when q_wait6653 =>
                        \$v6654\ := \$ram_ptr_take\;
                        if \$v6654\(0) = '1' then
                          state_var6948 <= q_wait6653;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                          state_var6948 <= pause_getI6651;
                        end if;
                      when q_wait6657 =>
                        \$v6658\ := \$ram_ptr_take\;
                        if \$v6658\(0) = '1' then
                          state_var6948 <= q_wait6657;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6655;
                        end if;
                      when q_wait6661 =>
                        \$v6662\ := \$ram_ptr_take\;
                        if \$v6662\(0) = '1' then
                          state_var6948 <= q_wait6661;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6659;
                        end if;
                      when q_wait6665 =>
                        \$v6666\ := \$ram_ptr_take\;
                        if \$v6666\(0) = '1' then
                          state_var6948 <= q_wait6665;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6663;
                        end if;
                      when q_wait6669 =>
                        \$v6670\ := \$ram_ptr_take\;
                        if \$v6670\(0) = '1' then
                          state_var6948 <= q_wait6669;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6667;
                        end if;
                      when q_wait6673 =>
                        \$v6674\ := \$ram_ptr_take\;
                        if \$v6674\(0) = '1' then
                          state_var6948 <= q_wait6673;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & eclat_resize(\$12268_argument1\,16)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12951_v\;
                          state_var6948 <= pause_setI6671;
                        end if;
                      when q_wait6677 =>
                        \$v6678\ := \$ram_ptr_take\;
                        if \$v6678\(0) = '1' then
                          state_var6948 <= q_wait6677;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6675;
                        end if;
                      when q_wait6681 =>
                        \$v6682\ := \$code_ptr_take\;
                        if \$v6682\(0) = '1' then
                          state_var6948 <= q_wait6681;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & \$12975_ofs\)));
                          state_var6948 <= pause_getI6679;
                        end if;
                      when q_wait6685 =>
                        \$v6686\ := \$ram_ptr_take\;
                        if \$v6686\(0) = '1' then
                          state_var6948 <= q_wait6685;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_resize(\$12212\(16 to 46),16)));
                          state_var6948 <= pause_getI6683;
                        end if;
                      when q_wait6690 =>
                        \$v6691\ := \$ram_ptr_take\;
                        if \$v6691\(0) = '1' then
                          state_var6948 <= q_wait6690;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"1") & eclat_resize(\$12268_argument1\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6688;
                        end if;
                      when q_wait6694 =>
                        \$v6695\ := \$ram_ptr_take\;
                        if \$v6695\(0) = '1' then
                          state_var6948 <= q_wait6694;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(\$12212\(104 to 119),31) & eclat_true;
                          state_var6948 <= pause_setI6692;
                        end if;
                      when q_wait6698 =>
                        \$v6699\ := \$ram_ptr_take\;
                        if \$v6699\(0) = '1' then
                          state_var6948 <= q_wait6698;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(\$12212\(48 to 63) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6696;
                        end if;
                      when q_wait6702 =>
                        \$v6703\ := \$ram_ptr_take\;
                        if \$v6703\(0) = '1' then
                          state_var6948 <= q_wait6702;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(\$12212\(96 to 103),31) & eclat_true;
                          state_var6948 <= pause_setI6700;
                        end if;
                      when q_wait6706 =>
                        \$v6707\ := \$ram_ptr_take\;
                        if \$v6707\(0) = '1' then
                          state_var6948 <= q_wait6706;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13045\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6704;
                        end if;
                      when q_wait6711 =>
                        \$v6712\ := \$ram_ptr_take\;
                        if \$v6712\(0) = '1' then
                          state_var6948 <= q_wait6711;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6709;
                        end if;
                      when q_wait6715 =>
                        \$v6716\ := \$ram_ptr_take\;
                        if \$v6716\(0) = '1' then
                          state_var6948 <= q_wait6715;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13074\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6713;
                        end if;
                      when q_wait6720 =>
                        \$v6721\ := \$ram_ptr_take\;
                        if \$v6721\(0) = '1' then
                          state_var6948 <= q_wait6720;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6718;
                        end if;
                      when q_wait6724 =>
                        \$v6725\ := \$ram_ptr_take\;
                        if \$v6725\(0) = '1' then
                          state_var6948 <= q_wait6724;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6722;
                        end if;
                      when q_wait6728 =>
                        \$v6729\ := \$ram_ptr_take\;
                        if \$v6729\(0) = '1' then
                          state_var6948 <= q_wait6728;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13110\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6726;
                        end if;
                      when q_wait6733 =>
                        \$v6734\ := \$ram_ptr_take\;
                        if \$v6734\(0) = '1' then
                          state_var6948 <= q_wait6733;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6731;
                        end if;
                      when q_wait6737 =>
                        \$v6738\ := \$ram_ptr_take\;
                        if \$v6738\(0) = '1' then
                          state_var6948 <= q_wait6737;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6735;
                        end if;
                      when q_wait6741 =>
                        \$v6742\ := \$ram_ptr_take\;
                        if \$v6742\(0) = '1' then
                          state_var6948 <= q_wait6741;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6739;
                        end if;
                      when q_wait6745 =>
                        \$v6746\ := \$ram_ptr_take\;
                        if \$v6746\(0) = '1' then
                          state_var6948 <= q_wait6745;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13156\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6743;
                        end if;
                      when q_wait6750 =>
                        \$v6751\ := \$ram_ptr_take\;
                        if \$v6751\(0) = '1' then
                          state_var6948 <= q_wait6750;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6748;
                        end if;
                      when q_wait6754 =>
                        \$v6755\ := \$ram_ptr_take\;
                        if \$v6755\(0) = '1' then
                          state_var6948 <= q_wait6754;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6752;
                        end if;
                      when q_wait6758 =>
                        \$v6759\ := \$ram_ptr_take\;
                        if \$v6759\(0) = '1' then
                          state_var6948 <= q_wait6758;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6756;
                        end if;
                      when q_wait6762 =>
                        \$v6763\ := \$ram_ptr_take\;
                        if \$v6763\(0) = '1' then
                          state_var6948 <= q_wait6762;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6760;
                        end if;
                      when q_wait6766 =>
                        \$v6767\ := \$ram_ptr_take\;
                        if \$v6767\(0) = '1' then
                          state_var6948 <= q_wait6766;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$13214\(80 to 95) & X"000" & X"1")));
                          state_var6948 <= pause_getI6764;
                        end if;
                      when q_wait6771 =>
                        \$v6772\ := \$ram_ptr_take\;
                        if \$v6772\(0) = '1' then
                          state_var6948 <= q_wait6771;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(64 to 95);
                          state_var6948 <= pause_setI6769;
                        end if;
                      when q_wait6775 =>
                        \$v6776\ := \$ram_ptr_take\;
                        if \$v6776\(0) = '1' then
                          state_var6948 <= q_wait6775;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6773;
                        end if;
                      when q_wait6779 =>
                        \$v6780\ := \$ram_ptr_take\;
                        if \$v6780\(0) = '1' then
                          state_var6948 <= q_wait6779;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6777;
                        end if;
                      when q_wait6783 =>
                        \$v6784\ := \$ram_ptr_take\;
                        if \$v6784\(0) = '1' then
                          state_var6948 <= q_wait6783;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(eclat_sub(\$12212\(48 to 63) & X"000" & X"1") & X"000" & X"1")));
                          state_var6948 <= pause_getI6781;
                        end if;
                      when q_wait6787 =>
                        \$v6788\ := \$ram_ptr_take\;
                        if \$v6788\(0) = '1' then
                          state_var6948 <= q_wait6787;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12212\(48 to 63) & X"000" & X"1")));
                          state_var6948 <= pause_getI6785;
                        end if;
                      when q_wait6791 =>
                        \$v6792\ := \$ram_ptr_take\;
                        if \$v6792\(0) = '1' then
                          state_var6948 <= q_wait6791;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6789;
                        end if;
                      when q_wait6795 =>
                        \$v6796\ := \$ram_ptr_take\;
                        if \$v6796\(0) = '1' then
                          state_var6948 <= q_wait6795;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_add(\$13300_f0\(0 to 30) & \$12268_argument1\) & eclat_true;
                          state_var6948 <= pause_setI6793;
                        end if;
                      when q_wait6799 =>
                        \$v6800\ := \$ram_ptr_take\;
                        if \$v6800\(0) = '1' then
                          state_var6948 <= q_wait6799;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6797;
                        end if;
                      when q_wait6803 =>
                        \$v6804\ := \$ram_ptr_take\;
                        if \$v6804\(0) = '1' then
                          state_var6948 <= q_wait6803;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_sub(eclat_sub(eclat_sub(\$12443_w600_arg\(16 to 31) & \$12443_w600_arg\(32 to 47)) & \$12443_w600_arg\(48 to 63)) & \$12443_w600_arg\(0 to 15))));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12477\;
                          state_var6948 <= pause_setI6801;
                        end if;
                      when q_wait6807 =>
                        \$v6808\ := \$ram_ptr_take\;
                        if \$v6808\(0) = '1' then
                          state_var6948 <= q_wait6807;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12443_w600_arg\(16 to 31) & \$12443_w600_arg\(0 to 15))));
                          state_var6948 <= pause_getI6805;
                        end if;
                      when q_wait6812 =>
                        \$v6813\ := \$ram_ptr_take\;
                        if \$v6813\(0) = '1' then
                          state_var6948 <= q_wait6812;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12212\(16 to 46),16) & X"000" & X"0") & X"000" & X"1")));
                          state_var6948 <= pause_getI6810;
                        end if;
                      when q_wait6816 =>
                        \$v6817\ := \$ram_ptr_take\;
                        if \$v6817\(0) = '1' then
                          state_var6948 <= q_wait6816;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12496_fill601_arg\(48 to 78),16) & \$12496_fill601_arg\(0 to 15)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12512_v\;
                          state_var6948 <= pause_setI6814;
                        end if;
                      when q_wait6820 =>
                        \$v6821\ := \$ram_ptr_take\;
                        if \$v6821\(0) = '1' then
                          state_var6948 <= q_wait6820;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12496_fill601_arg\(16 to 31) & X"000" & X"1")));
                          state_var6948 <= pause_getI6818;
                        end if;
                      when q_wait6825 =>
                        \$v6826\ := \$ram_ptr_take\;
                        if \$v6826\(0) = '1' then
                          state_var6948 <= q_wait6825;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12488\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"2") & eclat_resize(\$12270_argument2\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6823;
                        end if;
                      when q_wait6829 =>
                        \$v6830\ := \$ram_ptr_take\;
                        if \$v6830\(0) = '1' then
                          state_var6948 <= q_wait6829;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6827;
                        end if;
                      when q_wait6834 =>
                        \$v6835\ := \$ram_ptr_take\;
                        if \$v6835\(0) = '1' then
                          state_var6948 <= q_wait6834;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12546\(0 to 30),16) & eclat_resize(\$12270_argument2\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6832;
                        end if;
                      when q_wait6838 =>
                        \$v6839\ := \$ram_ptr_take\;
                        if \$v6839\(0) = '1' then
                          state_var6948 <= q_wait6838;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                          state_var6948 <= pause_getI6836;
                        end if;
                      when q_wait6842 =>
                        \$v6843\ := \$ram_ptr_take\;
                        if \$v6843\(0) = '1' then
                          state_var6948 <= q_wait6842;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12562\(0 to 30),16) & eclat_resize(\$12270_argument2\,16)) & X"000" & X"1")));
                          state_var6948 <= pause_getI6840;
                        end if;
                      when q_wait6846 =>
                        \$v6847\ := \$ram_ptr_take\;
                        if \$v6847\(0) = '1' then
                          state_var6948 <= q_wait6846;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_add(X"3e80" & eclat_resize(\$12268_argument1\,16))));
                          state_var6948 <= pause_getI6844;
                        end if;
                      when q_wait6850 =>
                        \$v6851\ := \$ram_ptr_take\;
                        if \$v6851\(0) = '1' then
                          state_var6948 <= q_wait6850;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6848;
                        end if;
                      when q_wait6854 =>
                        \$v6855\ := \$ram_ptr_take\;
                        if \$v6855\(0) = '1' then
                          state_var6948 <= q_wait6854;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12586_fill602_arg\(48 to 78),16) & \$12586_fill602_arg\(0 to 15)) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12602_v\;
                          state_var6948 <= pause_setI6852;
                        end if;
                      when q_wait6858 =>
                        \$v6859\ := \$ram_ptr_take\;
                        if \$v6859\(0) = '1' then
                          state_var6948 <= q_wait6858;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12586_fill602_arg\(16 to 31) & X"000" & X"1")));
                          state_var6948 <= pause_getI6856;
                        end if;
                      when q_wait6863 =>
                        \$v6864\ := \$ram_ptr_take\;
                        if \$v6864\(0) = '1' then
                          state_var6948 <= q_wait6863;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12578\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12578\(0 to 31);
                          state_var6948 <= pause_setI6861;
                        end if;
                      when q_wait6867 =>
                        \$v6868\ := \$ram_ptr_take\;
                        if \$v6868\(0) = '1' then
                          state_var6948 <= q_wait6867;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12287_w0597_arg\(64 to 94),16) & eclat_sub(eclat_add(\$12287_w0597_arg\(0 to 15) & eclat_mult(X"000" & X"2" & \$12287_w0597_arg\(32 to 47))) & X"000" & X"1")) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12398_v\;
                          state_var6948 <= pause_setI6865;
                        end if;
                      when q_wait6871 =>
                        \$v6872\ := \$ram_ptr_take\;
                        if \$v6872\(0) = '1' then
                          state_var6948 <= q_wait6871;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr\ <= to_integer(unsigned(eclat_sub(\$12287_w0597_arg\(16 to 31) & X"000" & X"1")));
                          state_var6948 <= pause_getI6869;
                        end if;
                      when q_wait6876 =>
                        \$v6877\ := \$ram_ptr_take\;
                        if \$v6877\(0) = '1' then
                          state_var6948 <= q_wait6876;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12289_w1598_arg\(48 to 78),16) & eclat_mult(X"000" & X"2" & \$12289_w1598_arg\(0 to 15))) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12289_w1598_arg\(16 to 31) & X"000" & X"2") & eclat_resize(\$12343\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6874;
                        end if;
                      when q_wait6880 =>
                        \$v6881\ := \$code_ptr_take\;
                        if \$v6881\(0) = '1' then
                          state_var6948 <= q_wait6880;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(eclat_add(\$12289_w1598_arg\(16 to 31) & X"000" & X"3") & \$12289_w1598_arg\(0 to 15))));
                          state_var6948 <= pause_getI6878;
                        end if;
                      when q_wait6884 =>
                        \$v6885\ := \$ram_ptr_take\;
                        if \$v6885\(0) = '1' then
                          state_var6948 <= q_wait6884;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12289_w1598_arg\(48 to 78),16) & eclat_sub(eclat_mult(X"000" & X"2" & \$12289_w1598_arg\(0 to 15)) & X"000" & X"1")) & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_lor(eclat_lsl(eclat_resize("11111001",31) & "000"& X"00000" & X"18") & eclat_lsl(eclat_resize(eclat_mult(X"000" & X"2" & \$12289_w1598_arg\(0 to 15)),31) & "000"& X"000000" & X"2")) & eclat_true;
                          state_var6948 <= pause_setI6882;
                        end if;
                      when q_wait6889 =>
                        \$v6890\ := \$ram_ptr_take\;
                        if \$v6890\(0) = '1' then
                          state_var6948 <= q_wait6889;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12292_w3599_arg\(16 to 31)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_resize(\$12292_w3599_arg\(48 to 78),16) & eclat_mult(X"000" & X"2" & \$12292_w3599_arg\(0 to 15))),31) & eclat_true;
                          state_var6948 <= pause_setI6887;
                        end if;
                      when q_wait6894 =>
                        \$v6895\ := \$ram_ptr_take\;
                        if \$v6895\(0) = '1' then
                          state_var6948 <= q_wait6894;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12288_sp\));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12279\(64 to 95);
                          state_var6948 <= pause_setI6892;
                        end if;
                      when q_wait6898 =>
                        \$v6899\ := \$ram_ptr_take\;
                        if \$v6899\(0) = '1' then
                          state_var6948 <= q_wait6898;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(eclat_add(eclat_add(eclat_resize(\$12279\(64 to 94),16) & X"000" & X"0") & X"000" & X"1")));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= eclat_resize(eclat_add(eclat_add(\$12212\(0 to 15) & X"000" & X"3") & eclat_resize(\$12272_argument3\,16)),31) & eclat_true;
                          state_var6948 <= pause_setI6896;
                        end if;
                      when q_wait6902 =>
                        \$v6903\ := \$ram_ptr_take\;
                        if \$v6903\(0) = '1' then
                          state_var6948 <= q_wait6902;
                        else
                          \$ram_ptr_take\(0) := '1';
                          \$ram_ptr_write\ <= to_integer(unsigned(\$12212\(48 to 63)));
                          \$ram_write_request\ <= '1';
                          \$ram_write\ <= \$12212\(16 to 47);
                          state_var6948 <= pause_setI6900;
                        end if;
                      when q_wait6907 =>
                        \$v6908\ := \$code_ptr_take\;
                        if \$v6908\(0) = '1' then
                          state_var6948 <= q_wait6907;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(\$12212\(0 to 15)));
                          state_var6948 <= pause_getI6905;
                        end if;
                      when q_wait6912 =>
                        \$v6913\ := \$code_ptr_take\;
                        if \$v6913\(0) = '1' then
                          state_var6948 <= q_wait6912;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12212\(0 to 15) & X"000" & X"3")));
                          state_var6948 <= pause_getI6910;
                        end if;
                      when q_wait6917 =>
                        \$v6918\ := \$code_ptr_take\;
                        if \$v6918\(0) = '1' then
                          state_var6948 <= q_wait6917;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12212\(0 to 15) & X"000" & X"2")));
                          state_var6948 <= pause_getI6915;
                        end if;
                      when q_wait6922 =>
                        \$v6923\ := \$code_ptr_take\;
                        if \$v6923\(0) = '1' then
                          state_var6948 <= q_wait6922;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(eclat_add(\$12212\(0 to 15) & X"000" & X"1")));
                          state_var6948 <= pause_getI6920;
                        end if;
                      when q_wait6927 =>
                        \$v6928\ := \$code_ptr_take\;
                        if \$v6928\(0) = '1' then
                          state_var6948 <= q_wait6927;
                        else
                          \$code_ptr_take\(0) := '1';
                          \$code_ptr\ <= to_integer(unsigned(\$12212\(0 to 15)));
                          state_var6948 <= pause_getI6925;
                        end if;
                      when compute5989 =>
                        rdy5988 := eclat_false;
                        eclat_print_string(of_string("pc:"));
                        
                        eclat_print_int(\$12212\(0 to 15));
                        
                        eclat_print_string(of_string("|acc:"));
                        
                        eclat_print_int(\$12212\(16 to 46));
                        
                        eclat_print_string(of_string("<"));
                        
                        \$v6930\ := ""&\$12212\(47);
                        if \$v6930\(0) = '1' then
                          eclat_print_string(of_string("int"));
                          
                          eclat_print_string(of_string(">"));
                          
                          eclat_print_string(of_string("|sp:"));
                          
                          eclat_print_int(\$12212\(48 to 63));
                          
                          eclat_print_string(of_string("|env:"));
                          
                          eclat_print_int(\$12212\(64 to 94));
                          
                          eclat_print_string(of_string("<"));
                          
                          \$v6929\ := ""&\$12212\(95);
                          if \$v6929\(0) = '1' then
                            eclat_print_string(of_string("int"));
                            
                            eclat_print_string(of_string(">"));
                            
                            eclat_print_newline(eclat_unit);
                            
                            assert eclat_lt(\$12212\(0 to 15) & std_logic_vector(to_unsigned(code'length,16))) = eclat_true report "assertion failed" severity error;
                            
                            \$v6928\ := \$code_ptr_take\;
                            if \$v6928\(0) = '1' then
                              state_var6948 <= q_wait6927;
                            else
                              \$code_ptr_take\(0) := '1';
                              \$code_ptr\ <= to_integer(unsigned(\$12212\(0 to 15)));
                              state_var6948 <= pause_getI6925;
                            end if;
                          else
                            eclat_print_string(of_string("ptr"));
                            
                            eclat_print_string(of_string(">"));
                            
                            eclat_print_newline(eclat_unit);
                            
                            assert eclat_lt(\$12212\(0 to 15) & std_logic_vector(to_unsigned(code'length,16))) = eclat_true report "assertion failed" severity error;
                            
                            \$v6928\ := \$code_ptr_take\;
                            if \$v6928\(0) = '1' then
                              state_var6948 <= q_wait6927;
                            else
                              \$code_ptr_take\(0) := '1';
                              \$code_ptr\ <= to_integer(unsigned(\$12212\(0 to 15)));
                              state_var6948 <= pause_getI6925;
                            end if;
                          end if;
                        else
                          eclat_print_string(of_string("ptr"));
                          
                          eclat_print_string(of_string(">"));
                          
                          eclat_print_string(of_string("|sp:"));
                          
                          eclat_print_int(\$12212\(48 to 63));
                          
                          eclat_print_string(of_string("|env:"));
                          
                          eclat_print_int(\$12212\(64 to 94));
                          
                          eclat_print_string(of_string("<"));
                          
                          \$v6929\ := ""&\$12212\(95);
                          if \$v6929\(0) = '1' then
                            eclat_print_string(of_string("int"));
                            
                            eclat_print_string(of_string(">"));
                            
                            eclat_print_newline(eclat_unit);
                            
                            assert eclat_lt(\$12212\(0 to 15) & std_logic_vector(to_unsigned(code'length,16))) = eclat_true report "assertion failed" severity error;
                            
                            \$v6928\ := \$code_ptr_take\;
                            if \$v6928\(0) = '1' then
                              state_var6948 <= q_wait6927;
                            else
                              \$code_ptr_take\(0) := '1';
                              \$code_ptr\ <= to_integer(unsigned(\$12212\(0 to 15)));
                              state_var6948 <= pause_getI6925;
                            end if;
                          else
                            eclat_print_string(of_string("ptr"));
                            
                            eclat_print_string(of_string(">"));
                            
                            eclat_print_newline(eclat_unit);
                            
                            assert eclat_lt(\$12212\(0 to 15) & std_logic_vector(to_unsigned(code'length,16))) = eclat_true report "assertion failed" severity error;
                            
                            \$v6928\ := \$code_ptr_take\;
                            if \$v6928\(0) = '1' then
                              state_var6948 <= q_wait6927;
                            else
                              \$code_ptr_take\(0) := '1';
                              \$code_ptr\ <= to_integer(unsigned(\$12212\(0 to 15)));
                              state_var6948 <= pause_getI6925;
                            end if;
                          end if;
                        end if;
                      end case;
                      \$v6932\ := eclat_not(rdy5988);
                      if \$v6932\(0) = '1' then
                        result5987 := \$12212\(0 to 121);
                      end if;
                      \$12218\ := result5987 & rdy5988;
                      \$12212\ := \$12218\(0 to 121) & ""&\$12218\(122);
                      rdy5985 := eclat_true;
                      state_var6947 <= compute5986;
                    end if;
                  end case;
                  \$12212\ := \$12212\;
                  \$12198\ := \$12212\;
                  \$12190\ := ""&\$12198\(120) & ""&\$12198\(122) & ""&\$12190\(2) & ""&\$12198\(121);
                  rdy5626 := eclat_true;
                  state_var6946 <= compute5627;
                end if;
              end case;
              \$12190\ := \$12190\;
              \$12165\ := \$12190\;
              \$v5625\ := ""&\$12165\(0);
              if \$v5625\(0) = '1' then
                eclat_print_string(of_string("(cy="));
                
                eclat_print_int(\$12164_cy\);
                
                eclat_print_string(of_string(")"));
                
                eclat_print_newline(eclat_unit);
                
                \$v5624\ := eclat_not(rdy5621);
                if \$v5624\(0) = '1' then
                  \$12182\ := X"0000000" & X"0";
                end if;
                case state_var6945 is
                when compute5622 =>
                  rdy5621 := eclat_false;
                  \$12182\ := eclat_if(eclat_eq(\$12182\ & eclat_add(X"00" & X"989680" & X"00" & X"989680")) & X"0000000" & X"0" & eclat_add(\$12182\ & X"0000000" & X"1"));
                  rdy5621 := eclat_true;
                  state_var6945 <= compute5622;
                end case;
                \$12182\ := \$12182\;
                \$12171\ := \$12182\;
                result5618 := ""&\$12165\(0) & eclat_not(""&\$12165\(1)) & eclat_gt(\$12171\ & X"00" & X"989680") & ""&\$12165\(3) & eclat_false & eclat_false & eclat_false & eclat_false & eclat_false & eclat_false & "00000011" & "00000011" & "00000011" & "00000011" & "00000011" & "00000011";
                rdy5619 := eclat_true;
                state <= compute5620;
              else
                \$v5624\ := eclat_not(rdy5621);
                if \$v5624\(0) = '1' then
                  \$12182\ := X"0000000" & X"0";
                end if;
                case state_var6945 is
                when compute5622 =>
                  rdy5621 := eclat_false;
                  \$12182\ := eclat_if(eclat_eq(\$12182\ & eclat_add(X"00" & X"989680" & X"00" & X"989680")) & X"0000000" & X"0" & eclat_add(\$12182\ & X"0000000" & X"1"));
                  rdy5621 := eclat_true;
                  state_var6945 <= compute5622;
                end case;
                \$12182\ := \$12182\;
                \$12171\ := \$12182\;
                result5618 := ""&\$12165\(0) & eclat_not(""&\$12165\(1)) & eclat_gt(\$12171\ & X"00" & X"989680") & ""&\$12165\(3) & eclat_false & eclat_false & eclat_false & eclat_false & eclat_false & eclat_false & "00000011" & "00000011" & "00000011" & "00000011" & "00000011" & "00000011";
                rdy5619 := eclat_true;
                state <= compute5620;
              end if;
            end if;
          end case;
          
          result <= result5618;
          rdy <= rdy5619;
          
        end if;
      end if;
    end if;
  end process;
end architecture;
